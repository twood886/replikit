Attribute VB_Name = "ReplikitApi"
' replikit_excel.bas  (DRAFT skeleton)
'
' Excel VBA client for the replikit Excel API. Orchestrates the two-call
' handshake and drives the Bloomberg Excel add-in (=BDP) for market data.
'
' DEPENDENCIES
'   * VBA-JSON (JsonConverter.bas) imported into this project.
'       https://github.com/VBA-tools/VBA-JSON
'   * Bloomberg Excel add-in installed and connected (=BDP available).
'   * HTTP via late-bound WinHttp (no reference needed).
'
' WORKBOOK LAYOUT (sheet names)
'   Input   B1 username, B2 password, B3 portfolio, B4 flow_to_derived
'           (TRUE/FALSE), B5 server URL (e.g. http://<host>:8000). Trade header row 6
'           (Security | Qty | Swap); trades from row 7 down. Results (holdings +
'           proposed trades + Direct/Swap totals) are written BELOW the trade
'           table on this same sheet, a few rows down.
'   Securities  scratch sheet for =BDP: A=id (hidden), B=bdp_id, C.. = one/field
'
' This is a skeleton: error handling, auth headers, and cell formatting are
' stubbed with TODOs.

Option Explicit

' Server URL comes from the Input!B5 cell ("Server URL"); this is the fallback if
' that cell is blank. Set B5 so you never re-edit VBA when the server moves.
Private Const DEFAULT_API_URL As String = "http://10.119.234.104:8000"
Private Const BDP_TIMEOUT_SEC As Double = 30                  ' max wait for BDP fill
Private Const REQUESTING As String = "Requesting Data"        ' BDP pending marker

' Bearer token from /login, cached for the whole workbook session (EnsureLoggedIn).
Private m_token As String
Private m_tokenExp As Date          ' when the cached token stops being valid

' Sub-phase timing from the last WriteSecuritiesSheet run (diagnostic; surfaced in
' the ProposeTrades timing popup so we can see which phase is slow).
Private m_bdpTiming As String

' ---- entry point -----------------------------------------------------------
Public Sub ProposeTrades()
    Dim t As Double: t = Timer          ' phase timer
    Dim timing As String

    EnsureLoggedIn                  ' B1/B2 -> /login -> bearer token
    timing = timing & Lap("login", t): t = Timer

    Dim portfolio As String, flowToDerived As Boolean
    Dim trades As Object            ' Collection of Dictionaries {security,qty,swap}
    ReadInputs portfolio, flowToDerived, trades
    timing = timing & Lap("read inputs", t): t = Timer

    ' 1) ask the API which securities/fields to price
    Dim reqBody As Object: Set reqBody = New Dictionary
    reqBody("portfolio") = portfolio
    reqBody("flow_to_derived") = flowToDerived
    Set reqBody("trades") = trades          ' object value -> needs Set
    Dim required As Object
    Set required = HttpPostJson(ApiBase() & "/required-inputs", reqBody)
    timing = timing & Lap("required-inputs API", t): t = Timer

    ' 2) lay out the Securities sheet with =BDP formulas and wait for fill
    Dim fields As Object: Set fields = required("fields")        ' Collection
    Dim secs As Object:   Set secs = required("securities")      ' Collection
    WriteSecuritiesSheet secs, fields
    timing = timing & Lap("write BDP formulas", t) & m_bdpTiming: t = Timer

    If Not WaitForBdp("Securities") Then
        MsgBox "Bloomberg data did not finish loading within " & _
               BDP_TIMEOUT_SEC & "s.", vbExclamation
        Exit Sub
    End If
    timing = timing & Lap("bloomberg fill (BDP)", t): t = Timer

    ' 3) read the filled block back and post it for optimization
    Dim marketData As Object: Set marketData = ReadSecuritiesSheet(fields)
    timing = timing & Lap("read securities", t): t = Timer
    Dim tradeBody As Object: Set tradeBody = New Dictionary
    tradeBody("request_id") = required("request_id")
    tradeBody("portfolio") = portfolio
    tradeBody("flow_to_derived") = flowToDerived
    Set tradeBody("trades") = trades            ' object value -> needs Set
    Set tradeBody("market_data") = marketData   ' object value -> needs Set

    Dim resp As Object
    Set resp = HttpPostJson(ApiBase() & "/proposed-trade", tradeBody)
    timing = timing & Lap("proposed-trade API", t): t = Timer

    ' 4) drop results below the inputs (trade table ends at row 6 + #trades)
    WriteResultsSheet resp, 6 + trades.Count + 3
    timing = timing & Lap("write results", t): t = Timer

    MsgBox ResultSummary(resp) & vbCrLf & vbCrLf & "Timing:" & timing, vbInformation
End Sub

' Elapsed since `since`, as a labelled line for the timing report.
Private Function Lap(label As String, since As Double) As String
    Lap = vbCrLf & "  " & label & ": " & Format$(Timer - since, "0.00") & "s"
End Function

' Rebuild the server's in-memory holdings from the database. Assign to a button
' and run after positions update in Supabase (instead of restarting the server).
Public Sub RefreshHoldings()
    EnsureLoggedIn
    Dim resp As Object
    Set resp = HttpPostJson(ApiBase() & "/refresh", New Dictionary)
    MsgBox "Holdings refreshed." & vbCrLf & "As of: " & _
           CStr(resp("data_as_of")) & " UTC", vbInformation
End Sub

' ---- rebalance -------------------------------------------------------------
' Shared flow for the rebalance buttons: price the WHOLE book via BDP (no trade
' input), then call /rebalance. Returns the parsed response.
Private Function RunRebalance() As Object
    EnsureLoggedIn
    Dim ws As Worksheet: Set ws = ThisWorkbook.Worksheets("Input")
    Dim portfolio As String: portfolio = CStr(ws.Range("B3").Value)
    If Len(portfolio) = 0 Then _
        Err.Raise vbObjectError + 3, "RunRebalance", "Enter a base portfolio in B3."

    ' whole-book price scope: /required-inputs with no trades
    Dim reqBody As Object: Set reqBody = New Dictionary
    reqBody("portfolio") = portfolio
    reqBody("flow_to_derived") = True
    Set reqBody("trades") = New Collection
    Dim required As Object
    Set required = HttpPostJson(ApiBase() & "/required-inputs", reqBody)

    WriteSecuritiesSheet required("securities"), required("fields")
    If Not WaitForBdp("Securities") Then _
        Err.Raise vbObjectError + 4, "RunRebalance", "Bloomberg data did not finish loading."

    Dim body As Object: Set body = New Dictionary
    body("portfolio") = portfolio
    Set body("market_data") = ReadSecuritiesSheet(required("fields"))
    Set RunRebalance = HttpPostJson(ApiBase() & "/rebalance", body)
End Function

' Row where a results block starts: a couple rows below the trade-input table.
Private Function ResultsTopRow(ws As Worksheet) As Long
    Dim r As Long: r = 7
    Do While Len(ws.Cells(r, 1).Value) > 0
        r = r + 1
    Loop
    ResultsTopRow = r + 2
End Function

' Write "Holdings as of: ..." beside a section title at topRow.
Private Sub WriteAsOf(ws As Worksheet, topRow As Long, resp As Object)
    Dim asOf As Variant: asOf = resp("data_as_of")
    If Not IsNull(asOf) Then _
        ws.Cells(topRow, 6).Value = "Holdings as of: " & CStr(asOf) & " UTC"
End Sub

' Return the worksheet with this name, creating it if it doesn't exist.
Private Function SheetByName(sheetName As String) As Worksheet
    On Error Resume Next
    Set SheetByName = ThisWorkbook.Worksheets(sheetName)
    On Error GoTo 0
    If SheetByName Is Nothing Then
        Set SheetByName = ThisWorkbook.Worksheets.Add( _
            After:=ThisWorkbook.Worksheets(ThisWorkbook.Worksheets.Count))
        SheetByName.Name = sheetName
    End If
End Function

' Button: full per-SMA rebalance trades, written below the inputs.
Public Sub RebalanceTrades()
    Dim resp As Object: Set resp = RunRebalance()
    Dim ws As Worksheet: Set ws = ThisWorkbook.Worksheets("Input")
    Dim topRow As Long: topRow = ResultsTopRow(ws)

    Application.ScreenUpdating = False
    ws.Range(ws.Cells(topRow, 1), ws.Cells(topRow + 2000, 20)).ClearContents
    ws.Cells(topRow, 1).Value = "Rebalance Trades"
    WriteAsOf ws, topRow, resp

    Dim tr As Object: Set tr = Nothing
    If Not IsNull(resp("rebalance_trades")) Then Set tr = resp("rebalance_trades")
    Dim tEnd As Long
    tEnd = WriteTable(ws, topRow + 1, 1, _
        Array("Portfolio", "Security", "Trade Quantity", "Chg % of NAV", _
              "Current Shares", "Target Shares"), _
        Array("Portfolio", "Security", "TradeQuantity", "TradePctNav", _
              "CurrentShares", "TargetShares"), tr)
    If tEnd >= topRow + 2 Then _
        ws.Range(ws.Cells(topRow + 2, 4), ws.Cells(tEnd, 4)).NumberFormat = "0.000000%"

    Dim totals As Object: Set totals = resp("totals")
    ws.Cells(topRow + 1, 8).Value = "Direct"
    ws.Cells(topRow + 1, 9).Value = totals("Direct")
    ws.Cells(topRow + 2, 8).Value = "Swap"
    ws.Cells(topRow + 2, 9).Value = totals("Swap")

    ws.UsedRange.EntireColumn.AutoFit
    Application.ScreenUpdating = True
    MsgBox ResultSummary(resp), vbInformation
End Sub

' Button: just the top 10 securities that need the most rebalancing, on its own
' "Top Movers" sheet (created if missing). Columns are Security | Base Fund Weight
' | one weight column per SMA (order from the server's top_movers_smas).
Public Sub TopMovers()
    Dim resp As Object: Set resp = RunRebalance()
    Dim ws As Worksheet: Set ws = SheetByName("Top Movers")

    Application.ScreenUpdating = False
    ws.Cells.ClearContents
    On Error Resume Next
    ws.Cells.ClearComments        ' drop stale notes; ClearContents leaves them
    On Error GoTo 0
    Dim nMov As Long: nMov = 0
    On Error Resume Next
    nMov = resp("top_movers").Count      ' actual count the server returned
    On Error GoTo 0
    ws.Range("A1").Value = "Top " & nMov & " Rebalance Movers"
    WriteAsOf ws, 1, resp

    ' SMA column order (server-supplied); each becomes its own weight column.
    Dim smas As Object: Set smas = resp("top_movers_smas")
    Dim nSma As Long: nSma = 0
    If Not smas Is Nothing Then nSma = smas.Count

    ' Header row (row 2): Security, Base Fund Weight, then one per SMA.
    Dim hdrRow As Long: hdrRow = 2
    ws.Cells(hdrRow, 1).Value = "Security"
    ws.Cells(hdrRow, 2).Value = "Base Fund Weight"
    Dim k As Long
    For k = 1 To nSma
        ws.Cells(hdrRow, 2 + k).Value = CStr(smas(k))
    Next k

    ' Data rows. Each SMA cell shows the current weight; a note carries the detail
    ' (current weight, constrained target weight, current shares, shares to trade).
    Dim data As Object: Set data = resp("top_movers")
    Dim r As Long, weights As Object
    Dim lastRow As Long: lastRow = hdrRow
    If Not data Is Nothing Then
        For r = 1 To data.Count
            Dim row As Object: Set row = data(r)
            ws.Cells(hdrRow + r, 1).Value = row("Security")
            ws.Cells(hdrRow + r, 2).Value = row("BaseWeight")
            Set weights = row("SmaWeights")
            For k = 1 To nSma
                Dim nm As String: nm = CStr(smas(k))
                Dim cell As Range: Set cell = ws.Cells(hdrRow + r, 2 + k)
                If weights.Exists(nm) Then
                    Dim d As Object: Set d = weights(nm)
                    cell.Value = NzNum(d("CurrentWeight"))
                    SetSmaComment cell, d
                Else
                    cell.Value = 0
                End If
            Next k
        Next r
        lastRow = hdrRow + data.Count
    End If

    ' Percent-format every weight column (Base Fund + all SMAs).
    If lastRow > hdrRow Then _
        ws.Range(ws.Cells(hdrRow + 1, 2), ws.Cells(lastRow, 2 + nSma)).NumberFormat = "0.000000%"

    ws.UsedRange.EntireColumn.AutoFit
    Application.ScreenUpdating = True
End Sub

' Attach a hover note to a Top Movers SMA cell with the per-SMA detail. Skips
' cells the SMA neither holds nor trades (all-zero), so notes only mark real
' positions. `d` is the SmaWeights entry for one (security, SMA).
Private Sub SetSmaComment(cell As Range, d As Object)
    Dim cw As Double, tw As Double, cs As Double, ts As Double
    cw = NzNum(d("CurrentWeight"))
    tw = NzNum(d("TargetWeight"))
    cs = NzNum(d("CurrentShares"))
    ts = NzNum(d("TradeShares"))
    If cw = 0 And tw = 0 And cs = 0 And ts = 0 Then Exit Sub

    Dim txt As String
    txt = "Current weight: " & Format(cw, "0.0000%") & vbLf & _
          "Target weight: " & Format(tw, "0.0000%") & vbLf & _
          "Current shares: " & Format(cs, "#,##0") & vbLf & _
          "Shares to trade: " & Format(ts, "+#,##0;-#,##0;0")

    On Error Resume Next
    cell.ClearComments
    On Error GoTo 0
    Dim cm As Comment: Set cm = cell.AddComment
    cm.Text Text:=txt
    cm.Shape.TextFrame.AutoSize = True
    cm.Visible = False
End Sub

' JSON null / missing -> 0, else numeric. (BDP/JsonConverter yield Null for null.)
Private Function NzNum(v As Variant) As Double
    If IsNull(v) Then NzNum = 0 Else NzNum = CDbl(v)
End Function

' Completion message; lists any SMAs the server dropped, with the cause.
Private Function ResultSummary(resp As Object) As String
    Dim msg As String: msg = "Done."
    Dim warns As Object
    On Error Resume Next
    Set warns = resp("warnings")
    On Error GoTo 0
    If warns Is Nothing Then ResultSummary = msg: Exit Function
    If warns.Count = 0 Then ResultSummary = msg: Exit Function

    msg = msg & vbCrLf & vbCrLf & warns.Count & " SMA(s) dropped:"
    Dim w As Variant
    For Each w In warns
        msg = msg & vbCrLf & "  - " & CStr(w("portfolio")) & ": " & CStr(w("error"))
    Next w
    ResultSummary = msg
End Function

' ---- Bloomberg BDP ---------------------------------------------------------
' Write id (hidden A), bdp_id (B), then one =BDP(bdp_id, field) column per field.
' Screen updating off + manual calculation is the real win: it stops BDP from
' recalc/fetching on every single cell write. Formulas are written cell-by-cell
' (reliable) while calculation is Manual so the requests queue, then ONE
' ws.Calculate (still Manual, scoped to this sheet) fires them as a single BDP
' batch. We deliberately do NOT use Application.CalculateFull here: that rebuilds
' every formula in every open workbook (Ctrl+Alt+F9), which is what made runs
' slow when other workbooks were open. The WaitForBdp poll loop likewise uses a
' cheap per-sheet ws.Calculate.
Private Sub WriteSecuritiesSheet(secs As Object, fields As Object)
    Dim ws As Worksheet
    Set ws = ThisWorkbook.Worksheets("Securities")
    Dim nFld As Long
    nFld = fields.Count

    m_bdpTiming = ""
    Dim tw As Double: tw = Timer

    Dim calcMode As Long
    calcMode = Application.Calculation
    Application.ScreenUpdating = False
    Application.EnableEvents = False
    Application.Calculation = xlCalculationManual
    On Error GoTo cleanup

    ws.Cells.Clear            ' scratch sheet: full clear gives BDP a clean slate

    ' Header row: id | bdp_id | <field> ... (one call).
    Dim nCol As Long: nCol = 2 + nFld
    Dim hdr() As Variant: ReDim hdr(1 To 1, 1 To nCol)
    hdr(1, 1) = "id"
    hdr(1, 2) = "bdp_id"
    Dim c As Long
    For c = 1 To nFld
        hdr(1, 2 + c) = CStr(fields(c))
    Next c
    ws.Range(ws.Cells(1, 1), ws.Cells(1, nCol)).Value = hdr

    ' Build the whole data block in memory, then write it in ONE range assignment.
    ' Cell-by-cell writes cross the COM boundary per cell and were the bottleneck
    ' (~19s for a whole-book list); a single array assignment is ~instant.
    Dim nRow As Long: nRow = secs.Count
    If nRow > 0 Then
        Dim body() As Variant: ReDim body(1 To nRow, 1 To nCol)
        Dim r As Long, s As Object, bdpId As String
        For r = 1 To nRow
            Set s = secs(r)
            bdpId = s("bdp_id")
            body(r, 1) = s("id")            ' plain strings -> literal values
            body(r, 2) = bdpId
            For c = 1 To nFld
                body(r, 2 + c) = "=BDP(""" & bdpId & """,""" & CStr(fields(c)) & """)"
            Next c
        Next r
        ws.Range(ws.Cells(2, 1), ws.Cells(1 + nRow, nCol)).Formula = body
    End If
    ws.Columns("A").Hidden = True
    m_bdpTiming = vbCrLf & "    write loop: " & Format$(Timer - tw, "0.00") & "s"

cleanup:
    Dim errNum As Long
    Dim errDesc As String
    errNum = Err.Number
    errDesc = Err.Description
    ' Fire the queued BDP requests as one batch for THIS sheet only, while still
    ' in Manual mode, so we never trigger an app-wide recalc of other workbooks.
    Dim tc As Double: tc = Timer
    On Error Resume Next
    ws.Calculate
    On Error GoTo 0
    m_bdpTiming = m_bdpTiming & vbCrLf & "    ws.Calculate: " & Format$(Timer - tc, "0.00") & "s"
    Dim tr As Double: tr = Timer
    Application.Calculation = calcMode
    m_bdpTiming = m_bdpTiming & vbCrLf & "    restore calc: " & Format$(Timer - tr, "0.00") & "s"
    Application.EnableEvents = True
    Application.ScreenUpdating = True
    If errNum <> 0 Then
        MsgBox "WriteSecuritiesSheet error " & errNum & ": " & errDesc, vbCritical
        Err.Raise errNum, "WriteSecuritiesSheet", errDesc
    End If
End Sub

' BDP fills asynchronously: cells show "#N/A Requesting Data..." until ready.
' Poll the field block until no cell is still requesting (or timeout).
Private Function WaitForBdp(sheetName As String) As Boolean
    Dim ws As Worksheet: Set ws = ThisWorkbook.Worksheets(sheetName)
    Dim lastRow As Long, lastCol As Long
    lastRow = ws.Cells(ws.Rows.Count, "B").End(xlUp).Row
    lastCol = ws.Cells(1, ws.Columns.Count).End(xlToLeft).Column
    If lastRow < 2 Or lastCol < 3 Then WaitForBdp = True: Exit Function

    Dim t0 As Double: t0 = Timer
    Do
        ws.Calculate
        DoEvents                                     ' let Bloomberg RTD push
        Dim pending As Boolean: pending = False
        Dim cell As Range
        For Each cell In ws.Range(ws.Cells(2, 3), ws.Cells(lastRow, lastCol))
            ' Only "#N/A Requesting Data..." means still fetching. A terminal
            ' error (e.g. OP006/delta = #N/A on an equity) is DONE, not pending.
            ' cell.Text is the displayed string, so it distinguishes the two.
            If InStr(1, cell.Text, REQUESTING, vbTextCompare) > 0 Then
                pending = True: Exit For
            End If
        Next cell
        If Not pending Then WaitForBdp = True: Exit Function
        WaitMs 250
    Loop While (Timer - t0) < BDP_TIMEOUT_SEC
    WaitForBdp = False
End Function

' Read the filled block into a Collection of Dictionaries {id, <field>:val,...}.
Private Function ReadSecuritiesSheet(fields As Object) As Object
    Dim ws As Worksheet: Set ws = ThisWorkbook.Worksheets("Securities")
    Dim lastRow As Long: lastRow = ws.Cells(ws.Rows.Count, "B").End(xlUp).Row
    Dim out As New Collection
    Dim r As Long, c As Long
    For r = 2 To lastRow
        Dim d As Object: Set d = New Dictionary
        d("id") = ws.Cells(r, 1).Value
        For c = 1 To fields.Count
            Dim v As Variant: v = ws.Cells(r, 2 + c).Value
            ' BDP has several "no value" markers: Excel errors (#N/A N/A,
            ' #N/A Field Not Applicable) AND plain strings ("Not Applicable").
            ' Map all of them to null so the server gets a clean missing value
            ' rather than a string it will choke on.
            If IsNaMarker(v) Then
                d(CStr(fields(c))) = Null
            Else
                d(CStr(fields(c))) = v
            End If
        Next c
        out.Add d
    Next r
    Set ReadSecuritiesSheet = out
End Function

' TRUE if a cell holds one of Bloomberg's "no data" markers: an Excel error
' (#N/A N/A, #N/A Field Not Applicable, #N/A Invalid Field, ...), an empty cell,
' or the plain-text "N/A" / "Not Applicable" strings BDP sometimes returns.
Private Function IsNaMarker(ByVal v As Variant) As Boolean
    If IsError(v) Then IsNaMarker = True: Exit Function
    Dim s As String: s = Trim$(CStr(v))
    If Len(s) = 0 Then IsNaMarker = True: Exit Function
    Dim u As String: u = UCase$(s)
    IsNaMarker = (u = "N/A" Or u = "NOT APPLICABLE" Or Left$(u, 4) = "#N/A")
End Function

' ---- IO helpers ------------------------------------------------------------
Private Sub ReadInputs(ByRef portfolio As String, ByRef flowToDerived As Boolean, _
                       ByRef trades As Object)
    Dim ws As Worksheet: Set ws = ThisWorkbook.Worksheets("Input")
    portfolio = ws.Range("B3").Value
    flowToDerived = (ws.Range("B4").Value <> False)

    Set trades = New Collection
    Dim r As Long: r = 7                              ' trade table starts row 7
    Do While Len(ws.Cells(r, 1).Value) > 0
        Dim t As Object: Set t = New Dictionary
        t("security") = ws.Cells(r, 1).Value
        t("qty") = ws.Cells(r, 2).Value
        t("swap") = (ws.Cells(r, 3).Value <> False)
        trades.Add t
        r = r + 1
    Loop
End Sub

Private Sub WriteResultsSheet(resp As Object, ByVal topRow As Long)
    Dim ws As Worksheet: Set ws = ThisWorkbook.Worksheets("Input")
    Application.ScreenUpdating = False

    ' clear the previous results block below the inputs (not the inputs themselves)
    ws.Range(ws.Cells(topRow, 1), ws.Cells(topRow + 500, 20)).ClearContents

    ' --- Current holdings ---
    ws.Cells(topRow, 1).Value = "Current Holdings"
    Dim asOf As Variant: asOf = resp("data_as_of")
    If Not IsNull(asOf) Then _
        ws.Cells(topRow, 6).Value = "Holdings as of: " & CStr(asOf) & " UTC"
    Dim hEnd As Long
    hEnd = WriteTable(ws, topRow + 1, 1, _
        Array("Portfolio", "Security", "Shares Held", "% of NAV", "Swap Flag", "Replacement"), _
        Array("Portfolio", "Security", "SharesHeld", "PctNav", "Swap", "Replacement"), _
        resp("holdings"))
    If hEnd >= topRow + 2 Then _
        ws.Range(ws.Cells(topRow + 2, 4), ws.Cells(hEnd, 4)).NumberFormat = "0.000000%"

    ' --- Proposed trades ---
    Dim tTop As Long: tTop = hEnd + 3
    ws.Cells(tTop - 1, 1).Value = "Proposed Trades"
    Dim tEnd As Long
    tEnd = WriteTable(ws, tTop, 1, _
        Array("Portfolio", "Security", "Trade Quantity", "% of NAV", _
              "Marginal Shares", "Drift Shares", "Current Shares", _
              "Target Shares", "Limiting Rule", "Replacement"), _
        Array("Portfolio", "Security", "TradeQuantity", "TradePctNav", _
              "MarginalShares", "DriftShares", "CurrentShares", _
              "TargetShares", "LimitingRule", "Replacement"), _
        resp("proposed_trades"))
    If tEnd >= tTop + 1 Then _
        ws.Range(ws.Cells(tTop + 1, 4), ws.Cells(tEnd, 4)).NumberFormat = "0.000000%"

    ' --- Direct / Swap totals, to the right of the trades table ---
    Dim totals As Object: Set totals = resp("totals")
    ws.Cells(tTop, 11).Value = "Direct"
    ws.Cells(tTop, 12).Value = totals("Direct")
    ws.Cells(tTop + 1, 11).Value = "Swap"
    ws.Cells(tTop + 1, 12).Value = totals("Swap")

    ws.UsedRange.EntireColumn.AutoFit         ' only used columns, not the sheet
    Application.ScreenUpdating = True
End Sub

' Writes a table at (topRow, leftCol): headers = display labels, keys = matching
' Dictionary keys in each data row. Returns the last row written (the header row
' if there is no data). Null values are written blank.
Private Function WriteTable(ws As Worksheet, topRow As Long, leftCol As Long, _
                            headers As Variant, keys As Variant, data As Object) As Long
    Dim c As Long
    For c = 0 To UBound(headers)
        ws.Cells(topRow, leftCol + c).Value = headers(c)
    Next c
    If data Is Nothing Then WriteTable = topRow: Exit Function

    Dim r As Long
    For r = 1 To data.Count
        Dim row As Object: Set row = data(r)
        For c = 0 To UBound(keys)
            Dim v As Variant: v = row(CStr(keys(c)))
            If IsNull(v) Then
                ws.Cells(topRow + r, leftCol + c).Value = ""
            Else
                ws.Cells(topRow + r, leftCol + c).Value = v
            End If
        Next c
    Next r
    WriteTable = topRow + data.Count
End Function

' ---- auth ------------------------------------------------------------------
' Read username (B1) + password (B2), exchange them at /login for a bearer token,
' cache it in m_token for the session. m_token stays empty for the /login call
' itself (it's public), so HttpPostJson sends no Authorization header there.
Private Sub EnsureLoggedIn()
    If Len(m_token) > 0 And m_tokenExp > Now Then Exit Sub   ' still logged in
    Login
End Sub

Private Sub Login()
    Dim ws As Worksheet: Set ws = ThisWorkbook.Worksheets("Input")
    Dim user As String, pass As String
    user = CStr(ws.Range("B1").Value)
    pass = CStr(ws.Range("B2").Value)
    If Len(user) = 0 Or Len(pass) = 0 Then
        Err.Raise vbObjectError + 2, "Login", "Enter DB username in B1 and password in B2."
    End If

    Dim body As Object: Set body = New Dictionary
    body("username") = user
    body("password") = pass
    m_token = ""                            ' ensure no stale bearer on /login
    Dim resp As Object
    Set resp = HttpPostJson(ApiBase() & "/login", body)
    m_token = resp("token")

    ' Cache expiry: re-login 60s before the server-stated TTL (default 8h).
    Dim ttl As Double: ttl = 28800
    On Error Resume Next
    ttl = CDbl(resp("expires_in"))
    On Error GoTo 0
    m_tokenExp = Now + (ttl - 60) / 86400#
End Sub

' ---- HTTP + JSON -----------------------------------------------------------
' Server base URL: read from the Input!B5 "Server URL" cell, falling back to
' DEFAULT_API_URL when it's blank. Lets each deployment point at the right host
' without editing VBA.
Private Function ApiBase() As String
    Dim v As String
    v = Trim$(CStr(ThisWorkbook.Worksheets("Input").Range("B5").Value))
    If Len(v) = 0 Then v = DEFAULT_API_URL
    ApiBase = v
End Function

Private Function HttpPostJson(url As String, body As Object) As Object
    Dim http As Object
    Set http = CreateObject("WinHttp.WinHttpRequest.5.1")
    http.Open "POST", url, False
    http.setRequestHeader "Content-Type", "application/json"
    If Len(m_token) > 0 Then
        http.setRequestHeader "Authorization", "Bearer " & m_token
    End If
    http.send JsonConverter.ConvertToJson(body)
    If http.Status < 200 Or http.Status >= 300 Then
        Err.Raise vbObjectError + 1, "HttpPostJson", _
            "HTTP " & http.Status & ": " & http.responseText
    End If
    Set HttpPostJson = JsonConverter.ParseJson(http.responseText)
End Function

Private Sub WaitMs(ms As Long)
    Dim t As Double: t = Timer
    Do While (Timer - t) * 1000 < ms
        DoEvents
    Loop
End Sub
