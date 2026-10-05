-- =============================================================================
-- Migration/how-to: aggregate_shares — firm-wide share cap across all portfolios
-- Run in Supabase SQL Editor
--
-- Caps the PHYSICAL SHARES of a security held across EVERY PORTFOLIO (base funds and SMAs) at a fraction of
-- a per-security reference field (e.g. Bloomberg HS021):
--
--   sum over all portfolios of shares(i)  <=  max_threshold x field(i)
--
-- Each SMA carries its own copy of the rule. When an SMA is optimized, the
-- shares currently held in every OTHER portfolio (read from the in-memory registry)
-- are subtracted from the cap, leaving that SMA the remaining headroom. The
-- headroom is floored at zero: an SMA is never forced short to repair a breach
-- created elsewhere. Only long share counts are bounded; shorts always pass.
-- Only SMAs carry rules, so the cap binds SMAs only: a base fund that on its
-- own exceeds the cap leaves every SMA with zero headroom in that name.
-- Options read the field from their underlying (they have none of their own)
-- but are capped against their own id, in physical shares (contracts x 100).
-- A security with no usable field value (NULL / non-positive) is unconstrained.
--
-- Because the other-portfolio shares are a snapshot of current holdings, rebalances
-- computed for several SMAs at once can jointly overshoot the cap. Apply each
-- SMA's trades before optimizing the next, or rely on the compliance check to
-- flag the firm-level breach afterwards.
--
-- Loaded via replikitdata (.build_sma_rules) into
--   replikit::.sma_rule(scope = "aggregate_shares", ...).
-- Columns used:
--   bbfields       JSON array; the FIRST entry is the reference field and is
--                  fetched with the other rule fields          (required)
--   max_threshold  fraction of the field                        (required)
--   definition     OPTIONAL R source evaluating to a NAMED LIST of options:
--                    list(field = "HS021", underlying = TRUE)
--                  field      overrides the bbfields entry
--                  underlying read the field from an option's underlying
--                             (default TRUE)
--   sma_rule_link.grandfather  TRUE lets an SMA hold (but not increase) a
--                  position already over the firm cap (default FALSE)
-- min_threshold, relative_to, include, swap_only and gross_exposure are ignored.
-- =============================================================================

-- If your schema constrains `scope` with a CHECK or an enum, admit the new
-- value first (no-op when scope is free text). Adjust the constraint name to
-- match your table if you use a CHECK:
--   ALTER TABLE sma_rule_definitions DROP CONSTRAINT IF EXISTS sma_rule_definitions_scope_check;
--   ALTER TABLE sma_rule_definitions
--     ADD  CONSTRAINT sma_rule_definitions_scope_check
--     CHECK (scope IN ('position','portfolio','count','covered_options','aggregate_shares'));

-- -----------------------------------------------------------------------------
-- One definition, linked to EVERY SMA (the cap is firm-wide, so every SMA must
-- carry it). Replace 21 with an unused rule_id (rules key on portfolio_id +
-- rule_id); if a portfolio already uses that rule_id, pick another or insert
-- the links per portfolio.
-- -----------------------------------------------------------------------------
WITH def AS (
  INSERT INTO sma_rule_definitions
    (rule_name, scope, definition, bbfields, max_threshold)
  VALUES (
    'Aggregate shares <= 20% of HS021',
    'aggregate_shares',
    NULL,
    '["HS021"]',
    0.20
  )
  RETURNING definition_id
)
INSERT INTO sma_rule_link (portfolio_id, definition_id, rule_id, active)
SELECT p.portfolio_id, def.definition_id, 21, TRUE
FROM def, portfolios p
WHERE p.type = 'sma';

-- Read the field from the option itself rather than its underlying:
--   INSERT INTO sma_rule_definitions (rule_name, scope, definition, bbfields, max_threshold)
--   VALUES ('Aggregate shares <= 20% of HS021', 'aggregate_shares',
--           'list(underlying = FALSE)', '["HS021"]', 0.20);
--
-- Let one SMA keep an existing over-cap position without adding to it:
--   UPDATE sma_rule_link SET grandfather = TRUE
--   WHERE portfolio_id = 42 AND rule_id = 21;
--
-- Exempt a name from the cap on one SMA: use sma_rule_exclusion (004) with
-- rule_id 21.
--
-- NOTE: if sma_rule_definitions / sma_rule_link have other NOT NULL columns
-- without defaults in your schema, add them to the column lists above.
-- =============================================================================
