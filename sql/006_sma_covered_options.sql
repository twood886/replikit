-- =============================================================================
-- Migration/how-to: covered_options — forbid naked (uncovered) short options
-- Run in Supabase SQL Editor
--
-- A structural rule (option <-> underlying coupling), not a factor/threshold
-- rule. It caps short options by the underlying share position, in PHYSICAL
-- shares (the stored option qty is already contracts x 100):
--
--   * short CALL shares may not exceed the LONG underlying shares held;
--   * short PUT  shares may not exceed the SHORT underlying shares held.
--
-- When the underlying is on the wrong side (or absent) the covering capacity is
-- zero, so the short leg is forced to zero (i.e. uncovered = disallowed).
--
-- Loaded via replikitdata (.build_sma_rules) into
--   replikit::.sma_rule(scope = "covered_options", ...).
-- Unlike the threshold rules it takes NO definition formula, bbfields,
-- thresholds, divisor, or include. Its options instead live in the `definition`
-- column as R source that evaluates to a NAMED LIST:
--
--   list(restrict_calls = TRUE, restrict_puts = TRUE, per_contract = FALSE)
--
--   restrict_calls  govern short calls          (default TRUE)
--   restrict_puts   govern short puts           (default TRUE)
--   per_contract    FALSE = coverage nets across an underlying's option legs
--                     (a long option frees capacity for a short one);
--                   TRUE  = coverage counts the SHORT legs only (a long call
--                     cannot offset a naked short call).           (default FALSE)
--
-- Leave `definition` NULL/empty to accept all three defaults.
-- =============================================================================

-- If your schema constrains `scope` with a CHECK or an enum, admit the new
-- value first (no-op when scope is free text). Adjust the constraint name to
-- match your table if you use a CHECK:
--   ALTER TABLE sma_rule_definitions DROP CONSTRAINT IF EXISTS sma_rule_definitions_scope_check;
--   ALTER TABLE sma_rule_definitions
--     ADD  CONSTRAINT sma_rule_definitions_scope_check
--     CHECK (scope IN ('position','portfolio','count','covered_options'));

-- -----------------------------------------------------------------------------
-- Add one rule to a specific SMA. Replace:
--   42  -> the SMA's portfolio_id
--   20  -> an unused rule_id for that portfolio (rules key on portfolio_id+rule_id)
-- and edit the option list to taste (strictly per-contract shown here).
-- -----------------------------------------------------------------------------
WITH def AS (
  INSERT INTO sma_rule_definitions (rule_name, scope, definition)
  VALUES (
    'No naked options',
    'covered_options',
    'list(restrict_calls = TRUE, restrict_puts = TRUE, per_contract = TRUE)'
  )
  RETURNING definition_id
)
INSERT INTO sma_rule_link (portfolio_id, definition_id, rule_id, active)
SELECT 42, definition_id, 20, TRUE
FROM def;

-- Netting variant (long options offset short ones) — leave definition empty:
--   INSERT INTO sma_rule_definitions (rule_name, scope, definition)
--   VALUES ('No naked options', 'covered_options', NULL);
--
-- Calls only (ignore short puts):
--   'list(restrict_calls = TRUE, restrict_puts = FALSE)'
--
-- NOTE: if sma_rule_definitions / sma_rule_link have other NOT NULL columns
-- without defaults in your schema, add them to the column lists above. The
-- covered_options loader path ignores max_threshold, min_threshold, relative_to,
-- include, bbfields, swap_only and gross_exposure, so those may stay NULL/default.
-- =============================================================================
