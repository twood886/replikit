-- =============================================================================
-- Migration: sma_replacement — per-SMA security replacements
-- Run in Supabase SQL Editor
--
-- Records, for a portfolio (usually an SMA), that an "original" security held in
-- the base should be replaced by one or more "replacement" securities. When a
-- rule prevents the SMA from holding the original in full, the un-holdable
-- weight (the "overflow") is pushed into the replacement(s), split by `weight`.
--
--   * One row per (portfolio, original, replacement).
--   * `weight` is each replacement's share of the original's overflow; the rows
--     for a given (portfolio, original) must sum to 1. A single replacement is
--     just one row with weight 1.
--   * `original_identifier` / `replacement_identifier` hold security identifiers
--     (the same value as securities.identifier / the bbid used everywhere else),
--     stored as text so a replacement may reference a security not currently in
--     the securities table (e.g. a listed option the SMA will trade into).
--
-- Loaded by replikitdata::fetch_sma_replacements() and applied during hydration
-- via Portfolio$add_replacement().
--
-- Example — replace an OTC LEAP with a listed option (70%) and the underlying
-- equity (30%) in SMA portfolio_id 42:
--   INSERT INTO sma_replacement
--     (portfolio_id, original_identifier, replacement_identifier, weight)
--   VALUES
--     (42, 'owl_2029_p8.5_otc', 'owl 03/21/25 p8 equity', 0.7),
--     (42, 'owl_2029_p8.5_otc', 'owl us equity',          0.3);
-- =============================================================================

CREATE TABLE IF NOT EXISTS sma_replacement (
  portfolio_id            INTEGER NOT NULL,
  original_identifier     TEXT    NOT NULL,
  replacement_identifier  TEXT    NOT NULL,
  weight                  NUMERIC NOT NULL DEFAULT 1,
  active                  BOOLEAN NOT NULL DEFAULT TRUE,
  PRIMARY KEY (portfolio_id, original_identifier, replacement_identifier)
);

-- Look-ups are per portfolio (and filtered to active rows).
CREATE INDEX IF NOT EXISTS sma_replacement_portfolio_idx
  ON sma_replacement (portfolio_id)
  WHERE active;
