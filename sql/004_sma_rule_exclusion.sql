-- =============================================================================
-- Migration: sma_rule_exclusion — per-SMA, per-rule security exemptions
-- Run in Supabase SQL Editor
--
-- Records that a specific security is EXEMPT from a specific rule on a specific
-- portfolio (an approved exception / grandfathered position). The security is
-- treated as unconstrained by that one rule: its rule factor is zeroed, so it
-- neither limits the optimizer nor trips the compliance check for that rule.
--
--   * One row per (portfolio, rule, security).
--   * Scoped to a rule by `rule_id`, matching sma_rule_link (portfolio_id,
--     rule_id) — the same identifier the loader keys rules on.
--   * `identifier` holds a security identifier (same value as
--     securities.identifier / the bbid used everywhere else), stored as text.
--   * `active` plus the optional effective_from / effective_to window control
--     whether the exemption currently applies (an expired exception simply
--     stops matching). reason / approved_by give the compliance audit trail.
--
-- NOTE: this is for EXEMPTING a name from a rule ("this SMA may hold this
-- name despite rule X"). To instead say "can't hold X, hold Y" use
-- sma_replacement (003), not this table.
--
-- Loaded by replikitdata::fetch_sma_rule_exclusions() and applied during
-- hydration via .sma_rule(exclusions = ...).
--
-- Example — exempt a specific PTP name from the "No PTP Positions" rule
-- (rule_id 7) on SMA portfolio_id 42, approved and time-boxed:
--   INSERT INTO sma_rule_exclusion
--     (portfolio_id, rule_id, identifier, reason, approved_by,
--      effective_from, effective_to)
--   VALUES
--     (42, 7, 'et us equity', 'Legacy grandfathered position',
--      'compliance', '2026-01-01', '2026-12-31');
-- =============================================================================

CREATE TABLE IF NOT EXISTS sma_rule_exclusion (
  portfolio_id    INTEGER NOT NULL,
  rule_id         INTEGER NOT NULL,
  identifier      TEXT    NOT NULL,
  active          BOOLEAN NOT NULL DEFAULT TRUE,
  reason          TEXT,
  approved_by     TEXT,
  effective_from  DATE,
  effective_to    DATE,
  PRIMARY KEY (portfolio_id, rule_id, identifier)
);

-- Look-ups are per (portfolio, rule) and filtered to active rows.
CREATE INDEX IF NOT EXISTS sma_rule_exclusion_portfolio_rule_idx
  ON sma_rule_exclusion (portfolio_id, rule_id)
  WHERE active;
