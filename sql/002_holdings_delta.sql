-- =============================================================================
-- Migration: add per-unit Enfusion delta to holdings
-- Run in Supabase SQL Editor
--
-- The Enfusion Position report gains a "Delta" column (per-unit signed delta;
-- puts negative). replikitdata's .update_db_holdings_data() writes it here, and
-- fetch_security_deltas() reads the latest value per identifier. This is how
-- OTC options — which Bloomberg cannot price on their own identifier — get a
-- delta; their price is then derived from their underlying's live price.
-- =============================================================================

ALTER TABLE holdings
  ADD COLUMN IF NOT EXISTS delta DOUBLE PRECISION;

-- holdings_actual / holdings_target are views over holdings with explicit
-- column lists, so they do NOT automatically expose the new column. They are
-- not needed for delta (fetch_security_deltas reads the raw holdings table),
-- but if a future need arises add `h.delta` / `eod.delta` to their SELECT lists
-- and re-run sql/001_trades.sql.
