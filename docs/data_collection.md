# Basketball Reference Data Collection

This repository includes a polite scraper that captures five seasons of NBA statistics from
[Basketball Reference](https://www.basketball-reference.com/). The collector follows the design
philosophy's data integrity and governance expectations by:

- loading and respecting `robots.txt` before making requests;
- throttling requests with a configurable delay (defaults to three seconds);
- capturing raw tables (games, team metrics, player metrics) in versioned parquet files;
- emitting metadata that records the source URL and schema for every saved table.

## Usage

```bash
# Install dependencies (ideally inside a virtual environment)
pip install -e .

# Collect the latest five completed seasons
python scripts/collect_basketball_reference_data.py

# Collect specific seasons with a custom output directory
python scripts/collect_basketball_reference_data.py \
  --seasons 2020 2021 2022 2023 2024 \
  --output-dir data/raw/basketball_reference \
  --rate-limit 3.5 \
  --contact-email data-team@example.com
```

Outputs are written to `data/raw/basketball_reference/season_<year>/`. Each directory contains
individual parquet tables (e.g., `games_october.parquet`, `team_per_game.parquet`,
`player_advanced.parquet`) alongside a `metadata.json` file summarizing row counts and sources.

### Troubleshooting & Logging

- The collector now emits log records that identify tables with the `table_name` attribute. Use
  this field to quickly spot missing or renamed Basketball Reference sections without clobbering
  Python's reserved log record attributes.
- Pandas 2.2+ deprecates passing literal HTML to `read_html`. The collector wraps HTML with
  `StringIO` to remain forward compatible; if you update the parsing logic, maintain this pattern to
  avoid future warnings.

## Downstream Expectations

The saved tables serve as raw inputs for feature engineering and unified game simulations. Future
work will layer betting market data, schema validation, and orchestration around this collector to
support daily backfills and production inference jobs.
