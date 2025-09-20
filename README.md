# BetAnalytics

BetAnalytics is an in-development Python package for building automated, coherent NBA betting insights focused on spread, total, and moneyline markets. The platform will combine five years of historical game and odds data with unified game simulations to deliver consistent predictions, mobile alerts, and optional social media syndication powered by AI-generated narratives.

## Current Status
- ✅ Repository scaffolding with a documented [design philosophy](docs/design_philosophy.md) that outlines architecture, tooling, and roadmap expectations.
- ✅ Initial Basketball Reference collector with polite scraping defaults that exports five seasons of games, team, and player tables to versioned parquet datasets.
- ✅ Sportsbook Review odds collector with CLI automation that materializes daily markets into season-partitioned parquet outputs.
- ✅ Exploratory analytics, feature engineering, and unified modelling pipelines that produce calibrated spread, total, and moneyline signals with profitability backtests.
- 🚧 Implementation of betting odds ingestion, unified modelling, automation, and delivery components is forthcoming.

## Design Highlights
- **Unified Predictions** – Maintain a single probabilistic game view from which spread, total, and moneyline edges are derived.
- **Automated Data Pipelines** – Collect, validate, and version at least five seasons of NBA and betting market data.
- **End-to-End Automation** – Schedule ingestion, training, and inference workflows that can notify mobile users and optionally post top picks to social channels.
- **Extensibility** – Modular package layout designed to support future prop bet modelling and additional distribution channels.
- **Governed MLOps** – Built-in coherence checks, model registry expectations, and responsible gaming guardrails sustain trust in automated delivery.

Refer to the [design philosophy](docs/design_philosophy.md) for detailed guidance on architecture, tooling choices, automation practices, and roadmap milestones. Keep this README synchronized with the codebase as implementation progresses.

## Unified Modelling Pipeline (Alpha)

The repository now provides an end-to-end experimentation loop for Sportsbook Review data:

1. **Data Loading** – `bet_analytics.data.load_sportsbookreview_odds` consolidates season-parquet drops into a clean dataset.
2. **Exploration** – `bet_analytics.analytics.summarize_odds_dataset` surfaces coverage, vig, and price movement diagnostics.
3. **Feature Engineering** – `bet_analytics.features.SportsbookReviewFeaturePipeline` builds team form, market movement, and vig-aware features with persisted feature-store artefacts.
4. **Model Training** – `bet_analytics.models.UnifiedModelTrainer` fits paired gradient boosting regressors over margin and total outcomes, applies out-of-sample probability calibration, and evaluates profitability with bankroll-aware fractional Kelly simulations that surface CLV, drawdown, and calibrated hit-rate diagnostics.

Example usage (condensed):

```python
from bet_analytics.data import load_sportsbookreview_odds
from bet_analytics.features import SportsbookReviewFeaturePipeline
from bet_analytics.models import UnifiedModelTrainer

dataset = load_sportsbookreview_odds()
feature_pipeline = SportsbookReviewFeaturePipeline()
feature_set = feature_pipeline.run(dataset, persist=True)

trainer = UnifiedModelTrainer()
report = trainer.train(feature_set)

print(report.regression_metrics)
for market, metrics in report.profitability.items():
    print(market, metrics.to_dict())

print("Spread CLV", report.profitability["spread"].avg_clv)
```

All artefacts are written to the configurable `data/feature_store` and `data/models` directories. See `bet_analytics/config/settings.py` for environment overrides.

## Next Steps
1. Expand project tooling (linting, testing, CI) following the documented best practices.
2. Add betting odds collectors and automate daily backfills for both stats and odds.
3. Build unified modelling pipelines, implement coherence enforcement tests, and register artefacts with a governed model registry.
4. Add delivery mechanisms (CLI/API, notification services, social posting) with monitoring, incident runbooks, and responsible gaming messaging.
5. Iterate on documentation (README, design philosophy, runbooks) to reflect new capabilities, ensuring coherence between README and implementation.

## Contributing
Until the package is feature-complete, contributions should focus on aligning new code with the documented design principles and maintaining high-quality, well-tested code. Update both this README and the design philosophy document whenever architectural decisions evolve.
