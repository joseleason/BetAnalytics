# BetAnalytics

BetAnalytics is an in-development Python package for building automated, coherent NBA betting insights focused on spread, total, and moneyline markets. The platform will combine five years of historical game and odds data with unified game simulations to deliver consistent predictions, mobile alerts, and optional social media syndication powered by AI-generated narratives.

## Current Status
- ✅ Repository scaffolding with a documented [design philosophy](docs/design_philosophy.md) that outlines architecture, tooling, and roadmap expectations.
- ✅ Initial Basketball Reference collector with polite scraping defaults that exports five seasons of games, team, and player tables to versioned parquet datasets.
- 🚧 Implementation of betting odds ingestion, unified modelling, automation, and delivery components is forthcoming.

## Design Highlights
- **Unified Predictions** – Maintain a single probabilistic game view from which spread, total, and moneyline edges are derived.
- **Automated Data Pipelines** – Collect, validate, and version at least five seasons of NBA and betting market data.
- **End-to-End Automation** – Schedule ingestion, training, and inference workflows that can notify mobile users and optionally post top picks to social channels.
- **Extensibility** – Modular package layout designed to support future prop bet modelling and additional distribution channels.
- **Governed MLOps** – Built-in coherence checks, model registry expectations, and responsible gaming guardrails sustain trust in automated delivery.

Refer to the [design philosophy](docs/design_philosophy.md) for detailed guidance on architecture, tooling choices, automation practices, and roadmap milestones. Keep this README synchronized with the codebase as implementation progresses.

## Next Steps
1. Expand project tooling (linting, testing, CI) following the documented best practices.
2. Add betting odds collectors and automate daily backfills for both stats and odds.
3. Build unified modelling pipelines, implement coherence enforcement tests, and register artefacts with a governed model registry.
4. Add delivery mechanisms (CLI/API, notification services, social posting) with monitoring, incident runbooks, and responsible gaming messaging.
5. Iterate on documentation (README, design philosophy, runbooks) to reflect new capabilities, ensuring coherence between README and implementation.

## Contributing
Until the package is feature-complete, contributions should focus on aligning new code with the documented design principles and maintaining high-quality, well-tested code. Update both this README and the design philosophy document whenever architectural decisions evolve.

