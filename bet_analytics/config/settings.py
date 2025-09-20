"""Runtime configuration encapsulation for BetAnalytics pipelines."""

from __future__ import annotations

import json
import os
from dataclasses import dataclass
from functools import lru_cache
from pathlib import Path
from typing import Any, Mapping

try:  # Python 3.11+
    import tomllib
except ModuleNotFoundError:  # pragma: no cover - fallback for older interpreters
    import tomli as tomllib


def _default_project_root() -> Path:
    return Path(__file__).resolve().parents[2]


def _load_tool_config(pyproject_path: Path) -> Mapping[str, Any]:
    if not pyproject_path.is_file():
        return {}
    with pyproject_path.open("rb") as handle:
        pyproject = tomllib.load(handle)
    tool_config = pyproject.get("tool", {}).get("bet_analytics", {})
    if not isinstance(tool_config, Mapping):
        return {}
    return tool_config


def _resolve_path(base: Path, value: str | None) -> Path:
    if not value:
        return base
    candidate = Path(value)
    if candidate.is_absolute():
        return candidate
    return (base / candidate).resolve()


@dataclass(frozen=True)
class BetAnalyticsSettings:
    """Container for top-level runtime configuration values."""

    project_root: Path
    package_root: Path
    raw_data_dir: Path
    feature_store_dir: Path
    model_registry_dir: Path
    reports_dir: Path

    @classmethod
    def from_env(cls) -> "BetAnalyticsSettings":
        project_root = Path(
            os.environ.get("BET_ANALYTICS_PROJECT_ROOT", _default_project_root())
        ).resolve()
        tool_config = _load_tool_config(project_root / "pyproject.toml")

        package_root_name = os.environ.get(
            "BET_ANALYTICS_PACKAGE_ROOT", str(tool_config.get("package-root", "bet_analytics"))
        )
        raw_data_dir_name = os.environ.get(
            "BET_ANALYTICS_RAW_DATA_DIR", str(tool_config.get("raw-data-dir", "data/raw"))
        )
        feature_store_dir_name = os.environ.get(
            "BET_ANALYTICS_FEATURE_STORE_DIR",
            str(tool_config.get("feature-store-dir", "data/feature_store")),
        )
        model_registry_dir_name = os.environ.get(
            "BET_ANALYTICS_MODEL_REGISTRY_DIR",
            str(tool_config.get("model-registry-dir", "data/models")),
        )
        reports_dir_name = os.environ.get(
            "BET_ANALYTICS_REPORTS_DIR", str(tool_config.get("reports-dir", "data/reports"))
        )

        package_root = _resolve_path(project_root, package_root_name)
        raw_data_dir = _resolve_path(project_root, raw_data_dir_name)
        feature_store_dir = _resolve_path(project_root, feature_store_dir_name)
        model_registry_dir = _resolve_path(project_root, model_registry_dir_name)
        reports_dir = _resolve_path(project_root, reports_dir_name)

        return cls(
            project_root=project_root,
            package_root=package_root,
            raw_data_dir=raw_data_dir,
            feature_store_dir=feature_store_dir,
            model_registry_dir=model_registry_dir,
            reports_dir=reports_dir,
        )

    def ensure_directories(self) -> None:
        """Create derived storage directories when missing."""

        for target in (
            self.raw_data_dir,
            self.feature_store_dir,
            self.model_registry_dir,
            self.reports_dir,
        ):
            target.mkdir(parents=True, exist_ok=True)

    def to_dict(self) -> Mapping[str, Any]:
        """Serialize settings as a JSON-friendly dictionary."""

        return {
            "project_root": str(self.project_root),
            "package_root": str(self.package_root),
            "raw_data_dir": str(self.raw_data_dir),
            "feature_store_dir": str(self.feature_store_dir),
            "model_registry_dir": str(self.model_registry_dir),
            "reports_dir": str(self.reports_dir),
        }

    def to_json(self, *, indent: int = 2) -> str:
        return json.dumps(self.to_dict(), indent=indent)


@lru_cache(maxsize=1)
def get_settings() -> BetAnalyticsSettings:
    """Return cached application settings."""

    return BetAnalyticsSettings.from_env()


__all__ = ["BetAnalyticsSettings", "get_settings"]
