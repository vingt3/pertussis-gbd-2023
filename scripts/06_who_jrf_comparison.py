"""Create country-level descriptive comparisons between GBD estimates and WHO/JRF notifications.

Inputs are user-provided and are not included in this repository. The script uses
relative repository paths only and treats WHO/JRF data as surveillance context.
"""
from pathlib import Path
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
DATA, OUTPUT = ROOT / "data", ROOT / "outputs"
OUTPUT.mkdir(exist_ok=True)
DISEASE = "PERTUSSIS"

who_cases = pd.read_excel(DATA / "who_reported_cases.xlsx", sheet_name="Data")
who_rates = pd.read_excel(DATA / "who_incidence_rates.xlsx", sheet_name="Data")
country_incidence = pd.read_csv(DATA / "country_incidence.csv")
hierarchy = pd.read_csv(DATA / "location_hierarchy.csv")

def country_records(frame: pd.DataFrame) -> pd.DataFrame:
    return frame.loc[(frame["DISEASE"] == DISEASE) & (frame["GROUP"] == "COUNTRIES")].copy()

who_cases, who_rates = country_records(who_cases), country_records(who_rates)
who_cases["CASES"] = pd.to_numeric(who_cases["CASES"], errors="coerce")
who_rates["INCIDENCE_RATE"] = pd.to_numeric(who_rates["INCIDENCE_RATE"], errors="coerce")
years = [2023, 2024, 2025]

case_wide = who_cases.loc[who_cases["YEAR"].isin(years)].pivot_table(index=["CODE", "NAME"], columns="YEAR", values="CASES", aggfunc="first").reset_index()
rate_wide = who_rates.loc[who_rates["YEAR"].isin(years)].pivot_table(index=["CODE", "NAME"], columns="YEAR", values="INCIDENCE_RATE", aggfunc="first").reset_index()
case_wide = case_wide.rename(columns={y: f"who_cases_{y}" for y in years})
rate_wide = rate_wide.rename(columns={y: f"who_incidence_per_million_{y}" for y in years})
who = case_wide.merge(rate_wide, on=["CODE", "NAME"], how="outer")

gbd = country_incidence.loc[(country_incidence["year"] == 2023) & (country_incidence["sex"] == "Both") & (country_incidence["measure"] == "Incidence")].copy()
def slice_metric(age: str, metric: str, name: str) -> pd.DataFrame:
    out = gbd.loc[(gbd["age"] == age) & (gbd["metric"] == metric), ["location", "val", "lower", "upper"]].copy()
    return out.rename(columns={"val": name, "lower": f"{name}_lower", "upper": f"{name}_upper"})
gbd_wide = slice_metric("All ages", "Number", "gbd_cases_2023").merge(slice_metric("All ages", "Rate", "gbd_all_age_rate_per_100k_2023"), on="location", how="outer")
gbd_wide = gbd_wide.merge(slice_metric("Age-standardized", "Rate", "gbd_asir_per_100k_2023"), on="location", how="outer")
gbd_wide["gbd_all_age_rate_per_million_2023"] = 10 * gbd_wide["gbd_all_age_rate_per_100k_2023"]

loc = hierarchy.loc[hierarchy["ihme_loc_id"].astype("string").str.fullmatch(r"[A-Z]{3}", na=False)].copy()
loc["admin0"] = (loc.get("location_type", "") == "admin0").astype(int)
loc = loc.sort_values(["location_name", "admin0"], ascending=[True, False]).drop_duplicates("location_name")
loc = loc[["location_name", "ihme_loc_id", "region_name", "super_region_name"]].rename(columns={"location_name": "location"})
comparison = gbd_wide.merge(loc, on="location", how="left").merge(who, left_on="ihme_loc_id", right_on="CODE", how="left")
comparison["gbd_to_who_case_ratio_2023"] = np.where(comparison["who_cases_2023"] > 0, comparison["gbd_cases_2023"] / comparison["who_cases_2023"], np.nan)
comparison["gbd_to_who_rate_ratio_2023"] = np.where(comparison["who_incidence_per_million_2023"] > 0, comparison["gbd_all_age_rate_per_million_2023"] / comparison["who_incidence_per_million_2023"], np.nan)
comparison.to_csv(OUTPUT / "who_jrf_vs_gbd_country_comparison.csv", index=False)

global_and_regions = who_cases.loc[(who_cases["GROUP"].isin(["GLOBAL", "WHO_REGIONS"])) & (who_cases["YEAR"].isin(years))].pivot_table(index=["GROUP", "CODE", "NAME"], columns="YEAR", values="CASES", aggfunc="first").reset_index().rename(columns={y: f"who_cases_{y}" for y in years})
global_and_regions.to_csv(OUTPUT / "who_jrf_global_and_region_cases.csv", index=False)

summary = pd.DataFrame({"metric": ["GBD locations", "WHO/JRF matched locations", "GBD 2023 cases", "WHO/JRF 2023 reported cases"], "value": [len(comparison), comparison["NAME"].notna().sum(), comparison["gbd_cases_2023"].sum(), comparison["who_cases_2023"].sum()]})
summary.to_csv(OUTPUT / "who_jrf_summary.csv", index=False)
print(f"Wrote comparison files to {OUTPUT}")
