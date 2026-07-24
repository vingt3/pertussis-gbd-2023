"""Generic supplementary analyses for sensitivity and descriptive revision outputs.

The calculations are descriptive. They do not rerun the GBD model and must not
be interpreted as alternative burden estimates or observed case-fatality rates.
"""
from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

ROOT = Path(__file__).resolve().parents[1]
DATA, OUTPUT = ROOT / "data", ROOT / "outputs"
OUTPUT.mkdir(exist_ok=True)

all_random_effects = pd.read_csv(DATA / "random_effects.csv").dropna(subset=["location_name", "random_effect"])
reference_location = "Switzerland"  # Change only when the model's reference location changes.
reference_effect = all_random_effects.loc[all_random_effects["location_name"] == reference_location, "random_effect"].iloc[0]
random_effects = all_random_effects.sort_values("random_effect", ascending=False).head(10).copy()
random_effects["multiplier_vs_reference"] = np.exp(random_effects["random_effect"] - reference_effect)
random_effects.to_csv(OUTPUT / "supplementary_table_random_effects.csv", index=False)
fig, ax = plt.subplots(figsize=(8, 4.8)); ax.barh(random_effects["location_name"], random_effects["random_effect"], color="#4E79A7"); ax.invert_yaxis(); ax.set(xlabel="Location-specific random effect", title="Top incidence random effects"); fig.tight_layout(); fig.savefig(OUTPUT / "supplementary_figure_s10.png", dpi=300); plt.close(fig)

hierarchy = pd.read_csv(DATA / "location_hierarchy.csv")
replacements = pd.read_csv(DATA / "case_notification_replacements.csv")
classification = pd.read_csv(DATA / "fatal_model_strategy.csv")
replacement_summary = replacements.groupby("location_name", dropna=False).agg(replaced_location_years=("location_year", "nunique"), first_year=("year_id", "min"), last_year=("year_id", "max"), reported_cases=("cases", "sum")).reset_index()
replacement_summary = replacement_summary.merge(hierarchy[["location_name", "region_name", "super_region_name"]], on="location_name", how="left")
replacement_summary.to_csv(OUTPUT / "supplementary_table_case_notification_replacement.csv", index=False)
deaths = pd.read_csv(DATA / "country_deaths.csv")
cases = pd.read_csv(DATA / "country_incidence.csv")
def burden_2023(frame, measure):
    return frame.loc[(frame.year == 2023) & (frame.sex == "Both") & (frame.age == "All ages") & (frame.metric == "Number") & (frame.measure == measure), ["location", "val"]]
ratio = burden_2023(deaths, "Deaths").merge(burden_2023(cases, "Incidence"), on="location", suffixes=("_deaths", "_cases")).merge(hierarchy[["location_name", "super_region_name"]], left_on="location", right_on="location_name", how="left").merge(classification[["location_name", "fatal_modeling_strategy"]], left_on="location", right_on="location_name", how="left")
ratio["deaths_per_1000_cases"] = 1000 * ratio.val_deaths / ratio.val_cases
super_ratio = ratio.groupby("super_region_name", dropna=False)[["val_deaths", "val_cases"]].sum().reset_index(); super_ratio["deaths_per_1000_cases"] = 1000 * super_ratio.val_deaths / super_ratio.val_cases
strategy_ratio = ratio.groupby("fatal_modeling_strategy", dropna=False)[["val_deaths", "val_cases"]].sum().reset_index(); strategy_ratio["deaths_per_1000_cases"] = 1000 * strategy_ratio.val_deaths / strategy_ratio.val_cases
super_ratio.to_csv(OUTPUT / "supplementary_table_mortality_incidence_ratio.csv", index=False)
strategy_ratio.to_csv(OUTPUT / "supplementary_table_fatal_model_strategy.csv", index=False)
fig, ax = plt.subplots(figsize=(9, 4.8)); plot = super_ratio.sort_values("deaths_per_1000_cases"); ax.barh(plot.super_region_name, plot.deaths_per_1000_cases, color="#C44E52"); ax.set(xlabel="Deaths per 1,000 estimated cases, 2023", title="Mortality-to-incidence ratio by super-region"); fig.tight_layout(); fig.savefig(OUTPUT / "supplementary_figure_s11.png", dpi=300); plt.close(fig)

age = pd.read_csv(DATA / "age_specific_burden.csv")
infants = age.loc[(age.measure == "Deaths") & (age.sex == "Both") & (age.age == "1-5 months") & (age.metric == "Rate") & age.year.between(2019, 2023)].copy()
infants.to_csv(OUTPUT / "supplementary_table_infant_mortality.csv", index=False)
fig, ax = plt.subplots(figsize=(8, 4.8))
for location, group in infants.groupby("location"):
    ax.plot(group.year, group.val, marker="o", label=location)
ax.set(xlabel="Year", ylabel="Mortality rate per 100,000", title="Infant mortality rate, age 1–5 months"); ax.legend(fontsize=7, ncol=2); fig.tight_layout(); fig.savefig(OUTPUT / "supplementary_figure_s12.png", dpi=300); plt.close(fig)
print(f"Wrote supplementary outputs to {OUTPUT}")
