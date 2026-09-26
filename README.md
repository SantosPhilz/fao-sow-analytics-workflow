# FAO SoW analytics workflow

Selected R code for transforming animal genetic resources questionnaire records into regional summaries, tables, figures, and map-ready exports. This repository is a portfolio view of the analytical methods. Its current public tree contains scripts and documentation, but no source workbooks or generated results.

## Explore the work

| File | What it illustrates |
| --- | --- |
| [`01_maps_figures_workflow.R`](fao_sow_analytics_workflow/scripts/01_maps_figures_workflow.R) | Filtering questionnaire responses, classifying countries, and preparing regional counts and map-ready tables. |
| [`02_table_3E1_2_workflow.R`](fao_sow_analytics_workflow/scripts/02_table_3E1_2_workflow.R) | Summarizing reported technology levels by region and species, including counts, percentages, and Excel table preparation. |
| [`03_figures_tables_collection.R`](fao_sow_analytics_workflow/scripts/03_figures_tables_collection.R) | A collection of figure and table analyses across questionnaire topics, with aggregation and visualization code. |

The code uses R packages including `dplyr`, `tidyr`, `stringr`, `readxl`, `ggplot2`, `openxlsx`, and `writexl`. Individual sections use additional packages such as `forcats`. The scripts show column normalization, question-level filtering, categorical scoring, country and regional aggregation, and export preparation.

## Repository map

```text
fao_sow_analytics_workflow/
├── README.md                 Project context and original portfolio notes
├── scripts/                  Three R workflow files
├── data/README.md            Guidance for shareable input examples
├── outputs/README.md         Guidance for shareable outputs
└── docs/SHARING_GUIDANCE.md  Public sharing guidance
```

The nested layout reflects how the project was originally uploaded. This root README provides a direct entry point without changing the source files or their history.

## Reproducibility and use

This is a **code archive**, rather than a ready-to-run pipeline. The scripts refer to local Excel files through absolute paths, and the required workbooks are not present in this repository. Some files contain multiple analytical sections and un-commented section headings. They need review and adaptation before running against a new dataset.

For an authorized dataset, start with the relevant script section, inspect its expected columns and question filters, replace its local input and output paths, install the packages used by that section, then review the resulting tables or figures. No published numerical results are claimed here because the inputs and outputs are not included.

## Sharing boundaries

The current [`data/`](fao_sow_analytics_workflow/data/README.md) and [`outputs/`](fao_sow_analytics_workflow/outputs/README.md) folders contain guidance files only. Add datasets, screenshots, maps, or tables only when their provenance and publication rights have been checked. See the repository's [sharing guidance](fao_sow_analytics_workflow/docs/SHARING_GUIDANCE.md).
