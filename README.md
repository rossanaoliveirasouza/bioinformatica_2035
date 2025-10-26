# Quantitative Synthesis & Graphical Abstract — Health Models Review

This repository contains R code to **extract, standardize, summarize and visualize** results from a literature review on applications of mathematical, statistical and AI models in health sciences. It builds normalized long tables, summary tabs, and a multi-panel **graphical abstract** (focus, methods, disease × focus bubble chart, and validation × focus heatmap).

> ✅ **Privacy note:** All scripts here avoid absolute/local file paths and machine identifiers. Configure input/output paths via variables or command-line arguments.

---

## Contents

- `R/analysis_graphical_abstract.R` — main analysis & plotting pipeline (data cleaning → long tables → summaries → visuals).
- `data/` — place your input spreadsheet here (see **Input format**).
- `out/` — generated tables and figures (created if missing).

You can keep everything in a single R script if you prefer; paths below assume this layout for clarity.

---

## Requirements

- **R ≥ 4.2**
- Packages: `tidyverse`, `readxl`, `writexl`, `janitor`, `stringr`, `tidyr`, `ggplot2`, `ggtext`, `scales`, `patchwork`, `ggrepel`, `glue`

Install dependencies in R:
```r
req_pkgs <- c(
  "tidyverse","readxl","writexl","janitor","stringr","tidyr",
  "ggplot2","ggtext","scales","patchwork","ggrepel","glue"
)
new_pkgs <- req_pkgs[!(req_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)
invisible(lapply(req_pkgs, library, character.only = TRUE))
```

---

## Input format

Provide an Excel file (e.g., `data/elicit_review_quantitative.xlsx`) with columns like:

- `study`, `year`
- `primary_focus`, `secondary_focus`
- `disease_or_area`
- `computational_methods`
- `validation_approach`
- (optional) `drug_candidates`, `biomarkers_or_targets`
- `full_text_retrieved` (optional)

Cells may contain **semicolon- or comma-separated lists**. The pipeline will normalize these into long tables.

> The code tolerates extra whitespace and typical variations (e.g., `"Support Vector Machine"` → `SVM`).

---

## What the script does (overview)

1. **Read & clean** the spreadsheet, squashing whitespace and coercing `year` to integer.
2. **Helpers** for:
   - Shortening method labels (`RF`, `SVM`, `GCN`, `LSTM`, `CNN`, `KG`, `MF`, `Ens.`, `Log.Reg.`).
   - Categorizing methods into **method groups** via keyword rules:
     - `Deep Learning`, `Classical ML / Ensemble`, `Logic/Boolean`, `Bayesian / Graph models`, `Systems Biology (ODE/PDE)`, `Feature Selection / Explainability`, `Statistical Tests`, `Other / Hybrid`.
   - Abbreviating **focus** (`Repurposing`, `Mechanisms`, `Biomarkers`, `Drug resp.`, `DDI`) and **validation** (`CV`, `SOTA`, `EHR`, `indep. set`, `dose–resp.`, `external val.`).
3. **Normalize multi-valued fields** (split on `;` or `,`, trim, drop empties / `"not_reported_in_summary"`).
4. Build **long tables**: focus, methods (with `method_group`), disease areas, validations; and optional drugs & biomarkers.
5. Compute **summary tabs** (`count` + `%`).
6. Build cross-tab datasets for the heatmaps:
   - `method_group × focus`
   - `validation × focus`
7. **Plot** the graphical abstract (four panels with a compact theme).
8. **Export**:
   - Tables: `out/quant_summaries_elicit_review.xlsx`
   - Figures: `out/graphical_abstract_elicit_review.png` and `.svg`
   - Extra heatmap: `out/heatmap_methodGroup_focus.png`

> Note: Many-to-many joins are expected when a study lists multiple foci/validations; you may see warnings that are harmless in this context.

---

## Minimal example script

Create `R/analysis_graphical_abstract.R` with the following template and run it (edit the two paths at the top as needed):

```r
# ---- 0) Paths ----
in_path  <- "data/elicit_review_quantitative.xlsx"
out_dir  <- "out"

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- 1) Packages ----
req_pkgs <- c(
  "tidyverse","readxl","writexl","janitor","stringr","tidyr",
  "ggplot2","ggtext","scales","patchwork","ggrepel","glue"
)
new_pkgs <- req_pkgs[!(req_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)
invisible(lapply(req_pkgs, library, character.only = TRUE))

# ---- 2) Helpers ----
wrap_lab <- function(x, width = 16) stringr::str_wrap(x, width = width)

shorten <- function(x){
  x <- str_replace_all(x, fixed("Support Vector Machine"), "SVM")
  x <- str_replace_all(x, fixed("Random Forest"), "RF")
  x <- str_replace_all(x, regex("\\bk[- ]?Nearest Neighbou?rs\\b", ignore_case = TRUE), "kNN")
  x <- str_replace_all(x, fixed("Graph Convolutional Network"), "GCN")
  x <- str_replace_all(x, fixed("Long Short-Term Memory"), "LSTM")
  x <- str_replace_all(x, fixed("Convolutional Neural Network"), "CNN")
  x <- str_replace_all(x, fixed("Knowledge Graph"), "KG")
  x <- str_replace_all(x, fixed("Matrix Factorization"), "MF")
  x <- str_replace_all(x, fixed("Ensemble"), "Ens.")
  x <- str_replace_all(x, fixed("Logistic regression"), "Log.Reg.")
  x <- str_squish(x); x
}

split_list_col <- function(data, col){
  data |>
    select(study, year, {{col}}) |>
    mutate(value = as.character({{col}})) |>
    select(-{{col}}) |>
    mutate(value = str_replace_all(value, "\\\\s*;\\\\s*|\\\\s*,\\\\s*", ";"),
           value = str_replace_all(value, "\\\\s+", " "),
           value = if_else(is.na(value), "", value)) |>
    tidyr::separate_rows(value, sep = ";") |>
    mutate(value = str_trim(value)) |>
    filter(value != "", !value %in% c("not_reported_in_summary")) |>
    distinct()
}

categorize_method <- function(x){
  x_low <- tolower(x)
  case_when(
    str_detect(x_low, "graph convolutional|gcn|lstm|transformer|autoencoder|vae|cnn|deep|neural network|basset|kan\\b|sc?kan") ~ "Deep Learning",
    str_detect(x_low, "\\\\brandom forest\\\\b|\\\\brf\\\\b|support vector|svm\\\\b|xgboost|k[- ]?nn\\\\b|logistic regression\\\\b|matrix factorization|ensemble") ~ "Classical ML / Ensemble",
    str_detect(x_low, "boolean|lobico|logic\\\\b") ~ "Logic/Boolean",
    str_detect(x_low, "bayesian|graph") & !str_detect(x_low, "graph convolutional") ~ "Bayesian / Graph models",
    str_detect(x_low, "ode|pde|finite element|systems biology|differential equation") ~ "Systems Biology (ODE/PDE)",
    str_detect(x_low, "knowledge graph|primekg") ~ "Knowledge Graphs",
    str_detect(x_low, "anova|chi[- ]?square|pearson|correlation|kolmogorov") ~ "Statistical Tests",
    str_detect(x_low, "feature selection|recursive feature elimination|rfe|mrmre|shap") ~ "Feature Selection / Explainability",
    TRUE ~ "Other / Hybrid"
  )
}

abbr_focus <- function(x){
  recode(x,
         "Biomarker identification" = "Biomarkers",
         "Mechanism"               = "Mechanisms",
         "Drug repurposing"        = "Repurposing",
         "Drug response"           = "Drug resp.",
         "Drug response prediction"= "Drug resp.",
         "DDI prediction"          = "DDI",
         .default = x
  )
}

abbr_validation <- function(x){
  x <- str_replace_all(x, "cross[- ]?validation", "CV")
  x <- str_replace_all(x, "state[- ]?of[- ]?the[- ]?art", "SOTA")
  x <- str_replace_all(x, "dose[- ]?response", "dose–resp.")
  x <- str_replace_all(x, "Electronic Health Record", "EHR")
  x <- str_replace_all(x, "insurance claims", "claims")
  x <- str_replace_all(x, "independent datasets?", "indep. set")
  x <- str_replace_all(x, "external validation", "external val.")
  x <- str_squish(x); x
}

axis_summarize_method <- function(x){
  xl <- tolower(x)
  dplyr::case_when(
    str_detect(xl, "\\\\brandom forest\\\\b|\\\\brf\\\\b") ~ "RF",
    str_detect(xl, "support vector|\\\\bsvm\\\\b")        ~ "SVM",
    str_detect(xl, "xgboost|extreme gradient")            ~ "XGB",
    str_detect(xl, "k[- ]?nn\\\\b|k[- ]?nearest")         ~ "kNN",
    str_detect(xl, "logistic regress")                    ~ "LogReg",
    str_detect(xl, "\\\\bcnn\\\\b|convolutional neural")  ~ "CNN",
    str_detect(xl, "\\\\bgcn\\\\b|graph convolutional")   ~ "GCN",
    str_detect(xl, "\\\\blstm\\\\b|long short")           ~ "LSTM",
    str_detect(xl, "\\\\bvae\\\\b|variational autoenc|autoencod") ~ "VAE/AE",
    str_detect(xl, "neural network|deep learning|basset|kan\\\\b|sc?kan") ~ "DL-other",
    str_detect(xl, "knowledge graph|primekg|kg\\\\b")     ~ "KG",
    str_detect(xl, "matrix factorization|grmf|graph regularized matrix") ~ "MF/GRMF",
    str_detect(xl, "boolean|lobico|logic\\\\b")           ~ "Boolean/LOBICO",
    str_detect(xl, "\\\\bshap\\\\b|shapley")              ~ "SHAP",
    str_detect(xl, "recursive feature elimination|\\\\brfe\\\\b|feature selection") ~ "RFE/FS",
    str_detect(xl, "\\\\banova\\\\b")                     ~ "ANOVA",
    str_detect(xl, "pearson")                             ~ "Pearson",
    str_detect(xl, "chi[- ]?square|\\\\bchi2\\\\b|\\\\bchi\\\\^?2\\\\b") ~ "Chi²",
    str_detect(xl, "\\\\bpca\\\\b|principal component")   ~ "PCA",
    str_detect(xl, "bayesian")                            ~ "Bayesian",
    TRUE ~ stringr::str_to_title(x)
  )
}

# ---- 3) Read & standardize ----
df_raw <- readxl::read_excel(in_path) |> janitor::clean_names()

df <- df_raw |>
  mutate(across(everything(), ~ ifelse(is.na(.x), "", .x))) |>
  mutate(
    study                 = str_squish(study),
    primary_focus         = str_squish(primary_focus),
    secondary_focus       = str_squish(secondary_focus),
    disease_or_area       = str_squish(disease_or_area),
    computational_methods = shorten(str_squish(computational_methods)),
    validation_approach   = abbr_validation(str_squish(validation_approach)),
    full_text_retrieved   = str_squish(full_text_retrieved),
    year = suppressWarnings(as.integer(year))
  )

# ---- 4) Long tables ----
long_focus <- df |>
  select(study, year, primary_focus, secondary_focus) |>
  pivot_longer(c(primary_focus, secondary_focus), names_to = "focus_type", values_to = "focus") |>
  mutate(focus = str_replace_all(focus, "\\\\s*;\\\\s*|\\\\s*,\\\\s*", ";")) |>
  separate_rows(focus, sep = ";") |>
  mutate(focus = abbr_focus(str_trim(focus))) |>
  filter(focus != "") |>
  distinct()

long_methods <- split_list_col(df, computational_methods) |>
  rename(method = value) |>
  mutate(method = shorten(method),
         method_group = categorize_method(method))

long_disease <- split_list_col(df, disease_or_area) |>
  rename(disease_area = value) |>
  mutate(disease_area = str_squish(disease_area))

long_validation <- split_list_col(df, validation_approach) |>
  rename(validation = value) |>
  mutate(validation = abbr_validation(validation))

# Optional extras
long_drugs <- split_list_col(df, drug_candidates) |> rename(drug_candidate = value)
long_biomarkers <- split_list_col(df, biomarkers_or_targets) |> rename(biomarker_or_target = value)

# ---- 5) Summaries ----
tab_focus         <- long_focus   |> count(focus, sort = TRUE) |> mutate(pct = n/sum(n))
tab_methods       <- long_methods |> count(method, sort = TRUE) |> mutate(pct = n/sum(n))
tab_method_groups <- long_methods |> count(method_group, sort = TRUE) |> mutate(pct = n/sum(n))
tab_disease       <- long_disease |> count(disease_area, sort = TRUE) |> mutate(pct = n/sum(n))
tab_validation    <- long_validation |> count(validation, sort = TRUE) |> mutate(pct = n/sum(n))

hm_mg_focus <- long_methods |>
  inner_join(long_focus, by = c("study","year")) |>
  distinct(study, method_group, focus) |>
  count(method_group, focus)

hm_val_focus <- long_validation |>
  inner_join(long_focus, by = c("study","year")) |>
  distinct(study, validation, focus) |>
  count(validation, focus)

# ---- 6) Export tables ----
writexl::write_xlsx(
  list(
    "summary_focus"         = tab_focus,
    "summary_methods"       = tab_methods,
    "summary_method_groups" = tab_method_groups,
    "summary_disease"       = tab_disease,
    "summary_validation"    = tab_validation,
    "hm_methodGroup_focus"  = hm_mg_focus,
    "hm_validation_focus"   = hm_val_focus,
    "long_focus"            = long_focus,
    "long_methods"          = long_methods,
    "long_disease"          = long_disease,
    "long_validation"       = long_validation,
    "long_drugs"            = long_drugs,
    "long_biomarkers"       = long_biomarkers
  ),
  path = file.path(out_dir, "quant_summaries_elicit_review.xlsx")
)

# ---- 7) Theme ----
theme_set(
  theme_minimal(base_size = 10) +
    theme(
      plot.title    = element_text(face="bold", size=12),
      plot.subtitle = element_text(size=9, color="grey30"),
      plot.caption  = element_text(size=8, color="grey40"),
      legend.title  = element_text(size=9),
      legend.text   = element_text(size=8),
      legend.key.height = unit(0.35, "cm"),
      legend.key.width  = unit(0.6, "cm"),
      panel.grid.minor  = element_blank()
    )
)

# ---- 8) Plots ----
pA <- tab_focus |>
  mutate(focus = fct_reorder(focus, n)) |>
  ggplot(aes(x = 1, y = n, fill = focus)) +
  geom_col(width = 0.65, color = "white") +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.06))) +
  labs(title = "Focus", x = NULL, y = "Count", subtitle = "Primary + secondary foci") +
  guides(fill = guide_legend(title = NULL, ncol = 2, byrow = TRUE)) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.margin = margin(t = 0, r = 0, b = 2, l = 0, unit = "mm"),
    plot.margin = margin(3, 6, 3, 6)
  )

# Top methods (alias for y axis)
tab_methods_axis <- long_methods |>
  mutate(method_axis = axis_summarize_method(method)) |>
  count(method_axis, sort = TRUE) |>
  mutate(pct = n / sum(n))

topN <- 10
pB <- tab_methods_axis |>
  slice_max(n, n = topN) |>
  mutate(method_axis = fct_reorder(method_axis, n)) |>
  ggplot(aes(x = method_axis, y = n)) +
  geom_col(width = 0.62, fill = "#5B8DEF") +
  geom_text(aes(label = n), hjust = -0.10, size = 3.0, lineheight = 1.0) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.20))) +
  labs(
    title = glue("Top {topN} methods"),
    x = NULL, y = "Count",
    subtitle = "Aliased labels (RF, SVM, XGB, kNN, LogReg, CNN, GCN, LSTM, ...)"
  ) +
  theme(
    axis.text.y = element_text(size = 11, lineheight = 1.1),
    plot.margin = margin(3, 12, 3, 6)
  )

# Bubble: Disease × Focus (Top-10 areas by total)
df_bubble <- long_disease |>
  inner_join(long_focus, by = c("study","year")) |>
  count(disease_area, focus, name = "n") |>
  group_by(disease_area) |>
  mutate(total = sum(n)) |>
  ungroup() |>
  arrange(desc(total)) |>
  mutate(disease_area = factor(disease_area, levels = unique(disease_area))) |>
  slice_max(order_by = total, n = 10, with_ties = FALSE)

pC <- df_bubble |>
  ggplot(aes(x = wrap_lab(focus, 10), y = wrap_lab(disease_area, 22), size = n)) +
  geom_point(alpha = 0.88) +
  scale_size_continuous(range = c(2, 9)) +
  labs(title = "Disease × Focus", x = "Focus", y = "Disease area",
       subtitle = "Top-10 areas; bubble = count") +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1),
    plot.margin = margin(3, 6, 3, 6)
  )

# Heatmap: Validation × Focus
pD <- hm_val_focus |>
  mutate(validation = fct_reorder(validation, n, .fun = sum),
         focus     = fct_reorder(focus, n, .fun = sum)) |>
  ggplot(aes(x = wrap_lab(focus, 9), y = wrap_lab(validation, 20), fill = n)) +
  geom_tile(color = "white", linewidth = 0.22) +
  scale_fill_gradient(low = "#E8F1FF", high = "#264D99") +
  labs(title = "Validation × Focus", x = "Focus", y = "Validation", fill = "Count") +
  theme(
    axis.text.x = element_text(angle = 28, hjust = 1, size = 8.5, lineheight = 0.95),
    axis.text.y = element_text(size = 8.5, lineheight = 0.95),
    legend.position = "right",
    legend.key.height = unit(0.45, "cm"),
    plot.margin = margin(3, 10, 3, 6)
  )

# ---- 9) Compose & export figures ----
upper <- (pA | pB) + patchwork::plot_layout(widths = c(0.9, 1.3))
lower <- (pC | pD) + patchwork::plot_layout(widths = c(0.9, 1.35))

ga <- (upper / lower) +
  patchwork::plot_layout(heights = c(0.9, 1.1), guides = "collect") +
  patchwork::plot_annotation(
    title = "Models in Health Sciences: Biomarkers • Mechanisms • Repurposing",
    subtitle = "Quantitative synthesis of study foci, methods, disease areas and validation",
    caption = "Data: Elicit review quantitative table • Viz: ggplot2 + patchwork"
  ) &
  theme(plot.title = element_text(face="bold", size=15))

png_path <- file.path(out_dir, "graphical_abstract_elicit_review.png")
svg_path <- file.path(out_dir, "graphical_abstract_elicit_review.svg")

ggsave(png_path, ga, width = 18, height = 11.33, dpi = 300, units = "in")
ggsave(svg_path, ga, width = 18, height = 11.33, dpi = 300, units = "in")

# Extra heatmap: MethodGroup × Focus
p_hm_mg <- hm_mg_focus |>
  mutate(method_group = fct_reorder(method_group, n, .fun = sum),
         focus        = fct_reorder(focus, n, .fun = sum)) |>
  ggplot(aes(x = wrap_lab(focus, 10), y = method_group, fill = n)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#EAF7F0", high = "#1B7F3A") +
  labs(title = "Method groups × Focus", x = "Focus", y = "Method group", fill = "Count") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

ggsave(file.path(out_dir, "heatmap_methodGroup_focus.png"), p_hm_mg,
       width = 7, height = 5.6, units = "in", dpi = 300)

message("Done.")
```

---

## How to run

From an R session:
```r
source("R/analysis_graphical_abstract.R")
```

Or from shell:
```bash
Rscript R/analysis_graphical_abstract.R
```

> Ensure your input file is at `data/elicit_review_quantitative.xlsx`. Outputs will be written to `out/`.

---

## Notes & caveats

- **Many-to-many joins** are expected when the same study lists multiple foci/validations; join warnings can be ignored.
- Counts are **occurrences (mentions)**, not mutually exclusive per study; one paper may contribute to multiple categories.
- Abbreviation and categorization rules are keyword-based; tweak the helper functions to fit your data vocabulary.

---

## Attribution

If you use or adapt this code, please cite the corresponding paper and mention this repository.

## License

Specify a license (e.g., MIT) here.
