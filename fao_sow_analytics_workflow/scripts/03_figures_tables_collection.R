# ============================================================
# FIGURE 3A4 — Overview of the state of institutions (2024)
# Source: C:/Users/LENOVO/Documents/Figure3A2Data.xlsx
# Outputs: PNG, PDF, and Excel (multiple sheets)
# ============================================================

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readxl)
library(writexl)

# ---------- 0) Load the file you provided ----------
infile <- "C:/Users/LENOVO/Documents/Figure3A2Data.xlsx"
d <- read_excel(infile)   # uses the first sheet by default

blank <- function(x) ifelse(is.na(x), "", x)

# Find the tag column to use (PdfTagNew or pdftag)
tag_col <- intersect(c("PdfTagNew","pdftag","pdftagnew","PdfTag"), names(d))[1]
if (is.na(tag_col)) stop("Could not find PdfTagNew / pdftag column in the file.")

# ---------- 1) Fixed publication order ----------
cat_levels <- c(
  "Infrastructure",
  "Stakeholder participation",
  "Education",
  "Research",
  "Knowledge",
  "Awareness",
  "Policies",
  "Policy implementation",
  "Laws",
  "Implementation of laws"
)

region_levels <- c(
  "Africa","Asia","South west Pacific","Europe",
  "Latin America and the Caribbean","North America","Near East"
)

# Label map (fallback if BreedType_2014 is missing)
tag_labels <- c(
  "2024.2.7.1"  = "Education",
  "2024.2.7.2"  = "Research",
  "2024.2.7.3"  = "Knowledge",
  "2024.2.7.4"  = "Awareness",
  "2024.2.7.5"  = "Infrastructure",
  "2024.2.7.6"  = "Stakeholder participation",
  "2024.2.7.7"  = "Policies",
  "2024.2.7.8"  = "Policy implementation",
  "2024.2.7.9"  = "Laws",
  "2024.2.7.10" = "Implementation of laws"
)

# ---------- 2) Filter & score ----------
q7 <- d %>%
  # keep Q7 in case other questions are present
  filter(as.character(Question_2024) == "7") %>%
  # exactly the ten tags 2024.2.7.1 .. 2024.2.7.10 (no trailing dot)
  filter(str_detect(.data[[tag_col]], "^2024\\.2\\.7\\.(10|[1-9])$")) %>%
  mutate(
    # prefer BreedType_2014 text if present, otherwise map from tag
    category = coalesce(
      na_if(str_squish(BreedType_2014), ""),
      unname(tag_labels[.data[[tag_col]]])
    ),
    category = factor(category, levels = cat_levels),
    ans_raw  = tolower(str_squish(blank(Answer))),
    score = case_when(
      ans_raw %in% c("none","0")   ~ 0,
      ans_raw %in% c("low","1")    ~ 1,
      ans_raw %in% c("medium","2") ~ 2,
      ans_raw %in% c("high","3")   ~ 3,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(category), !is.na(score)) %>%
  mutate(Region = factor(as.character(Region), levels = region_levels))

if (nrow(q7) == 0) stop("No rows left after filtering/scoring. Check the file’s columns and values.")

# ---------- 3) Tables ----------
# A) Regional average score per category (for the figure)
regional_avg <- q7 %>%
  group_by(Region, category) %>%
  summarise(Average_Score = mean(score, na.rm = TRUE),
            n_rows = dplyr::n(), .groups = "drop") %>%
  arrange(Region, category)

# B) Wide Region × Category
regional_wide <- regional_avg %>%
  select(Region, category, Average_Score) %>%
  pivot_wider(names_from = category, values_from = Average_Score) %>%
  arrange(Region)

# C) Counts by Region × Category × Answer (QA)
counts_by_answer <- q7 %>%
  mutate(Answer = factor(ans_raw, levels = c("none","low","medium","high"))) %>%
  count(Region, category, Answer, name = "Count") %>%
  arrange(Region, category, Answer)

# ---------- 4) Plot (Figure 3A4) ----------
p <- ggplot(regional_avg, aes(x = category, y = Average_Score, fill = Region)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8) +
  scale_y_continuous(limits = c(0,3), breaks = 0:3) +
  labs(
    title = "Overview of the state of institutions in animal genetic resources management (2024)",
    x = NULL, y = "Average score (0–3)"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        plot.title  = element_text(face = "bold"),
        legend.position = "bottom")

print(p)

# ---------- 5) Save outputs ----------
png_path  <- "C:/Users/LENOVO/Documents/Figure_3A4_institutions_2024.png"
pdf_path  <- "C:/Users/LENOVO/Documents/Figure_3A4_institutions_2024.pdf"
xlsx_path <- "C:/Users/LENOVO/Documents/Figure_3A4_institutions_2024_DATA.xlsx"

ggsave(png_path, p, width = 12, height = 7, dpi = 300)
ggsave(pdf_path, p, width = 12, height = 7)

write_xlsx(
  list(
    "q7_filtered_scored"     = q7 %>% select(Year, Region, Country, all_of(tag_col), BreedType_2014, category, Answer = ans_raw, score),
    "regional_averages"      = regional_avg,
    "regional_wide_matrix"   = regional_wide,
    "counts_by_region_answer"= counts_by_answer,
    "tag_labels_reference"   = tibble(PdfTagNew = names(tag_labels), category = unname(tag_labels))
  ),
  xlsx_path
)

cat("Saved:\n  PNG  -> ", png_path,
    "\n  PDF  -> ", pdf_path,
    "\n  XLSX -> ", xlsx_path, "\n", sep = "")





Figure 3A9
# ============================================================
# FIGURE 3A9 — State of infrastructure & stakeholder participation (2024)
# Input : C:/Users/LENOVO/Documents/Figure3A2Data.xlsx  (your sample)
# Output: PNG, PDF, and Excel workbook with multiple sheets
# ============================================================

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readxl)
library(writexl)

# -------- 0) Load data --------
infile <- "C:/Users/LENOVO/Documents/Figure3A2Data.xlsx"
d <- read_excel(infile)   # first sheet

blank <- function(x) ifelse(is.na(x), "", x)

# Find the tag column to use (PdfTagNew or pdftag…)
tag_col <- intersect(c("PdfTagNew","pdftag","pdftagnew","PdfTag"), names(d))[1]
if (is.na(tag_col)) stop("Could not find PdfTagNew/pdftag column in the file.")

# -------- 1) Fixed label map & orders --------
region_levels <- c(
  "Africa","Asia","South west Pacific","Europe",
  "Latin America and the Caribbean","North America","Near East"
)

tag_labels <- c(
  "2024.2.7.5" = "Infrastructure",
  "2024.2.7.6" = "Stakeholder participation"
)
cat_levels <- c("Infrastructure","Stakeholder participation")

# -------- 2) Filter to Q7 and the two tags; score the answers --------
q7_pair <- d %>%
  filter(as.character(Question_2024) == "7") %>%                                         # Q7 only
  filter(stringr::str_detect(.data[[tag_col]], "^2024\\.2\\.7\\.(5|6)$")) %>%            # .5 & .6 only
  mutate(
    # Prefer BreedType_2014 if present; else map from tag
    category = dplyr::coalesce(
      dplyr::na_if(stringr::str_squish(BreedType_2014), ""),
      unname(tag_labels[.data[[tag_col]]])
    ),
    category = factor(category, levels = cat_levels),

    ans_raw = tolower(stringr::str_squish(blank(Answer))),
    score = dplyr::case_when(
      ans_raw %in% c("none","0")   ~ 0,
      ans_raw %in% c("low","1")    ~ 1,
      ans_raw %in% c("medium","2") ~ 2,
      ans_raw %in% c("high","3")   ~ 3,
      TRUE ~ NA_real_
    ),
    Region = factor(as.character(Region), levels = region_levels)
  ) %>%
  filter(!is.na(category), !is.na(score), !is.na(Region))

if (nrow(q7_pair) == 0) stop("No rows left after filtering/scoring. Check columns/values.")

# -------- 3) Tables --------
# A) Regional average score per category (for the figure)
regional_avg <- q7_pair %>%
  group_by(Region, category) %>%
  summarise(Average_Score = mean(score, na.rm = TRUE),
            n_rows = dplyr::n(), .groups = "drop") %>%
  arrange(Region, category)

# B) Wide Region × Category
regional_wide <- regional_avg %>%
  select(Region, category, Average_Score) %>%
  tidyr::pivot_wider(names_from = category, values_from = Average_Score) %>%
  arrange(Region)

# C) QA table: counts by Region × Category × raw Answer
counts_by_answer <- q7_pair %>%
  mutate(Answer = factor(ans_raw, levels = c("none","low","medium","high"))) %>%
  count(Region, category, Answer, name = "Count") %>%
  arrange(Region, category, Answer)

# -------- 4) Plot (Figure 3A9) --------
p <- ggplot(regional_avg, aes(x = Region, y = Average_Score, fill = category)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8) +
  scale_y_continuous(limits = c(0,3), breaks = 0:3) +
  labs(
    title = "State of infrastructure and stakeholder participation (2024)",
    x = NULL, y = "Average score (0–3)",
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1),
    plot.title  = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(p)

# -------- 5) Save outputs --------
png_path  <- "C:/Users/LENOVO/Documents/Figure_3A9_infrastructure_stakeholders_2024.png"
pdf_path  <- "C:/Users/LENOVO/Documents/Figure_3A9_infrastructure_stakeholders_2024.pdf"
xlsx_path <- "C:/Users/LENOVO/Documents/Figure_3A9_infrastructure_stakeholders_2024_DATA.xlsx"

ggsave(png_path, p, width = 11, height = 6.5, dpi = 300)
ggsave(pdf_path, p, width = 11, height = 6.5)

write_wxlsx <- function(path) {
  writexl::write_xlsx(
    list(
      "q7_filtered_scored"   = q7_pair %>% 
        select(Year, Region, Country, all_of(tag_col),
               BreedType_2014, category, Answer = ans_raw, score),
      "regional_averages"    = regional_avg,
      "regional_wide_matrix" = regional_wide,
      "counts_by_answer"     = counts_by_answer,
      "tag_labels_reference" = tibble(PdfTagNew = names(tag_labels),
                                      category = unname(tag_labels))
    ),
    path
  )
}
write_wxlsx(xlsx_path)

cat("Saved:\n  PNG  -> ", png_path,
    "\n  PDF  -> ", pdf_path,
    "\n  XLSX -> ", xlsx_path, "\n", sep = "")



Figure3A9 with World
# ============================================================
# FIGURE 3A9 — State of infrastructure & stakeholder participation (2024)
# With World row added
# ============================================================

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readxl)
library(writexl)

# -------- 0) Load data --------
infile <- "C:/Users/LENOVO/Documents/Figure3A2Data.xlsx"
d <- read_excel(infile)

blank <- function(x) ifelse(is.na(x), "", x)
tag_col <- intersect(c("PdfTagNew","pdftag","pdftagnew","PdfTag"), names(d))[1]

region_levels <- c(
  "Africa","Asia","South west Pacific","Europe",
  "Latin America and the Caribbean","North America","Near East","World"
)

tag_labels <- c(
  "2024.2.7.5" = "Infrastructure",
  "2024.2.7.6" = "Stakeholder participation"
)
cat_levels <- c("Infrastructure","Stakeholder participation")

# -------- 1) Filter to Q7, tags .5 & .6, score answers --------
q7_pair <- d %>%
  filter(as.character(Question_2024) == "7") %>%
  filter(str_detect(.data[[tag_col]], "^2024\\.2\\.7\\.(5|6)$")) %>%
  mutate(
    category = coalesce(
      na_if(str_squish(BreedType_2014), ""),
      unname(tag_labels[.data[[tag_col]]])
    ),
    category = factor(category, levels = cat_levels),
    ans_raw  = tolower(str_squish(blank(Answer))),
    score = case_when(
      ans_raw %in% c("none","0")   ~ 0,
      ans_raw %in% c("low","1")    ~ 1,
      ans_raw %in% c("medium","2") ~ 2,
      ans_raw %in% c("high","3")   ~ 3,
      TRUE ~ NA_real_
    ),
    Region = factor(as.character(Region), levels = region_levels)
  ) %>%
  filter(!is.na(category), !is.na(score))

# -------- 2) Regional averages --------
regional_avg <- q7_pair %>%
  group_by(Region, category) %>%
  summarise(Average_Score = mean(score, na.rm = TRUE),
            n_countries = n_distinct(Country),
            .groups = "drop")

# -------- 3) Add World row (countries-weighted mean) --------
world_avg <- q7_pair %>%
  group_by(Country, category) %>%
  summarise(country_avg = mean(score, na.rm = TRUE), .groups = "drop") %>%
  group_by(category) %>%
  summarise(Average_Score = mean(country_avg, na.rm = TRUE),
            n_countries = n_distinct(Country),
            .groups = "drop") %>%
  mutate(Region = "World")

regional_avg <- bind_rows(regional_avg, world_avg) %>%
  mutate(
    Region   = factor(Region, levels = region_levels),
    category = factor(category, levels = cat_levels)
  ) %>%
  arrange(Region, category)

# -------- 4) Wide table --------
regional_wide <- regional_avg %>%
  select(Region, category, Average_Score) %>%
  pivot_wider(names_from = category, values_from = Average_Score) %>%
  arrange(Region)

# -------- 5) Plot --------
p <- ggplot(regional_avg, aes(x = Region, y = Average_Score, fill = category)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8) +
  scale_y_continuous(limits = c(0,3), breaks = 0:3) +
  labs(
    title = "State of infrastructure and stakeholder participation (2024)",
    x = NULL, y = "Average score (0–3)", fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1),
    plot.title  = element_text(face = "bold"),
    legend.position = "bottom"
  )

# -------- 6) Save outputs --------
png_path  <- "C:/Users/LENOVO/Documents/Figure_3A9_infra_stakeholder_2024.png"
pdf_path  <- "C:/Users/LENOVO/Documents/Figure_3A9_infra_stakeholder_2024.pdf"
xlsx_path <- "C:/Users/LENOVO/Documents/Figure_3A9_infra_stakeholder_2024_DATA.xlsx"

ggsave(png_path, p, width = 11, height = 6.5, dpi = 300)
ggsave(pdf_path, p, width = 11, height = 6.5)

write_xlsx(
  list(
    "q7_filtered_scored"   = q7_pair %>% 
      select(Year, Region, Country, all_of(tag_col), BreedType_2014, category, Answer = ans_raw, score),
    "regional_averages"    = regional_avg,
    "regional_wide_matrix" = regional_wide
  ),
  xlsx_path
)

cat("Saved:\n  PNG  -> ", png_path,
    "\n  PDF  -> ", pdf_path,
    "\n  XLSX -> ", xlsx_path, "\n", sep = "")




Figure 3A10
# ============================================================
# FIGURE 3A10 — State of education, research and knowledge (2024)
# Input : C:/Users/LENOVO/Documents/New Analysis/Figure3A4/Figure3A2Data.xlsx
# Output: PNG, PDF, and Excel workbook with multiple sheets
# ============================================================

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readxl)
library(writexl)

# -------- 0) Load data --------
infile <- "C:/Users/LENOVO/Documents/New Analysis/Figure3A4/Figure3A2Data.xlsx"
d <- read_excel(infile)   # first sheet

blank <- function(x) ifelse(is.na(x), "", x)

# Find the tag column to use (PdfTagNew or pdftag…)
tag_col <- intersect(c("PdfTagNew","pdftag","pdftagnew","PdfTag"), names(d))[1]
if (is.na(tag_col)) stop("Could not find PdfTagNew/pdftag column in the file.")

# -------- 1) Fixed label map & orders --------
region_levels <- c(
  "Africa","Asia","South west Pacific","Europe",
  "Latin America and the Caribbean","North America","Near East","World"
)

tag_labels <- c(
  "2024.2.7.1" = "Education",
  "2024.2.7.2" = "Research",
  "2024.2.7.3" = "Knowledge"
)
cat_levels <- c("Education","Research","Knowledge")

# -------- 2) Filter to Q7 and the three tags; score the answers --------
q7_triple <- d %>%
  filter(as.character(Question_2024) == "7") %>%                         # Q7 only
  filter(str_detect(.data[[tag_col]], "^2024\\.2\\.7\\.(1|2|3)$")) %>%   # .1 .2 .3 only
  mutate(
    # Prefer BreedType_2014 if present; else map from tag
    category = coalesce(
      na_if(str_squish(BreedType_2014), ""),
      unname(tag_labels[.data[[tag_col]]])
    ),
    category = factor(category, levels = cat_levels),

    ans_raw = tolower(str_squish(blank(Answer))),
    score = case_when(
      ans_raw %in% c("none","0")   ~ 0,
      ans_raw %in% c("low","1")    ~ 1,
      ans_raw %in% c("medium","2") ~ 2,
      ans_raw %in% c("high","3")   ~ 3,
      TRUE ~ NA_real_
    ),
    Region = factor(as.character(Region),
                    levels = region_levels[region_levels != "World"])  # set regional order first
  ) %>%
  filter(!is.na(category), !is.na(score), !is.na(Region))

if (nrow(q7_triple) == 0) stop("No rows left after filtering/scoring. Check columns/values.")

# -------- 3) Tables --------
# A) Regional average score per category (for the figure)
regional_avg <- q7_triple %>%
  group_by(Region, category) %>%
  summarise(Average_Score = mean(score, na.rm = TRUE),
            n_rows = dplyr::n(), .groups = "drop") %>%
  arrange(Region, category)

# Add World = pooled average across all countries (for each category)
world_avg <- q7_triple %>%
  group_by(category) %>%
  summarise(Region = "World",
            Average_Score = mean(score, na.rm = TRUE),
            n_rows = dplyr::n(), .groups = "drop")

regional_avg_w <- bind_rows(regional_avg, world_avg) %>%
  mutate(
    Region = factor(Region, levels = region_levels),
    category = factor(category, levels = cat_levels)
  ) %>%
  arrange(Region, category)

# B) Wide Region × Category (with World)
regional_wide <- regional_avg_w %>%
  select(Region, category, Average_Score) %>%
  pivot_wider(names_from = category, values_from = Average_Score) %>%
  arrange(Region)

# C) QA table: counts by Region × Category × raw Answer
counts_by_answer <- q7_triple %>%
  mutate(Answer = factor(ans_raw, levels = c("none","low","medium","high"))) %>%
  count(Region, category, Answer, name = "Count") %>%
  arrange(Region, category, Answer)

# -------- 4) Plot (Figure 3A10) --------
p <- ggplot(regional_avg_w, aes(x = Region, y = Average_Score, fill = category)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8) +
  scale_y_continuous(limits = c(0,3), breaks = 0:3) +
  labs(
    title = "State of education, research and knowledge (2024)",
    x = NULL, y = "Average score (0–3)",
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1),
    plot.title  = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(p)

# -------- 5) Save outputs --------
out_dir <- "C:/Users/LENOVO/Documents/New Analysis/Figure3A10"
png_path  <- file.path(out_dir, "Figure_3A10_education_research_knowledge_20242.png")
pdf_path  <- file.path(out_dir, "Figure_3A10_education_research_knowledge_20242.pdf")
xlsx_path <- file.path(out_dir, "Figure_3A10_education_research_knowledge_20242_DATA.xlsx")

ggsave(png_path, p, width = 11, height = 6.5, dpi = 300)
ggsave(pdf_path, p, width = 11, height = 6.5)

writexl::write_xlsx(
  list(
    "q7_filtered_scored"   = q7_triple %>%
      select(Year, Region, Country, all_of(tag_col),
             BreedType_2014, category, Answer = ans_raw, score),
    "regional_averages"    = regional_avg_w,
    "regional_wide_matrix" = regional_wide,
    "counts_by_answer"     = counts_by_answer,
    "tag_labels_reference" = tibble(PdfTagNew = names(tag_labels),
                                    category = unname(tag_labels))
  ),
  xlsx_path
)

cat("Saved:\n  PNG  -> ", png_path,
    "\n  PDF  -> ", pdf_path,
    "\n  XLSX -> ", xlsx_path, "\n", sep = "")




Figure 3A10 with world
# ============================================================
# FIGURE 3A10 — State of education, research and knowledge (2024)
# Input : C:/Users/LENOVO/Documents/New Analysis/Figure3A4/Figure3A2Data.xlsx
# Output: PNG, PDF, and Excel workbook with multiple sheets
# ============================================================

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readxl)
library(writexl)

# -------- 0) Load data --------
infile <- "C:/Users/LENOVO/Documents/New Analysis/Figure3A4/Figure3A2Data.xlsx"
d <- read_excel(infile)

blank <- function(x) ifelse(is.na(x), "", x)

tag_col <- intersect(c("PdfTagNew","pdftag","pdftagnew","PdfTag"), names(d))[1]
if (is.na(tag_col)) stop("Could not find PdfTagNew/pdftag column in the file.")

# -------- 1) Labels --------
region_levels <- c(
  "Africa","Asia","South west Pacific","Europe",
  "Latin America and the Caribbean","North America","Near East","World"
)

tag_labels <- c(
  "2024.2.7.1" = "Education",
  "2024.2.7.2" = "Research",
  "2024.2.7.3" = "Knowledge"
)
cat_levels <- c("Education","Research","Knowledge")

# -------- 2) Filter Q7 + scoring --------
q7_triple <- d %>%
  filter(as.character(Question_2024) == "7") %>%
  filter(str_detect(.data[[tag_col]], "^2024\\.2\\.7\\.(1|2|3)$")) %>%
  mutate(
    category = coalesce(
      na_if(str_squish(BreedType_2014), ""),
      unname(tag_labels[.data[[tag_col]]])
    ),
    category = factor(category, levels = cat_levels),
    ans_raw  = tolower(str_squish(blank(Answer))),
    score = case_when(
      ans_raw %in% c("none","0")   ~ 0,
      ans_raw %in% c("low","1")    ~ 1,
      ans_raw %in% c("medium","2") ~ 2,
      ans_raw %in% c("high","3")   ~ 3,
      TRUE ~ NA_real_
    ),
    Region = factor(as.character(Region),
                    levels = region_levels[region_levels != "World"])
  ) %>%
  filter(!is.na(category), !is.na(score), !is.na(Region))

# -------- 3) Tables --------
# A) Regional averages
regional_avg <- q7_triple %>%
  group_by(Region, category) %>%
  summarise(Average_Score = mean(score, na.rm = TRUE),
            n_rows = n(), .groups = "drop")

# Add World (pooled average across all countries)
world_avg <- q7_triple %>%
  group_by(category) %>%
  summarise(Region = "World",
            Average_Score = mean(score, na.rm = TRUE),
            n_rows = n(), .groups = "drop")

regional_avg_w <- bind_rows(regional_avg, world_avg) %>%
  mutate(
    Region = factor(Region, levels = region_levels),
    category = factor(category, levels = cat_levels)
  ) %>%
  arrange(Region, category)

# B) Wide Region × Category
regional_wide <- regional_avg_w %>%
  select(Region, category, Average_Score) %>%
  pivot_wider(names_from = category, values_from = Average_Score) %>%
  arrange(Region)

# C) QA table
counts_by_answer <- q7_triple %>%
  mutate(Answer = factor(ans_raw, levels = c("none","low","medium","high"))) %>%
  count(Region, category, Answer, name = "Count")

# -------- 4) Plot --------
p <- ggplot(regional_avg_w, aes(x = Region, y = Average_Score, fill = category)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8) +
  scale_y_continuous(limits = c(0,3), breaks = 0:3) +
  labs(
    title = "State of education, research and knowledge (2024)",
    x = NULL, y = "Average score (0–3)", fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1),
    plot.title  = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(p)

# -------- 5) Save --------
out_dir <- "C:/Users/LENOVO/Documents/New Analysis/Figure3A10"
dir.create(out_dir, showWarnings = FALSE)

png_path  <- file.path(out_dir, "Figure_3A10_education_research_knowledge_20242.png")
pdf_path  <- file.path(out_dir, "Figure_3A10_education_research_knowledge_20242.pdf")
xlsx_path <- file.path(out_dir, "Figure_3A10_education_research_knowledge_20242_DATA.xlsx")

ggsave(png_path, p, width = 11, height = 6.5, dpi = 300)
ggsave(pdf_path, p, width = 11, height = 6.5)

write_xlsx(
  list(
    "q7_filtered_scored"   = q7_triple %>%
      select(Year, Region, Country, all_of(tag_col),
             BreedType_2014, category, Answer = ans_raw, score),
    "regional_averages"    = regional_avg_w,
    "regional_wide_matrix" = regional_wide,
    "counts_by_answer"     = counts_by_answer,
    "tag_labels_reference" = tibble(PdfTagNew = names(tag_labels),
                                    category = unname(tag_labels))
  ),
  xlsx_path
)

cat("Saved:\n  PNG  -> ", png_path,
    "\n  PDF  -> ", pdf_path,
    "\n  XLSX -> ", xlsx_path, "\n")




Figure 3A11
# ============================================================
# FIGURE 3A11 — State of policy development (2024)
# Input : C:/Users/LENOVO/Documents/New Analysis/Figure3A4/Figure3A2Data.xlsx
# Output: PNG, PDF, and Excel workbook with multiple sheets
# ============================================================

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readxl)
library(writexl)

# -------- 0) Load data --------
infile <- "C:/Users/LENOVO/Documents/New Analysis/Figure3A4/Figure3A2Data.xlsx"
d <- read_excel(infile)

blank <- function(x) ifelse(is.na(x), "", x)

# Tag column (PdfTagNew / pdftag / etc.)
tag_col <- intersect(c("PdfTagNew","pdftag","pdftagnew","PdfTag"), names(d))[1]
if (is.na(tag_col)) stop("Could not find PdfTagNew/pdftag column in the file.")

# -------- 1) Labels & orders --------
region_levels <- c(
  "Africa","Asia","South west Pacific","Europe",
  "Latin America and the Caribbean","North America","Near East","World"
)

# Q7 sub-tags:
# 2.7.4 Awareness, 2.7.7 Policies, 2.7.9 Laws,
# 2.7.10 Implementation of laws, 2.7.8 Policy implementation
tag_labels <- c(
  "2024.2.7.4"  = "Awareness",
  "2024.2.7.7"  = "Policies",
  "2024.2.7.9"  = "Laws",
  "2024.2.7.10" = "Implementation of laws",
  "2024.2.7.8"  = "Policy implementation"
)
cat_levels <- c("Awareness","Laws","Policies","Policy implementation","Implementation of laws")

# -------- 2) Filter Q7 + map categories + score answers --------
q7_policy <- d %>%
  filter(as.character(Question_2024) == "7") %>%
  filter(str_detect(.data[[tag_col]], "^2024\\.2\\.7\\.(4|7|8|9|10)$")) %>%
  mutate(
    category = coalesce(
      na_if(str_squish(BreedType_2014), ""),
      unname(tag_labels[.data[[tag_col]]])
    ),
    # normalize to our preferred order/labels
    category = recode(category,
                      "Awareness"               = "Awareness",
                      "Laws"                    = "Laws",
                      "Policies"                = "Policies",
                      "Policy implementation"   = "Policy implementation",
                      "Implementation of laws"  = "Implementation of laws"),
    category = factor(category, levels = cat_levels),

    ans_raw = tolower(str_squish(blank(Answer))),
    score = case_when(
      ans_raw %in% c("none","0")   ~ 0,
      ans_raw %in% c("low","1")    ~ 1,
      ans_raw %in% c("medium","2") ~ 2,
      ans_raw %in% c("high","3")   ~ 3,
      TRUE ~ NA_real_
    ),
    Region = factor(as.character(Region),
                    levels = region_levels[region_levels != "World"])
  ) %>%
  filter(!is.na(category), !is.na(score), !is.na(Region))

if (nrow(q7_policy) == 0) stop("No rows after filtering/scoring. Check the file contents.")

# -------- 3) Regional averages + World --------
regional_avg <- q7_policy %>%
  group_by(Region, category) %>%
  summarise(Average_Score = mean(score, na.rm = TRUE),
            n_rows = n(), .groups = "drop")

world_avg <- q7_policy %>%
  group_by(category) %>%
  summarise(Region = "World",
            Average_Score = mean(score, na.rm = TRUE),
            n_rows = n(), .groups = "drop")

regional_avg_w <- bind_rows(regional_avg, world_avg) %>%
  mutate(
    Region = factor(Region, levels = region_levels),
    category = factor(category, levels = cat_levels)
  ) %>%
  arrange(Region, category)

# Wide matrix (Region × Category)
regional_wide <- regional_avg_w %>%
  select(Region, category, Average_Score) %>%
  pivot_wider(names_from = category, values_from = Average_Score) %>%
  arrange(Region)

# QA counts: Region × Category × Answer
counts_by_answer <- q7_policy %>%
  mutate(Answer = factor(ans_raw, levels = c("none","low","medium","high"))) %>%
  count(Region, category, Answer, name = "Count") %>%
  arrange(Region, category, Answer)

# -------- 4) Plot --------
p <- ggplot(regional_avg_w, aes(x = Region, y = Average_Score, fill = category)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.8) +
  scale_y_continuous(limits = c(0,3), breaks = 0:3) +
  labs(
    title = "State of policy development (2024)",
    x = NULL, y = "Average score (0–3)", fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1),
    plot.title  = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(p)

# -------- 5) Save outputs --------
out_dir <- "C:/Users/LENOVO/Documents/New Analysis/Figure3A11"
dir.create(out_dir, showWarnings = FALSE)

png_path  <- file.path(out_dir, "Figure_3A11_policy_development_2024.png")
pdf_path  <- file.path(out_dir, "Figure_3A11_policy_development_2024.pdf")
xlsx_path <- file.path(out_dir, "Figure_3A11_policy_development_2024_DATA.xlsx")

ggsave(png_path, p, width = 11, height = 6.5, dpi = 300)
ggsave(pdf_path, p, width = 11, height = 6.5)

write_xlsx(
  list(
    "q7_filtered_scored"   = q7_policy %>%
      select(Year, Region, Country, all_of(tag_col),
             BreedType_2014, category, Answer = ans_raw, score),
    "regional_averages"    = regional_avg_w,
    "regional_wide_matrix" = regional_wide,
    "counts_by_answer"     = counts_by_answer,
    "tag_labels_reference" = tibble(PdfTagNew = names(tag_labels),
                                    category = unname(tag_labels))
  ),
  xlsx_path
)

cat("Saved:\n  PNG  -> ", png_path,
    "\n  PDF  -> ", pdf_path,
    "\n  XLSX -> ", xlsx_path, "\n")








TABLE 3B1
# ================== TABLE 3B1 (2024) from Q5+Q6 ==================
# Denominator: sum of Q5 answers (SubQ 1+2) for big-7 species
# Numerator:   sum of Q6 answers (SubQ 1 baseline, SubQ 2 monitoring)
# Output:      Regional % columns + #countries + #breed populations
# ================================================================

# install.packages(c("readxl","dplyr","stringr","tidyr","openxlsx"))  # if needed
library(readxl); library(dplyr); library(stringr); library(tidyr); library(openxlsx)

infile   <- "C:/Users/LENOVO/Documents/breedD.xlsx"       # <-- the file that has Q5 and Q6 together
out_xlsx <- "C:/Users/LENOVO/Documents/Table3B1_2024.xlsx"

# -------- helpers --------
pick_col <- function(df, candidates){
  nm <- tolower(names(df))
  for (c in tolower(candidates)) {
    hit <- which(nm == c)
    if (length(hit)) return(names(df)[hit[1]])
  }
  NA_character_
}
norm_region <- function(x){
  x <- trimws(as.character(x))
  x <- ifelse(startsWith(x,"South west Pacific"), "Southwest Pacific", x)
  x <- ifelse(x=="Europe", "Europe and the Caucasus", x)
  x <- ifelse(startsWith(x,"Near"), "Near and Middle East", x)
  x <- ifelse(startsWith(x,"Latin Ame"), "Latin America and the Caribbean", x)
  x
}

big7 <- c("009","042","021","010","007","008","037")

# -------- load + normalize --------
d0 <- read_excel(infile)

col_year <- pick_col(d0, c("year"))
col_reg  <- pick_col(d0, c("region"))
col_cty  <- pick_col(d0, c("country"))
col_q    <- pick_col(d0, c("question_2024","question"))
col_subq <- pick_col(d0, c("subquestion_2024","subquestion"))
col_ans  <- pick_col(d0, c("answer"))
col_sp   <- pick_col(d0, c("specietag","speciestag","species_tag","species","speciecode","speciescode"))

need <- c(col_year,col_reg,col_cty,col_q,col_subq,col_ans,col_sp)
if (any(is.na(need))) stop("Missing needed columns. Found: ", paste(names(d0), collapse=", "))

df <- d0 %>%
  transmute(
    Year      = suppressWarnings(as.integer(.data[[col_year]])),
    Region    = norm_region(.data[[col_reg]]),
    Country   = as.character(.data[[col_cty]]),
    Question  = suppressWarnings(as.integer(.data[[col_q]])),
    SubQ      = suppressWarnings(as.integer(.data[[col_subq]])),
    SpecieTag = str_pad(str_replace_all(as.character(.data[[col_sp]]), "\\D", ""), width = 3, pad = "0"),
    Answer    = suppressWarnings(as.numeric(str_replace_all(as.character(.data[[col_ans]]), "[^0-9.\\-]", "")))
  ) %>%
  filter(Year == 2024, SpecieTag %in% big7, !is.na(Answer))

# -------- Q5: denominator & #countries --------
q5 <- df %>% filter(Question == 5, SubQ %in% c(1,2))   # 1=locally adapted, 2=exotic (as used in your sheet)
denom_by_region <- q5 %>%
  group_by(Region) %>%
  summarise(
    breeds_total = sum(Answer, na.rm = TRUE),           # denominator
    n_countries  = n_distinct(Country),                 # #countries that reported in Q5
    .groups = "drop"
  )

# -------- Q6: numerators --------
q6 <- df %>% filter(Question == 6, SubQ %in% c(1,2))
num_baseline <- q6 %>% filter(SubQ == 1) %>%
  group_by(Region) %>%
  summarise(baseline_count = sum(Answer, na.rm = TRUE), .groups = "drop")

num_monitor  <- q6 %>% filter(SubQ == 2) %>%
  group_by(Region) %>%
  summarise(monitor_count = sum(Answer, na.rm = TRUE), .groups = "drop")

# -------- combine -> regional table --------
regional_sums <- denom_by_region %>%
  left_join(num_baseline, by = "Region") %>%
  left_join(num_monitor,  by = "Region") %>%
  mutate(
    baseline_count = replace_na(baseline_count, 0),
    monitor_count  = replace_na(monitor_count, 0),
    `Baseline survey of population size (%)`    = ifelse(breeds_total > 0, round(100 * baseline_count / breeds_total, 1), NA_real_),
    `Regular monitoring of population size (%)` = ifelse(breeds_total > 0, round(100 * monitor_count  / breeds_total, 1), NA_real_)
  ) %>%
  rename(`Number of national breed populations` = breeds_total,
         `Number of countries` = n_countries) %>%
  arrange(Region)

# -------- World row (recompute from sums) --------
world_row <- regional_sums %>%
  summarise(
    Region = "World",
    `Number of countries`                 = sum(`Number of countries`, na.rm = TRUE),
    `Number of national breed populations`= sum(`Number of national breed populations`, na.rm = TRUE),
    `Baseline survey of population size (%)` =
      round(100 * sum(baseline_count, na.rm = TRUE) / sum(`Number of national breed populations`, na.rm = TRUE), 1),
    `Regular monitoring of population size (%)` =
      round(100 * sum(monitor_count,  na.rm = TRUE) / sum(`Number of national breed populations`, na.rm = TRUE), 1)
  )

table3B1_2024 <- regional_sums %>%
  select(Region, `Number of countries`, `Number of national breed populations`,
         `Baseline survey of population size (%)`,
         `Regular monitoring of population size (%)`) %>%
  bind_rows(world_row)

# -------- sanity sheets (optional) --------
checks_q5_q6 <- list(
  q5_by_region_subq = q5 %>% group_by(Region, SubQ) %>%
    summarise(sum_answer = sum(Answer, na.rm=TRUE), .groups="drop") %>% arrange(Region, SubQ),
  q6_by_region_subq = q6 %>% group_by(Region, SubQ) %>%
    summarise(sum_answer = sum(Answer, na.rm=TRUE), .groups="drop") %>% arrange(Region, SubQ)
)

# -------- write Excel (multi-sheet) --------
wb <- createWorkbook()
addWorksheet(wb, "Regional_Sums")
addWorksheet(wb, "Table3B1_2024")
addWorksheet(wb, "Checks_Q5_Q6")

writeData(wb, "Regional_Sums", regional_sums)
writeData(wb, "Table3B1_2024", table3B1_2024)
writeData(wb, "Checks_Q5_Q6", checks_q5_q6$q5_by_region_subq, startRow = 1, startCol = 1)
writeData(wb, "Checks_Q5_Q6", checks_q5_q6$q6_by_region_subq, startRow = nrow(checks_q5_q6$q5_by_region_subq) + 4, startCol = 1)

saveWorkbook(wb, out_xlsx, overwrite = TRUE)

message("Done -> ", out_xlsx)






FOR FULL TABLE
# ================== TABLE 3B1 (2024) – full export ==================
# Input : C:/Users/LENOVO/Documents/breedD.xlsx  (contains Q5 + Q6)
# Output: C:/Users/LENOVO/Documents/Table3B1_2024_FULL.xlsx
# Sheets: Filtered_2024_Q6, Filtered_2024_Q5, Regional_Sums, Table3B1_2024,
#         Regional_Matrix, QA_Counts, Tag_Map, Species, Breed_Pop_Counts, Checks_Q5_Q6
# ================================================================

library(readxl); library(dplyr); library(stringr); library(tidyr); library(openxlsx)

infile   <- "C:/Users/LENOVO/Documents/breedD.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Table3B1_2024_FULL.xlsx"

# ---------- helpers ----------
pick_col <- function(df, candidates){
  nm <- tolower(names(df))
  for (c in tolower(candidates)) {
    hit <- which(nm == c); if (length(hit)) return(names(df)[hit[1]])
  }
  NA_character_
}
norm_region <- function(x){
  x <- trimws(as.character(x))
  x <- ifelse(startsWith(x,"South west Pacific"), "Southwest Pacific", x)
  x <- ifelse(x=="Europe", "Europe and the Caucasus", x)
  x <- ifelse(startsWith(x,"Near"), "Near and Middle East", x)
  x <- ifelse(startsWith(x,"Latin Ame"), "Latin America and the Caribbean", x)
  x
}

# Big seven species
species_map <- tibble::tribble(
  ~code, ~label,
  "009","Cattle (specialized dairy)",
  "042","Sheep",
  "021","Goats",
  "010","Chickens",
  "007","Cattle (multipurpose)",
  "008","Cattle (specialized beef)",
  "037","Pigs"
)
big7 <- species_map$code

# ---------- load + normalize ----------
d0 <- read_excel(infile)

col_year <- pick_col(d0, c("year"))
col_reg  <- pick_col(d0, c("region"))
col_cty  <- pick_col(d0, c("country"))
col_q    <- pick_col(d0, c("question_2024","question"))
col_subq <- pick_col(d0, c("subquestion_2024","subquestion"))
col_ans  <- pick_col(d0, c("answer"))
col_sp   <- pick_col(d0, c("specietag","speciestag","species_tag","species","speciecode","speciescode"))

need <- c(col_year,col_reg,col_cty,col_q,col_subq,col_ans,col_sp)
if (any(is.na(need))) stop("Missing needed columns. Found: ", paste(names(d0), collapse=", "))

df <- d0 %>%
  transmute(
    Year      = suppressWarnings(as.integer(.data[[col_year]])),
    Region    = norm_region(.data[[col_reg]]),
    Country   = as.character(.data[[col_cty]]),
    Question  = suppressWarnings(as.integer(.data[[col_q]])),
    SubQ      = suppressWarnings(as.integer(.data[[col_subq]])),
    SpecieTag = str_pad(str_replace_all(as.character(.data[[col_sp]]), "\\D", ""), width = 3, pad = "0"),
    Answer    = suppressWarnings(as.numeric(str_replace_all(as.character(.data[[col_ans]]), "[^0-9.\\-]", "")))
  ) %>%
  filter(Year == 2024, SpecieTag %in% big7, !is.na(Answer))

# ---------- Q5 (denominator) & Q6 (numerators) ----------
q5 <- df %>% filter(Question == 5, SubQ %in% c(1,2))
q6 <- df %>% filter(Question == 6, SubQ %in% c(1,2))

# Denominator: total national breed populations per region (sum of Q5 answers over SubQ 1+2)
denom_by_region <- q5 %>%
  group_by(Region) %>%
  summarise(
    breeds_total = sum(Answer, na.rm = TRUE),
    n_countries  = n_distinct(Country),       # countries reporting in Q5
    .groups = "drop"
  )

# Numerators from Q6
num_baseline <- q6 %>% filter(SubQ == 1) %>%
  group_by(Region) %>% summarise(baseline_count = sum(Answer, na.rm = TRUE), .groups = "drop")
num_monitor  <- q6 %>% filter(SubQ == 2) %>%
  group_by(Region) %>% summarise(monitor_count  = sum(Answer, na.rm = TRUE), .groups = "drop")

# Regional table: counts + percentages
regional_sums <- denom_by_region %>%
  left_join(num_baseline, by="Region") %>%
  left_join(num_monitor,  by="Region") %>%
  mutate(
    baseline_count = replace_na(baseline_count, 0),
    monitor_count  = replace_na(monitor_count, 0),
    `Baseline survey of population size (%)`    = ifelse(breeds_total > 0, round(100 * baseline_count / breeds_total, 1), NA_real_),
    `Regular monitoring of population size (%)` = ifelse(breeds_total > 0, round(100 * monitor_count  / breeds_total, 1), NA_real_)
  ) %>%
  rename(`Number of national breed populations` = breeds_total,
         `Number of countries` = n_countries) %>%
  arrange(Region)

# World row
world_row <- regional_sums %>%
  summarise(
    Region = "World",
    `Number of countries`                  = sum(`Number of countries`, na.rm = TRUE),
    `Number of national breed populations` = sum(`Number of national breed populations`, na.rm = TRUE),
    `Baseline survey of population size (%)` =
      round(100 * sum(baseline_count, na.rm = TRUE) / sum(`Number of national breed populations`, na.rm = TRUE), 1),
    `Regular monitoring of population size (%)` =
      round(100 * sum(monitor_count,  na.rm = TRUE) / sum(`Number of national breed populations`, na.rm = TRUE), 1)
  )

table3B1_2024 <- regional_sums %>%
  select(Region, `Number of countries`, `Number of national breed populations`,
         `Baseline survey of population size (%)`,
         `Regular monitoring of population size (%)`) %>%
  bind_rows(world_row)

# A matrix without World (handy for plotting)
regional_matrix <- table3B1_2024 %>% filter(Region != "World")

# QA counts for Q6
qa_counts <- q6 %>%
  mutate(measure = ifelse(SubQ==1,"Baseline survey of population size (%)",
                          "Regular monitoring of population size (%)")) %>%
  group_by(Region, measure) %>%
  summarise(
    n_rows  = n(),
    sum_ans = sum(Answer, na.rm = TRUE),
    min_ans = suppressWarnings(min(Answer, na.rm = TRUE)),
    max_ans = suppressWarnings(max(Answer, na.rm = TRUE)),
    .groups = "drop"
  ) %>% arrange(Region, measure)

# Tag-map style (light)
tag_map <- bind_rows(
  q5 %>% mutate(QuestionName = "Q5"),
  q6 %>% mutate(QuestionName = ifelse(SubQ==1,"Q6.1 Baseline","Q6.2 Monitoring"))
) %>%
  select(Region, Country, SpecieTag, Question, SubQ, QuestionName, Answer) %>%
  arrange(Region, Country, SpecieTag, Question, SubQ)

# Species sheet + Breed_Pop_Counts
species_sheet <- species_map
breed_pop_counts <- regional_sums %>%
  select(Region, `Number of national breed populations`) %>%
  bind_rows(world_row %>% select(Region, `Number of national breed populations`))

# ========== WRITE MULTI-SHEET EXCEL ==========
wb <- createWorkbook()
addWorksheet(wb, "Filtered_2024_Q6")
addWorksheet(wb, "Filtered_2024_Q5")
addWorksheet(wb, "Regional_Sums")
addWorksheet(wb, "Table3B1_2024")
addWorksheet(wb, "Regional_Matrix")
addWorksheet(wb, "QA_Counts")
addWorksheet(wb, "Tag_Map")
addWorksheet(wb, "Species")
addWorksheet(wb, "Breed_Pop_Counts")
addWorksheet(wb, "Checks_Q5_Q6")

writeData(wb, "Filtered_2024_Q6", q6 %>% arrange(Region, Country, SpecieTag, SubQ))
writeData(wb, "Filtered_2024_Q5", q5 %>% arrange(Region, Country, SpecieTag, SubQ))
writeData(wb, "Regional_Sums", regional_sums)
writeData(wb, "Table3B1_2024", table3B1_2024)
writeData(wb, "Regional_Matrix", regional_matrix)
writeData(wb, "QA_Counts", qa_counts)
writeData(wb, "Tag_Map", tag_map)
writeData(wb, "Species", species_sheet)
writeData(wb, "Breed_Pop_Counts", breed_pop_counts)

# Checks: raw sums per region & SubQ for Q5 and Q6 (put both on one sheet)
checks_q5 <- q5 %>% group_by(Region, SubQ) %>%
  summarise(sum_answer = sum(Answer, na.rm=TRUE), .groups="drop") %>% arrange(Region, SubQ)
checks_q6 <- q6 %>% group_by(Region, SubQ) %>%
  summarise(sum_answer = sum(Answer, na.rm=TRUE), .groups="drop") %>% arrange(Region, SubQ)

writeData(wb, "Checks_Q5_Q6", checks_q5, startRow = 1, startCol = 1)
writeData(wb, "Checks_Q5_Q6", checks_q6, startRow = nrow(checks_q5) + 4, startCol = 1)

# light number format for % columns
pct_cols <- c("Baseline survey of population size (%)","Regular monitoring of population size (%)")
for (cn in intersect(pct_cols, names(table3B1_2024))) {
  addStyle(wb, "Table3B1_2024", createStyle(numFmt = "0.0"),
           rows = 2:(nrow(table3B1_2024)+1), cols = which(names(table3B1_2024)==cn),
           gridExpand = TRUE)
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Done -> ", out_xlsx)





Figure 3B1
# ================= Figure 3B1 (2024) =================
# Input : C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx
# Output: PNG, PDF, and multi-sheet Excel
# =====================================================

library(readxl); library(dplyr); library(stringr)
library(tidyr);   library(ggplot2); library(openxlsx)

infile   <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_png  <- "C:/Users/LENOVO/Documents/Fig3B1_2024.png"
out_pdf  <- "C:/Users/LENOVO/Documents/Fig3B1_2024.pdf"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3B1_2024_tables.xlsx"

# ---------- helpers ----------
pick_col <- function(df, candidates){
  nm <- tolower(names(df))
  for (c in tolower(candidates)) {
    hit <- which(nm == c); if (length(hit)) return(names(df)[hit[1]])
  }
  NA_character_
}
norm_region <- function(x){
  x <- str_squish(as.character(x))
  x <- ifelse(x %in% c("South west Pacific","South-west Pacific","South West Pacific"),
              "Southwest Pacific", x)
  x <- ifelse(x == "Europe", "Europe and the Caucasus", x)
  x <- ifelse(startsWith(x,"Near"), "Near East", x)
  x <- ifelse(startsWith(x,"Latin Ame"), "Latin America and the Caribbean", x)
  x
}

# ✅ Desired top → bottom order
region_order <- c(
  "World",
  "Southwest Pacific",
  "North America",
  "Near East",
  "Latin America and the Caribbean",
  "Europe and the Caucasus",
  "Asia",
  "Africa"
)
region_levels_for_factor <- rev(region_order)  # flipped for coord_flip()

# 5 categories (exact phrasing)
cat_levels <- c(
  "Completed before GPA",
  "Completed after GPA",
  "Partially completed (progress since GPA)",
  "Partially completed (no progress since GPA)",
  "No country report"
)

# colors
cat_cols <- c(
  "Completed before GPA"                         = "#137C31",
  "Completed after GPA"                          = "#56B851",
  "Partially completed (progress since GPA)"     = "#E6D24A",
  "Partially completed (no progress since GPA)"  = "#F0765A",
  "No country report"                            = "#BFBFBF"
)

# ---------- load & detect columns ----------
d0 <- read_excel(infile)

col_year <- pick_col(d0, c("year"))
col_reg  <- pick_col(d0, c("region"))
col_cty  <- pick_col(d0, c("country"))
col_sec  <- pick_col(d0, c("section"))
col_q    <- pick_col(d0, c("question_2024","question"))
col_tag  <- pick_col(d0, c("pdftag","pdftagnew"))
col_ans  <- pick_col(d0, c("answer"))
col_code <- pick_col(d0, c("code"))
col_ind  <- pick_col(d0, c("indicator"))

if (any(is.na(c(col_reg,col_cty,col_ans)))) {
  stop("Missing required columns. Found: ", paste(names(d0), collapse=", "))
}

df <- d0 %>%
  transmute(
    Year     = if (!is.na(col_year)) suppressWarnings(as.integer(.data[[col_year]])) else 2024L,
    Region   = norm_region(.data[[col_reg]]),
    Country  = as.character(.data[[col_cty]]),
    Section  = if (!is.na(col_sec)) as.character(.data[[col_sec]]) else NA_character_,
    Question = if (!is.na(col_q))   suppressWarnings(as.integer(.data[[col_q]])) else NA_integer_,
    pdftag   = if (!is.na(col_tag)) as.character(.data[[col_tag]]) else NA_character_,
    Answer   = as.character(.data[[col_ans]]),
    Code     = if (!is.na(col_code)) suppressWarnings(as.integer(.data[[col_code]])) else NA_integer_,
    Indicator= if (!is.na(col_ind)) as.character(.data[[col_ind]]) else NA_character_
  )

# ---------- filter to Section 3 / Question 1 / pdftag 2024.3.1.0* ----------
df24 <- df %>% filter(Year == 2024)

q1 <- df24 %>%
  filter(
    (is.na(Section) | Section %in% c("3", "03", 3)) |
    (!is.na(Question) & Question == 1) |
    (!is.na(pdftag) & str_detect(pdftag, "^2024\\.3\\.1\\.0"))
  )

# Map categories
q1 <- q1 %>%
  mutate(
    a_letter = tolower(str_sub(str_squish(Answer), 1, 1)),
    cat = case_when(
      a_letter == "a" ~ "Completed before GPA",
      a_letter == "b" ~ "Completed after GPA",
      a_letter == "c" ~ "Partially completed (progress since GPA)",
      a_letter == "d" ~ "Partially completed (no progress since GPA)",
      a_letter == "e" ~ "No country report",
      !is.na(Code) & Code == 1 ~ "Completed before GPA",
      !is.na(Code) & Code == 2 ~ "Completed after GPA",
      !is.na(Code) & Code == 3 ~ "Partially completed (progress since GPA)",
      !is.na(Code) & Code == 4 ~ "Partially completed (no progress since GPA)",
      !is.na(Code) & Code == 0 ~ "No country report",
      TRUE ~ NA_character_
    ),
    Region  = str_squish(Region),
    Country = str_squish(Country)
  )

# ---------- build universe ----------
universe <- df24 %>%
  mutate(Region = str_squish(Region), Country = str_squish(Country)) %>%
  distinct(Region, Country)

reported <- q1 %>%
  filter(!is.na(cat)) %>%
  group_by(Region, Country) %>%
  summarise(cat = first(cat), .groups = "drop")

by_country <- universe %>%
  left_join(reported, by = c("Region","Country")) %>%
  mutate(cat = ifelse(is.na(cat), "No country report", cat),
         cat = factor(cat, levels = cat_levels))

# ---------- Region x category ----------
tab_reg <- by_country %>%
  count(Region, cat, name = "n") %>%
  group_by(Region) %>%
  mutate(n_total = sum(n), share = n / n_total) %>%
  ungroup()

# ---------- World ----------
world_n <- sum(tab_reg %>% distinct(Region, n_total) %>% pull(n_total), na.rm = TRUE)
world_tab <- tab_reg %>%
  group_by(cat) %>% summarise(n = sum(n), .groups = "drop") %>%
  mutate(Region = "World", n_total = world_n, share = n / n_total)

tab_all <- bind_rows(world_tab, tab_reg) %>%
  mutate(
    Region = factor(Region, levels = region_levels_for_factor),
    cat    = factor(as.character(cat), levels = cat_levels)
  ) %>% arrange(Region, cat)

# ---------- plotting ----------
n_labels <- tab_all %>% distinct(Region, n_total)

p <- ggplot(tab_all, aes(x = Region, y = share, fill = cat)) +
  geom_col(width = 0.8) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = cat_cols, drop = FALSE, name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = "Progress in the establishment of national breed inventories (2024)",
       x = NULL, y = "Share of countries") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 55, 10, 10)
  ) +
  geom_text(data = n_labels,
            aes(x = Region, y = 1.005, label = n_total),
            inherit.aes = FALSE, hjust = 0, size = 3.6)

ggsave(out_png, p, width = 11, height = 7, dpi = 300)
ggsave(out_pdf, p, width = 11, height = 7)

# ---------- Excel (multi-sheet) ----------
wide_pct <- tab_all %>%
  mutate(share_pct = round(share*100, 1)) %>%
  select(Region, Category = cat, share_pct) %>%
  tidyr::pivot_wider(names_from = Category, values_from = share_pct) %>%
  arrange(Region)

wb <- createWorkbook()
addWorksheet(wb, "By_Region_Long")
addWorksheet(wb, "By_Region_Wide")
addWorksheet(wb, "World_and_Regions")
addWorksheet(wb, "Universe_vs_Reported")

writeData(wb, "By_Region_Long",
          tab_all %>% arrange(Region, cat) %>%
            mutate(share_pct = round(share*100,1)) %>%
            select(Region, Category = cat, Countries_in_category = n,
                   Total_countries = n_total, Percent = share_pct))
writeData(wb, "By_Region_Wide", wide_pct)
writeData(wb, "World_and_Regions", tab_all)
writeData(wb, "Universe_vs_Reported",
          universe %>% count(Region, name = "n_universe") %>%
            left_join(reported %>% count(Region, name = "n_reported"), by="Region") %>%
            mutate(n_reported = coalesce(n_reported, 0L),
                   n_no_report = pmax(n_universe - n_reported, 0L)) %>%
            arrange(Region))

saveWorkbook(wb, out_xlsx, overwrite = TRUE)

message("Saved:\n  ", out_png, "\n  ", out_pdf, "\n  ", out_xlsx)







FIGURE 3B2
# ===================== FIGURE 3B2 (2024) =====================
# Characterization activities for the big five species – frequency of responses
# Input : C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx
# Output: PNG/PDF figure + multi-sheet Excel with all tables
# =============================================================

library(readxl); library(dplyr); library(stringr); library(tidyr)
library(ggplot2); library(openxlsx); library(grid)

infile   <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_png  <- "C:/Users/LENOVO/Documents/Fig3B2_2024.png"
out_pdf  <- "C:/Users/LENOVO/Documents/Fig3B2_2024.pdf"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3B2_2024_tables.xlsx"

# ---------- helpers ----------
`%||%` <- function(x, y) if (is.null(x)) y else x
pick_col <- function(df, candidates){
  nm <- tolower(names(df))
  for (c in tolower(candidates)) {
    hit <- which(nm == c); if (length(hit)) return(names(df)[hit[1]])
  }
  NA_character_
}
norm_region <- function(x){
  x <- str_squish(as.character(x))
  x <- ifelse(x %in% c("South west Pacific","South-west Pacific","South West Pacific"),
              "Southwest Pacific", x)
  x <- ifelse(x %in% c("Europe and the Caucasus","Europe (including Caucasus)"),
              "Europe", x)
  x <- ifelse(startsWith(x,"Near"), "Near East", x)
  x <- ifelse(startsWith(x,"Latin Ame"), "Latin America and the Caribbean", x)
  x
}

# Species (order shown in left column)
species_map <- tibble::tribble(
  ~code, ~species,
  "009","Cattle (specialized dairy)",
  "008","Cattle (specialized beef)",
  "007","Cattle (multipurpose)",
  "042","Sheep",
  "021","Goats",
  "037","Pigs",
  "010","Chickens"
)

# Flip order so Dairy at TOP, Chickens at BOTTOM
species_levels <- rev(species_map$species)
species_codes  <- species_map$code

# Activities (6 rows)
activity_levels <- c(
  "Phenotypic characterization",
  "Genetic diversity studies based on pedigree",
  "Molecular genetic diversity studies – between breeds",
  "Molecular genetic diversity studies – within breed",
  "Genetic variance component estimation",
  "Molecular genetic evaluation"
)

# Categories + colors (close to book)
cat_levels <- c("None","Low","Medium","High")
cat_cols   <- c("None"="#7F0000","Low"="#C62828","Medium"="#FDD835","High"="#66BB6A")

# Regional order (requested) — for display in the RIGHT panel
region_order_reg         <- c("Africa","Asia","Europe","Latin America and the Caribbean",
                              "Near East","North America","Southwest Pacific","World")
# With coord_flip(), first level appears at the bottom; reverse to put Africa at TOP:
region_levels_plot_right <- rev(region_order_reg)

# ---------- load ----------
d0 <- read_excel(infile)

# tolerant header detection
col_year   <- pick_col(d0, c("year"))
col_reg    <- pick_col(d0, c("region"))
col_cty    <- pick_col(d0, c("country"))
col_q      <- pick_col(d0, c("question_2024","question"))
col_subq   <- pick_col(d0, c("subquestion_2024","subquestion"))
col_ans    <- pick_col(d0, c("answer"))
col_code   <- pick_col(d0, c("code"))
col_tag    <- pick_col(d0, c("pdftag","pdftagnew"))
col_sp     <- pick_col(d0, c("specietag","speciestag","speciecode","species"))
col_subtxt <- pick_col(d0, c("subquestiontype_2014","subquestion_text","subquestiontype","questiontype_2014"))

if (any(is.na(c(col_reg,col_cty,col_ans,col_sp)))) {
  stop("Missing required columns. Found: ", paste(names(d0), collapse=", "))
}

df <- d0 %>%
  transmute(
    Year      = if (!is.na(col_year)) suppressWarnings(as.integer(.data[[col_year]])) else 2024L,
    Region    = norm_region(.data[[col_reg]]),
    Country   = as.character(.data[[col_cty]]),
    Question  = if (!is.na(col_q)) suppressWarnings(as.integer(.data[[col_q]])) else NA_integer_,
    SubQ      = if (!is.na(col_subq)) suppressWarnings(as.integer(.data[[col_subq]])) else NA_integer_,
    Answer    = as.character(.data[[col_ans]]),
    Code      = if (!is.na(col_code)) suppressWarnings(as.integer(.data[[col_code]])) else NA_integer_,
    pdftag    = if (!is.na(col_tag)) as.character(.data[[col_tag]]) else NA_character_,
    SpecieTag = str_pad(str_replace_all(as.character(.data[[col_sp]]), "\\D", ""), width = 3, pad = "0"),
    subtxt    = if (!is.na(col_subtxt)) as.character(.data[[col_subtxt]]) else NA_character_
  ) %>%
  filter(Year == 2024, SpecieTag %in% species_codes) %>%
  filter(is.na(Question) | Question == 6) # keep Q6 characterization rows

# ---- normalize category (None/Low/Medium/High) ----
df <- df %>%
  mutate(
    ans_key = str_to_lower(str_squish(Answer)),
    Category = case_when(
      str_detect(ans_key, "^none")   ~ "None",
      str_detect(ans_key, "^low")    ~ "Low",
      str_detect(ans_key, "^medium") ~ "Medium",
      str_detect(ans_key, "^high")   ~ "High",
      !is.na(Code) & Code == 0 ~ "None",
      !is.na(Code) & Code == 1 ~ "Low",
      !is.na(Code) & Code == 2 ~ "Medium",
      !is.na(Code) & Code == 3 ~ "High",
      TRUE ~ NA_character_
    ),
    Category = factor(Category, levels = cat_levels)
  ) %>% filter(!is.na(Category))

# ---- normalize activity ----
norm_activity <- function(txt, tag){
  t <- str_to_lower(str_squish(txt %||% ""))
  out <- case_when(
    str_detect(t, "phenotypic") ~ "Phenotypic characterization",
    str_detect(t, "based on pedigree|pedigree") ~ "Genetic diversity studies based on pedigree",
    str_detect(t, "between breed|between breeds") ~ "Molecular genetic diversity studies – between breeds",
    str_detect(t, "within breed|within-breed") ~ "Molecular genetic diversity studies – within breed",
    str_detect(t, "variance component") ~ "Genetic variance component estimation",
    str_detect(t, "genetic evaluation") ~ "Molecular genetic evaluation",
    TRUE ~ NA_character_
  )
  if (is.na(out) && !is.na(tag)) {
    id <- suppressWarnings(as.integer(stringr::str_match(tag, "^\\s*2024\\.2\\.6\\.(\\d+)")[,2]))
    if (!is.na(id)) out <- case_when(
      id == 3 ~ "Phenotypic characterization",
      id == 4 ~ "Genetic diversity studies based on pedigree",
      id == 5 ~ "Molecular genetic diversity studies – between breeds",
      id == 6 ~ "Molecular genetic diversity studies – within breed",
      id == 7 ~ "Genetic variance component estimation",
      id == 8 ~ "Molecular genetic evaluation",
      TRUE ~ NA_character_
    )
  }
  out
}

df <- df %>%
  mutate(
    Activity = mapply(norm_activity, subtxt, pdftag),
    Activity = factor(Activity, levels = activity_levels)
  ) %>% filter(!is.na(Activity)) %>%
  left_join(species_map, by = c("SpecieTag" = "code")) %>%
  mutate(species = factor(species, levels = species_levels))

# ---------- 1) SPECIES BREAKDOWN (world-wide) ----------
univ_species <- df %>% distinct(Activity, species, Country) %>%
  count(Activity, species, name = "n_univ")

species_long <- df %>%
  distinct(Activity, species, Country, Category) %>%
  count(Activity, species, Category, name = "n_cat") %>%
  right_join(expand_grid(Activity = factor(activity_levels, levels = activity_levels),
                         species  = factor(species_levels, levels = species_levels),
                         Category = factor(cat_levels, levels = cat_levels)),
             by = c("Activity","species","Category")) %>%
  mutate(n_cat = coalesce(n_cat, 0L)) %>%
  left_join(univ_species, by = c("Activity","species")) %>%
  mutate(share = ifelse(n_univ > 0, n_cat / n_univ, 0))

# ---------- 2) REGIONAL BREAKDOWN ----------
univ_region <- df %>%
  distinct(Activity, Region, Country, species) %>%
  count(Activity, Region, name = "n_univ") %>%
  group_by(Activity) %>% bind_rows(summarise(., Region = "World", n_univ = sum(n_univ), .groups="drop")) %>%
  ungroup()

region_long <- df %>%
  distinct(Activity, Region, Country, species, Category) %>%
  count(Activity, Region, Category, name = "n_cat") %>%
  group_by(Activity, Category) %>% bind_rows(summarise(., Region = "World", n_cat = sum(n_cat), .groups="drop")) %>%
  ungroup() %>%
  right_join(expand_grid(Activity = factor(activity_levels, levels = activity_levels),
                         Region   = factor(region_order_reg, levels = region_order_reg),
                         Category = factor(cat_levels, levels = cat_levels)),
             by = c("Activity","Region","Category")) %>%
  mutate(n_cat = coalesce(n_cat, 0L)) %>%
  left_join(univ_region, by = c("Activity","Region")) %>%
  mutate(share = ifelse(n_univ > 0, n_cat / n_univ, 0))

# ---------- 3) Build two independent panels ----------
scale_fill_activities <- scale_fill_manual(values = cat_cols, drop = FALSE, name = NULL)

# Left panel: Species breakdown
species_plot_df <- species_long %>%
  mutate(Activity = factor(Activity, levels = activity_levels),
         species  = factor(species, levels = species_levels),
         Category = factor(Category, levels = cat_levels))

p_species <- ggplot(species_plot_df, aes(x = species, y = share, fill = Category)) +
  geom_col(width = 0.75) +
  coord_flip(clip = "off") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     expand = expansion(mult = c(0,0.02))) +
  scale_fill_activities +
  facet_grid(Activity ~ ., scales = "free_y") +
  labs(x = NULL, y = "Share of country–species combinations",
       title = "Characterization activities for the big five species – frequency of responses (2024)",
       subtitle = "Species breakdown") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 9)
  )

# Right panel: Regional breakdown
region_plot_df <- region_long %>%
  mutate(Activity = factor(Activity, levels = activity_levels),
         Region   = factor(Region, levels = region_levels_plot_right),
         Category = factor(Category, levels = cat_levels))

p_region <- ggplot(region_plot_df, aes(x = Region, y = share, fill = Category)) +
  geom_col(width = 0.75) +
  coord_flip(clip = "off") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     expand = expansion(mult = c(0,0.02))) +
  scale_fill_activities +
  facet_grid(Activity ~ ., scales = "free_y") +
  labs(x = NULL, y = NULL, subtitle = "Regional breakdown") +
  theme_minimal(base_size = 11) +
  theme(
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 9)
  )

# Function to extract legend without cowplot/gridExtra
get_legend_grob <- function(p){
  gt <- ggplotGrob(p)
  idx <- which(sapply(gt$grobs, function(x) x$name) == "guide-box")
  if (length(idx)) gt$grobs[[idx]] else NULL
}

legend_g <- get_legend_grob(p_region)
p_region_noleg  <- p_region + theme(legend.position = "none")

# ---------- 4) Draw & save ----------
png(out_png, width = 12*96, height = 12*96, res = 96)
grid.newpage()
lay <- grid.layout(nrow = 2, ncol = 2,
                   heights = unit.c(unit(1, "null"), unit(0.10, "npc")),
                   widths  = unit.c(unit(1, "null"), unit(1, "null")))
pushViewport(viewport(layout = lay))
print(p_species, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p_region_noleg, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
if (!is.null(legend_g)) {
  pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1:2))
  grid.draw(legend_g)
  upViewport()
}
dev.off()

pdf(out_pdf, width = 12, height = 12)
grid.newpage()
pushViewport(viewport(layout = lay))
print(p_species, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p_region_noleg, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
if (!is.null(legend_g)) {
  pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1:2))
  grid.draw(legend_g)
  upViewport()
}
dev.off()

# ---------- 5) Export Excel ----------
species_wide <- species_long %>%
  mutate(Percent = round(100*share,1)) %>%
  select(Activity, species, Category, Percent) %>%
  pivot_wider(names_from = Category, values_from = Percent) %>%
  arrange(Activity, species)

region_wide <- region_long %>%
  mutate(Percent = round(100*share,1)) %>%
  select(Activity, Region, Category, Percent) %>%
  pivot_wider(names_from = Category, values_from = Percent) %>%
  arrange(Activity, match(Region, region_order_reg))

wb <- createWorkbook()
addWorksheet(wb, "Species_Long")
addWorksheet(wb, "Species_Wide")
addWorksheet(wb, "Region_Long")
addWorksheet(wb, "Region_Wide")
addWorksheet(wb, "Tag_Map")
addWorksheet(wb, "Meta")

writeData(wb, "Species_Long",
          species_long %>% mutate(Percent = round(100*share,1)) %>%
            arrange(Activity, species, Category))
writeData(wb, "Species_Wide", species_wide)
writeData(wb, "Region_Long",
          region_long %>% mutate(Percent = round(100*share,1)) %>%
            arrange(Activity, match(Region, region_order_reg), Category))
writeData(wb, "Region_Wide", region_wide)

writeData(wb, "Tag_Map",
          df %>% select(Year, Region, Country, species, Activity, Category, pdftag) %>%
            arrange(Activity, Region, Country, species))

writeData(wb, "Meta",
          bind_rows(
            univ_species %>% mutate(Type="Species", Region=NA_character_) %>%
              select(Type, Activity, Region, Species=species, `Denominator (n)` = n_univ),
            univ_region %>% mutate(Type="Region", Species=NA_character_) %>%
              select(Type, Activity, Region, Species, `Denominator (n)` = n_univ)
          ))

saveWorkbook(wb, out_xlsx, overwrite = TRUE)

message("Saved:\n  PNG  -> ", out_png,
        "\n  PDF  -> ", out_pdf,
        "\n  XLSX -> ", out_xlsx)





Table 3B4 
# =============================== TABLE 3B4 (2024) ===============================
# Characterization activities for the big five species — average scores
# Input : C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx
# Output: two PNG pages, one tall PNG, a 2-page PDF, and a multi-sheet Excel
# ===============================================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr); library(tidyr)
  library(ggplot2); library(openxlsx); library(grid)
})

# ---------- paths ----------
infile       <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_png1     <- "C:/Users/LENOVO/Documents/Table3B4_2024_p1.png"
out_png2     <- "C:/Users/LENOVO/Documents/Table3B4_2024_p2.png"
out_png_full <- "C:/Users/LENOVO/Documents/Table3B4_2024_full.png"
out_pdf      <- "C:/Users/LENOVO/Documents/Table3B4_2024.pdf"
out_xlsx     <- "C:/Users/LENOVO/Documents/Table3B4_2024_tables.xlsx"

# ---------- helpers ----------
`%||%` <- function(x, y) if (is.null(x)) y else x
blank  <- function(x) ifelse(is.na(x), "", x)

pick_col <- function(df, candidates){
  nm <- tolower(names(df))
  for (c in tolower(candidates)) {
    hit <- which(nm == c); if (length(hit)) return(names(df)[hit[1]])
  }
  NA_character_
}

norm_region <- function(x){
  x <- str_squish(as.character(x))
  x <- ifelse(x %in% c("South west Pacific","South-west Pacific","South West Pacific"),
              "Southwest Pacific", x)
  x <- ifelse(x %in% c("Europe and the Caucasus","Europe (including Caucasus)"),
              "Europe", x)
  x <- ifelse(startsWith(x,"Near"), "Near East", x)
  x <- ifelse(startsWith(x,"Latin Ame"), "Latin America and the Caribbean", x)
  x
}

# Big seven species (codes + display order)
species_map <- tibble::tribble(
  ~code, ~species,
  "009","Cattle (specialized dairy)",
  "008","Cattle (specialized beef)",
  "007","Cattle (multipurpose)",
  "042","Sheep",
  "021","Goats",
  "037","Pigs",
  "010","Chickens"
)
species_levels <- species_map$species
species_codes  <- species_map$code

# Activities (Q6 subquestions 3..8) in the book’s order
activity_levels <- c(
  "Phenotypic characterization",
  "Genetic diversity studies based on pedigree",
  "Molecular genetic diversity studies – between breeds",
  "Molecular genetic diversity studies – within breed",
  "Genetic variance component estimation",
  "Molecular genetic evaluation"
)

# Category → score
cat_levels <- c("None","Low","Medium","High")
score_map  <- c(None = 0, Low = 1, Medium = 2, High = 3)

# Region display order (columns in the table/figure)
region_levels <- c("Africa","Asia","Southwest Pacific","Europe",
                   "Latin America and the Caribbean","North America","Near East","World")

# ---------- load & detect columns ----------
d0 <- read_excel(infile)

col_year   <- pick_col(d0, c("year"))
col_reg    <- pick_col(d0, c("region"))
col_cty    <- pick_col(d0, c("country"))
col_q      <- pick_col(d0, c("question_2024","question"))
col_ans    <- pick_col(d0, c("answer"))
col_code   <- pick_col(d0, c("code"))
col_tag    <- pick_col(d0, c("pdftag","pdftagnew"))
col_sp     <- pick_col(d0, c("specietag","speciestag","speciecode","species"))
col_subtxt <- pick_col(d0, c("subquestiontype_2014","subquestion_text","subquestiontype","questiontype_2014"))

if (any(is.na(c(col_reg,col_cty,col_q,col_ans,col_sp,col_tag)))) {
  stop("Missing required columns. Found: ", paste(names(d0), collapse=", "))
}

df0 <- d0 %>%
  transmute(
    Year      = if (!is.na(col_year)) suppressWarnings(as.integer(.data[[col_year]])) else 2024L,
    Region    = norm_region(.data[[col_reg]]),
    Country   = as.character(.data[[col_cty]]),
    Question  = suppressWarnings(as.integer(.data[[col_q]])),
    Answer    = as.character(.data[[col_ans]]),
    Code      = suppressWarnings(as.integer(.data[[col_code]])),
    pdftag    = as.character(.data[[col_tag]]),
    SpecieTag = str_pad(str_replace_all(as.character(.data[[col_sp]]), "\\D", ""), width = 3, pad = "0"),
    subtxt    = if (!is.na(col_subtxt)) as.character(.data[[col_subtxt]]) else NA_character_
  ) %>%
  filter(Year == 2024, Question == 6, SpecieTag %in% species_codes)

# Map species labels
df0 <- df0 %>%
  left_join(species_map, by = c("SpecieTag" = "code")) %>%
  mutate(species = factor(species, levels = species_levels))

# Map category → score
df0 <- df0 %>%
  mutate(
    ans_key = str_to_lower(str_squish(Answer)),
    Category = case_when(
      str_detect(ans_key, "^none")   ~ "None",
      str_detect(ans_key, "^low")    ~ "Low",
      str_detect(ans_key, "^medium") ~ "Medium",
      str_detect(ans_key, "^high")   ~ "High",
      !is.na(Code) & Code == 0 ~ "None",
      !is.na(Code) & Code == 1 ~ "Low",
      !is.na(Code) & Code == 2 ~ "Medium",
      !is.na(Code) & Code == 3 ~ "High",
      TRUE ~ NA_character_
    ),
    score = unname(score_map[Category])
  ) %>%
  filter(!is.na(score))

# Map activity from subquestion text or pdftag (3..8)
norm_activity <- function(txt, tag){
  t <- str_to_lower(str_squish(txt %||% ""))
  out <- case_when(
    str_detect(t, "phenotypic") ~ "Phenotypic characterization",
    str_detect(t, "based on pedigree|pedigree") ~ "Genetic diversity studies based on pedigree",
    str_detect(t, "between breed|between breeds") ~ "Molecular genetic diversity studies – between breeds",
    str_detect(t, "within breed|within-breed") ~ "Molecular genetic diversity studies – within breed",
    str_detect(t, "variance component") ~ "Genetic variance component estimation",
    str_detect(t, "genetic evaluation") ~ "Molecular genetic evaluation",
    TRUE ~ NA_character_
  )
  if (is.na(out) && !is.na(tag)) {
    id <- suppressWarnings(as.integer(stringr::str_match(tag, "^\\s*2024\\.2\\.6\\.(\\d+)")[,2]))
    if (!is.na(id)) out <- dplyr::case_when(
      id == 3 ~ "Phenotypic characterization",
      id == 4 ~ "Genetic diversity studies based on pedigree",
      id == 5 ~ "Molecular genetic diversity studies – between breeds",
      id == 6 ~ "Molecular genetic diversity studies – within breed",
      id == 7 ~ "Genetic variance component estimation",
      id == 8 ~ "Molecular genetic evaluation",
      TRUE ~ NA_character_
    )
  }
  out
}

df <- df0 %>%
  mutate(Activity = mapply(norm_activity, subtxt, pdftag)) %>%
  filter(!is.na(Activity)) %>%
  mutate(Activity = factor(Activity, levels = activity_levels))

# ---------- averages ----------
# Per-country score then regional average of countries
country_avg <- df %>%
  group_by(Activity, species, Region, Country) %>%
  summarise(score_country = mean(score, na.rm = TRUE), .groups = "drop")

reg_avg <- country_avg %>%
  group_by(Activity, species, Region) %>%
  summarise(avg_score = mean(score_country, na.rm = TRUE),
            n_countries = n_distinct(Country), .groups = "drop")

# World (average of ALL countries, not regional means)  **FIXED**
world_avg <- df %>%
  group_by(Activity, species, Country) %>%                       # 1) per-country score
  summarise(score_country = mean(score, na.rm = TRUE), .groups = "drop") %>%
  group_by(Activity, species) %>%                                # 2) regroup before averaging
  summarise(avg_score   = mean(score_country, na.rm = TRUE),
            n_countries = n_distinct(Country), .groups = "drop") %>%
  mutate(Region = factor("World", levels = "World")) %>%
  select(Activity, species, Region, avg_score, n_countries)

# Combine region + world
heat_long <- bind_rows(reg_avg, world_avg) %>%
  mutate(
    Activity = factor(Activity, levels = activity_levels),
    species  = factor(species,  levels = species_levels),
    Region   = factor(as.character(Region), levels = region_levels)
  )

# ---------- plotting ----------
tile_theme <- theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        strip.placement = "outside",
        strip.text.y = element_text(face = "bold"))

# Palette and mid-bin labels (six bins across 0–3)
pal  <- c("#7F0000","#C62828","#EF6C00","#FDD835","#7CB342","#1B5E20")
brks <- seq(0.25, 2.75, by = 0.5)                       # 6 break positions
lbls <- c("0–0.5","0.5–1","1–1.5","1.5–2","2–2.5","2.5–3")  # 6 labels

scale_fill_score <- scale_fill_gradientn(
  colours = pal, limits = c(0,3),
  breaks = brks, labels = lbls,
  name = NULL,
  guide = guide_colorbar(barheight = unit(5, "lines"), ticks = TRUE)
)

make_page_plot <- function(df_page, subtitle_text=""){
  ggplot(df_page,
         aes(x = Region, y = species, fill = avg_score)) +
    geom_tile(color = "grey90", linewidth = 0.2) +
    facet_grid(Activity ~ ., scales = "free_y", switch = "y") +
    scale_y_discrete(limits = rev(levels(df_page$species))) +
    scale_x_discrete(drop = FALSE) +
    scale_fill_score +
    tile_theme +
    labs(subtitle = subtitle_text)
}

# Split activities into two pages (like the scanned sample)
page1_acts <- activity_levels[1:3]
page2_acts <- activity_levels[4:6]

p1 <- make_page_plot(filter(heat_long, Activity %in% page1_acts),
                     "Characterization activities for the big five species — average scores (Page 1)")
p2 <- make_page_plot(filter(heat_long, Activity %in% page2_acts),
                     "Characterization activities for the big five species — average scores (Cont.)")

# ---------- save images ----------
ggsave(out_png1,  p1, width = 11, height = 9, units = "in", dpi = 300)
ggsave(out_png2,  p2, width = 11, height = 9, units = "in", dpi = 300)
ggsave(out_png_full,
       make_page_plot(heat_long, "Characterization activities — average scores (all rows)"),
       width = 11, height = 18, units = "in", dpi = 300)

# ---------- 2-page PDF ----------
pdf(out_pdf, width = 11, height = 9)
grid.newpage(); grid.draw(ggplotGrob(p1))
grid.newpage(); grid.draw(ggplotGrob(p2))
dev.off()

# ---------- Excel ----------
wide_matrix <- heat_long %>%
  mutate(avg_score = round(avg_score, 2)) %>%
  select(Activity, species, Region, avg_score) %>%
  pivot_wider(names_from = Region, values_from = avg_score) %>%
  arrange(match(Activity, activity_levels), match(species, species_levels))

wb <- createWorkbook()
addWorksheet(wb, "Long")
addWorksheet(wb, "Wide")
addWorksheet(wb, "ByRegion_n")

writeData(wb, "Long",
          heat_long %>% arrange(Activity, species, Region) %>%
            mutate(avg_score = round(avg_score, 3)))
writeData(wb, "Wide", wide_matrix)
writeData(wb, "ByRegion_n",
          reg_avg %>% select(Activity, species, Region, n_countries) %>%
            arrange(Activity, species, Region))

saveWorkbook(wb, out_xlsx, overwrite = TRUE)

message("Saved:\n  PNGs -> ", out_png1, " & ", out_png2,
        "\n  Tall  -> ", out_png_full,
        "\n  PDF  -> ", out_pdf,
        "\n  XLSX -> ", out_xlsx)




Figure 3B2
# ===================== FIGURE 3B3 (2024) =====================
# Characterization activities for "minor" species – frequency of responses
# =============================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr); library(tidyr)
  library(ggplot2); library(openxlsx); library(grid); library(purrr)
})

# -------- paths --------
infile   <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_dir  <- "C:/Users/LENOVO/Documents"
out_png_combined <- file.path(out_dir, "Fig3B3_minor_species_2024_combined.png")
out_pdf_combined <- file.path(out_dir, "Fig3B3_minor_species_2024_combined.pdf")
out_xlsx <- file.path(out_dir, "Fig3B3_minor_species_2024_tables.xlsx")

# -------- helpers --------
`%||%` <- function(x, y) if (is.null(x)) y else x
pick_col <- function(df, candidates){
  nm <- tolower(names(df))
  for (c in tolower(candidates)) {
    hit <- which(nm == c); if (length(hit)) return(names(df)[hit[1]])
  }
  NA_character_
}
norm_region <- function(x){
  x <- str_squish(as.character(x))
  x <- ifelse(x %in% c("South west Pacific","South-west Pacific","South West Pacific"),
              "Southwest Pacific", x)
  x <- ifelse(x %in% c("Europe and the Caucasus","Europe (including Caucasus)"),
              "Europe", x)
  x <- ifelse(startsWith(x,"Near"), "Near East", x)
  x <- ifelse(startsWith(x,"Latin Ame"), "Latin America and the Caribbean", x)
  x
}

# -------- minor species --------
minor_species_map <- tibble::tribble(
  ~code, ~species,
  "002","Alpacas","003","Asses","005","Buffaloes","004","Bactrian camels",
  "013","Deer","015","Dromedaries","017","Ducks","022","Geese",
  "023","Guinea fowls","024","Guinea pigs","026","Horses","027","Llamas",
  "028","Managed bee","029","Mithun","030","Muscovy ducks","033","Ostriches",
  "038","Pigeons","040","Quails","041","Rabbits","044","Turkeys","046","Yaks"
)
species_levels <- minor_species_map$species
species_codes  <- minor_species_map$code

# -------- activities --------
activity_levels <- c(
  "Phenotypic characterization",
  "Genetic diversity studies based on pedigree",
  "Molecular genetic diversity studies – between breeds",
  "Molecular genetic diversity studies – within breed",
  "Genetic variance component estimation",
  "Molecular genetic evaluation"
)

# -------- categories --------
cat_levels <- c("No response","None","Low","Medium","High")
cat_cols   <- c(
  "No response"="#BFBFBF",
  "None"       ="#7F0000",
  "Low"        ="#C62828",
  "Medium"     ="#FDD835",
  "High"       ="#66BB6A"
)

# -------- load data --------
d0 <- read_excel(infile)

col_year   <- pick_col(d0, c("year"))
col_reg    <- pick_col(d0, c("region"))
col_cty    <- pick_col(d0, c("country"))
col_q      <- pick_col(d0, c("question_2024","question"))
col_ans    <- pick_col(d0, c("answer"))
col_code   <- pick_col(d0, c("code"))
col_tag    <- pick_col(d0, c("pdftag","pdftagnew"))
col_sp     <- pick_col(d0, c("specietag","speciestag","speciecode","species"))
col_subtxt <- pick_col(d0, c("subquestion_text","subquestiontype_2014","subquestiontype"))

df0 <- d0 %>%
  transmute(
    Year      = if (!is.na(col_year)) suppressWarnings(as.integer(.data[[col_year]])) else 2024L,
    Region    = norm_region(.data[[col_reg]]),
    Country   = as.character(.data[[col_cty]]),
    Question  = suppressWarnings(as.integer(.data[[col_q]])),
    Answer    = as.character(.data[[col_ans]]),
    Code      = suppressWarnings(as.integer(.data[[col_code]])),
    pdftag    = as.character(.data[[col_tag]]),
    SpecieTag = str_pad(str_replace_all(as.character(.data[[col_sp]]), "\\D", ""), width = 3, pad = "0"),
    subtxt    = if (!is.na(col_subtxt)) as.character(.data[[col_subtxt]]) else NA_character_
  ) %>%
  filter(Year == 2024, Question == 6, SpecieTag %in% species_codes) %>%
  left_join(minor_species_map, by = c("SpecieTag" = "code")) %>%
  mutate(species = factor(species, levels = species_levels))

# Activity mapping
norm_activity <- function(txt, tag){
  t <- str_to_lower(str_squish(txt %||% ""))
  out <- case_when(
    str_detect(t, "phenotypic") ~ "Phenotypic characterization",
    str_detect(t, "based on pedigree") ~ "Genetic diversity studies based on pedigree",
    str_detect(t, "between breed") ~ "Molecular genetic diversity studies – between breeds",
    str_detect(t, "within breed") ~ "Molecular genetic diversity studies – within breed",
    str_detect(t, "variance component") ~ "Genetic variance component estimation",
    str_detect(t, "genetic evaluation") ~ "Molecular genetic evaluation",
    TRUE ~ NA_character_
  )
  if (is.na(out) && !is.na(tag)) {
    id <- suppressWarnings(as.integer(stringr::str_match(tag, "^2024\\.2\\.6\\.(\\d+)")[,2]))
    if (!is.na(id)) out <- case_when(
      id == 3 ~ "Phenotypic characterization",
      id == 4 ~ "Genetic diversity studies based on pedigree",
      id == 5 ~ "Molecular genetic diversity studies – between breeds",
      id == 6 ~ "Molecular genetic diversity studies – within breed",
      id == 7 ~ "Genetic variance component estimation",
      id == 8 ~ "Molecular genetic evaluation",
      TRUE ~ NA_character_
    )
  }
  out
}

df1 <- df0 %>%
  mutate(Activity = mapply(norm_activity, subtxt, pdftag),
         Activity = factor(Activity, levels = activity_levels)) %>%
  filter(!is.na(Activity)) %>%
  mutate(
    ans_key = str_to_lower(str_squish(Answer)),
    Category = case_when(
      str_detect(ans_key, "^none")   ~ "None",
      str_detect(ans_key, "^low")    ~ "Low",
      str_detect(ans_key, "^medium") ~ "Medium",
      str_detect(ans_key, "^high")   ~ "High",
      !is.na(Code) & Code == 0 ~ "None",
      !is.na(Code) & Code == 1 ~ "Low",
      !is.na(Code) & Code == 2 ~ "Medium",
      !is.na(Code) & Code == 3 ~ "High",
      TRUE ~ NA_character_
    )
  )

# Universe = species × countries × activities
universe_sc <- df1 %>% distinct(species, Country)
grid_sca <- universe_sc %>% crossing(Activity = activity_levels)

sca_cat <- df1 %>%
  group_by(species, Country, Activity) %>%
  summarise(Category = first(na.omit(Category)), .groups="drop")

sca_full <- grid_sca %>%
  left_join(sca_cat, by=c("species","Country","Activity")) %>%
  mutate(Category = if_else(is.na(Category),"No response",Category),
         Category = factor(Category, levels=cat_levels))

# Shares
species_long <- sca_full %>%
  count(Activity, species, Category, name="n") %>%
  group_by(Activity, species) %>%
  mutate(share = n/sum(n)) %>% ungroup()

# -------- plotting function --------
plot_one_activity <- function(act){
  df <- species_long %>% filter(Activity==act)
  p <- ggplot(df, aes(x=species,y=share,fill=Category)) +
    geom_col(width=0.75) +
    coord_flip() +
    scale_y_continuous(labels=scales::percent_format(accuracy=1)) +
    scale_fill_manual(values=cat_cols,drop=FALSE,name=NULL) +
    labs(x=NULL,y="Share of countries", title=paste("Minor species –", act)) +
    theme_minimal(base_size=11)+
    theme(legend.position="bottom",panel.grid.major.y=element_blank())
  outfile <- file.path(out_dir, paste0("Fig3B3_",gsub("[^A-Za-z0-9]+","_",act),".png"))
  ggsave(outfile,p,width=8,height=10,dpi=300)
  return(outfile)
}

# Save all 6
activity_files <- map_chr(activity_levels, plot_one_activity)

# Combined
p_combined <- ggplot(species_long, aes(x=species,y=share,fill=Category))+
  geom_col(width=0.75)+coord_flip()+
  scale_y_continuous(labels=scales::percent_format(accuracy=1))+
  scale_fill_manual(values=cat_cols,drop=FALSE,name=NULL)+
  facet_wrap(~Activity,ncol=2,scales="free_y")+
  labs(x=NULL,y="Share of countries",
       title="Characterization activities for minor species – 2024")+
  theme_minimal(base_size=11)+
  theme(legend.position="bottom",panel.grid.major.y=element_blank())

ggsave(out_png_combined,p_combined,width=12,height=16,dpi=300)
ggsave(out_pdf_combined,p_combined,width=12,height=16)

# -------- Excel --------
wb <- createWorkbook()
addWorksheet(wb,"Species_Long")
writeData(wb,"Species_Long",species_long %>% mutate(Percent=round(100*share,1)))

# Shorten names for sheets
short_names <- c("Phenotypic","Pedigree","Mol_Between","Mol_Within","Variance","Evaluation")
for (i in seq_along(activity_levels)){
  act <- activity_levels[i]; wnm <- short_names[i]
  addWorksheet(wb,wnm)
  wide_tab <- species_long %>% filter(Activity==act) %>%
    mutate(Percent=round(100*share,1)) %>%
    select(species,Category,Percent) %>%
    pivot_wider(names_from=Category,values_from=Percent)
  writeData(wb,wnm,wide_tab)
}

saveWorkbook(wb,out_xlsx,overwrite=TRUE)

message("Saved PNGs -> ",paste(activity_files,collapse="\n "),
        "\nCombined PNG -> ",out_png_combined,
        "\nCombined PDF -> ",out_pdf_combined,
        "\nExcel -> ",out_xlsx)





TABLE 3C1
# ===================== TABLE 3C1 (2024) — like the 2014 figure =====================
# Proportion of countries reporting the existence of breeding programmes (Sec 2, Q10)
# Regions (2024): Africa, Asia, Europe, Latin America and the Caribbean,
#                 Near East, North America, Southwest Pacific, World
# Output sheets:
#   - Table3C1_likeFigure_2024  (exact column headings/structure as the book)
#   - Stakeholders_byRegionSpecie
#   - Country_Species_Binary (QA)
# ================================================================================

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(openxlsx)

infile   <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Table3C1_2024_with_Stakeholders.xlsx"

# ---------- helpers ----------
norm_region <- function(x){
  x <- str_squish(as.character(x))
  x <- ifelse(x %in% c("South west Pacific","South-west Pacific","South West Pacific"),
              "Southwest Pacific", x)
  x <- ifelse(x %in% c("Europe and the Caucasus","Europe (including Caucasus)"),
              "Europe", x)
  x <- ifelse(startsWith(x, "Near"), "Near East", x)
  x <- ifelse(startsWith(x, "Latin Ame"), "Latin America and the Caribbean", x)
  x
}

# robust header finder
pick_col <- function(df, exact = character(), regex = NULL){
  nm  <- names(df); low <- tolower(nm)
  for (c in tolower(exact)) {
    hit <- which(low == c)
    if (length(hit)) return(nm[hit[1]])
  }
  if (!is.null(regex)) {
    hit <- which(grepl(regex, low, perl = TRUE))
    if (length(hit)) return(nm[hit[1]])
  }
  NA_character_
}

# Species mapping (codes → the 7 headings in the table)
spec_map <- c(
  "009" = "Dairy cattle",
  "008" = "Beef cattle",
  "007" = "Multipurpose cattle",
  "042" = "Sheep",
  "021" = "Goats",
  "037" = "Pigs",
  "010" = "Chickens"
)
species_headings <- c("Dairy cattle","Beef cattle","Multipurpose cattle",
                      "Sheep","Goats","Pigs","Chickens")

# 2024 region order (like you requested)
region_order <- c("Africa","Asia","Europe","Latin America and the Caribbean",
                  "Near East","North America","Southwest Pacific","World")

# Stakeholder canonical labels (you requested these)
stake_levels <- c(
  "Government",
  "Livestock keepers organized at community level",
  "Breeders’ associations or cooperatives",
  "National commercial companies",
  "External commercial companies",
  "Non-governmental organizations"
)
# Normalize various phrasings to the six labels
norm_stake <- function(x){
  t <- str_squish(as.character(x))
  tl <- tolower(t)
  case_when(
    str_detect(tl, "^government") ~ "Government",
    str_detect(tl, "livestock.*keeper|community") ~
      "Livestock keepers organized at community level",
    str_detect(tl, "breeder|co-?op|cooperativ") ~
      "Breeders’ associations or cooperatives",
    str_detect(tl, "external.*commercial|foreign.*company") ~
      "External commercial companies",
    str_detect(tl, "national.*commercial|private.*national") ~
      "National commercial companies",
    str_detect(tl, "non-?government|\\bngo\\b") ~
      "Non-governmental organizations",
    TRUE ~ t  # leave as-is if already clean
  )
}

# ---------- load ----------
d0 <- read_excel(infile)

# detect relevant columns
col_year    <- pick_col(d0, exact = "year")
col_region  <- pick_col(d0, exact = "region")
col_country <- pick_col(d0, exact = "country")
col_section <- pick_col(d0, exact = "section")
col_q       <- pick_col(d0, exact = c("question_2024","question"))
col_answer  <- pick_col(d0, exact = "answer")
col_specie  <- pick_col(d0, exact = c("specietag","speciestag","speciecode","species"),
                        regex = "specie.*tag|species")
# stakeholder column (your screenshot showed “_2estionType_2014”)
col_stake   <- pick_col(
  d0,
  exact = c("_2estiontype_2014","questiontype_2014","stakeholder","2estiontype_2014"),
  regex = "(^|_)\\??2?estiontype(_)?(2014)?$|question.*type.*2014|stakeholder"
)

if (any(is.na(c(col_region, col_country, col_answer, col_specie)))){
  stop("Missing a required column. Found headers: ", paste(names(d0), collapse = ", "))
}

# ---------- keep Section 2 / Question 10 (2024) ----------
df <- d0 %>%
  transmute(
    Year     = if (!is.na(col_year)) !!sym(col_year) else 2024,
    Region   = norm_region(!!sym(col_region)),
    Country  = str_squish(as.character(!!sym(col_country))),
    Section  = if (!is.na(col_section)) !!sym(col_section) else NA,
    Question = if (!is.na(col_q)) !!sym(col_q) else NA,
    Answer   = !!sym(col_answer),
    SpecieRaw= !!sym(col_specie),
    StakeRaw = if (!is.na(col_stake)) !!sym(col_stake) else NA
  ) %>%
  filter(Year == 2024, Section %in% c("2",2), as.integer(Question) == 10)

# species name
df <- df %>%
  mutate(
    SpecieTag = str_pad(str_extract(as.character(SpecieRaw), "\\d{1,3}$"), 3, pad = "0"),
    Species = spec_map[SpecieTag],
    # allow recovery from text if code missing
    Species = case_when(
      !is.na(Species) ~ Species,
      str_detect(str_to_lower(SpecieRaw), "specialized\\s*dairy|dairy\\s*cattle") ~ "Dairy cattle",
      str_detect(str_to_lower(SpecieRaw), "specialized\\s*beef|beef\\s*cattle")   ~ "Beef cattle",
      str_detect(str_to_lower(SpecieRaw), "multipurpose|multi\\s*purpose")        ~ "Multipurpose cattle",
      str_detect(str_to_lower(SpecieRaw), "\\bsheep\\b")                           ~ "Sheep",
      str_detect(str_to_lower(SpecieRaw), "\\bgoats?\\b")                          ~ "Goats",
      str_detect(str_to_lower(SpecieRaw), "\\bpigs?|swine\\b")                     ~ "Pigs",
      str_detect(str_to_lower(SpecieRaw), "\\bchickens?\\b")                       ~ "Chickens",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(Species %in% species_headings)

# yes/no
df <- df %>%
  mutate(
    ans_l = str_to_lower(str_squish(as.character(Answer))),
    YesNo = case_when(
      str_detect(ans_l, "^y") ~ 1,
      str_detect(ans_l, "^n") ~ 0,
      TRUE ~ NA_real_
    ),
    Stakeholder = if (!is.na(col_stake)) norm_stake(StakeRaw) else NA_character_
  )

# ---------- Country × Species binary (any programme yes?)
country_species <- df %>%
  group_by(Region, Country, Species) %>%
  summarise(has_prog = max(YesNo, na.rm = TRUE), .groups = "drop")

# ---------- Regional % (denominator = # countries that reported that species)
reg_pct <- country_species %>%
  group_by(Region, Species) %>%
  summarise(
    `Number of countries` = n_distinct(Country),
    pct_yes = 100 * mean(has_prog, na.rm = TRUE),
    .groups = "drop"
  )

# ---------- World row (from all countries, not regional means)
world_pct <- country_species %>%
  group_by(Species) %>%
  summarise(
    Region = "World",
    `Number of countries` = n_distinct(Country),
    pct_yes = 100 * mean(has_prog, na.rm = TRUE),
    .groups = "drop"
  )

reg_pct_all <- bind_rows(reg_pct, world_pct) %>%
  mutate(Region = factor(Region, levels = region_order)) %>%
  arrange(Region, match(Species, species_headings))

# ---------- Build the sheet exactly like the figure headings ----------
# First compute one “Number of countries” per Region (unique countries across all species)
n_countries_region <- country_species %>%
  group_by(Region) %>% summarise(`Number of countries` = n_distinct(Country), .groups = "drop")

# wide matrix with one row per Region
table_like <- reg_pct_all %>%
  mutate(pct_yes = round(pct_yes)) %>%       # figure shows whole %
  select(Region, Species, pct_yes) %>%
  pivot_wider(names_from = Species, values_from = pct_yes) %>%
  left_join(n_countries_region, by = "Region") %>%
  relocate(Region, `Number of countries`, all_of(species_headings)) %>%
  arrange(match(Region, region_order))

# ---------- Stakeholder table (per Region × Species × Stakeholder) ----------
stake_country <- df %>%
  filter(!is.na(Stakeholder) & Stakeholder != "") %>%
  group_by(Region, Country, Species, Stakeholder) %>%
  summarise(any_prog = max(YesNo, na.rm = TRUE), .groups = "drop")

stake_reg <- stake_country %>%
  group_by(Region, Species, Stakeholder) %>%
  summarise(
    `Number of countries` = n_distinct(Country),
    `Percent with programme` = 100 * mean(any_prog, na.rm = TRUE),
    .groups = "drop"
  )

stake_world <- stake_country %>%
  group_by(Species, Stakeholder) %>%
  summarise(
    Region = "World",
    `Number of countries` = n_distinct(Country),
    `Percent with programme` = 100 * mean(any_prog, na.rm = TRUE),
    .groups = "drop"
  )

stake_all <- bind_rows(stake_reg, stake_world) %>%
  mutate(
    Region = factor(Region, levels = region_order),
    Stakeholder = factor(Stakeholder, levels = stake_levels)
  ) %>%
  arrange(Region, Species, Stakeholder)

# ---------- Write Excel ----------
wb <- createWorkbook()

# Sheet 1: like the figure (2024 regions)
addWorksheet(wb, "Table3C1_likeFigure_2024")
writeData(wb, "Table3C1_likeFigure_2024", table_like)

# Sheet 2: stakeholders
addWorksheet(wb, "Stakeholders_byRegionSpecie")
writeData(wb, "Stakeholders_byRegionSpecie",
          stake_all %>%
            mutate(`Percent with programme` = round(`Percent with programme`, 1)))

# Sheet 3: QA (country × species binary)
addWorksheet(wb, "Country_Species_Binary")
writeData(wb, "Country_Species_Binary", country_species)

# tidy widths & number formats
setColWidths(wb, "Table3C1_likeFigure_2024", cols = 1:ncol(table_like), widths = "auto")
setColWidths(wb, "Stakeholders_byRegionSpecie", cols = 1:ncol(stake_all)+1, widths = "auto")
perc_cols <- match(species_headings, names(table_like))
addStyle(wb, "Table3C1_likeFigure_2024",
         createStyle(numFmt = "0"), rows = 2:(nrow(table_like)+1),
         cols = perc_cols, gridExpand = TRUE)

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved -> ", out_xlsx)



TABLE 3C2
# ===================== TABLE 3C2 (2024) =====================
# Proportion of countries reporting the existence of breeding programmes
# Species breakdown (World total)
# Input : C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx  (Section 2, Question 10)
# Output: Excel table + (optional) horizontal bar PNG/PDF
# ============================================================

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(openxlsx)

infile <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Table3C2_2024.xlsx"
out_png  <- "C:/Users/LENOVO/Documents/Table3C2_2024_bars.png"
out_pdf  <- "C:/Users/LENOVO/Documents/Table3C2_2024_bars.pdf"

# ---------- helpers ----------
norm_region <- function(x){
  x <- str_squish(as.character(x))
  x <- ifelse(x %in% c("South west Pacific","South-west Pacific","South West Pacific"),
              "Southwest Pacific", x)
  x <- ifelse(x %in% c("Europe and the Caucasus","Europe (including Caucasus)"),
              "Europe", x)
  x <- ifelse(startsWith(x, "Near"), "Near East", x)
  x <- ifelse(startsWith(x, "Latin Ame"), "Latin America and the Caribbean", x)
  x
}

pick_col <- function(df, exact = character(), regex = NULL){
  nm  <- names(df); low <- tolower(nm)
  for (c in tolower(exact)) {
    hit <- which(low == c)
    if (length(hit)) return(nm[hit[1]])
  }
  if (!is.null(regex)) {
    hit <- which(grepl(regex, low, perl = TRUE))
    if (length(hit)) return(nm[hit[1]])
  }
  NA_character_
}

# Full species code map (your list + big five)
sp_map <- c(
  "009"="Cattle (specialized dairy)",
  "008"="Cattle (specialized beef)",
  "007"="Cattle (multipurpose)",
  "042"="Sheep",
  "021"="Goats",
  "037"="Pigs",
  "010"="Chickens",
  "002"="Alpacas",
  "003"="Asses",
  "005"="Buffaloes",
  "004"="Bactrian camels",
  "013"="Deer",
  "015"="Dromedaries",
  "017"="Ducks",
  "022"="Geese",
  "023"="Guinea fowls",
  "024"="Guinea pigs",
  "026"="Horses",
  "027"="Llamas",
  "028"="Managed bee",
  "029"="Mithun",
  "030"="Muscovy ducks",
  "033"="Ostriches",
  "038"="Pigeons",
  "040"="Quails",
  "041"="Rabbits",
  "044"="Turkeys",
  "046"="Yaks"
)

# preferred species display order (big cattle first, then others alphabetically)
species_levels <- c(
  "Cattle (specialized dairy)","Cattle (specialized beef)","Cattle (multipurpose)",
  "Sheep","Goats","Pigs","Chickens",
  sort(setdiff(unname(sp_map), c(
    "Cattle (specialized dairy)","Cattle (specialized beef)","Cattle (multipurpose)",
    "Sheep","Goats","Pigs","Chickens"
  )))
)

# ---------- load ----------
d0 <- read_excel(infile)

col_year    <- pick_col(d0, exact="year")
col_region  <- pick_col(d0, exact="region")
col_country <- pick_col(d0, exact="country")
col_section <- pick_col(d0, exact="section")
col_q       <- pick_col(d0, exact=c("question_2024","question"))
col_answer  <- pick_col(d0, exact="answer")
# species identifier is messy—try tag first, then text
col_sp      <- pick_col(d0, exact=c("specietag","speciestag","speciecode","species"),
                        regex="specie.*tag|species")
stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_answer), !is.na(col_sp))

df <- d0 %>%
  transmute(
    Year     = if (!is.na(col_year)) !!sym(col_year) else 2024,
    Region   = norm_region(!!sym(col_region)),
    Country  = str_squish(as.character(!!sym(col_country))),
    Section  = if (!is.na(col_section)) !!sym(col_section) else NA,
    Question = if (!is.na(col_q)) !!sym(col_q) else NA,
    Answer   = !!sym(col_answer),
    SpRaw    = !!sym(col_sp)
  ) %>%
  filter(Year == 2024, Section %in% c("2",2), as.integer(Question) == 10)

# Robust species name:
df <- df %>%
  mutate(
    SpCode = str_pad(str_extract(as.character(SpRaw), "\\d{1,3}$"), 3, pad = "0"),
    Species = unname(sp_map[SpCode]),
    # fallback to text if code missing/unmapped
    Species = ifelse(is.na(Species) | Species=="NA",
                     str_to_sentence(str_squish(as.character(SpRaw))), Species)
  ) %>%
  filter(!is.na(Species) & Species != "")

# Normalize Yes/No
df <- df %>%
  mutate(ans_l = str_to_lower(str_squish(as.character(Answer))),
         flag_yes = ifelse(str_detect(ans_l, "^y"), 1L,
                           ifelse(str_detect(ans_l, "^n"), 0L, NA_integer_)))

# ---------- species (WORLD) tallies ----------
# Collapse stakeholder rows to a single country–species record: any "Yes" wins
by_cty_sp <- df %>%
  group_by(Country, Species) %>%
  summarise(any_yes = max(flag_yes, na.rm = TRUE), .groups = "drop")

# If a country had only NAs for a species (no clear yes/no), exclude it from denom
by_cty_sp <- by_cty_sp %>% filter(!is.infinite(any_yes))  # removes -Inf produced by all NA

tbl_world <- by_cty_sp %>%
  group_by(Species) %>%
  summarise(
    `Number of countries reporting presence` = n_distinct(Country),  # denominator
    `Countries with breeding programmes (>=1)` = sum(any_yes == 1L, na.rm = TRUE), # numerator
    .groups = "drop"
  ) %>%
  mutate(
    `Percentage of countries with breeding programmes (at least one)` =
      round(100 * `Countries with breeding programmes (>=1)` /
                  pmax(`Number of countries reporting presence`, 1), 1)
  ) %>%
  mutate(Species = factor(Species, levels = species_levels)) %>%
  arrange(Species)

# ---------- Excel ----------
wb <- createWorkbook()
addWorksheet(wb, "Table3C2_2024")
addWorksheet(wb, "Long_raw")

writeData(wb, "Table3C2_2024", tbl_world)
writeData(wb, "Long_raw",
          by_cty_sp %>% arrange(factor(Species, levels = species_levels), Country))

# Autosizes & simple header style
setColWidths(wb, "Table3C2_2024", cols = 1:ncol(tbl_world), widths = "auto")
addStyle(wb, "Table3C2_2024",
         createStyle(textDecoration = "bold"),
         rows = 1, cols = 1:ncol(tbl_world), gridExpand = TRUE)

saveWorkbook(wb, out_xlsx, overwrite = TRUE)

# ---------- Optional figure (horizontal bars like the book) ----------
bars_df <- tbl_world %>%
  mutate(Species = factor(Species, levels = rev(levels(Species))))

p <- ggplot(bars_df,
            aes(x = `Percentage of countries with breeding programmes (at least one)`,
                y = Species)) +
  geom_col(fill = "#69b3a2", height = 0.7) +
  geom_text(aes(label = `Percentage of countries with breeding programmes (at least one)`),
            hjust = -0.2, size = 3) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0,100,10),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Percent of countries", y = NULL,
       title = "Proportion of countries reporting the existence of breeding programmes (2024)\nSpecies breakdown (World)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank())

ggsave(out_png, p, width = 9, height = max(6, nrow(bars_df)*0.35), dpi = 300)
ggsave(out_pdf, p, width = 9, height = max(6, nrow(bars_df)*0.35))

cat("Saved:\n  Excel -> ", out_xlsx,
    "\n  PNG   -> ", out_png,
    "\n  PDF   -> ", out_pdf, "\n", sep = "")



TABLE 3C3
# ===================== TABLE 3C3 (2024) — FINAL NGO FIX =====================
# Extent of involvement of stakeholder groups as operators of breeding programmes
# Data: Section 2, Question 10  (C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx)
# Output: C:/Users/LENOVO/Documents/Table3C3_stakeholders_2024.xlsx
# ===========================================================================

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(openxlsx)
library(purrr)

infile   <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Table3C3_stakeholders_2024.xlsx"

# ---------- helpers ----------
norm_region <- function(x){
  x <- str_squish(as.character(x))
  x <- ifelse(x %in% c("South west Pacific","South-west Pacific","South West Pacific"),
              "Southwest Pacific", x)
  x <- ifelse(x %in% c("Europe and the Caucasus","Europe (including Caucasus)"),
              "Europe", x)
  x <- ifelse(startsWith(x,"Near"), "Near East", x)
  x <- ifelse(startsWith(x,"Latin Ame"), "Latin America and the Caribbean", x)
  x
}
pick_col <- function(df, exact = character(), regex = NULL){
  nm  <- names(df); low <- tolower(nm)
  for (c in tolower(exact)) { hit <- which(low == c); if (length(hit)) return(nm[hit[1]]) }
  if (!is.null(regex)) { hit <- which(grepl(regex, low, perl = TRUE)); if (length(hit)) return(nm[hit[1]]) }
  NA_character_
}

# Big seven (three cattle + four others)
big7_codes <- c("009","008","007","042","021","037","010")
big7_names <- c("Cattle (specialized dairy)","Cattle (specialized beef)","Cattle (multipurpose)",
                "Sheep","Goats","Pigs","Chickens")

# 2024 region order
region_levels <- c("Africa","Asia","Europe","Latin America and the Caribbean",
                   "Near East","North America","Southwest Pacific","World")

# Stakeholder headings (exact order/wording)
stake_levels <- c(
  "Government",
  "Livestock keepers organized at community level",
  "Breeders’ associations or cooperatives",
  "National commercial companies",
  "External commercial companies",
  "Non-governmental organizations",
  "Others"
)

# ---- Robust text normalization (handles Unicode hyphens/apostrophes) ----
normalize_text <- function(x){
  x <- tolower(as.character(x))
  # all kinds of dashes/hyphens -> space
  x <- gsub("[\u2010-\u2015\u2212\u2043\u00AD\\-]", " ", x, perl = TRUE)
  # different apostrophes/quotes -> remove
  x <- gsub("[\u2018\u2019\u201A\u201B\u0060\u00B4\u2032']", "", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  trimws(x)
}

# Canonical normalized stakeholder labels (so exact matches succeed first)
canon <- tibble::tibble(
  Label = stake_levels,
  Norm  = normalize_text(stake_levels)
)

# Accept a few common variants
variant_map <- tibble::tibble(
  Variant = c(
    "livestock keepers at community level",
    "breeders associations or cooperatives",
    "national commercial company",
    "external commercial company",
    "non governmental organizations",
    "non governmental organisation",
    "non governmental organisations",
    "ngos", "ngo"
  ),
  Label = c(
    "Livestock keepers organized at community level",
    "Breeders’ associations or cooperatives",
    "National commercial companies",
    "External commercial companies",
    "Non-governmental organizations",
    "Non-governmental organizations",
    "Non-governmental organizations",
    "Non-governmental organizations",
    "Non-governmental organizations"
  ),
  Norm = normalize_text(Variant)
)

# Vectorized mapper
norm_stake_vec <- function(x){
  t <- normalize_text(x)
  out <- rep(NA_character_, length(t))

  # 1) exact match against canonical normalized labels
  m1 <- match(t, canon$Norm)
  out[!is.na(m1)] <- canon$Label[m1]

  # 2) match against variant normalized list
  need <- which(is.na(out))
  if (length(need)) {
    m2 <- match(t[need], variant_map$Norm)
    ok  <- !is.na(m2)
    out[need[ok]] <- variant_map$Label[m2[ok]]
  }

  # 3) tolerant contains checks (only for remaining NAs)
  need <- which(is.na(out))
  if (length(need)) {
    tt <- t[need]
    set_if <- function(idx, label) out[need[idx & is.na(out[need])]] <<- label

    set_if(grepl("\\bgovernment\\b", tt),
           "Government")
    set_if(grepl("livestock\\s*keepers|community\\s*level", tt),
           "Livestock keepers organized at community level")
    set_if(grepl("breeder|cooperativ", tt),
           "Breeders’ associations or cooperatives")
    set_if(grepl("national\\s+commercial|domestic\\s+commercial", tt),
           "National commercial companies")
    set_if(grepl("external\\s+commercial|foreign\\s+commercial|based\\s+outside", tt),
           "External commercial companies")
    set_if(grepl("non\\s*governmental\\s+organ|\\bngos?\\b", tt),
           "Non-governmental organizations")
    set_if(grepl("\\bothers?\\b", tt),
           "Others")
  }
  out
}

# ---------- load ----------
d0 <- read_excel(infile)

col_year    <- pick_col(d0, exact="year")
col_region  <- pick_col(d0, exact="region")
col_country <- pick_col(d0, exact="country")
col_section <- pick_col(d0, exact="section")
col_q       <- pick_col(d0, exact=c("question_2024","question"))
col_answer  <- pick_col(d0, exact="answer")
col_sp      <- pick_col(d0, exact=c("specietag","speciestag","speciecode","species"),
                        regex="specie.*tag|species")
# Your screenshot shows this exact spellings in W: "2estionType_2014"
col_holder  <- pick_col(
  d0,
  exact = c("2estionType_2014","_2estionType_2014","questiontype_2014","questiontype",
            "cleaLabel_2edType_2","label_2edtype_2","label_2"),
  regex = "(type|label).*2014|stakeholder|operator|breeder|government|commercial|ngo"
)

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_answer), !is.na(col_sp))

# -------- filter rows FIRST and keep row index to scan within the same rows --------
rows_keep <- which(
  (if (!is.na(col_year)) d0[[col_year]] else 2024) == 2024 &
  d0[[col_section]] %in% c("2",2) &
  grepl("^\\s*10(\\.|$)", as.character(d0[[col_q]]))
)

d_sub <- d0[rows_keep, , drop = FALSE]

df0 <- tibble::tibble(
  Year     = if (!is.na(col_year)) d_sub[[col_year]] else 2024,
  Region   = norm_region(d_sub[[col_region]]),
  Country  = str_squish(as.character(d_sub[[col_country]])),
  Section  = d_sub[[col_section]],
  Question = d_sub[[col_q]],
  Answer   = d_sub[[col_answer]],
  SpRaw    = d_sub[[col_sp]],
  StakeRaw = if (!is.na(col_holder)) d_sub[[col_holder]] else NA_character_
)

# If StakeRaw is missing/mostly blank, scan *within these filtered rows* only
if (all(is.na(df0$StakeRaw)) || mean(is.na(df0$StakeRaw)) > 0.9) {
  text_cols <- names(d_sub)[map_lgl(d_sub, ~is.character(.x) || is.factor(.x))]
  text_cols <- setdiff(text_cols, c(col_answer, col_region, col_country, col_section, col_q, col_year))
  if (length(text_cols)) {
    scanned <- apply(d_sub[text_cols], 1, function(vec){
      v <- paste(na.omit(as.character(vec)), collapse = " | ")
      # return the *original label* if found in any text
      lab <- norm_stake_vec(v)[1]
      if (is.na(lab)) NA_character_ else lab
    })
    df0$StakeRaw <- scanned
  }
}

# -------- species mapping & keep big7 --------
df <- df0 %>%
  mutate(
    SpCode  = str_pad(str_extract(as.character(SpRaw), "\\d{1,3}$"), 3, pad = "0"),
    Species = case_when(
      SpCode %in% big7_codes ~ big7_names[match(SpCode, big7_codes)],
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Species))

# -------- stakeholder + yes/no (NOW robust) --------
df <- df %>%
  mutate(
    Stakeholder = norm_stake_vec(StakeRaw),
    ans_l = normalize_text(Answer),
    flag_yes = case_when(
      grepl("^y", ans_l) ~ 1L,
      grepl("^n", ans_l) ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(Stakeholder %in% stake_levels)

# ---------- QA: NGOs should now be non-zero ----------
cat("\n[QA] Stakeholder row counts after normalization:\n")
print(df %>% count(Stakeholder, sort = TRUE))

ngo_rows <- df %>% filter(Stakeholder == "Non-governmental organizations")
if (nrow(ngo_rows) > 0) {
  set.seed(42)
  cat("\n[QA] NGO sample rows (up to 10):\n")
  print(ngo_rows %>% 
          select(Region, Country, Species, StakeRaw, Answer) %>% 
          slice_sample(n = min(10, nrow(ngo_rows))))
} else {
  cat("\n[QA] NGO sample rows: 0 matched — check 'Map_Check' sheet for StakeRaw examples.\n")
}

# ---------- region×species percentages by stakeholder ----------
# Collapse duplicates per Region×Country×Species×Stakeholder
cty_sp_stake <- df %>%
  group_by(Region, Country, Species, Stakeholder) %>%
  summarise(
    any_yes = {
      v <- flag_yes
      if (all(is.na(v))) NA_integer_ else as.integer(max(v, na.rm = TRUE))
    },
    .groups = "drop"
  ) %>%
  filter(!is.na(any_yes))

# Denominators: countries that reported the species
denom <- df %>% distinct(Region, Country, Species)

# Percent per Region×Species×Stakeholder
reg_sp_stake_pct <- denom %>%
  group_by(Region, Species) %>%
  summarise(n_countries = n_distinct(Country), .groups = "drop") %>%
  left_join(
    cty_sp_stake %>%
      group_by(Region, Species, Stakeholder) %>%
      summarise(n_yes = sum(any_yes == 1L, na.rm = TRUE), .groups = "drop"),
    by = c("Region","Species")
  ) %>%
  mutate(n_yes = coalesce(n_yes, 0L),
         pct_yes = 100 * n_yes / pmax(n_countries, 1))

# Regional averages across the seven species
reg_avg_stake <- reg_sp_stake_pct %>%
  group_by(Region, Stakeholder) %>%
  summarise(Percent = mean(pct_yes, na.rm = TRUE), .groups = "drop") %>%
  mutate(Stakeholder = factor(Stakeholder, levels = stake_levels))

# World row (recompute from countries)
world_indep <- df %>%
  group_by(Country, Species, Stakeholder) %>%
  summarise(yes = {
              v <- flag_yes
              if (all(is.na(v))) NA_integer_ else as.integer(max(v, na.rm = TRUE))
            },
            .groups = "drop") %>%
  filter(!is.na(yes)) %>%
  group_by(Species, Stakeholder) %>%
  summarise(n_yes = sum(yes == 1L),
            n_countries = n_distinct(Country), .groups = "drop") %>%
  mutate(pct = 100 * n_yes / pmax(n_countries,1)) %>%
  group_by(Stakeholder) %>%
  summarise(Percent = mean(pct, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = "World",
         Stakeholder = factor(Stakeholder, levels = stake_levels))

# Number of countries per region (and World)
n_countries_region <- denom %>% distinct(Region, Country) %>% count(Region, name = "Number of countries")
world_n <- df %>% distinct(Country) %>% nrow()

# Final table (like the book)
final_tbl <- bind_rows(reg_avg_stake, world_indep) %>%
  mutate(Region = factor(Region, levels = region_levels)) %>%
  tidyr::pivot_wider(names_from = Stakeholder, values_from = Percent, values_fill = 0)

for (st in stake_levels) if (!st %in% names(final_tbl)) final_tbl[[st]] <- 0

final_tbl <- final_tbl %>%
  left_join(n_countries_region, by = "Region") %>%
  mutate(`Number of countries` = ifelse(as.character(Region)=="World", world_n, `Number of countries`)) %>%
  select(Region, `Number of countries`, all_of(stake_levels)) %>%
  arrange(Region) %>%
  mutate(across(all_of(stake_levels), ~round(.x, 0)))

# ---------- Excel ----------
wb <- createWorkbook()
addWorksheet(wb, "Stakeholder_3C3_2024")
addWorksheet(wb, "RegionSpecies_Pcts")
addWorksheet(wb, "Map_Check")

writeData(wb, "Stakeholder_3C3_2024", final_tbl)
writeData(wb, "RegionSpecies_Pcts",
          reg_sp_stake_pct %>%
            mutate(pct_yes = round(pct_yes, 1)) %>%
            arrange(factor(Region, levels = region_levels),
                    match(Species, big7_names), Stakeholder))
writeData(wb, "Map_Check",
          df %>%
            select(Region, Country, Species, Stakeholder, Answer, StakeRaw) %>%
            arrange(Region, Country, Species, Stakeholder))

setColWidths(wb, "Stakeholder_3C3_2024", cols = 1:ncol(final_tbl), widths = "auto")
addStyle(wb, "Stakeholder_3C3_2024",
         createStyle(textDecoration = "bold"),
         rows = 1, cols = 1:ncol(final_tbl), gridExpand = TRUE)

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
cat("Saved table to:\n", out_xlsx, "\n", sep = "")



TABLE 3C3 Species tables
# ===================== TABLE 3C3 (BY SPECIES, 2024) =====================
# Proportion of countries in which stakeholders operate breeding programmes
# Data: Section 2, Question 10  (C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx)
# Output: C:/Users/LENOVO/Documents/Table3C3_stakeholders_BY_SPECIES_2024.xlsx
# ========================================================================

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(openxlsx)

infile   <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Table3C3_stakeholders_BY_SPECIES_2024.xlsx"

# ---------- helpers ----------
norm_region <- function(x){
  x <- str_squish(as.character(x))
  x <- ifelse(x %in% c("South west Pacific","South-west Pacific","South West Pacific"),
              "Southwest Pacific", x)
  x <- ifelse(x %in% c("Europe and the Caucasus","Europe (including Caucasus)"),
              "Europe", x)
  x <- ifelse(startsWith(x,"Near"), "Near East", x)
  x <- ifelse(startsWith(x,"Latin Ame"), "Latin America and the Caribbean", x)
  x
}
pick_col <- function(df, exact = character(), regex = NULL){
  nm  <- names(df); low <- tolower(nm)
  for (c in tolower(exact)) { hit <- which(low == c); if (length(hit)) return(nm[hit[1]]) }
  if (!is.null(regex)) { hit <- which(grepl(regex, low, perl = TRUE)); if (length(hit)) return(nm[hit[1]]) }
  NA_character_
}

# Normalize text: lower, replace various hyphens/dashes, collapse spaces, strip accents
.normalize_text <- function(x){
  t <- tolower(as.character(x))
  # replace unicode dashes with hyphen
  t <- gsub("[\u2010\u2011\u2012\u2013\u2014\u2212]+", "-", t)
  # remove accents (if stringi available); otherwise skip
  if (requireNamespace("stringi", quietly = TRUE)) t <- stringi::stri_trans_general(t, "Latin-ASCII")
  # collapse multiple spaces
  t <- gsub("[[:space:]]+", " ", t)
  t <- str_squish(t)
  t
}

# Big seven species
big7_codes <- c("009","008","007","042","021","037","010")
big7_names <- c("Cattle (specialized dairy)","Cattle (specialized beef)","Cattle (multipurpose)",
                "Sheep","Goats","Pigs","Chickens")
species_levels <- big7_names

# 2024 region order
region_levels <- c("Africa","Asia","Europe","Latin America and the Caribbean",
                   "Near East","North America","Southwest Pacific","World")

# Stakeholder headings (fixed order)
stake_levels <- c(
  "Government",
  "Livestock keepers organized at community level",
  "Breeders’ associations or cooperatives",
  "National commercial companies",
  "External commercial companies",
  "Non-governmental organizations",
  "Others"
)

# --- Patterns for stakeholder detection (ORDER MATTERS: NGO BEFORE GOVERNMENT) ---
# We work on normalized text from .normalize_text()
stake_patterns <- list(
  # NGO: match multiple spellings and separators
  "Non-governmental organizations" = "\\bnon[- ]?governmental\\b|\\bnon[- ]?governmental organizations?\\b|\\bngo\\b|\\bnon[- ]?gov\\b",
  # Community livestock keepers
  "Livestock keepers organized at community level" = "livestock\\s*keepers|community\\s*level",
  # Breeders/coops
  "Breeders’ associations or cooperatives" = "breeder|cooperativ",
  # National (domestic) commercial
  "National commercial companies" = "\\bnational\\s+commercial\\b|\\bdomestic\\s+commercial\\b",
  # External/foreign commercial
  "External commercial companies" = "\\bexternal\\s+commercial\\b|\\bforeign\\s+commercial\\b|based\\s+outside",
  # Government — avoid “non-governmental”
  "Government" = "(?<!non[- ]?)\\bgovernment\\b|\\bgovt?\\b",
  # Others
  "Others" = "\\bother(s)?\\b"
)

# Vectorized stakeholder normalizer (uses normalized text and ordered patterns)
norm_stake_vec <- function(x){
  tvec <- .normalize_text(x)
  vapply(
    tvec,
    function(one){
      if (is.na(one) || one == "") return(NA_character_)
      for (nm in names(stake_patterns)) {
        if (grepl(stake_patterns[[nm]], one, perl = TRUE)) return(nm)
      }
      NA_character_
    },
    character(1)
  )
}

# ---------- load ----------
d0 <- read_excel(infile)

col_year    <- pick_col(d0, exact="year")
col_region  <- pick_col(d0, exact="region")
col_country <- pick_col(d0, exact="country")
col_section <- pick_col(d0, exact="section")
col_q       <- pick_col(d0, exact=c("question_2024","question"))
col_answer  <- pick_col(d0, exact="answer")
col_sp      <- pick_col(d0, exact=c("specietag","speciestag","speciecode","species"),
                        regex="specie.*tag|species")
# likely stakeholder column if present
col_holder  <- pick_col(
  d0,
  exact = c("questiontype_2014","label_2","questiontype"),
  regex = "(type|label).*2014|stakeholder|operator|breeder|government|commercial|ngo"
)

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_answer), !is.na(col_sp))

# Filter rows for Section 2, Q10
rows_keep <- which(
  (if (!is.na(col_year)) d0[[col_year]] else 2024) == 2024 &
    d0[[col_section]] %in% c("2",2) &
    grepl("^\\s*10(\\.|$)", as.character(d0[[col_q]]))
)
d_sub <- d0[rows_keep, , drop = FALSE]

df0 <- tibble(
  Year     = if (!is.na(col_year)) d_sub[[col_year]] else 2024,
  Region   = norm_region(d_sub[[col_region]]),
  Country  = str_squish(as.character(d_sub[[col_country]])),
  Section  = d_sub[[col_section]],
  Question = d_sub[[col_q]],
  Answer   = d_sub[[col_answer]],
  SpRaw    = d_sub[[col_sp]],
  StakeRaw = if (!is.na(col_holder)) d_sub[[col_holder]] else NA_character_
)

# If StakeRaw mostly blank, scan text columns within d_sub
if (all(is.na(df0$StakeRaw)) || mean(is.na(df0$StakeRaw)) > 0.9) {
  text_cols <- names(d_sub)[sapply(d_sub, function(x) is.character(x) || is.factor(x))]
  text_cols <- setdiff(text_cols, c(col_answer,col_region,col_country,col_section,col_q,col_year))
  if (length(text_cols)) {
    df0$StakeRaw <- apply(d_sub[text_cols], 1, function(vec){
      v <- .normalize_text(paste(na.omit(as.character(vec)), collapse=" | "))
      for (nm in names(stake_patterns)) if (grepl(stake_patterns[[nm]], v, perl=TRUE)) return(nm)
      NA_character_
    })
  }
}

# -------- species + stakeholder --------
df <- df0 %>%
  mutate(
    SpCode  = str_pad(str_extract(as.character(SpRaw), "\\d{1,3}$"), 3, pad="0"),
    Species = case_when(
      SpCode %in% big7_codes ~ big7_names[match(SpCode, big7_codes)],
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Species)) %>%
  mutate(
    Stakeholder = norm_stake_vec(StakeRaw),
    ans_l = .normalize_text(Answer),
    flag_yes = case_when(
      grepl("^y", ans_l) ~ 1L,
      grepl("^n", ans_l) ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(Stakeholder %in% stake_levels)

# ---- Quick QA: Stakeholder tallies (NGO should be > 0 if present) ----
cat("\n[QA] Stakeholder tallies (expect NGOs > 0 if present):\n")
print(df %>% count(Stakeholder, sort = TRUE))

# ---------- per-species table builder ----------
make_sheet_table <- function(spec_nm){
  df_s <- df %>% filter(Species == spec_nm)
  if (nrow(df_s) == 0) {
    return(tibble(Region = region_levels,
                  `Number of countries` = NA_integer_) %>%
             mutate(across(all_of(stake_levels), ~NA_real_)))
  }

  # 1) denominators per region
  denom_reg <- df_s %>% distinct(Region, Country) %>%
    count(Region, name="Number of countries")

  # 2) any-YES per Region × Stakeholder
  cty_yes <- df_s %>%
    group_by(Region, Country, Stakeholder) %>%
    summarise(any_yes = {
      v <- flag_yes
      if (all(is.na(v))) NA_integer_ else as.integer(max(v, na.rm=TRUE))
    }, .groups="drop") %>%
    filter(!is.na(any_yes)) %>%
    group_by(Region, Stakeholder) %>%
    summarise(n_yes = sum(any_yes == 1L), .groups="drop")

  # grid guarantees Stakeholder column exists
  grid_rs <- tidyr::crossing(Region = unique(denom_reg$Region), Stakeholder = stake_levels)

  reg_tbl <- grid_rs %>%
    left_join(denom_reg, by="Region") %>%
    left_join(cty_yes, by=c("Region","Stakeholder")) %>%
    mutate(n_yes = coalesce(n_yes, 0L),
           Percent = 100 * n_yes / pmax(`Number of countries`, 1)) %>%
    select(Region, Stakeholder, Percent, `Number of countries`)

  # 3) World row for this species
  denom_world <- df_s %>% distinct(Country) %>%
    summarise(`Number of countries` = n(), .groups="drop")
  cty_world <- df_s %>%
    group_by(Country, Stakeholder) %>%
    summarise(any_yes = {
      v <- flag_yes
      if (all(is.na(v))) NA_integer_ else as.integer(max(v, na.rm=TRUE))
    }, .groups="drop") %>%
    filter(!is.na(any_yes)) %>%
    group_by(Stakeholder) %>%
    summarise(n_yes = sum(any_yes == 1L), .groups="drop")

  world_tbl <- tidyr::crossing(Stakeholder = stake_levels, denom_world) %>%
    left_join(cty_world, by="Stakeholder") %>%
    mutate(Region = "World",
           n_yes = coalesce(n_yes, 0L),
           Percent = 100 * n_yes / pmax(`Number of countries`, 1)) %>%
    select(Region, Stakeholder, Percent, `Number of countries`)

  # 4) Bind + cast wide like the book
  out <- bind_rows(reg_tbl, world_tbl) %>%
    mutate(
      Region = factor(Region, levels = region_levels),
      Stakeholder = factor(Stakeholder, levels = stake_levels)
    ) %>%
    arrange(Region, Stakeholder) %>%
    mutate(Percent = round(Percent, 0)) %>%
    pivot_wider(names_from = Stakeholder, values_from = Percent) %>%
    arrange(Region)

  for (st in stake_levels) if (!st %in% names(out)) out[[st]] <- NA_real_

  out %>% select(Region, `Number of countries`, all_of(stake_levels))
}

# ---------- build workbook ----------
wb <- createWorkbook()
sheet_name <- function(sp){
  recode(sp,
         "Cattle (specialized dairy)"="Dairy cattle",
         "Cattle (specialized beef)"="Beef cattle",
         "Cattle (multipurpose)"="Multipurpose cattle",
         .default=sp) %>% gsub("[^A-Za-z0-9]+","_", .)
}

for (sp in species_levels) {
  nm <- sheet_name(sp)
  addWorksheet(wb, nm)
  tab <- make_sheet_table(sp)
  writeData(wb, nm, tab)
  setColWidths(wb, nm, cols=1:ncol(tab), widths="auto")
  addStyle(wb, nm, createStyle(textDecoration="bold"),
           rows=1, cols=1:ncol(tab), gridExpand=TRUE)
}

saveWorkbook(wb, out_xlsx, overwrite=TRUE)
cat("\nSaved BY-SPECIES workbook to:\n", out_xlsx, "\n")





FIGURE 3C1
# ===================== FIGURE 3C1 (2024) – FIXED =====================
# Stakeholder involvement in breeding-related activities – global averages
# Data: C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx
# Section 2, Question 15
# Outputs:
#   Fig3C1_ruminants_2024.(png|pdf)
#   Fig3C1_monogastrics_2024.(png|pdf)
#   Fig3C1_2024_tables.xlsx
# =====================================================================

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(openxlsx)

# -------- paths --------
infile     <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_png_r  <- "C:/Users/LENOVO/Documents/Fig3C1_ruminants_2024.png"
out_pdf_r  <- "C:/Users/LENOVO/Documents/Fig3C1_ruminants_2024.pdf"
out_png_m  <- "C:/Users/LENOVO/Documents/Fig3C1_monogastrics_2024.png"
out_pdf_m  <- "C:/Users/LENOVO/Documents/Fig3C1_monogastrics_2024.pdf"
out_xlsx   <- "C:/Users/LENOVO/Documents/Fig3C1_2024_tables.xlsx"

# -------- helpers --------
norm_region_simple <- function(x){
  x <- str_squish(as.character(x))
  x <- ifelse(x %in% c("South west Pacific","South-west Pacific","South West Pacific"),
              "Southwest Pacific", x)
  x <- ifelse(x %in% c("Europe and the Caucasus","Europe (including Caucasus)"),
              "Europe", x)
  x <- ifelse(startsWith(x,"Near"), "Near East", x)
  x <- ifelse(startsWith(x,"Latin Ame"), "Latin America and the Caribbean", x)
  x
}

# Collapse sub-regions to the 7 macro regions you want
to_macro_region <- function(x){
  x <- norm_region_simple(x)
  key <- str_to_lower(x)
  dplyr::case_when(
    key %in% str_to_lower(c(
      "Africa","East Africa","North and West Africa","Southern Africa","West and Central Africa","North Africa"
    )) ~ "Africa",
    key %in% str_to_lower(c(
      "Asia","Central Asia","East Asia","South Asia","Southeast Asia","Western and Central Asia"
    )) ~ "Asia",
    key %in% str_to_lower(c("Europe","Europe (including Caucasus)","Europe and the Caucasus")) ~ "Europe",
    key %in% str_to_lower(c("Latin America and the Caribbean","Caribbean","Central America","South America")) ~
      "Latin America and the Caribbean",
    key %in% str_to_lower(c("Near East","Near and Middle East")) ~ "Near East",
    key %in% str_to_lower(c("North America")) ~ "North America",
    key %in% str_to_lower(c("Southwest Pacific","South West Pacific","South-west Pacific")) ~ "Southwest Pacific",
    TRUE ~ x  # leave as is if already one of the 7
  )
}

pick_col <- function(df, exact = character(), regex = NULL){
  nm <- names(df); low <- tolower(nm)
  for (c in tolower(exact)) { hit <- which(low == c); if (length(hit)) return(nm[hit[1]]) }
  if (!is.null(regex)) { hit <- which(grepl(regex, low, perl = TRUE)); if (length(hit)) return(nm[hit[1]]) }
  NA_character_
}

# Canonical lists (use exact strings seen in your file/figure)
stake_levels <- c(
  "Government",
  "Research organizations",
  "Breeders’ associations or cooperatives",
  "Individual breeders/livestock keepers",
  "National commercial companies",
  "External commercial companies",
  "Non-governmental organizations",
  "Others"
)
activity_levels <- c(
  "Setting breeding goals",
  "Animal identification",
  "Recording",
  "Provision of artificial insemination services",
  "Genetic evaluation"
)

# Species codes → names
sp_map <- c(
  `009` = "Cattle (specialized dairy)",
  `008` = "Cattle (specialized beef)",
  `007` = "Cattle (multipurpose)",
  `042` = "Sheep",
  `021` = "Goats",
  `037` = "Pigs",
  `010` = "Chickens"
)
sp_values <- unname(sp_map)
ruminants    <- c("Cattle (specialized dairy)","Cattle (specialized beef)",
                  "Cattle (multipurpose)","Goats","Sheep")
monogastrics <- c("Pigs","Chickens")

region_levels <- c("Africa","Asia","Europe","Latin America and the Caribbean",
                   "Near East","North America","Southwest Pacific","World")

# Answer → numeric score
cat_to_score <- function(ans, code = NA){
  a <- str_to_lower(str_squish(as.character(ans)))
  out <- dplyr::case_when(
    a %in% c("none","no","0") ~ 0,
    a %in% c("low","1")       ~ 1,
    a %in% c("medium","2")    ~ 2,
    a %in% c("high","3")      ~ 3,
    TRUE ~ NA_real_
  )
  if (!all(is.na(code))) {
    code_num <- suppressWarnings(as.numeric(code))
    out <- ifelse(is.na(out) & !is.na(code_num) & code_num %in% 0:3, code_num, out)
  }
  out
}

# -------- load & identify columns --------
d0 <- read_excel(infile)

col_year    <- pick_col(d0, exact = "year")
col_region  <- pick_col(d0, exact = "region")
col_country <- pick_col(d0, exact = "country")
col_section <- pick_col(d0, exact = "section")
col_q       <- pick_col(d0, exact = c("question_2024","question"))
col_answer  <- pick_col(d0, exact = "answer")
col_code    <- pick_col(d0, exact = "code")
col_sp      <- pick_col(d0, exact = c("specietag","speciestag","speciecode","species"),
                        regex = "specie.*tag|species")
col_stake   <- pick_col(d0, exact = c("subquestiontype_2014","subquestiontype"))
col_act     <- pick_col(d0, exact = c("breedtype_2014","breedtype"))

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_answer), !is.na(col_sp),
          !is.na(col_stake), !is.na(col_act), !is.na(col_q), !is.na(col_section))

# Filter to Sec 2, Q15 (accept 15 or 15.x)
rows_keep <- which(
  (if (!is.na(col_year)) d0[[col_year]] else 2024) == 2024 &
  d0[[col_section]] %in% c("2",2) &
  grepl("^\\s*15(\\.|$)", as.character(d0[[col_q]]))
)
d_sub <- d0[rows_keep, , drop = FALSE]

# Build normalized frame (EXACT matching for stakeholders/activities)
to_key <- function(x) str_trim(str_to_lower(as.character(x)))
stake_key <- setNames(stake_levels, to_key(stake_levels))
act_key   <- setNames(activity_levels, to_key(activity_levels))

df <- tibble(
  Region   = to_macro_region(d_sub[[col_region]]),
  Country  = str_squish(as.character(d_sub[[col_country]])),
  StakeTxt = d_sub[[col_stake]],
  ActTxt   = d_sub[[col_act]],
  Answer   = d_sub[[col_answer]],
  Code     = if (!is.na(col_code)) d_sub[[col_code]] else NA,
  SpRaw    = d_sub[[col_sp]]
) %>%
  mutate(
    SpCode  = stringr::str_pad(stringr::str_extract(as.character(SpRaw), "\\d{1,3}$"), 3, pad = "0"),
    Species = unname(sp_map[SpCode]),
    Stakeholder = unname(stake_key[to_key(StakeTxt)]),
    Activity    = unname(act_key[to_key(ActTxt)]),
    score       = cat_to_score(Answer, Code)
  ) %>%
  filter(!is.na(Species), !is.na(Stakeholder), !is.na(Activity)) %>%
  mutate(
    Species     = factor(Species,     levels = sp_values),       # <-- FIX: values as levels
    Activity    = factor(Activity,    levels = activity_levels),
    Stakeholder = factor(Stakeholder, levels = stake_levels),
    Region      = factor(Region,      levels = region_levels[region_levels!="World"])
  )

# ---------- QA: you should see all 8 stakeholders/5 activities with counts ----------
cat("\n[QA] Non-empty counts per stakeholder × activity:\n")
print(df %>% count(Stakeholder, Activity) %>% arrange(Stakeholder, Activity), n = 100)

# Group labels
df <- df %>% mutate(Group = ifelse(as.character(Species) %in% ruminants, "Ruminants", "Monogastrics"))

# Collapse to country×species×activity×stakeholder cell
country_cell <- df %>%
  group_by(Group, Species, Region, Country, Activity, Stakeholder) %>%
  summarise(score = mean(score, na.rm = TRUE), .groups = "drop")

# ---------- GLOBAL averages ----------
global_avg <- country_cell %>%
  group_by(Group, Activity, Stakeholder) %>%
  summarise(avg_score = mean(score, na.rm = TRUE), .groups = "drop") %>%
  mutate(Activity = factor(Activity, levels = activity_levels),
         Stakeholder = factor(Stakeholder, levels = stake_levels))

# ---------- REGION tables ----------
region_avg <- country_cell %>%
  group_by(Group, Region, Activity, Stakeholder) %>%
  summarise(avg_score = mean(score, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = factor(Region, levels = region_levels[region_levels!="World"]),
         Activity = factor(Activity, levels = activity_levels),
         Stakeholder = factor(Stakeholder, levels = stake_levels))

# ---------- plotting ----------
stake_cols <- c(
  "Government"                                     = "#1f77b4",
  "Research organizations"                         = "#7f3c8d",
  "Breeders’ associations or cooperatives"         = "#2ca02c",
  "Individual breeders/livestock keepers"          = "#d62728",
  "National commercial companies"                  = "#bcbd22",
  "External commercial companies"                  = "#9467bd",
  "Non-governmental organizations"                 = "#17becf",
  "Others"                                         = "#8c564b"
)

make_plot <- function(grp){
  ggplot(global_avg %>% filter(Group == grp),
         aes(x = Activity, y = avg_score, fill = Stakeholder)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.72) +
    scale_y_continuous(limits = c(0,2), breaks = seq(0,2,0.2)) +
    scale_fill_manual(values = stake_cols, name = NULL, drop = FALSE) +
    labs(
      title = paste0("Stakeholder involvement in breeding-related activities — ", grp, " (Global averages, 2024)"),
      x = NULL, y = "Average score (0 = none, 3 = high)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 15, hjust = 1),
      legend.position = "bottom"
    )
}

p_r <- make_plot("Ruminants")
p_m <- make_plot("Monogastrics")

ggsave(out_png_r, p_r, width = 12, height = 7.5, dpi = 300)
ggsave(out_pdf_r, p_r, width = 12, height = 7.5)
ggsave(out_png_m, p_m, width = 12, height = 7.5, dpi = 300)
ggsave(out_pdf_m, p_m, width = 12, height = 7.5)

# ---------- Excel ----------
wb <- createWorkbook()

# 1) long data used for plots
addWorksheet(wb, "LONG_country_cells")
writeData(wb, "LONG_country_cells",
          country_cell %>% arrange(Group, Species, Region, Country, Activity, Stakeholder))

# 2) global tables (wide)
glob_rum <- global_avg %>% filter(Group=="Ruminants") %>%
  select(Activity, Stakeholder, avg_score) %>%
  pivot_wider(names_from = Stakeholder, values_from = avg_score) %>%
  arrange(Activity) %>% mutate(across(-Activity, ~round(.x, 2)))

glob_mon <- global_avg %>% filter(Group=="Monogastrics") %>%
  select(Activity, Stakeholder, avg_score) %>%
  pivot_wider(names_from = Stakeholder, values_from = avg_score) %>%
  arrange(Activity) %>% mutate(across(-Activity, ~round(.x, 2)))

addWorksheet(wb, "Global_Ruminants")
addWorksheet(wb, "Global_Monogastrics")
writeData(wb, "Global_Ruminants",    glob_rum)
writeData(wb, "Global_Monogastrics", glob_mon)

# 3) per-region sheets (collapse subregions already handled)
sheet_name <- function(reg){
  recode(reg,
         "Latin America and the Caribbean" = "LAC",
         "Southwest Pacific"               = "SW_Pacific",
         .default = reg) %>% gsub("[^A-Za-z0-9]+","_", .)
}

make_region_block <- function(grp, reg){
  region_avg %>%
    filter(Group==grp, as.character(Region)==reg) %>%
    select(Activity, Stakeholder, avg_score) %>%
    pivot_wider(names_from = Stakeholder, values_from = avg_score) %>%
    arrange(Activity) %>%
    mutate(across(-Activity, ~round(.x, 2)))
}

for (reg in levels(region_avg$Region)) {
  nm <- paste0("Region_", sheet_name(reg))
  addWorksheet(wb, nm)

  tbl1 <- make_region_block("Ruminants", reg)
  tbl2 <- make_region_block("Monogastrics", reg)

  writeData(wb, nm, "Ruminants", startCol = 1, startRow = 1)
  writeData(wb, nm, tbl1,       startCol = 1, startRow = 2)

  start_r <- nrow(tbl1) + 4
  writeData(wb, nm, "Monogastrics", startCol = 1, startRow = start_r)
  writeData(wb, nm, tbl2,          startCol = 1, startRow = start_r + 1)

  setColWidths(wb, nm, cols = 1:max(2, ncol(tbl1), ncol(tbl2)), widths = "auto")
}

# header styling
for (sh in c("Global_Ruminants","Global_Monogastrics")) {
  addStyle(wb, sh, createStyle(textDecoration="bold"),
           rows = 1, cols = 1:ncol(readWorkbook(wb, sh)), gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:20, widths = "auto")
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)

message(
  "Saved:\n  Ruminants PNG -> ", out_png_r,
  "\n  Ruminants PDF -> ", out_pdf_r,
  "\n  Monogastrics PNG -> ", out_png_m,
  "\n  Monogastrics PDF -> ", out_pdf_m,
  "\n  Excel tables     -> ", out_xlsx
)







Old Table 3C3
# ===================== TABLE 3C3 (2024) — FIXED =====================
# Extent of involvement of stakeholder groups as operators of breeding programmes
# Data: Section 2, Question 10  (C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx)
# Output: C:/Users/LENOVO/Documents/Table3C3_stakeholders_2024.xlsx
# =====================================================================

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(openxlsx)
library(purrr)

infile   <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Table3C3_stakeholders_2024.xlsx"

# ---------- helpers ----------
norm_region <- function(x){
  x <- str_squish(as.character(x))
  x <- ifelse(x %in% c("South west Pacific","South-west Pacific","South West Pacific"),
              "Southwest Pacific", x)
  x <- ifelse(x %in% c("Europe and the Caucasus","Europe (including Caucasus)"),
              "Europe", x)
  x <- ifelse(startsWith(x,"Near"), "Near East", x)
  x <- ifelse(startsWith(x,"Latin Ame"), "Latin America and the Caribbean", x)
  x
}
pick_col <- function(df, exact = character(), regex = NULL){
  nm  <- names(df); low <- tolower(nm)
  for (c in tolower(exact)) { hit <- which(low == c); if (length(hit)) return(nm[hit[1]]) }
  if (!is.null(regex)) { hit <- which(grepl(regex, low, perl = TRUE)); if (length(hit)) return(nm[hit[1]]) }
  NA_character_
}

# Big seven (three cattle + four others)
big7_codes <- c("009","008","007","042","021","037","010")
big7_names <- c("Cattle (specialized dairy)","Cattle (specialized beef)","Cattle (multipurpose)",
                "Sheep","Goats","Pigs","Chickens")

# 2024 region order
region_levels <- c("Africa","Asia","Europe","Latin America and the Caribbean",
                   "Near East","North America","Southwest Pacific","World")

# Stakeholder headings (as in the 2014 figure)
stake_levels <- c(
  "Government",
  "Livestock keepers organized at community level",
  "Breeders’ associations or cooperatives",
  "National commercial companies",
  "External commercial companies",
  "Non-governmental organizations",
  "Others"
)

# Regex patterns to detect stakeholders
stake_patterns <- list(
  "Government" = "(^|\\b)gov|\\bgovernment\\b",
  "Livestock keepers organized at community level" = "livestock\\s*keepers|community\\s*level",
  "Breeders’ associations or cooperatives" = "breeder|cooperativ",
  "National commercial companies" = "national\\s+commercial|domestic\\s+commercial",
  "External commercial companies" = "external\\s+commercial|foreign\\s+commercial|based\\s+outside",
  "Non-governmental organizations" = "ngo|non-?governmental",
  "Others" = "\\bother(s)?\\b"
)

# Vectorized stakeholder normalizer
norm_stake_vec <- function(x){
  t <- tolower(str_squish(as.character(x)))
  vapply(
    t,
    function(one){
      if (is.na(one) || one == "") return(NA_character_)
      for (nm in names(stake_patterns)) {
        if (grepl(stake_patterns[[nm]], one, perl = TRUE)) return(nm)
      }
      NA_character_
    },
    character(1)
  )
}

# ---------- load ----------
d0 <- read_excel(infile)

col_year    <- pick_col(d0, exact="year")
col_region  <- pick_col(d0, exact="region")
col_country <- pick_col(d0, exact="country")
col_section <- pick_col(d0, exact="section")
col_q       <- pick_col(d0, exact=c("question_2024","question"))
col_answer  <- pick_col(d0, exact="answer")
col_sp      <- pick_col(d0, exact=c("specietag","speciestag","speciecode","species"),
                        regex="specie.*tag|species")
# likely stakeholder column if present
col_holder  <- pick_col(
  d0,
  exact = c("questiontype_2014","_2estionType_2014","2estionType_2014",
            "cleaLabel_2edType_2","label_2edtype_2","label_2","questiontype"),
  regex = "(type|label).*2014|stakeholder|operator|breeder|government|commercial|ngo"
)

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_answer), !is.na(col_sp))

# -------- filter rows FIRST and keep row index to scan within the same rows --------
rows_keep <- which(
  (if (!is.na(col_year)) d0[[col_year]] else 2024) == 2024 &
  d0[[col_section]] %in% c("2",2) &
  grepl("^\\s*10(\\.|$)", as.character(d0[[col_q]]))
)

d_sub <- d0[rows_keep, , drop = FALSE]

df0 <- tibble::tibble(
  Year     = if (!is.na(col_year)) d_sub[[col_year]] else 2024,
  Region   = norm_region(d_sub[[col_region]]),
  Country  = str_squish(as.character(d_sub[[col_country]])),
  Section  = d_sub[[col_section]],
  Question = d_sub[[col_q]],
  Answer   = d_sub[[col_answer]],
  SpRaw    = d_sub[[col_sp]],
  StakeRaw = if (!is.na(col_holder)) d_sub[[col_holder]] else NA_character_
)

# If StakeRaw is missing/mostly blank, scan *within these filtered rows* only
if (all(is.na(df0$StakeRaw)) || mean(is.na(df0$StakeRaw)) > 0.9) {
  text_cols <- names(d_sub)[map_lgl(d_sub, ~is.character(.x) || is.factor(.x))]
  text_cols <- setdiff(text_cols, c(col_answer, col_region, col_country, col_section, col_q, col_year))
  if (length(text_cols)) {
    scanned <- apply(d_sub[text_cols], 1, function(vec){
      v <- tolower(paste(na.omit(as.character(vec)), collapse = " | "))
      for (nm in names(stake_patterns)) if (grepl(stake_patterns[[nm]], v, perl = TRUE)) return(nm)
      NA_character_
    })
    df0$StakeRaw <- scanned
  }
}

# -------- species mapping & keep big7 --------
df <- df0 %>%
  mutate(
    SpCode  = str_pad(str_extract(as.character(SpRaw), "\\d{1,3}$"), 3, pad = "0"),
    Species = case_when(
      SpCode %in% big7_codes ~ big7_names[match(SpCode, big7_codes)],
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Species))

# -------- stakeholder + yes/no --------
df <- df %>%
  mutate(
    Stakeholder = norm_stake_vec(StakeRaw),
    ans_l = str_to_lower(str_squish(as.character(Answer))),
    flag_yes = dplyr::case_when(
      str_detect(ans_l, "^y") ~ 1L,
      str_detect(ans_l, "^n") ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(Stakeholder %in% stake_levels)

if (nrow(df) == 0) stop("No stakeholder rows detected after scanning. Please share one example row with stakeholder text so I can extend the patterns.")

# ---------- region×species percentages by stakeholder ----------
# Collapse duplicates per Region×Country×Species×Stakeholder
cty_sp_stake <- df %>%
  group_by(Region, Country, Species, Stakeholder) %>%
  summarise(
    any_yes = {
      v <- flag_yes
      if (all(is.na(v))) NA_integer_ else as.integer(max(v, na.rm = TRUE))
    },
    .groups = "drop"
  ) %>%
  filter(!is.na(any_yes))

# Denominators: countries that reported the species
denom <- df %>% distinct(Region, Country, Species)

# Percent per Region×Species×Stakeholder
reg_sp_stake_pct <- denom %>%
  group_by(Region, Species) %>%
  summarise(n_countries = n_distinct(Country), .groups = "drop") %>%
  left_join(
    cty_sp_stake %>%
      group_by(Region, Species, Stakeholder) %>%
      summarise(n_yes = sum(any_yes == 1L, na.rm = TRUE), .groups = "drop"),
    by = c("Region","Species")
  ) %>%
  mutate(n_yes = coalesce(n_yes, 0L),
         pct_yes = 100 * n_yes / pmax(n_countries, 1))

# Regional averages across the seven species
reg_avg_stake <- reg_sp_stake_pct %>%
  group_by(Region, Stakeholder) %>%
  summarise(Percent = mean(pct_yes, na.rm = TRUE), .groups = "drop") %>%
  mutate(Stakeholder = factor(Stakeholder, levels = stake_levels))

# World row (recompute at world level from countries)
world_sp_stake <- denom %>%
  distinct(Country, Species) %>%
  group_by(Species) %>%
  summarise(n_countries = n_distinct(Country), .groups = "drop") %>%
  left_join(
    cty_sp_stake %>%
      group_by(Country, Species, Stakeholder) %>%
      summarise(any_yes = {
        v <- any_yes
        if (all(is.na(v))) NA_integer_ else as.integer(max(v, na.rm=TRUE))
      }, .groups = "drop") %>%
      filter(!is.na(any_yes)) %>%
      group_by(Species, Stakeholder) %>%
      summarise(n_yes = sum(any_yes == 1L, na.rm = TRUE), .groups = "drop"),
    by = "Species"
  ) %>%
  mutate(n_yes = coalesce(n_yes, 0L),
         pct_yes = 100 * n_yes / pmax(n_countries, 1)) %>%
  group_by(Stakeholder) %>%
  summarise(Percent = mean(pct_yes, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = "World",
         Stakeholder = factor(Stakeholder, levels = stake_levels))

# Number of countries per region
n_countries_region <- denom %>%
  distinct(Region, Country) %>%
  count(Region, name = "Number of countries")

# Final table laid out like the book (guarantee all 7 stakeholder columns)
final_tbl <- bind_rows(reg_avg_stake, world_sp_stake) %>%
  mutate(Region = factor(Region, levels = region_levels)) %>%
  tidyr::pivot_wider(
    names_from  = Stakeholder,
    values_from = Percent,
    values_fill = 0
  )

for (st in stake_levels) if (!st %in% names(final_tbl)) final_tbl[[st]] <- 0

final_tbl <- final_tbl %>%
  left_join(n_countries_region, by = "Region") %>%
  select(Region, `Number of countries`, all_of(stake_levels)) %>%
  arrange(Region) %>%
  mutate(across(all_of(stake_levels), ~round(.x, 0)))

# ---------- Excel ----------
wb <- createWorkbook()
addWorksheet(wb, "Stakeholder_3C3_2024")
addWorksheet(wb, "RegionSpecies_Pcts")
addWorksheet(wb, "Map_Check")

writeData(wb, "Stakeholder_3C3_2024", final_tbl)
writeData(wb, "RegionSpecies_Pcts",
          reg_sp_stake_pct %>%
            mutate(pct_yes = round(pct_yes, 1)) %>%
            arrange(factor(Region, levels = region_levels),
                    match(Species, big7_names), Stakeholder))
writeData(wb, "Map_Check",
          df %>%
            select(Region, Country, Species, Stakeholder, Answer, StakeRaw) %>%
            arrange(Region, Country, Species, Stakeholder))

setColWidths(wb, "Stakeholder_3C3_2024", cols = 1:ncol(final_tbl), widths = "auto")
addStyle(wb, "Stakeholder_3C3_2024",
         createStyle(textDecoration = "bold"),
         rows = 1, cols = 1:ncol(final_tbl), gridExpand = TRUE)

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
cat("Saved table to:\n", out_xlsx, "\n", sep = "")



Figure 3c3
# ===================== FIGURE 3C3 (2024) — TRAINING ONLY =====================
# State of training in the field of animal breeding (Training)
# Data: C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx
# Section 2, Question 13
# Outputs:
#   Fig3C3_training_2024.png
#   Fig3C3_training_2024.pdf
#   Fig3C3_training_2024_tables.xlsx
# ============================================================================

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(openxlsx)

# -------- paths --------
infile   <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_png  <- "C:/Users/LENOVO/Documents/Fig3C3_training_2024.png"
out_pdf  <- "C:/Users/LENOVO/Documents/Fig3C3_training_2024.pdf"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3C3_training_2024_tables.xlsx"

# -------- helpers --------
pick_col <- function(df, exact = character(), regex = NULL){
  nm <- names(df); low <- tolower(nm)
  for (c in tolower(exact)) {
    hit <- which(low == c); if (length(hit)) return(nm[hit[1]])
  }
  if (!is.null(regex)) {
    hit <- which(grepl(regex, low, perl = TRUE)); if (length(hit)) return(nm[hit[1]])
  }
  NA_character_
}

norm_region <- function(x){
  x0 <- str_squish(as.character(x))
  x0 <- ifelse(x0 %in% c("South west Pacific","South-west Pacific","South West Pacific"),
               "Southwest Pacific", x0)
  x0 <- ifelse(x0 %in% c("Europe and the Caucasus","Europe (including Caucasus)"),
               "Europe & the Caucasus", x0)
  x0 <- ifelse(startsWith(x0,"Latin Ame"), "Latin America and the Caribbean", x0)
  x0 <- ifelse(startsWith(x0,"Near"), "Near & Middle East", x0)
  x0
}
# collapse subregions to the 7 macro names used in the figure
to_macro_region <- function(x){
  key <- str_to_lower(norm_region(x))
  out <- dplyr::case_when(
    key %in% str_to_lower(c("africa","east africa","north africa","west and central africa",
                            "southern africa","north and west africa")) ~ "Africa",
    key %in% str_to_lower(c("asia","east asia","south asia","southeast asia",
                            "central asia","western and central asia")) ~ "Asia",
    key %in% str_to_lower(c("southwest pacific","south west pacific","south-west pacific")) ~ "Southwest Pacific",
    key %in% str_to_lower(c("europe","europe & the caucasus","europe (including caucasus)")) ~ "Europe & the Caucasus",
    key %in% str_to_lower(c("latin america and the caribbean","caribbean","central america","south america")) ~
      "Latin America and the Caribbean",
    key %in% str_to_lower(c("north america")) ~ "North America",
    key %in% str_to_lower(c("near & middle east","near east","near and middle east")) ~ "Near & Middle East",
    TRUE ~ norm_region(x) # fall back to normalized text if already macro
  )
  out
}

# map answer → numeric score
cat_to_score <- function(ans){
  a <- str_to_lower(str_squish(as.character(ans)))
  dplyr::case_when(
    a %in% c("none","no","0") ~ 0,
    a %in% c("low","1")       ~ 1,
    a %in% c("medium","2")    ~ 2,
    a %in% c("high","3")      ~ 3,
    TRUE ~ NA_real_
  )
}

# species code → names (big seven)
sp_map <- c(
  `009` = "Dairy cattle",
  `008` = "Beef cattle",
  `007` = "Multipurpose cattle",
  `042` = "Sheep",
  `021` = "Goats",
  `037` = "Pigs",
  `010` = "Chickens"
)
species_levels <- unname(sp_map)

# 2024 (figure) region order
region_levels <- c("Africa","Asia","Southwest Pacific","Europe & the Caucasus",
                   "Latin America and the Caribbean","North America","Near & Middle East","World")

# -------- load & identify columns --------
d0 <- read_excel(infile)

col_year    <- pick_col(d0, exact="year")
col_region  <- pick_col(d0, exact="region")
col_country <- pick_col(d0, exact="country")
col_section <- pick_col(d0, exact="section")
col_q       <- pick_col(d0, exact=c("question_2024","question"))
col_answer  <- pick_col(d0, exact="answer")
col_sp      <- pick_col(d0, exact=c("specietag","speciestag","speciecode","species"),
                        regex="specie.*tag|species")
# "Training"/"Research" column (your screenshot shows SubquestionType_2014)
col_sub     <- pick_col(d0, exact=c("subquestiontype_2014","subquestiontype","subquestion_2024"),
                        regex="subquestion.*type|subquestion")

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_answer),
          !is.na(col_sp), !is.na(col_section), !is.na(col_q), !is.na(col_sub))

# keep 2024, Sec 2, Q13 (accept 13 or 13.x), TRAINING only
rows_keep <- which(
  (if (!is.na(col_year)) d0[[col_year]] else 2024) == 2024 &
  d0[[col_section]] %in% c("2",2) &
  grepl("^\\s*13(\\.|$)", as.character(d0[[col_q]])) &
  str_to_lower(str_squish(as.character(d0[[col_sub]]))) == "training"
)
d_sub <- d0[rows_keep, , drop = FALSE]

# ---------- build normalized analysis frame ----------
df <- tibble(
  Region0  = to_macro_region(d_sub[[col_region]]),
  Country  = str_squish(as.character(d_sub[[col_country]])),
  SpCode   = stringr::str_pad(stringr::str_extract(as.character(d_sub[[col_sp]]), "\\d{1,3}$"), 3, pad = "0"),
  Answer   = d_sub[[col_answer]]
) %>%
  mutate(
    Species = unname(sp_map[SpCode]),
    score   = cat_to_score(Answer)
  ) %>%
  filter(!is.na(Species), !is.na(score)) %>%
  mutate(
    Region  = factor(Region0, levels = region_levels[region_levels!="World"]),
    Species = factor(Species, levels = species_levels)
  )

# ---------- collapse to country×species cells, then Region means ----------
# (If a country reported multiple rows for a Species in Training, average them first)
country_cells <- df %>%
  group_by(Region, Country, Species) %>%
  summarise(score = mean(score, na.rm = TRUE), .groups = "drop")

# Region × Species averages
region_species_avg <- country_cells %>%
  group_by(Region, Species) %>%
  summarise(avg_score = mean(score, na.rm = TRUE), .groups = "drop")

# add World row (recompute from countries directly, not mean of regions)
world_species_avg <- country_cells %>%
  group_by(Species) %>%
  summarise(avg_score = mean(score, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = factor("World", levels = "World"))

region_species_avg <- bind_rows(region_species_avg, world_species_avg) %>%
  mutate(Region = factor(as.character(Region), levels = region_levels))

# -------- wide table for Excel / plotting --------
wide_tbl <- region_species_avg %>%
  tidyr::pivot_wider(names_from = Species, values_from = avg_score) %>%
  arrange(Region) %>%
  mutate(Total = rowSums(across(all_of(species_levels)), na.rm = TRUE))

# -------- plot (stacked columns, 0..18) --------
plot_data <- region_species_avg %>%
  mutate(Region = factor(as.character(Region), levels = region_levels)) %>%
  tidyr::complete(Region, Species = factor(species_levels, levels = species_levels),
                  fill = list(avg_score = 0))

p <- ggplot(plot_data, aes(x = Region, y = avg_score, fill = Species)) +
  geom_col(position = "stack", width = 0.75) +
  scale_y_continuous(limits = c(0, 18), breaks = seq(0, 18, 2), expand = expansion(mult = c(0, 0.02))) +
  scale_fill_brewer(palette = "Set3", drop = FALSE) +
  labs(title = "State of training in the field of animal breeding — 2024",
       x = NULL, y = "Score") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "bottom"
  )

ggsave(out_png, p, width = 12, height = 7.5, dpi = 300)
ggsave(out_pdf, p, width = 12, height = 7.5)

# -------- Excel outputs --------
wb <- createWorkbook()
addWorksheet(wb, "Region_Species_Avg")
addWorksheet(wb, "Region_Species_Long")
addWorksheet(wb, "Country_Cells_Long")

writeData(wb, "Region_Species_Avg",
          wide_tbl %>% mutate(across(all_of(species_levels), ~round(.x, 3))))
writeData(wb, "Region_Species_Long",
          region_species_avg %>% arrange(Region, Species) %>% mutate(avg_score = round(avg_score, 3)))
writeData(wb, "Country_Cells_Long",
          country_cells %>% arrange(Region, Country, Species))

# some light formatting
for (sh in c("Region_Species_Avg","Region_Species_Long","Country_Cells_Long")){
  addStyle(wb, sh, createStyle(textDecoration="bold"), rows=1, cols=1:50, gridExpand=TRUE)
  setColWidths(wb, sh, cols = 1:50, widths = "auto")
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)

message(
  "Saved:\n  PNG  -> ", out_png,
  "\n  PDF  -> ", out_pdf,
  "\n  XLSX -> ", out_xlsx
)


RESEARCH
# ===================== FIGURE 3C3 (2024) — RESEARCH ONLY =====================
# State of research in the field of animal breeding (Research)
# Data: C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx
# Section 2, Question 13
# Outputs:
#   Fig3C3_research_2024.png
#   Fig3C3_research_2024.pdf
#   Fig3C3_research_2024_tables.xlsx
# ============================================================================

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(openxlsx)

# -------- paths --------
infile   <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_png  <- "C:/Users/LENOVO/Documents/Fig3C3_research_2024.png"
out_pdf  <- "C:/Users/LENOVO/Documents/Fig3C3_research_2024.pdf"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3C3_research_2024_tables.xlsx"

# -------- helpers --------
pick_col <- function(df, exact = character(), regex = NULL){
  nm <- names(df); low <- tolower(nm)
  for (c in tolower(exact)) {
    hit <- which(low == c); if (length(hit)) return(nm[hit[1]])
  }
  if (!is.null(regex)) {
    hit <- which(grepl(regex, low, perl = TRUE)); if (length(hit)) return(nm[hit[1]])
  }
  NA_character_
}

norm_region <- function(x){
  x0 <- str_squish(as.character(x))
  x0 <- ifelse(x0 %in% c("South west Pacific","South-west Pacific","South West Pacific"),
               "Southwest Pacific", x0)
  x0 <- ifelse(x0 %in% c("Europe and the Caucasus","Europe (including Caucasus)"),
               "Europe & the Caucasus", x0)
  x0 <- ifelse(startsWith(x0,"Latin Ame"), "Latin America and the Caribbean", x0)
  x0 <- ifelse(startsWith(x0,"Near"), "Near & Middle East", x0)
  x0
}
to_macro_region <- function(x){
  key <- str_to_lower(norm_region(x))
  dplyr::case_when(
    key %in% str_to_lower(c("africa","east africa","north africa","west and central africa",
                            "southern africa","north and west africa")) ~ "Africa",
    key %in% str_to_lower(c("asia","east asia","south asia","southeast asia",
                            "central asia","western and central asia")) ~ "Asia",
    key %in% str_to_lower(c("southwest pacific","south west pacific","south-west pacific")) ~ "Southwest Pacific",
    key %in% str_to_lower(c("europe","europe & the caucasus","europe (including caucasus)")) ~ "Europe & the Caucasus",
    key %in% str_to_lower(c("latin america and the caribbean","caribbean","central america","south america")) ~
      "Latin America and the Caribbean",
    key %in% str_to_lower(c("north america")) ~ "North America",
    key %in% str_to_lower(c("near & middle east","near east","near and middle east")) ~ "Near & Middle East",
    TRUE ~ norm_region(x)
  )
}

cat_to_score <- function(ans){
  a <- str_to_lower(str_squish(as.character(ans)))
  dplyr::case_when(
    a %in% c("none","no","0") ~ 0,
    a %in% c("low","1")       ~ 1,
    a %in% c("medium","2")    ~ 2,
    a %in% c("high","3")      ~ 3,
    TRUE ~ NA_real_
  )
}

# species (big seven)
sp_map <- c(
  `009` = "Dairy cattle",
  `008` = "Beef cattle",
  `007` = "Multipurpose cattle",
  `042` = "Sheep",
  `021` = "Goats",
  `037` = "Pigs",
  `010` = "Chickens"
)
species_levels <- unname(sp_map)

region_levels <- c("Africa","Asia","Southwest Pacific","Europe & the Caucasus",
                   "Latin America and the Caribbean","North America","Near & Middle East","World")

# -------- load & identify columns --------
d0 <- read_excel(infile)

col_year    <- pick_col(d0, exact="year")
col_region  <- pick_col(d0, exact="region")
col_country <- pick_col(d0, exact="country")
col_section <- pick_col(d0, exact="section")
col_q       <- pick_col(d0, exact=c("question_2024","question"))
col_answer  <- pick_col(d0, exact="answer")
col_sp      <- pick_col(d0, exact=c("specietag","speciestag","speciecode","species"),
                        regex="specie.*tag|species")

# research indicator columns (any one is enough)
col_subtxt  <- pick_col(d0, exact=c("subquestiontype_2014","subquestiontype","subquestion_2024"),
                        regex="subquestion.*type|subquestion")
col_subnum  <- pick_col(d0, exact=c("question_2","subquestion","subq","subqnum"))
col_pdftag  <- pick_col(d0, exact=c("pdftagnew","pdftag","pdf_tag","pdftagn"))

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_answer),
          !is.na(col_sp), !is.na(col_section), !is.na(col_q))

# keep 2024 Sec 2 Q13 and **RESEARCH**
rows_q13 <- which(
  (if (!is.na(col_year)) d0[[col_year]] else 2024) == 2024 &
  d0[[col_section]] %in% c("2",2) &
  grepl("^\\s*13(\\.|$)", as.character(d0[[col_q]]))
)

# detect research logical vector
is_research <- rep(NA, nrow(d0))
if (!is.na(col_subtxt)) {
  is_research <- tolower(str_squish(as.character(d0[[col_subtxt]]))) == "research"
}
if (!is.na(col_subnum)) {
  is_research <- ifelse(is.na(is_research),
                        suppressWarnings(as.numeric(d0[[col_subnum]]) == 2),
                        is_research | suppressWarnings(as.numeric(d0[[col_subnum]]) == 2))
}
if (!is.na(col_pdftag)) {
  # pdf tag like 2024.2.13.2.xxx  → the ".2." is research
  is_research <- ifelse(is.na(is_research),
                        grepl("\\.2\\.", as.character(d0[[col_pdftag]]), perl = TRUE),
                        is_research | grepl("\\.2\\.", as.character(d0[[col_pdftag]]), perl = TRUE))
}
# if still all NA (user said the file now contains only research), assume TRUE
if (all(is.na(is_research))) is_research <- TRUE

d_sub <- d0[ rows_q13[ which(is_research[rows_q13]) ], , drop = FALSE ]

# ---------- normalized analysis frame ----------
df <- tibble(
  Region0  = to_macro_region(d_sub[[col_region]]),
  Country  = str_squish(as.character(d_sub[[col_country]])),
  SpCode   = stringr::str_pad(stringr::str_extract(as.character(d_sub[[col_sp]]), "\\d{1,3}$"), 3, pad = "0"),
  Answer   = d_sub[[col_answer]]
) %>%
  mutate(
    Species = unname(sp_map[SpCode]),
    score   = cat_to_score(Answer)
  ) %>%
  filter(!is.na(Species), !is.na(score)) %>%
  mutate(
    Region  = factor(Region0, levels = region_levels[region_levels!="World"]),
    Species = factor(Species, levels = species_levels)
  )

# ---------- collapse to country×species cells, then Region means ----------
country_cells <- df %>%
  group_by(Region, Country, Species) %>%
  summarise(score = mean(score, na.rm = TRUE), .groups = "drop")

region_species_avg <- country_cells %>%
  group_by(Region, Species) %>%
  summarise(avg_score = mean(score, na.rm = TRUE), .groups = "drop")

world_species_avg <- country_cells %>%
  group_by(Species) %>%
  summarise(avg_score = mean(score, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = factor("World", levels = "World"))

region_species_avg <- bind_rows(region_species_avg, world_species_avg) %>%
  mutate(Region = factor(as.character(Region), levels = region_levels))

# -------- wide table + totals --------
wide_tbl <- region_species_avg %>%
  tidyr::pivot_wider(names_from = Species, values_from = avg_score) %>%
  arrange(Region) %>%
  mutate(Total = rowSums(across(all_of(species_levels)), na.rm = TRUE))

# -------- plot (stacked columns, 0..18) --------
plot_data <- region_species_avg %>%
  mutate(Region = factor(as.character(Region), levels = region_levels)) %>%
  tidyr::complete(Region, Species = factor(species_levels, levels = species_levels),
                  fill = list(avg_score = 0))

p <- ggplot(plot_data, aes(x = Region, y = avg_score, fill = Species)) +
  geom_col(position = "stack", width = 0.75) +
  scale_y_continuous(limits = c(0, 18), breaks = seq(0, 18, 2), expand = expansion(mult = c(0, 0.02))) +
  scale_fill_brewer(palette = "Set3", drop = FALSE) +
  labs(title = "State of research in the field of animal breeding — 2024",
       x = NULL, y = "Score") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "bottom"
  )

ggsave(out_png, p, width = 12, height = 7.5, dpi = 300)
ggsave(out_pdf, p, width = 12, height = 7.5)

# -------- Excel outputs --------
wb <- createWorkbook()
addWorksheet(wb, "Region_Species_Avg")
addWorksheet(wb, "Region_Species_Long")
addWorksheet(wb, "Country_Cells_Long")

writeData(wb, "Region_Species_Avg",
          wide_tbl %>% mutate(across(all_of(species_levels), ~round(.x, 3))))
writeData(wb, "Region_Species_Long",
          region_species_avg %>% arrange(Region, Species) %>% mutate(avg_score = round(avg_score, 3)))
writeData(wb, "Country_Cells_Long",
          country_cells %>% arrange(Region, Country, Species))

for (sh in c("Region_Species_Avg","Region_Species_Long","Country_Cells_Long")){
  addStyle(wb, sh, createStyle(textDecoration="bold"), rows=1, cols=1:50, gridExpand=TRUE)
  setColWidths(wb, sh, cols = 1:50, widths = "auto")
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)

message(
  "Saved:\n  PNG  -> ", out_png,
  "\n  PDF  -> ", out_pdf,
  "\n  XLSX -> ", out_xlsx
)



FIGURE 3C4
# ===================== FIGURE 3C4 (2024) =====================
# State of implementation of training & technical support programmes
# Question: Section 3, Q24  (indicator SP4)
# Data: C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx
# Outputs:
#   Fig3C4_training_support_2024.(png|pdf)  -- WIDE aspect
#   Fig3C4_2024_tables.xlsx                 -- underlying tables
# =============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(forcats)
  library(ggplot2)
  library(openxlsx)
})

# -------- paths --------
infile   <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_png  <- "C:/Users/LENOVO/Documents/Fig3C4_training_support_2024.png"
out_pdf  <- "C:/Users/LENOVO/Documents/Fig3C4_training_support_2024.pdf"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3C4_2024_tables.xlsx"

# -------- options (2024 wording) --------
opt_levels <- c(
  "a. Yes, sufficient programmes have existed since before the adoption of the GPA",
  "b. Yes, sufficient programmes exist because of progress made since the adoption of the GPA",
  "c. Yes, some programmes exist (progress has been made since the adoption of the GPA)",
  "d. Yes, some programmes exist (but no progress has been made since the adoption of the GPA)",
  "e. No, but action is planned and funding identified",
  "f. No, but action is planned and funding is sought",
  "g. No"
)

# pleasant, book-like palette (dark→light green then yellows to reds)
opt_colors <- c(
  "#2ca02c", # a
  "#6cc36c", # b
  "#fff4a3", # c
  "#ffe073", # d
  "#ffb347", # e
  "#e74c3c", # f
  "#7f1d1d"  # g
)
names(opt_colors) <- opt_levels

# -------- region normalization (2024 names) --------
norm_region <- function(x){
  y <- str_squish(as.character(x))
  y <- ifelse(grepl("^South\\s*-?west\\s*Pacific$", y, ignore.case = TRUE),
              "Southwest Pacific", y)
  y <- ifelse(grepl("Europe", y, ignore.case = TRUE), "Europe", y)
  y <- ifelse(grepl("^Near", y, ignore.case = TRUE), "Near East", y)
  y <- ifelse(grepl("^Latin Ame", y, ignore.case = TRUE) |
                y %in% c("Caribbean","Central America","South America"),
              "Latin America and the Caribbean", y)
  y
}

# display order (World at top)
region_levels_display <- c(
  "World",
  "Southwest Pacific",
  "North America",
  "Near East",
  "Latin America and the Caribbean",
  "Europe",
  "Asia",
  "Africa"
)

# -------- helper: pick a column by name or regex --------
pick_col <- function(df, exact = character(), regex = NULL){
  nm <- names(df); low <- tolower(nm)
  for (c in tolower(exact)) {
    hit <- which(low == c); if (length(hit)) return(nm[hit[1]])
  }
  if (!is.null(regex)) {
    hit <- which(grepl(regex, low, perl = TRUE)); if (length(hit)) return(nm[hit[1]])
  }
  NA_character_
}

# -------- load & identify columns --------
d0 <- read_excel(infile)

col_year    <- pick_col(d0, exact = "year")
col_region  <- pick_col(d0, exact = "region")
col_country <- pick_col(d0, exact = "country")
col_section <- pick_col(d0, exact = "section")
col_q       <- pick_col(d0, exact = c("question_2024","question"))
col_answer  <- pick_col(d0, exact = "answer")
col_code    <- pick_col(d0, exact = "code")  # numeric code for options

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_answer),
          !is.na(col_section), !is.na(col_q))

# keep 2024, Section 3, Question 24 (allow "24" or "24.x")
rows_keep <- which(
  (if (!is.na(col_year)) d0[[col_year]] else 2024) == 2024 &
  d0[[col_section]] %in% c("3", 3) &
  grepl("^\\s*24(\\.|$)", as.character(d0[[col_q]]))
)
d_sub <- d0[rows_keep, , drop = FALSE]

# -------- map answer -> 2024 option --------
map_option_by_code <- function(code){
  # Code expected 1..7 in the sheet (see screenshot)
  key <- c(
    "1" = opt_levels[1],
    "2" = opt_levels[2],
    "3" = opt_levels[3],
    "4" = opt_levels[4],
    "5" = opt_levels[5],
    "6" = opt_levels[6],
    "7" = opt_levels[7]
  )
  v <- as.character(suppressWarnings(as.integer(code)))
  unname(key[v])
}
map_option_by_text <- function(txt){
  t <- tolower(str_squish(as.character(txt)))
  lead <- substr(t, 1, 2) # "a.", "b.", ...
  out  <- character(length(t))
  out[lead == "a."] <- opt_levels[1]
  out[lead == "b."] <- opt_levels[2]
  out[lead == "c."] <- opt_levels[3]
  out[lead == "d."] <- opt_levels[4]
  out[lead == "e."] <- opt_levels[5]
  out[lead == "f."] <- opt_levels[6]
  out[lead == "g."] <- opt_levels[7]
  out[is.na(out) | out == ""] <- NA_character_
  out
}

# -------- normalize dataframe --------
df <- tibble(
  Region  = norm_region(d_sub[[col_region]]),
  Country = str_squish(as.character(d_sub[[col_country]])),
  Answer0 = d_sub[[col_answer]],
  Code0   = if (!is.na(col_code)) d_sub[[col_code]] else NA
) %>%
  mutate(
    Option = map_option_by_code(Code0),
    Option = ifelse(is.na(Option), map_option_by_text(Answer0), Option),
    Region = factor(Region,
                    levels = region_levels_display[region_levels_display != "World"])
  ) %>%
  filter(!is.na(Region), !is.na(Option)) %>%
  mutate(Option = factor(Option, levels = opt_levels))

# -------- counts & percentages by region --------
reg_counts <- df %>%
  count(Region, Option, name = "n") %>%
  complete(Region, Option, fill = list(n = 0)) %>%
  group_by(Region) %>%
  mutate(N_total = sum(n),
         pct = 100 * n / pmax(N_total, 1)) %>%
  ungroup()

# -------- WORLD row --------
world_counts <- df %>%
  count(Option, name = "n") %>%
  complete(Option, fill = list(n = 0)) %>%
  mutate(Region = factor("World", levels = "World"),
         N_total = sum(n),
         pct = 100 * n / pmax(N_total, 1)) %>%
  select(Region, Option, n, N_total, pct)

# -------- combine in display order (World first) --------
stacked_tbl <- bind_rows(
  world_counts,
  reg_counts %>% mutate(Region = fct_relevel(Region, region_levels_display[region_levels_display != "World"]))
) %>%
  mutate(
    Region = fct_relevel(Region, region_levels_display),
    Option = factor(Option, levels = opt_levels),
    pct    = ifelse(is.na(pct), 0, pct)
  ) %>%
  arrange(Region, Option)

# -------- percent table for plotting (0..100, keep 0s for complete stacks) --------
stacked_pct <- stacked_tbl %>%
  group_by(Region) %>% arrange(Region, Option) %>% ungroup()

# -------- plot (WIDE aspect) --------
p <- ggplot(stacked_pct, aes(x = fct_rev(Region), y = pct, fill = Option)) +
  geom_col(width = 0.72) +
  scale_y_continuous(labels = function(v) paste0(v, "%"),
                     limits = c(0, 105)) +
  scale_fill_manual(values = opt_colors, name = NULL, drop = FALSE) +
  coord_flip() +
  geom_text(
    data = stacked_pct %>% distinct(Region, N_total),
    aes(x = fct_rev(Region), y = 102, label = N_total),
    inherit.aes = FALSE, hjust = 0, size = 3.8
  ) +
  labs(
    title = "State of implementation of training & technical-support programmes (SP4), 2024",
    subtitle = "Share of responses by option within each region (n shown at right)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "right",
    panel.grid.major.y = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 11, hjust = 0),
    plot.margin = margin(t = 10, r = 45, b = 10, l = 10)
  )

# ---- wider figure: 16 × 7 inches ----
ggsave(out_png, p, width = 16, height = 7, dpi = 300)
ggsave(out_pdf, p, width = 16, height = 7)

# -------- Excel outputs --------
wb <- createWorkbook()

# 1) Long clean rows used
addWorksheet(wb, "LONG_clean")
writeData(wb, "LONG_clean", df %>% arrange(Region, Country, Option))

# 2) Table used for the figure (counts, %, N)
addWorksheet(wb, "Stacked_100pct")
writeData(wb, "Stacked_100pct",
          stacked_pct %>% mutate(pct = round(pct, 2)) %>% arrange(Region, Option))

# 3) Per-region sheets (World included) – keep names ≤31 chars
short_region <- function(reg){
  recode(reg,
         "Latin America and the Caribbean" = "LAC",
         "Southwest Pacific"               = "SW_Pacific",
         .default = reg) %>% gsub("[^A-Za-z0-9]+","_", .)
}
make_region_sheet <- function(reg){
  tab <- stacked_pct %>%
    filter(as.character(Region) == reg) %>%
    select(Option, n, pct, N_total) %>%
    arrange(Option) %>%
    mutate(pct = round(pct, 2))
  nm <- paste0("Region_", short_region(reg))
  addWorksheet(wb, nm)
  writeData(wb, nm, tab)
  setColWidths(wb, nm, cols = 1:ncol(tab), widths = "auto")
}
for (reg in region_levels_display) make_region_sheet(reg)

# 4) QA mapping (unique raw answers -> mapped option)
qa_map <- df %>% select(Answer0, Code0, Option) %>% distinct() %>% arrange(Option, Answer0)
addWorksheet(wb, "QA_Mapping")
writeData(wb, "QA_Mapping", qa_map)

# header styling & save
for (sh in names(wb)) {
  addStyle(wb, sh, createStyle(textDecoration = "bold"),
           rows = 1, cols = 1:50, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:50, widths = "auto")
}
saveWorkbook(wb, out_xlsx, overwrite = TRUE)

message(
  "Saved:\n  Figure -> ", out_png, " and ", out_pdf,
  "\n  Excel  -> ", out_xlsx
)



IN FULL
# ===================== FIGURE 3C4 (2024) =====================
# State of implementation of training & technical-support programmes
# Q24, Section 3  (SP4 Action 1)
# Data file: C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx
# Outputs:
#   C:/Users/LENOVO/Documents/Fig3C4_training_support_2024.png
#   C:/Users/LENOVO/Documents/Fig3C4_training_support_2024.pdf
#   C:/Users/LENOVO/Documents/Fig3C4_2024_tables.xlsx
# =============================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr); library(tidyr)
  library(forcats); library(ggplot2); library(openxlsx)
})

# -------- paths --------
infile   <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_png  <- "C:/Users/LENOVO/Documents/Fig3C4_training_support_2024.png"
out_pdf  <- "C:/Users/LENOVO/Documents/Fig3C4_training_support_2024.pdf"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3C4_2024_tables.xlsx"

# -------- 2024 option texts (exact order) --------
opt_levels <- c(
  "a. Yes, sufficient programmes have existed since before the adoption of the GPA",
  "b. Yes, sufficient programmes exist because of progress made since the adoption of the GPA",
  "c. Yes, some programmes exist (progress has been made since the adoption of the GPA)",
  "d. Yes, some programmes exist (but no progress has been made since the adoption of the GPA)",
  "e. No, but action is planned and funding identified",
  "f. No, but action is planned and funding is sought",
  "g. No"
)

# Colours (left-to-right a→g). You can swap to your palette if you like.
opt_colors <- c(
  "#2ca02c", "#86c776", "#f5e87a", "#ffd862",
  "#ff9d2b", "#d44c33", "#7e1f1f"
)
names(opt_colors) <- opt_levels

# -------- 2024 region names (display order: World at top) --------
region_levels_display <- c(
  "World",
  "Southwest Pacific",
  "North America",
  "Near East",
  "Latin America and the Caribbean",
  "Europe",
  "Asia",
  "Africa"
)

# -------- helpers --------
norm_region <- function(x){
  # Vectorised, 2024 names
  y <- str_squish(as.character(x))
  y <- ifelse(y %in% c("South west Pacific","South-west Pacific","South West Pacific"),
              "Southwest Pacific", y)
  y <- ifelse(y %in% c("Europe and the Caucasus","Europe (including Caucasus)","Europe & the Caucasus"),
              "Europe", y)
  y <- ifelse(str_detect(y, regex("^Near", ignore_case = TRUE)), "Near East", y)
  y <- ifelse(str_detect(y, regex("^Latin Ame", ignore_case = TRUE)) |
                y %in% c("Caribbean","Central America","South America"),
              "Latin America and the Caribbean", y)
  y
}

# Map by numeric Code (1..7); returns factor with opt_levels
map_option_by_code <- function(code_vec){
  code_num <- suppressWarnings(as.integer(code_vec))
  fct <- factor(code_num, levels = 1:7,
                labels = opt_levels)
  as.character(fct)
}

# Fallback: map by text found in Answer (very tolerant)
map_option_by_text <- function(ans_vec){
  a <- tolower(str_squish(as.character(ans_vec)))
  res <- case_when(
    str_detect(a, "^a\\.|sufficient.*before")                              ~ opt_levels[1],
    str_detect(a, "^b\\.|sufficient.*progress")                            ~ opt_levels[2],
    str_detect(a, "^c\\.|some programme.*progress")                        ~ opt_levels[3],
    str_detect(a, "^d\\.|some programme.*no progress")                     ~ opt_levels[4],
    str_detect(a, "^e\\.|no.*action.*identified")                          ~ opt_levels[5],
    str_detect(a, "^f\\.|no.*action.*sought")                              ~ opt_levels[6],
    str_detect(a, "^g\\.|^no$")                                            ~ opt_levels[7],
    TRUE                                                                   ~ NA_character_
  )
  res
}

short_region <- function(reg){
  recode(reg,
         "Latin America and the Caribbean" = "LAC",
         .default = reg) |>
    gsub("[^A-Za-z0-9]+","_", x = _)
}

# -------- load --------
d0 <- read_excel(infile)

# identify columns (robust)
pick <- function(df, exact = NULL, regex = NULL){
  nm <- names(df); lo <- tolower(nm)
  if (!is.null(exact)) {
    for (e in tolower(exact)) {
      hit <- which(lo == e)
      if (length(hit)) return(nm[hit[1]])
    }
  }
  if (!is.null(regex)) {
    hit <- which(grepl(regex, lo, perl = TRUE))
    if (length(hit)) return(nm[hit[1]])
  }
  NA_character_
}
col_year    <- pick(d0, exact="year")
col_region  <- pick(d0, exact="region")
col_country <- pick(d0, exact="country")
col_section <- pick(d0, exact="section")
col_q       <- pick(d0, regex="^question(_2024)?$")
col_answer  <- pick(d0, exact="answer")
col_code    <- pick(d0, exact="code")

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_q), !is.na(col_answer), !is.na(col_code))

# -------- filter to Sec 3, Q24 (accept 24 or 24.xx) --------
rows_keep <- which(
  (if (!is.na(col_year)) d0[[col_year]] else 2024) == 2024 &
    d0[[col_section]] %in% c("3", 3) &
    grepl("^\\s*24(\\.|$)", as.character(d0[[col_q]]))
)
d_sub <- d0[rows_keep, , drop = FALSE]

# -------- normalise long table --------
df <- tibble(
  Region  = norm_region(d_sub[[col_region]]),
  Country = str_squish(as.character(d_sub[[col_country]])),
  Answer0 = d_sub[[col_answer]],
  Code0   = d_sub[[col_code]]
) |>
  mutate(
    Option = map_option_by_code(Code0),
    Option = ifelse(is.na(Option), map_option_by_text(Answer0), Option),
    Region = factor(Region,
                    levels = region_levels_display[region_levels_display != "World"])
  ) |>
  filter(!is.na(Region), !is.na(Option)) |>
  mutate(
    Option = factor(Option, levels = opt_levels)
  )

# ---- QA (quick sanity, optional) ----
cat("\n[QA] Rows kept by Region:\n")
print(df |> count(Region) |> arrange(desc(n)))
cat("\n[QA] Example breakdown (Africa):\n")
print(df |> filter(Region=="Africa") |> count(Option) |> arrange(Option))

# -------- counts & percentages by region --------
reg_counts <- df |>
  count(Region, Option, name = "n") |>
  complete(Region, Option, fill = list(n = 0)) |>
  group_by(Region) |>
  mutate(N_total = sum(n),
         pct = 100 * n / pmax(N_total, 1)) |>
  ungroup()

# -------- WORLD row --------
world_counts <- df |>
  count(Option, name = "n") |>
  complete(Option, fill = list(n = 0)) |>
  mutate(Region  = factor("World", levels = "World"),
         N_total = sum(n),
         pct     = 100 * n / pmax(N_total, 1)) |>
  select(Region, Option, n, N_total, pct)

# -------- combine in display order (World first) --------
stacked_tbl <- bind_rows(
  world_counts,
  reg_counts |>
    mutate(Region = fct_relevel(Region,
                                region_levels_display[region_levels_display != "World"]))
) |>
  mutate(
    Region = fct_relevel(Region, region_levels_display),
    Option = factor(Option, levels = opt_levels),
    pct    = ifelse(is.na(pct), 0, pct)
  ) |>
  arrange(Region, Option)

# -------- figure table (0..100%) --------
stacked_pct <- stacked_tbl

# -------- plot (stack real % so every colour that exists appears) --------
p <- ggplot(stacked_pct, aes(x = fct_rev(Region), y = pct, fill = Option)) +
  geom_col(width = 0.7) +
  scale_y_continuous(labels = function(v) paste0(v, "%"), limits = c(0, 105)) +
  scale_fill_manual(values = opt_colors, name = NULL, drop = FALSE) +
  coord_flip() +
  geom_text(
    data = stacked_pct %>% distinct(Region, N_total),
    aes(x = fct_rev(Region), y = 102, label = N_total),
    inherit.aes = FALSE, hjust = 0, size = 3.8
  ) +
  labs(
    title = "State of implementation of training & technical-support programmes\nfor the breeding activities of livestock-keeping communities (2024, SP4)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 11),
    legend.position = "right",
    panel.grid.major.y = element_blank(),
    plot.title = element_text(size = 13.5, face = "bold", hjust = 0),
    plot.margin = margin(t = 10, r = 40, b = 10, l = 10)
  )

ggsave(out_png, p, width = 12, height = 7.5, dpi = 300)
ggsave(out_pdf, p, width = 12, height = 7.5)

# -------- Excel --------
wb <- createWorkbook()

# LONG clean
addWorksheet(wb, "LONG_clean")
writeData(wb, "LONG_clean", df %>% arrange(Region, Country, Option))

# Figure table (counts, %, N)
addWorksheet(wb, "Stacked_100pct")
writeData(wb, "Stacked_100pct",
          stacked_pct %>% mutate(pct = round(pct, 2)) %>% arrange(Region, Option))

# Per-region sheets (World included; safe names)
make_region_sheet <- function(reg){
  tab <- stacked_pct %>%
    filter(as.character(Region) == reg) %>%
    select(Option, n, pct, N_total) %>%
    arrange(Option) %>%
    mutate(pct = round(pct, 2))
  nm <- paste0("Region_", short_region(reg))
  addWorksheet(wb, nm)
  writeData(wb, nm, tab)
  setColWidths(wb, nm, cols = 1:ncol(tab), widths = "auto")
}
for (reg in region_levels_display) make_region_sheet(reg)

# Mapping QA (unique raw answers → mapped option)
qa_map <- df %>% select(Answer0, Code0, Option) %>% distinct() %>% arrange(Option, Answer0)
addWorksheet(wb, "QA_Mapping")
writeData(wb, "QA_Mapping", qa_map)

# Style & save
for (sh in names(wb)) {
  addStyle(wb, sh, createStyle(textDecoration = "bold"),
           rows = 1, cols = 1:50, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:50, widths = "auto")
}
saveWorkbook(wb, out_xlsx, overwrite = TRUE)

cat("\nSaved outputs:\n  Figure ->", out_png, "and", out_pdf,
    "\n  Excel  ->", out_xlsx, "\n")







Table 3C5
# ==============================
# Fig 3B (Question 11, 2024) — Regions tables incl. WORLD
# ==============================

# --- Packages ---
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(openxlsx)
library(rlang)

# --- Paths ---
in_path  <- "C:\\Users\\LENOVO\\Documents\\Fig3B1_Data.xlsx"
out_path <- "C:\\Users\\LENOVO\\Documents\\Fig3B1_Output.xlsx"

# --- Species (as they appear in TableLabel_2014, case-insensitive) ---
main_species <- c(
  "cattle (specialized dairy)",
  "cattle (specialized beef)",
  "cattle (multipurpose)",
  "goats", "sheep", "pigs", "chickens"
)

# --- Region list / order (EXACT labels for 2024) ---
region_levels <- c(
  "Africa","Asia","Southwest Pacific","Europe",
  "Latin America and the Carribean","North America",
  "Near East","World"
)

# --- Programme elements per table (exact BreedType_2014 labels) ---
table1_elements <- c(
  "Animal identification",
  "Pedigree recording",
  "Performance recording",
  "Artificial insemination"
)

table2_elements <- c(
  "Breeding goal defined",
  "Genetic evaluation (classic approach)",
  "Genetic evaluation including genomic information",
  "Management of genetic variation (by maximizing effective population size or minimizing rate of inbreeding)"
)

# Map Ex/Loc codes to nice labels
type_map <- c("Ex" = "Exotic", "Loc" = "Locally adapted")

# ---------------------------
# Read and normalize columns
# ---------------------------
d0 <- read_excel(in_path)

# Normalize header names: trim, replace non-alphanum by "_", tolower
names(d0) <- names(d0) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+", "_") |>
  tolower()

nm <- names(d0)

# Helper: fuzzy find a column
find_col <- function(primary_regex, fallback_regex = NULL) {
  hit <- nm[grepl(primary_regex, nm, ignore.case = TRUE)]
  if (length(hit) == 0 && !is.null(fallback_regex)) {
    hit <- nm[grepl(fallback_regex, nm, ignore.case = TRUE)]
  }
  if (length(hit) == 0) NA_character_ else hit[1]
}

region_col      <- find_col("^region$")
section_col     <- find_col("^section$")
question_col    <- find_col("^question_?2024$", "question.*2024")
answer_col      <- find_col("^answer$")
breedtype_col   <- find_col("^breedtype_?2014$")
subqtype_col    <- find_col("^subquestiontype_?2014$")
tablelabel_col  <- find_col("^tablelabel_?2014$")

needed <- c(region_col, section_col, question_col, answer_col,
            breedtype_col, subqtype_col, tablelabel_col)
if (any(is.na(needed))) {
  stop("Required columns not found. Matched columns:\n",
       " region=", region_col, "\n section=", section_col, "\n question=", question_col,
       "\n answer=", answer_col, "\n breedtype=", breedtype_col,
       "\n subquestiontype=", subqtype_col, "\n tablelabel=", tablelabel_col, "\n")
}

# Standardize + rename to canonical names
d <- d0 %>%
  rename(
    Region_raw           = !!region_col,
    Section              = !!section_col,
    Question             = !!question_col,
    Answer               = !!answer_col,
    BreedType_2014       = !!breedtype_col,
    SubquestionType_2014 = !!subqtype_col,
    TableLabel_2014_raw  = !!tablelabel_col
  ) %>%
  mutate(
    Section = suppressWarnings(as.numeric(Section)),
    Question = suppressWarnings(as.numeric(Question)),
    Answer   = suppressWarnings(as.numeric(Answer)),
    TableLabel_2014 = str_squish(tolower(as.character(TableLabel_2014_raw))),
    BreedType_2014  = str_squish(as.character(BreedType_2014)),
    SubquestionType_2014 = str_squish(as.character(SubquestionType_2014))
  )

# Normalize Region names to EXACT labels you provided
d <- d %>%
  mutate(
    Region = case_when(
      str_detect(Region_raw, regex("^africa$", TRUE)) ~ "Africa",
      str_detect(Region_raw, regex("^asia$", TRUE)) ~ "Asia",
      str_detect(Region_raw, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
      str_detect(Region_raw, regex("^europe", TRUE)) ~ "Europe",
      str_detect(Region_raw, regex("^latin\\s+america", TRUE)) ~ "Latin America and the Carribean",
      str_detect(Region_raw, regex("^north\\s+america", TRUE)) ~ "North America",
      str_detect(Region_raw, regex("near\\s*east", TRUE)) ~ "Near East",
      str_detect(Region_raw, regex("^world$", TRUE)) ~ "World",
      TRUE ~ NA_character_
    )
  )

# ---------------------------
# Filter to scope
# ---------------------------
d <- d %>%
  filter(
    Section == 2,
    Question == 11,
    TableLabel_2014 %in% main_species,
    Region %in% region_levels
  ) %>%
  mutate(
    Type = recode(SubquestionType_2014, !!!type_map),
    Region = factor(Region, levels = region_levels),
    Type   = factor(Type, levels = c("Exotic","Locally adapted"))
  )

# ---------------------------
# Denominator (YOUR rule)
# Sum of Answer by Region × Type (across the 7 species)
# + Add WORLD = sum across regions by Type
# ---------------------------
N_by_region_type <- d %>%
  group_by(Region, Type) %>%
  summarise(N_breeds = sum(Answer, na.rm = TRUE), .groups = "drop")

# Compute WORLD denominators (from the filtered data)
world_den <- d %>%
  group_by(Type) %>%
  summarise(N_breeds = sum(Answer, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = factor("World", levels = region_levels)) %>%
  select(Region, Type, N_breeds)

# Bind (drop existing World if present to avoid double)
N_by_region_type <- N_by_region_type %>%
  filter(as.character(Region) != "World") %>%
  bind_rows(world_den) %>%
  mutate(
    Region = factor(as.character(Region), levels = region_levels),
    Type   = factor(Type, levels = c("Exotic","Locally adapted"))
  ) %>%
  arrange(Region, Type)

# ---------------------------
# Coverage % for each block (incl. WORLD)
# ---------------------------
make_percent_long <- function(elements_vec) {
  # region-level numerators
  reg_num <- d %>%
    filter(BreedType_2014 %in% elements_vec) %>%
    group_by(Region, Type, BreedType_2014) %>%
    summarise(n_cov = sum(Answer, na.rm = TRUE), .groups = "drop")
  # WORLD numerators
  world_num <- d %>%
    filter(BreedType_2014 %in% elements_vec) %>%
    group_by(Type, BreedType_2014) %>%
    summarise(n_cov = sum(Answer, na.rm = TRUE), .groups = "drop") %>%
    mutate(Region = factor("World", levels = region_levels)) %>%
    select(Region, Type, BreedType_2014, n_cov)

  num_all <- reg_num %>%
    filter(as.character(Region) != "World") %>%
    bind_rows(world_num)

  out <- num_all %>%
    left_join(N_by_region_type, by = c("Region","Type")) %>%
    mutate(
      pct = ifelse(N_breeds > 0, 100 * n_cov / N_breeds, NA_real_),
      BreedType_2014 = factor(BreedType_2014, levels = elements_vec),
      Region = factor(Region, levels = region_levels),
      Type   = factor(Type, levels = c("Exotic","Locally adapted"))
    ) %>%
    arrange(Region, BreedType_2014, Type)

  out
}

long_tbl1 <- make_percent_long(table1_elements)
long_tbl2 <- make_percent_long(table2_elements)

to_wide <- function(long_tbl) {
  long_tbl %>%
    mutate(col = paste0(as.character(BreedType_2014), "—", as.character(Type))) %>%
    select(Region, col, pct) %>%
    pivot_wider(names_from = col, values_from = pct) %>%
    arrange(Region)
}

wide_tbl1 <- to_wide(long_tbl1)
wide_tbl2 <- to_wide(long_tbl2)

# ---------------------------
# Build report-style sheets (match figure layout) incl. WORLD
# ---------------------------
# Counts (Number of national breed populations) side-by-side
counts_wide <- N_by_region_type %>%
  mutate(col = paste0("Number of national breed populations—", as.character(Type))) %>%
  select(Region, col, N_breeds) %>%
  pivot_wider(names_from = col, values_from = N_breeds) %>%
  arrange(Region) %>%
  select(
    Region,
    `Number of national breed populations—Exotic`,
    `Number of national breed populations—Locally adapted`
  )

order_report_cols <- function(wide_tbl, elements_vec) {
  # interleave pairs: Exotic then Locally adapted for each element
  pairs <- as.vector(rbind(
    paste0(elements_vec, "—Exotic"),
    paste0(elements_vec, "—Locally adapted")
  ))
  counts_wide %>%
  left_join(wide_tbl %>% select(Region, all_of(pairs)), by = "Region")
}

report_tbl1 <- order_report_cols(wide_tbl1, table1_elements)
report_tbl2 <- order_report_cols(wide_tbl2, table2_elements)

# ---------------------------
# Write workbook
# ---------------------------
wb <- createWorkbook()

addWorksheet(wb, "README")
addWorksheet(wb, "Filtered_data")
addWorksheet(wb, "Denominator_ByRegion")
addWorksheet(wb, "Table1_long")
addWorksheet(wb, "Table1_wide")
addWorksheet(wb, "Table2_long")
addWorksheet(wb, "Table2_wide")
addWorksheet(wb, "Table1_report")
addWorksheet(wb, "Table2_report")

writeData(wb, "README",
"Build: 2024, Section = 2, Question = 11. Species: 3 cattle types + goats + sheep + pigs + chickens.
Regions (exact): Africa, Asia, Southwest Pacific, Europe, Latin America and the Carribean, North America, Near East, World.

Denominator rule: Number of national breed populations for a region & type
= SUM of 'Answer' across the 7 species (Region × Type). WORLD is computed as the sum across regions by Type.

For each breeding-programme element, coverage = SUM of 'Answer' for that element × region × type.
Percent = 100 * coverage / denominator.")

writeData(wb, "Filtered_data",
          d %>% select(Region, Type, TableLabel_2014, BreedType_2014, Answer) %>%
            arrange(Region, Type, TableLabel_2014, BreedType_2014))
writeData(wb, "Denominator_ByRegion", N_by_region_type)
writeData(wb, "Table1_long", long_tbl1)
writeData(wb, "Table1_wide", wide_tbl1)
writeData(wb, "Table2_long", long_tbl2)
writeData(wb, "Table2_wide", wide_tbl2)
writeData(wb, "Table1_report", report_tbl1)
writeData(wb, "Table2_report", report_tbl2)

# Style numeric columns as integers for display
int_style <- createStyle(numFmt = "0")
for (sh in c("Table1_report","Table2_report","Table1_wide","Table2_wide",
             "Table1_long","Table2_long","Denominator_ByRegion")) {
  df <- readWorkbook(wb, sh)
  if (!is.null(df) && nrow(df) > 0) {
    num_cols <- which(vapply(df, is.numeric, TRUE))
    if (length(num_cols)) {
      addStyle(wb, sh, style = int_style,
               rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
    }
  }
}

saveWorkbook(wb, out_path, overwrite = TRUE)
message("Done. Excel written to: ", out_path)



TABLE 3C6
# ==============================
# Fig 3B (Question 11, 2024)
# Regions (incl. WORLD) + Species tables
# ==============================

# --- Packages ---
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(openxlsx)
library(rlang)

# --- Paths ---
in_path  <- "C:\\Users\\LENOVO\\Documents\\Fig3B1_Data.xlsx"
out_path <- "C:\\Users\\LENOVO\\Documents\\Fig3B1_Output.xlsx"

# --- Species (as they appear in TableLabel_2014, case-insensitive) ---
main_species <- c(
  "cattle (specialized dairy)",
  "cattle (specialized beef)",
  "cattle (multipurpose)",
  "goats", "sheep", "pigs", "chickens"
)

# Display names for species rows (match screenshot order/labels)
species_display_map <- c(
  "cattle (specialized dairy)"       = "Dairy cattle",
  "cattle (specialized beef)"        = "Beef cattle",
  "cattle (multipurpose)"            = "Multipurpose cattle",
  "sheep"                            = "Sheep",
  "goats"                            = "Goats",
  "pigs"                             = "Pigs",
  "chickens"                         = "Chickens"
)
species_order <- unname(species_display_map[main_species])

# --- Region list / order (EXACT labels for 2024) ---
region_levels <- c(
  "Africa","Asia","Southwest Pacific","Europe",
  "Latin America and the Carribean","North America",
  "Near East","World"
)

# --- Programme elements per table (exact BreedType_2014 labels) ---
table1_elements <- c(
  "Animal identification",
  "Pedigree recording",
  "Performance recording",
  "Artificial insemination"
)

table2_elements <- c(
  "Breeding goal defined",
  "Genetic evaluation (classic approach)",
  "Genetic evaluation including genomic information",
  "Management of genetic variation (by maximizing effective population size or minimizing rate of inbreeding)"
)

# Map Ex/Loc codes to nice labels
type_map <- c("Ex" = "Exotic", "Loc" = "Locally adapted")
type_levels <- c("Exotic","Locally adapted")

# ---------------------------
# Read and normalize columns
# ---------------------------
d0 <- read_excel(in_path)

# Normalize header names
names(d0) <- names(d0) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+", "_") |>
  tolower()

nm <- names(d0)
find_col <- function(primary_regex, fallback_regex = NULL) {
  hit <- nm[grepl(primary_regex, nm, ignore.case = TRUE)]
  if (length(hit) == 0 && !is.null(fallback_regex)) {
    hit <- nm[grepl(fallback_regex, nm, ignore.case = TRUE)]
  }
  if (length(hit) == 0) NA_character_ else hit[1]
}

region_col      <- find_col("^region$")
section_col     <- find_col("^section$")
question_col    <- find_col("^question_?2024$", "question.*2024")
answer_col      <- find_col("^answer$")
breedtype_col   <- find_col("^breedtype_?2014$")
subqtype_col    <- find_col("^subquestiontype_?2014$")
tablelabel_col  <- find_col("^tablelabel_?2014$")

needed <- c(region_col, section_col, question_col, answer_col,
            breedtype_col, subqtype_col, tablelabel_col)
if (any(is.na(needed))) {
  stop("Required columns not found. Matched columns:\n",
       " region=", region_col, "\n section=", section_col, "\n question=", question_col,
       "\n answer=", answer_col, "\n breedtype=", breedtype_col,
       "\n subquestiontype=", subqtype_col, "\n tablelabel=", tablelabel_col, "\n")
}

d <- d0 %>%
  rename(
    Region_raw           = !!region_col,
    Section              = !!section_col,
    Question             = !!question_col,
    Answer               = !!answer_col,
    BreedType_2014       = !!breedtype_col,
    SubquestionType_2014 = !!subqtype_col,
    TableLabel_2014_raw  = !!tablelabel_col
  ) %>%
  mutate(
    Section = suppressWarnings(as.numeric(Section)),
    Question = suppressWarnings(as.numeric(Question)),
    Answer   = suppressWarnings(as.numeric(Answer)),
    TableLabel_2014 = str_squish(tolower(as.character(TableLabel_2014_raw))),
    BreedType_2014  = str_squish(as.character(BreedType_2014)),
    SubquestionType_2014 = str_squish(as.character(SubquestionType_2014))
  )

# Normalize Region names to EXACT labels
d <- d %>%
  mutate(
    Region = case_when(
      str_detect(Region_raw, regex("^africa$", TRUE)) ~ "Africa",
      str_detect(Region_raw, regex("^asia$", TRUE)) ~ "Asia",
      str_detect(Region_raw, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
      str_detect(Region_raw, regex("^europe", TRUE)) ~ "Europe",
      str_detect(Region_raw, regex("^latin\\s+america", TRUE)) ~ "Latin America and the Carribean",
      str_detect(Region_raw, regex("^north\\s+america", TRUE)) ~ "North America",
      str_detect(Region_raw, regex("near\\s*east", TRUE)) ~ "Near East",
      str_detect(Region_raw, regex("^world$", TRUE)) ~ "World",
      TRUE ~ NA_character_
    )
  )

# ---------------------------
# Filter scope
# ---------------------------
d <- d %>%
  filter(
    Section == 2,
    Question == 11,
    TableLabel_2014 %in% main_species,
    Region %in% region_levels
  ) %>%
  mutate(
    Type = recode(SubquestionType_2014, !!!type_map),
    Region = factor(Region, levels = region_levels),
    Type   = factor(Type, levels = type_levels),
    Species_key = TableLabel_2014,
    Species = factor(unname(species_display_map[Species_key]), levels = species_order)
  )

# ===========================
# PART A: REGION TABLES (incl. WORLD)
# ===========================
# Denominator by Region × Type (sum of Answer across all species)
N_by_region_type <- d %>%
  group_by(Region, Type) %>%
  summarise(N_breeds = sum(Answer, na.rm = TRUE), .groups = "drop")

# WORLD denominators from filtered data
world_den <- d %>%
  group_by(Type) %>%
  summarise(N_breeds = sum(Answer, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = factor("World", levels = region_levels)) %>%
  select(Region, Type, N_breeds)

N_by_region_type <- N_by_region_type %>%
  filter(as.character(Region) != "World") %>%
  bind_rows(world_den) %>%
  mutate(
    Region = factor(as.character(Region), levels = region_levels),
    Type   = factor(Type, levels = type_levels)
  ) %>%
  arrange(Region, Type)

# Helper to compute % by Region × Type for a set of elements
make_percent_long_region <- function(elements_vec) {
  reg_num <- d %>%
    filter(BreedType_2014 %in% elements_vec) %>%
    group_by(Region, Type, BreedType_2014) %>%
    summarise(n_cov = sum(Answer, na.rm = TRUE), .groups = "drop")
  world_num <- d %>%
    filter(BreedType_2014 %in% elements_vec) %>%
    group_by(Type, BreedType_2014) %>%
    summarise(n_cov = sum(Answer, na.rm = TRUE), .groups = "drop") %>%
    mutate(Region = factor("World", levels = region_levels)) %>%
    select(Region, Type, BreedType_2014, n_cov)

  num_all <- reg_num %>%
    filter(as.character(Region) != "World") %>%
    bind_rows(world_num)

  num_all %>%
    left_join(N_by_region_type, by = c("Region","Type")) %>%
    mutate(
      pct = ifelse(N_breeds > 0, 100 * n_cov / N_breeds, NA_real_),
      BreedType_2014 = factor(BreedType_2014, levels = elements_vec),
      Region = factor(Region, levels = region_levels),
      Type   = factor(Type, levels = type_levels)
    ) %>%
    arrange(Region, BreedType_2014, Type)
}

long_reg_1 <- make_percent_long_region(table1_elements)
long_reg_2 <- make_percent_long_region(table2_elements)

to_wide <- function(long_tbl) {
  long_tbl %>%
    mutate(col = paste0(as.character(BreedType_2014), "—", as.character(Type))) %>%
    select(Region, col, pct) %>%
    pivot_wider(names_from = col, values_from = pct) %>%
    arrange(Region)
}

wide_reg_1 <- to_wide(long_reg_1)
wide_reg_2 <- to_wide(long_reg_2)

# Build report-style region tables (counts + pairs per element)
counts_region_wide <- N_by_region_type %>%
  mutate(col = paste0("Number of national breed populations—", as.character(Type))) %>%
  select(Region, col, N_breeds) %>%
  pivot_wider(names_from = col, values_from = N_breeds) %>%
  arrange(Region) %>%
  select(
    Region,
    `Number of national breed populations—Exotic`,
    `Number of national breed populations—Locally adapted`
  )

order_report_cols_region <- function(wide_tbl, elements_vec) {
  pairs <- as.vector(rbind(
    paste0(elements_vec, "—Exotic"),
    paste0(elements_vec, "—Locally adapted")
  ))
  counts_region_wide %>%
    left_join(wide_tbl %>% select(Region, all_of(pairs)), by = "Region")
}

report_reg_1 <- order_report_cols_region(wide_reg_1, table1_elements)
report_reg_2 <- order_report_cols_region(wide_reg_2, table2_elements)

# ===========================
# PART B: SPECIES TABLES (world totals by species)
# ===========================
# Denominator by Species × Type (sum across ALL regions)
N_by_species_type <- d %>%
  group_by(Species, Type) %>%
  summarise(N_breeds = sum(Answer, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Species = factor(Species, levels = species_order),
    Type    = factor(Type, levels = type_levels)
  ) %>%
  arrange(Species, Type)

# Helper: % by Species × Type for a set of elements
make_percent_long_species <- function(elements_vec) {
  d %>%
    filter(BreedType_2014 %in% elements_vec) %>%
    group_by(Species, Type, BreedType_2014) %>%
    summarise(n_cov = sum(Answer, na.rm = TRUE), .groups = "drop") %>%
    left_join(N_by_species_type, by = c("Species","Type")) %>%
    mutate(
      pct = ifelse(N_breeds > 0, 100 * n_cov / N_breeds, NA_real_),
      BreedType_2014 = factor(BreedType_2014, levels = elements_vec),
      Species = factor(Species, levels = species_order),
      Type    = factor(Type, levels = type_levels)
    ) %>%
    arrange(Species, BreedType_2014, Type)
}

long_sp_1 <- make_percent_long_species(table1_elements)
long_sp_2 <- make_percent_long_species(table2_elements)

to_wide_species <- function(long_tbl) {
  long_tbl %>%
    mutate(col = paste0(as.character(BreedType_2014), "—", as.character(Type))) %>%
    select(Species, col, pct) %>%
    pivot_wider(names_from = col, values_from = pct) %>%
    arrange(Species)
}

wide_sp_1 <- to_wide_species(long_sp_1)
wide_sp_2 <- to_wide_species(long_sp_2)

# Build report-style SPECIES tables (counts + pairs per element)
counts_species_wide <- N_by_species_type %>%
  mutate(col = paste0("Number of national breed populations—", as.character(Type))) %>%
  select(Species, col, N_breeds) %>%
  pivot_wider(names_from = col, values_from = N_breeds) %>%
  arrange(Species) %>%
  select(
    Species,
    `Number of national breed populations—Exotic`,
    `Number of national breed populations—Locally adapted`
  )

order_report_cols_species <- function(wide_tbl, elements_vec) {
  pairs <- as.vector(rbind(
    paste0(elements_vec, "—Exotic"),
    paste0(elements_vec, "—Locally adapted")
  ))
  counts_species_wide %>%
    left_join(wide_tbl %>% select(Species, all_of(pairs)), by = "Species")
}

report_sp_1 <- order_report_cols_species(wide_sp_1, table1_elements)
report_sp_2 <- order_report_cols_species(wide_sp_2, table2_elements)

# ===========================
# Write workbook
# ===========================
wb <- createWorkbook()

addWorksheet(wb, "README")
addWorksheet(wb, "Filtered_data")
addWorksheet(wb, "Denom_Region")
addWorksheet(wb, "Denom_Species")
addWorksheet(wb, "Region_Table1_long")
addWorksheet(wb, "Region_Table1_wide")
addWorksheet(wb, "Region_Table2_long")
addWorksheet(wb, "Region_Table2_wide")
addWorksheet(wb, "Species_Table1_long")
addWorksheet(wb, "Species_Table1_wide")
addWorksheet(wb, "Species_Table2_long")
addWorksheet(wb, "Species_Table2_wide")
addWorksheet(wb, "Table1_report_regions")
addWorksheet(wb, "Table2_report_regions")
addWorksheet(wb, "Table1_report_species")
addWorksheet(wb, "Table2_report_species")

writeData(wb, "README",
"Build: 2024, Section = 2, Question = 11. Species: 3 cattle types + goats + sheep + pigs + chickens.
Regions (exact): Africa, Asia, Southwest Pacific, Europe, Latin America and the Carribean, North America, Near East, World.

REGION TABLES:
- Denominator = SUM of 'Answer' by Region × Type (across all species). WORLD is the sum across regions by Type.

SPECIES TABLES:
- Denominator = SUM of 'Answer' by Species × Type across ALL regions (i.e., world totals for each species).

For each breeding-programme element, coverage is the SUM of 'Answer' for that element × (Region/Species) × Type.
Percent = 100 * coverage / denominator.

Report sheets match the figure layout: counts first, then Exotic/Locally adapted pairs for each element.")

writeData(wb, "Filtered_data",
          d %>% select(Region, Species, Type, Species_key, BreedType_2014, Answer) %>%
            arrange(Region, Species, Type, BreedType_2014))

writeData(wb, "Denom_Region",  N_by_region_type)
writeData(wb, "Denom_Species", N_by_species_type)

writeData(wb, "Region_Table1_long", long_reg_1)
writeData(wb, "Region_Table1_wide", wide_reg_1)
writeData(wb, "Region_Table2_long", long_reg_2)
writeData(wb, "Region_Table2_wide", wide_reg_2)

writeData(wb, "Species_Table1_long", long_sp_1)
writeData(wb, "Species_Table1_wide", wide_sp_1)
writeData(wb, "Species_Table2_long", long_sp_2)
writeData(wb, "Species_Table2_wide", wide_sp_2)

writeData(wb, "Table1_report_regions", report_reg_1)
writeData(wb, "Table2_report_regions", report_reg_2)
writeData(wb, "Table1_report_species", report_sp_1)
writeData(wb, "Table2_report_species", report_sp_2)

# Style numeric columns as integers for display
int_style <- createStyle(numFmt = "0")
for (sh in sheets(wb)) {
  df <- readWorkbook(wb, sh)
  if (!is.null(df) && nrow(df) > 0) {
    num_cols <- which(vapply(df, is.numeric, TRUE))
    if (length(num_cols)) {
      addStyle(wb, sh, style = int_style,
               rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
    }
  }
}

saveWorkbook(wb, out_path, overwrite = TRUE)
message("Done. Excel written to: ", out_path)



TABLE 3C7
# ============================================
# Q12 (Section 2) – Regions x Species tables
#  - Table A: Straight/pure-breeding AND cross-breeding
#  - Table B: Straight/pure-breeding ONLY
# Denominator (per user): total of Answer by Region × Type within each table
# (i.e., across all 7 species for that programme in that region & type)
# ============================================

# --- Packages ---
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(openxlsx)
library(rlang)

# --- Paths ---
in_path  <- "C:\\Users\\LENOVO\\Documents\\Fig3B1_Data.xlsx"
out_path <- "C:\\Users\\LENOVO\\Documents\\Fig3B1_Q12_Output.xlsx"

# --- Species to include (exactly seven) ---
main_species <- c(
  "cattle (specialized dairy)",
  "cattle (specialized beef)",
  "cattle (multipurpose)",
  "sheep","goats","pigs","chickens"
)

# Display labels & order (as in the book)
species_display_map <- c(
  "cattle (specialized dairy)" = "Dairy cattle",
  "cattle (specialized beef)"  = "Beef cattle",
  "cattle (multipurpose)"      = "Multipurpose cattle",
  "sheep"                      = "Sheep",
  "goats"                      = "Goats",
  "pigs"                       = "Pigs",
  "chickens"                   = "Chickens"
)
species_order <- unname(species_display_map[main_species])

# --- Regions (your exact 2024 list) ---
region_levels <- c(
  "Africa","Asia","Southwest Pacific","Europe",
  "Latin America and the Carribean","North America","Near East","World"
)

# --- Type mapping ---
type_map    <- c("Ex" = "Exotic", "Loc" = "Locally adapted")
type_levels <- c("Exotic","Locally adapted")

# --- Q12 programme detection (robust to punctuation/case) ---
prog_regex_both <- regex("straight.*pure.*breeding.*cross", ignore_case = TRUE)
prog_regex_only <- regex("straight.*pure.*breeding.*only",  ignore_case = TRUE)

# ---------------------------
# Read & normalize columns
# ---------------------------
d0 <- read_excel(in_path)

# Normalize headers
names(d0) <- names(d0) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+", "_") |>
  tolower()

nm <- names(d0)
find_col <- function(primary_regex, fallback_regex = NULL) {
  hit <- nm[grepl(primary_regex, nm, ignore.case = TRUE)]
  if (length(hit) == 0 && !is.null(fallback_regex)) {
    hit <- nm[grepl(fallback_regex, nm, ignore.case = TRUE)]
  }
  if (length(hit) == 0) NA_character_ else hit[1]
}

region_col      <- find_col("^region$")
section_col     <- find_col("^section$")
question_col    <- find_col("^question_?2024$", "question.*2024")
answer_col      <- find_col("^answer$")
breedtype_col   <- find_col("^breedtype_?2014$")
subqtype_col    <- find_col("^subquestiontype_?2014$")
tablelabel_col  <- find_col("^tablelabel_?2014$")

needed <- c(region_col, section_col, question_col, answer_col,
            breedtype_col, subqtype_col, tablelabel_col)
if (any(is.na(needed))) {
  stop("Required columns not found. Matched columns:\n",
       " region=", region_col, "\n section=", section_col, "\n question=", question_col,
       "\n answer=", answer_col, "\n breedtype=", breedtype_col,
       "\n subquestiontype=", subqtype_col, "\n tablelabel=", tablelabel_col)
}

d_all <- d0 %>%
  rename(
    Region_raw           = !!region_col,
    Section              = !!section_col,
    Question             = !!question_col,
    Answer               = !!answer_col,
    BreedType_2014       = !!breedtype_col,
    SubquestionType_2014 = !!subqtype_col,
    TableLabel_2014_raw  = !!tablelabel_col
  ) %>%
  mutate(
    Section = suppressWarnings(as.numeric(Section)),
    Question = suppressWarnings(as.numeric(Question)),
    Answer   = suppressWarnings(as.numeric(Answer)),
    TableLabel_2014 = str_squish(tolower(as.character(TableLabel_2014_raw))),
    BreedType_2014  = str_squish(as.character(BreedType_2014)),
    SubquestionType_2014 = str_squish(as.character(SubquestionType_2014))
  ) %>%
  # Normalize regions to your exact labels
  mutate(
    Region = case_when(
      str_detect(Region_raw, regex("^africa$", TRUE)) ~ "Africa",
      str_detect(Region_raw, regex("^asia$", TRUE)) ~ "Asia",
      str_detect(Region_raw, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
      str_detect(Region_raw, regex("^europe", TRUE)) ~ "Europe",
      str_detect(Region_raw, regex("^latin\\s+america", TRUE)) ~ "Latin America and the Carribean",
      str_detect(Region_raw, regex("^north\\s+america", TRUE)) ~ "North America",
      str_detect(Region_raw, regex("near\\s*east", TRUE)) ~ "Near East",
      str_detect(Region_raw, regex("^world$", TRUE)) ~ "World",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Region)) %>%
  mutate(
    Type   = recode(SubquestionType_2014, !!!type_map),
    Type   = factor(Type, levels = type_levels),
    Species_key = TableLabel_2014,
    Species     = unname(species_display_map[Species_key])
  ) %>%
  # Keep only the 7 target species
  filter(Species_key %in% main_species) %>%
  mutate(
    Species = factor(Species, levels = species_order)
  )

# ---------------------------
# Filter: Q12 only
# ---------------------------
d_q12 <- d_all %>% filter(Section == 2, Question == 12)

# Tag programme for Q12 rows
d_q12 <- d_q12 %>%
  mutate(
    Programme = case_when(
      str_detect(BreedType_2014, prog_regex_both) ~ "Straight + Cross",
      str_detect(BreedType_2014, prog_regex_only) ~ "Straight only",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Programme))

# ---------------------------
# Helper to compute one table (A or B)
# ---------------------------
build_table <- function(programme_label) {
  d_prog <- d_q12 %>% filter(Programme == programme_label)

  # Numerators: Region × Species × Type
  num_rst <- d_prog %>%
    group_by(Region, Species, Type) %>%
    summarise(n_cov = sum(Answer, na.rm = TRUE), .groups = "drop")

  # WORLD numerators: sum across regions
  num_world <- d_prog %>%
    group_by(Species, Type) %>%
    summarise(n_cov = sum(Answer, na.rm = TRUE), .groups = "drop") %>%
    mutate(Region = "World")

  num_all <- num_rst %>%
    filter(Region != "World") %>%
    bind_rows(num_world) %>%
    mutate(
      Region  = factor(Region,  levels = region_levels),
      Species = factor(Species, levels = species_order),
      Type    = factor(Type,    levels = type_levels)
    )

  # Denominators (YOUR RULE): Region × Type totals for this programme
  den_rt <- d_prog %>%
    group_by(Region, Type) %>%
    summarise(N_total = sum(Answer, na.rm = TRUE), .groups = "drop")

  den_world <- d_prog %>%
    group_by(Type) %>%
    summarise(N_total = sum(Answer, na.rm = TRUE), .groups = "drop") %>%
    mutate(Region = "World")

  den_all <- den_rt %>%
    filter(Region != "World") %>%
    bind_rows(den_world) %>%
    mutate(
      Region = factor(Region, levels = region_levels),
      Type   = factor(Type,   levels = type_levels)
    )

  # Join & compute percentages
  pct_long <- num_all %>%
    left_join(den_all, by = c("Region","Type")) %>%
    mutate(
      pct = ifelse(N_total > 0, 100 * n_cov / N_total, NA_real_)
    ) %>%
    arrange(Region, Species, Type)

  # Pivot to report layout: Region + pairs per species
  wide <- pct_long %>%
    mutate(col = paste0(as.character(Species), "—", as.character(Type))) %>%
    select(Region, col, pct) %>%
    pivot_wider(names_from = col, values_from = pct) %>%
    arrange(Region)

  # Force column order: Exotic then Locally adapted for each species
  ordered_cols <- as.vector(rbind(
    paste0(species_order, "—Exotic"),
    paste0(species_order, "—Locally adapted")
  ))

  wide %>%
    select(Region, all_of(ordered_cols))
}

# Build both tables
table_A <- build_table("Straight + Cross")  # Table A
table_B <- build_table("Straight only")     # Table B

# ---------------------------
# Write workbook
# ---------------------------
wb <- createWorkbook()

addWorksheet(wb, "README")
addWorksheet(wb, "Q12_Table_A_Straight+Cross")
addWorksheet(wb, "Q12_Table_B_StraightOnly")
addWorksheet(wb, "Q12_Filtered_rows")    # optional helper

writeData(wb, "README",
"Q12 (Section 2) – Regions x Species.
Two tables:
- Table A: Straight/pure-breeding AND cross-breeding
- Table B: Straight/pure-breeding ONLY

Denominator (per user): for each table, N = total of 'Answer' by Region × Type (sum across all 7 species for that programme).
Numerator: sum of 'Answer' by Region × Species × Type for that programme.
WORLD rows are computed as sums across regions.
Columns are Region | [Species—Exotic, Species—Locally adapted, …] in the book’s order.")

writeData(wb, "Q12_Table_A_Straight+Cross", table_A)
writeData(wb, "Q12_Table_B_StraightOnly",  table_B)
writeData(wb, "Q12_Filtered_rows",
          d_q12 %>% select(Region, Species, Type, BreedType_2014, Answer) %>%
            arrange(Region, Species, Type))

# Style numeric columns as integers like the publication
int_style <- createStyle(numFmt = "0")
for (sh in c("Q12_Table_A_Straight+Cross","Q12_Table_B_StraightOnly")) {
  df <- readWorkbook(wb, sh)
  if (!is.null(df) && nrow(df) > 0) {
    num_cols <- which(vapply(df, is.numeric, TRUE))
    if (length(num_cols)) {
      addStyle(wb, sh, style = int_style,
               rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
    }
  }
}

saveWorkbook(wb, out_path, overwrite = TRUE)
message("Done. Excel written to: ", out_path)



TO GET TOTALS BY REGION
# ============================================
# Q12 (Section 2) totals by Region x Table x Species (+ World)
# Writes: C:/Users/LENOVO/Documents/Fig3B1_Q12_Totals.xlsx
# ============================================

# --- Packages ---
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(openxlsx)
library(rlang)

# --- Paths ---
in_path  <- "C:\\Users\\LENOVO\\Documents\\Fig3B1_Data.xlsx"
out_path <- "C:\\Users\\LENOVO\\Documents\\Fig3B1_Q12_Totals.xlsx"

# --- Species (keep exactly these seven) ---
main_species <- c(
  "cattle (specialized dairy)",
  "cattle (specialized beef)",
  "cattle (multipurpose)",
  "sheep","goats","pigs","chickens"
)

# Pretty labels and order
species_display_map <- c(
  "cattle (specialized dairy)" = "Dairy cattle",
  "cattle (specialized beef)"  = "Beef cattle",
  "cattle (multipurpose)"      = "Multipurpose cattle",
  "sheep"                      = "Sheep",
  "goats"                      = "Goats",
  "pigs"                       = "Pigs",
  "chickens"                   = "Chickens"
)
species_order <- unname(species_display_map[main_species])

# --- Regions (2024 list + World) ---
region_levels <- c(
  "Africa","Asia","Southwest Pacific","Europe",
  "Latin America and the Carribean","North America","Near East","World"
)

# --- Type mapping ---
type_map    <- c("Ex" = "Exotic", "Loc" = "Locally adapted")
type_levels <- c("Exotic","Locally adapted")

# --- Programmes (robust detection) ---
prog_regex_both <- regex("straight.*pure.*breeding.*cross", ignore_case = TRUE)
prog_regex_only <- regex("straight.*pure.*breeding.*only",  ignore_case = TRUE)
table_name_map  <- c("Straight + Cross" = "Table A (Straight+Cross)",
                     "Straight only"    = "Table B (Straight only)")

# ---------------------------
# Read & normalize columns
# ---------------------------
d0 <- read_excel(in_path)

# Normalize headers
names(d0) <- names(d0) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+", "_") |>
  tolower()

nm <- names(d0)
find_col <- function(primary_regex, fallback_regex = NULL) {
  hit <- nm[grepl(primary_regex, nm, ignore.case = TRUE)]
  if (length(hit) == 0 && !is.null(fallback_regex)) {
    hit <- nm[grepl(fallback_regex, nm, ignore.case = TRUE)]
  }
  if (length(hit) == 0) NA_character_ else hit[1]
}

region_col      <- find_col("^region$")
section_col     <- find_col("^section$")
question_col    <- find_col("^question_?2024$", "question.*2024")
answer_col      <- find_col("^answer$")
breedtype_col   <- find_col("^breedtype_?2014$")
subqtype_col    <- find_col("^subquestiontype_?2014$")
tablelabel_col  <- find_col("^tablelabel_?2014$")

needed <- c(region_col, section_col, question_col, answer_col,
            breedtype_col, subqtype_col, tablelabel_col)
if (any(is.na(needed))) {
  stop("Required columns not found. Matched columns:\n",
       " region=", region_col, "\n section=", section_col, "\n question=", question_col,
       "\n answer=", answer_col, "\n breedtype=", breedtype_col,
       "\n subquestiontype=", subqtype_col, "\n tablelabel=", tablelabel_col)
}

d_all <- d0 %>%
  rename(
    Region_raw           = !!region_col,
    Section              = !!section_col,
    Question             = !!question_col,
    Answer               = !!answer_col,
    BreedType_2014       = !!breedtype_col,
    SubquestionType_2014 = !!subqtype_col,
    TableLabel_2014_raw  = !!tablelabel_col
  ) %>%
  mutate(
    Section = suppressWarnings(as.numeric(Section)),
    Question = suppressWarnings(as.numeric(Question)),
    Answer   = suppressWarnings(as.numeric(Answer)),
    TableLabel_2014 = str_squish(tolower(as.character(TableLabel_2014_raw))),
    BreedType_2014  = str_squish(as.character(BreedType_2014)),
    SubquestionType_2014 = str_squish(as.character(SubquestionType_2014))
  ) %>%
  # Normalize Regions to your exact labels
  mutate(
    Region = case_when(
      str_detect(Region_raw, regex("^africa$", TRUE)) ~ "Africa",
      str_detect(Region_raw, regex("^asia$", TRUE)) ~ "Asia",
      str_detect(Region_raw, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
      str_detect(Region_raw, regex("^europe", TRUE)) ~ "Europe",
      str_detect(Region_raw, regex("^latin\\s+america", TRUE)) ~ "Latin America and the Carribean",
      str_detect(Region_raw, regex("^north\\s+america", TRUE)) ~ "North America",
      str_detect(Region_raw, regex("near\\s*east", TRUE)) ~ "Near East",
      str_detect(Region_raw, regex("^world$", TRUE)) ~ "World",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Region)) %>%
  mutate(
    Type   = recode(SubquestionType_2014, !!!type_map),
    Type   = factor(Type, levels = type_levels),
    Species_key = TableLabel_2014,
    Species     = unname(species_display_map[Species_key])
  ) %>%
  # Keep only the 7 target species
  filter(Species_key %in% main_species) %>%
  mutate(
    Species = factor(Species, levels = species_order)
  )

# ---------------------------
# Filter: Q12 rows and tag programme
# ---------------------------
d_q12 <- d_all %>% filter(Section == 2, Question == 12) %>%
  mutate(
    Programme = case_when(
      str_detect(BreedType_2014, prog_regex_both) ~ "Straight + Cross",
      str_detect(BreedType_2014, prog_regex_only) ~ "Straight only",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Programme))

# ---------------------------
# Totals needed
# ---------------------------
# 1) Numerator: totals by Region x Programme x Species x Type
num_RPST <- d_q12 %>%
  group_by(Region, Programme, Species, Type) %>%
  summarise(Count = sum(Answer, na.rm = TRUE), .groups = "drop")

# Add WORLD numerators (sum across regions)
num_world <- d_q12 %>%
  group_by(Programme, Species, Type) %>%
  summarise(Count = sum(Answer, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = "World")

num_RPST <- num_RPST %>%
  filter(Region != "World") %>%
  bind_rows(num_world) %>%
  mutate(
    Region    = factor(Region,    levels = region_levels),
    Programme = factor(Programme, levels = c("Straight + Cross","Straight only")),
    Species   = factor(Species,   levels = species_order),
    Type      = factor(Type,      levels = type_levels)
  ) %>%
  arrange(Region, Programme, Species, Type)

# 2) Denominator (your instruction for these totals file as well):
#    RegionType totals per Programme (sum across all 7 species)
den_RPT <- d_q12 %>%
  group_by(Region, Programme, Type) %>%
  summarise(RegionType_Total_for_Table = sum(Answer, na.rm = TRUE), .groups = "drop")

# WORLD denominators
den_world <- d_q12 %>%
  group_by(Programme, Type) %>%
  summarise(RegionType_Total_for_Table = sum(Answer, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = "World")

den_RPT <- den_RPT %>%
  filter(Region != "World") %>%
  bind_rows(den_world) %>%
  mutate(
    Region    = factor(Region,    levels = region_levels),
    Programme = factor(Programme, levels = c("Straight + Cross","Straight only")),
    Type      = factor(Type,      levels = type_levels)
  ) %>%
  arrange(Region, Programme, Type)

# 3) Combine to produce the single long sheet with percents
totals_long <- num_RPST %>%
  left_join(den_RPT, by = c("Region","Programme","Type")) %>%
  mutate(
    Percent = ifelse(RegionType_Total_for_Table > 0,
                     100 * Count / RegionType_Total_for_Table, NA_real_),
    Table   = recode(Programme, !!!table_name_map)
  ) %>%
  select(Table, Programme, Region, Species, Type, Count, RegionType_Total_for_Table, Percent) %>%
  arrange(Programme, Region, Species, Type)

# 4) Species totals at World (quick check)
species_world <- totals_long %>%
  filter(Region == "World") %>%
  select(Table, Programme, Region, Species, Type, Count, Percent) %>%
  arrange(Programme, Species, Type)

# ---------------------------
# Write the Excel workbook
# ---------------------------
wb <- createWorkbook()

addWorksheet(wb, "README")
addWorksheet(wb, "Q12_Totals_All")          # main deliverable
addWorksheet(wb, "Q12_Totals_RegionType")   # denominators
addWorksheet(wb, "Q12_Totals_SpeciesWorld") # species world totals (optional check)

writeData(wb, "README",
"Totals for Question 12 (Section 2).

Definition:
- Table A (Straight+Cross) = straight/pure-breeding and cross-breeding.
- Table B (Straight only)  = straight/pure-breeding only.
- Count  = SUM of 'Answer' for Region × Programme × Species × Type (Exotic/Locally adapted).
- RegionType_Total_for_Table = SUM of 'Answer' for Region × Programme × Type across all 7 species (denominator).
- Percent = 100 * Count / RegionType_Total_for_Table.
- World rows are computed by summing across regions.

Sheets:
- Q12_Totals_All: one tidy table with Table, Programme, Region, Species, Type, Count, Denominator, Percent.
- Q12_Totals_RegionType: denominators by Region × Programme × Type.
- Q12_Totals_SpeciesWorld: species totals (World) by Programme × Type.")

writeData(wb, "Q12_Totals_All", totals_long)
writeData(wb, "Q12_Totals_RegionType", den_RPT)
writeData(wb, "Q12_Totals_SpeciesWorld", species_world)

# Style numeric columns as integers for Count/Denominator and 0 decimals for Percent
int_style <- createStyle(numFmt = "0")
pct_style <- createStyle(numFmt = "0")

for (sh in c("Q12_Totals_All","Q12_Totals_RegionType","Q12_Totals_SpeciesWorld")) {
  df <- readWorkbook(wb, sh)
  if (!is.null(df) && nrow(df) > 0) {
    # integer columns
    int_cols <- which(names(df) %in% c("Count","RegionType_Total_for_Table"))
    if (length(int_cols)) addStyle(wb, sh, int_style, rows = 2:(nrow(df)+1), cols = int_cols, gridExpand = TRUE)
    # percent column
    if ("Percent" %in% names(df)) {
      col_idx <- which(names(df) == "Percent")
      addStyle(wb, sh, pct_style, rows = 2:(nrow(df)+1), cols = col_idx, gridExpand = TRUE)
    }
  }
}

saveWorkbook(wb, out_path, overwrite = TRUE)
message("Done. Excel written to: ", out_path)


AUDIT AND VERIFY
# ==========================================================
# Q12 (Section 2) Audit — prove the numbers used in the tables
#   - Programme A: Straight/pure-breeding AND cross-breeding
#   - Programme B: Straight/pure-breeding ONLY
# Denominator rule: Region × Type totals within each programme (sum of Answer across 7 species)
# Output: Fig3B1_Q12_Audit.xlsx with multiple verification sheets
# ==========================================================

# --- Packages ---
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(openxlsx)
library(rlang)

# --- Paths ---
in_path  <- "C:\\Users\\LENOVO\\Documents\\Fig3B1_Data.xlsx"
out_path <- "C:\\Users\\LENOVO\\Documents\\Fig3B1_Q12_Audit.xlsx"

# --- Species: keep exactly these 7 ---
main_species <- c(
  "cattle (specialized dairy)",
  "cattle (specialized beef)",
  "cattle (multipurpose)",
  "sheep","goats","pigs","chickens"
)
species_display_map <- c(
  "cattle (specialized dairy)" = "Dairy cattle",
  "cattle (specialized beef)"  = "Beef cattle",
  "cattle (multipurpose)"      = "Multipurpose cattle",
  "sheep"                      = "Sheep",
  "goats"                      = "Goats",
  "pigs"                       = "Pigs",
  "chickens"                   = "Chickens"
)
species_order <- unname(species_display_map[main_species])

# --- Regions (your exact 2024 list) ---
region_levels <- c(
  "Africa","Asia","Southwest Pacific","Europe",
  "Latin America and the Carribean","North America","Near East","World"
)

# --- Types ---
type_map    <- c("Ex" = "Exotic", "Loc" = "Locally adapted")
type_levels <- c("Exotic","Locally adapted")

# --- Q12 programme detection ---
prog_regex_both <- regex("straight.*pure.*breeding.*cross", ignore_case = TRUE)
prog_regex_only <- regex("straight.*pure.*breeding.*only",  ignore_case = TRUE)

# --- Parameters for the “AnyRegion_AnyTable” check (change if you like) ---
check_region    <- "Africa"
check_programme <- "Straight + Cross"   # or "Straight only"

# ---------------------------
# Read & normalize columns
# ---------------------------
d0 <- read_excel(in_path)

names(d0) <- names(d0) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+", "_") |>
  tolower()

nm <- names(d0)
find_col <- function(primary_regex, fallback_regex = NULL) {
  hit <- nm[grepl(primary_regex, nm, ignore.case = TRUE)]
  if (length(hit) == 0 && !is.null(fallback_regex)) {
    hit <- nm[grepl(fallback_regex, nm, ignore.case = TRUE)]
  }
  if (length(hit) == 0) NA_character_ else hit[1]
}

region_col      <- find_col("^region$")
section_col     <- find_col("^section$")
question_col    <- find_col("^question_?2024$", "question.*2024")
answer_col      <- find_col("^answer$")
breedtype_col   <- find_col("^breedtype_?2014$")
subqtype_col    <- find_col("^subquestiontype_?2014$")
tablelabel_col  <- find_col("^tablelabel_?2014$")

needed <- c(region_col, section_col, question_col, answer_col,
            breedtype_col, subqtype_col, tablelabel_col)
if (any(is.na(needed))) {
  stop("Required columns not found. Matched columns:\n",
       " region=", region_col, "\n section=", section_col, "\n question=", question_col,
       "\n answer=", answer_col, "\n breedtype=", breedtype_col,
       "\n subquestiontype=", subqtype_col, "\n tablelabel=", tablelabel_col)
}

d_all <- d0 %>%
  rename(
    Region_raw           = !!region_col,
    Section              = !!section_col,
    Question             = !!question_col,
    Answer               = !!answer_col,
    BreedType_2014       = !!breedtype_col,
    SubquestionType_2014 = !!subqtype_col,
    TableLabel_2014_raw  = !!tablelabel_col
  ) %>%
  mutate(
    Section = suppressWarnings(as.numeric(Section)),
    Question = suppressWarnings(as.numeric(Question)),
    Answer   = suppressWarnings(as.numeric(Answer)),
    TableLabel_2014 = str_squish(tolower(as.character(TableLabel_2014_raw))),
    BreedType_2014  = str_squish(as.character(BreedType_2014)),
    SubquestionType_2014 = str_squish(as.character(SubquestionType_2014))
  ) %>%
  # normalize regions
  mutate(
    Region = case_when(
      str_detect(Region_raw, regex("^africa$", TRUE)) ~ "Africa",
      str_detect(Region_raw, regex("^asia$", TRUE)) ~ "Asia",
      str_detect(Region_raw, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
      str_detect(Region_raw, regex("^europe", TRUE)) ~ "Europe",
      str_detect(Region_raw, regex("^latin\\s+america", TRUE)) ~ "Latin America and the Carribean",
      str_detect(Region_raw, regex("^north\\s+america", TRUE)) ~ "North America",
      str_detect(Region_raw, regex("near\\s*east", TRUE)) ~ "Near East",
      str_detect(Region_raw, regex("^world$", TRUE)) ~ "World",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Region)) %>%
  mutate(
    Type    = recode(SubquestionType_2014, !!!type_map),
    Type    = factor(Type, levels = type_levels),
    Species_key = TableLabel_2014,
    Species     = unname(species_display_map[Species_key])
  ) %>%
  filter(Species_key %in% main_species) %>%
  mutate(Species = factor(Species, levels = species_order))

# ---------------------------
# Focus on Q12 and tag programme
# ---------------------------
d_q12 <- d_all %>%
  filter(Section == 2, Question == 12) %>%
  mutate(
    Programme = case_when(
      str_detect(BreedType_2014, prog_regex_both) ~ "Straight + Cross",
      str_detect(BreedType_2014, prog_regex_only) ~ "Straight only",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Programme))

# ---------------------------
# Denominators (Region × Programme × Type)
# ---------------------------
den_RPT <- d_q12 %>%
  group_by(Region, Programme, Type) %>%
  summarise(RegionType_Total = sum(Answer, na.rm = TRUE), .groups = "drop")

# WORLD denominators
den_world <- d_q12 %>%
  group_by(Programme, Type) %>%
  summarise(RegionType_Total = sum(Answer, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = "World")

den_RPT <- den_RPT %>%
  filter(Region != "World") %>%
  bind_rows(den_world) %>%
  mutate(
    Region    = factor(Region, levels = region_levels),
    Programme = factor(Programme, levels = c("Straight + Cross","Straight only")),
    Type      = factor(Type, levels = type_levels)
  ) %>%
  arrange(Region, Programme, Type)

# ---------------------------
# Numerators (Region × Programme × Species × Type)
# ---------------------------
num_RPST <- d_q12 %>%
  group_by(Region, Programme, Species, Type) %>%
  summarise(Numerator = sum(Answer, na.rm = TRUE), .groups = "drop")

# WORLD numerators
num_world <- d_q12 %>%
  group_by(Programme, Species, Type) %>%
  summarise(Numerator = sum(Answer, na.rm = TRUE), .groups = "drop") %>%
  mutate(Region = "World")

num_RPST <- num_RPST %>%
  filter(Region != "World") %>%
  bind_rows(num_world) %>%
  mutate(
    Region    = factor(Region, levels = region_levels),
    Programme = factor(Programme, levels = c("Straight + Cross","Straight only")),
    Species   = factor(Species, levels = species_order),
    Type      = factor(Type, levels = type_levels)
  ) %>%
  arrange(Region, Programme, Species, Type)

# ---------------------------
# Join & compute percents (what your table shows)
# ---------------------------
pct_RPST <- num_RPST %>%
  left_join(den_RPT, by = c("Region","Programme","Type")) %>%
  mutate(
    Percent = ifelse(RegionType_Total > 0, 100 * Numerator / RegionType_Total, NA_real_)
  )

# ---------------------------
# Build the specific “Africa + Table A” breakdown you mentioned
# ---------------------------
africa_tableA <- pct_RPST %>%
  filter(Region == check_region, Programme == check_programme) %>%
  arrange(Species, Type) %>%
  select(Region, Programme, Species, Type, Numerator, RegionType_Total, Percent)

# Row-sum checks: sum of percents across species for each type should be 100
rowsum_tableA <- africa_tableA %>%
  group_by(Type) %>%
  summarise(SumPercent = sum(Percent, na.rm = TRUE), .groups = "drop")

# ---------------------------
# Global row-sum checks for ALL regions (Tables A & B)
# ---------------------------
rowsum_all_A <- pct_RPST %>%
  filter(Programme == "Straight + Cross") %>%
  group_by(Region, Type) %>%
  summarise(SumPercent = sum(Percent, na.rm = TRUE), .groups = "drop") %>%
  arrange(Region, Type)

rowsum_all_B <- pct_RPST %>%
  filter(Programme == "Straight only") %>%
  group_by(Region, Type) %>%
  summarise(SumPercent = sum(Percent, na.rm = TRUE), .groups = "drop") %>%
  arrange(Region, Type)

# ---------------------------
# Overall totals by Region (your 1380 example)
#   = sum of Answer across BOTH programmes, BOTH types, ALL species
# ---------------------------
overall_region_totals <- d_q12 %>%
  group_by(Region) %>%
  summarise(Overall_Total_Q12 = sum(Answer, na.rm = TRUE), .groups = "drop") %>%
  arrange(factor(Region, levels = region_levels))

# ---------------------------
# Write the audit workbook
# ---------------------------
wb <- createWorkbook()

addWorksheet(wb, "README")
addWorksheet(wb, "Denominators")           # Region × Programme × Type
addWorksheet(wb, "Numerators")             # Region × Programme × Species × Type
addWorksheet(wb, "Percents")               # same with percent
addWorksheet(wb, "RowSums_Check_TableA")   # per Region × Type
addWorksheet(wb, "RowSums_Check_TableB")
addWorksheet(wb, "OverallTotals_ByRegion")
addWorksheet(wb, "Africa_TableA_Breakdown")
addWorksheet(wb, "AnyRegion_AnyTable")

writeData(wb, "README",
"Audit for Q12 (Section 2).
- Programme A = Straight/pure-breeding AND cross-breeding
- Programme B = Straight/pure-breeding ONLY
Denominator rule: within each programme, Region × Type total (sum of Answer across the 7 species).
Numerator: Region × Programme × Species × Type (sum of Answer).
Percent = 100 * Numerator / Denominator.

Use 'RowSums_Check_*' to confirm sums of species percentages = 100 per Region × Type.
Use 'OverallTotals_ByRegion' to confirm grand totals (e.g., Africa).")

writeData(wb, "Denominators", den_RPT)
writeData(wb, "Numerators",   num_RPST)
writeData(wb, "Percents",     pct_RPST)
writeData(wb, "RowSums_Check_TableA", rowsum_all_A)
writeData(wb, "RowSums_Check_TableB", rowsum_all_B)
writeData(wb, "OverallTotals_ByRegion", overall_region_totals)
writeData(wb, "Africa_TableA_Breakdown", africa_tableA)

# A generic sheet for any region + programme (defaults set above)
any_region_any_table <- pct_RPST %>%
  filter(Region == check_region, Programme == check_programme) %>%
  arrange(Species, Type)
writeData(wb, "AnyRegion_AnyTable", any_region_any_table)

# Style numeric columns as integers for counts & 0 decimals for % (to match your table)
int_style <- createStyle(numFmt = "0")
pct_style <- createStyle(numFmt = "0")  # integer % display

for (sh in c("Denominators","Numerators","Percents","RowSums_Check_TableA","RowSums_Check_TableB","OverallTotals_ByRegion","Africa_TableA_Breakdown","AnyRegion_AnyTable")) {
  df <- readWorkbook(wb, sh)
  if (!is.null(df) && nrow(df) > 0) {
    int_cols <- which(vapply(df, is.numeric, TRUE))
    if (length(int_cols)) addStyle(wb, sh, int_style, rows = 2:(nrow(df)+1), cols = int_cols, gridExpand = TRUE)
  }
}

saveWorkbook(wb, out_path, overwrite = TRUE)

# Console helper messages for your specific example
africa_A_ex_den <- den_RPT %>% filter(Region=="Africa", Programme=="Straight + Cross", Type=="Exotic") %>% pull(RegionType_Total)
africa_A_loc_den <- den_RPT %>% filter(Region=="Africa", Programme=="Straight + Cross", Type=="Locally adapted") %>% pull(RegionType_Total)
africa_overall   <- overall_region_totals %>% filter(Region=="Africa") %>% pull(Overall_Total_Q12)

africa_dairy_ex_num <- num_RPST %>% filter(Region=="Africa", Programme=="Straight + Cross", Species=="Dairy cattle", Type=="Exotic") %>% pull(Numerator)
africa_dairy_ex_pct <- pct_RPST %>% filter(Region=="Africa", Programme=="Straight + Cross", Species=="Dairy cattle", Type=="Exotic") %>% pull(Percent)

message("CHECK — Africa, Table A (Straight+Cross):")
message("  Denominator (Exotic, Region×Type total) = ", africa_A_ex_den)
message("  Denominator (Locally adapted)            = ", africa_A_loc_den)
message("  Dairy cattle (Exotic) numerator          = ", africa_dairy_ex_num)
message("  Dairy cattle (Exotic) percent            = ", round(africa_dairy_ex_pct))
message("OVERALL Africa (Q12, both programmes & types, all species) = ", africa_overall)
message("Audit workbook written: ", out_path)




Figure 3C6
# ==========================================================
# Q16-like figures: "Policies or programmes" (YES) vs "None" (NO)
# Uses: Region, Answer (yes/no), Species from TableLabel_2014 / BreedType_2014 / SpecieTag
# Outputs: 7 PNGs + 7 PDFs (figures only) and an Excel with tables only
# ==========================================================

# --- Packages ---
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(openxlsx)
library(ggplot2)

# --- Paths ---
in_path   <- "C:\\Users\\LENOVO\\Documents\\Fig3B1_Data.xlsx"
out_xlsx  <- "C:\\Users\\LENOVO\\Documents\\Fig3B1_Q16_Policies_tables.xlsx"
png_dir   <- "C:\\Users\\LENOVO\\Documents\\Q16_Policies_Figures_PNG"
pdf_dir   <- "C:\\Users\\LENOVO\\Documents\\Q16_Policies_Figures_PDF"
dir.create(png_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(pdf_dir, showWarnings = FALSE, recursive = TRUE)

# --- Species universe & display names ---
main_species <- c(
  "cattle (specialized dairy)",
  "cattle (specialized beef)",
  "cattle (multipurpose)",
  "sheep","goats","pigs","chickens"
)
species_display_map <- c(
  "cattle (specialized dairy)" = "Dairy cattle",
  "cattle (specialized beef)"  = "Beef cattle",
  "cattle (multipurpose)"      = "Multipurpose cattle",
  "sheep"                      = "Sheep",
  "goats"                      = "Goats",
  "pigs"                       = "Pigs",
  "chickens"                   = "Chickens"
)
species_order <- unname(species_display_map[main_species])

# --- Map the codes you provided to species (SpecieTag / Species code) ---
code_to_species <- c(
  "009" = "cattle (specialized dairy)",
  "008" = "cattle (specialized beef)",
  "007" = "cattle (multipurpose)",
  "042" = "sheep",
  "021" = "goats",
  "037" = "pigs",
  "010" = "chickens"
)

# --- 2024 Region order including World at bottom ---
region_levels <- c(
  "Africa","Asia","Southwest Pacific","Europe",
  "Latin America and the Carribean","North America","Near East","World"
)

# --- The two plotted categories + colors ---
status_levels <- c("Policies or programmes","None")
status_colors <- c("Policies or programmes" = "#3B6FB6",  # blue
                   "None"                   = "#BFBFBF")   # grey

# ---------------------------
# Load & normalize headers
# ---------------------------
raw <- read_excel(in_path)

names(raw) <- names(raw) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+", "_") |>
  tolower()

# Convenience function: return first existing col from a list
pick_col <- function(...) {
  cands <- unlist(list(...))
  cands[cands %in% names(raw)][1]
}

# Pick columns from your sheet
region_col     <- pick_col("region")
country_col    <- pick_col("country")   # optional but useful for dedup
answer_col     <- pick_col("answer")
tablelabel_col <- pick_col("tablelabel_2014")
breedtype_col  <- pick_col("breedtype_2014")
specietag_col  <- pick_col("specietag", "species", "specietagnew", "specietag_new")

# Basic guardrails
if (is.na(region_col) || is.na(answer_col)) {
  stop("Could not find required columns 'Region' and 'Answer' in the file.")
}

# Build a working frame with only what we need
d <- raw %>%
  transmute(
    Region_raw  = .data[[region_col]],
    Country     = if (!is.na(country_col)) .data[[country_col]] else NA_character_,
    Answer_text = str_to_lower(str_squish(as.character(.data[[answer_col]]))),
    TableLabel  = if (!is.na(tablelabel_col)) str_squish(tolower(as.character(.data[[tablelabel_col]]))) else NA_character_,
    BreedType   = if (!is.na(breedtype_col))  str_squish(tolower(as.character(.data[[breedtype_col]])))  else NA_character_,
    SpecieTag   = if (!is.na(specietag_col))  str_squish(as.character(.data[[specietag_col]]))            else NA_character_
  )

# Normalize Region names to your 2024 labels
d <- d %>%
  mutate(
    Region = case_when(
      str_detect(Region_raw, regex("^africa$", TRUE)) ~ "Africa",
      str_detect(Region_raw, regex("^asia$", TRUE)) ~ "Asia",
      str_detect(Region_raw, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
      str_detect(Region_raw, regex("^europe", TRUE)) ~ "Europe",
      str_detect(Region_raw, regex("^latin\\s+america", TRUE)) ~ "Latin America and the Carribean",
      str_detect(Region_raw, regex("^north\\s+america", TRUE)) ~ "North America",
      str_detect(Region_raw, regex("near\\s*east", TRUE)) ~ "Near East",
      str_detect(Region_raw, regex("^world$", TRUE)) ~ "World",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Region))

# --- Derive SPECIES robustly ---
# 1) If TableLabel contains our species names, use it
# 2) Else if BreedType contains our species names, use it
# 3) Else map by SpecieTag codes -> species
# 4) Finally, drop rows not in our 7 species

# helper: does a vector contain any of the main species strings?
contains_species_names <- function(x) {
  any(str_detect(x, paste0("^(", paste0(main_species, collapse="|"), ")$")), na.rm = TRUE)
}

species_source <- NA_character_
if (!all(is.na(d$TableLabel)) && contains_species_names(d$TableLabel)) {
  d$Species_key <- d$TableLabel
  species_source <- "TableLabel_2014"
} else if (!all(is.na(d$BreedType)) && contains_species_names(d$BreedType)) {
  d$Species_key <- d$BreedType
  species_source <- "BreedType_2014"
} else if (!all(is.na(d$SpecieTag))) {
  # pad to 3 chars then map
  d$Species_key <- recode(str_pad(d$SpecieTag, width = 3, side = "left", pad = "0"), !!!code_to_species)
  species_source <- "SpecieTag (code)"
} else {
  stop("Could not determine species from TableLabel_2014, BreedType_2014, or SpecieTag.")
}

# Keep only the 7 species we care about
d <- d %>%
  filter(Species_key %in% main_species) %>%
  mutate(Species = factor(unname(species_display_map[Species_key]), levels = species_order))

# --- Recode YES/NO robustly ---
yes_set <- c("yes","y","1","true")
no_set  <- c("no","n","0","false")

d <- d %>%
  mutate(Answer_text = str_to_lower(str_squish(Answer_text))) %>%
  mutate(
    is_yes = Answer_text %in% yes_set,
    is_no  = Answer_text %in% no_set
  )

# If Country missing in the file, synthesize a unique id per original row so counting still works
if (all(is.na(d$Country))) {
  d$Country <- paste0("row_", seq_len(nrow(d)))
}

# Collapse duplicates by Region–Country–Species:
# any YES -> Policies or programmes; else if any NO -> None; else drop
d_country <- d %>%
  group_by(Region, Country, Species) %>%
  summarise(
    Status = case_when(
      any(is_yes, na.rm = TRUE) ~ "Policies or programmes",
      any(is_no,  na.rm = TRUE) ~ "None",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  ) %>%
  filter(!is.na(Status)) %>%
  mutate(
    Region = factor(Region, levels = region_levels),
    Status = factor(Status, levels = status_levels)
  )

# ---- Counts and percents ----
# Counts per Region × Species × Status (countries)
cnt <- d_country %>%
  group_by(Region, Species, Status) %>%
  summarise(n = n(), .groups = "drop")

# Denominator per Region × Species
den <- cnt %>%
  group_by(Region, Species) %>%
  summarise(denom = sum(n), .groups = "drop")

# WORLD from regions (ignore any pre-existing 'World' in input)
cnt_world <- cnt %>%
  filter(Region != "World") %>%
  group_by(Species, Status) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  mutate(Region = factor("World", levels = region_levels)) %>%
  select(Region, Species, Status, n)

den_world <- den %>%
  filter(Region != "World") %>%
  group_by(Species) %>%
  summarise(denom = sum(denom), .groups = "drop") %>%
  mutate(Region = factor("World", levels = region_levels)) %>%
  select(Region, Species, denom)

cnt_all <- bind_rows(cnt %>% filter(Region != "World"), cnt_world)
den_all <- bind_rows(den %>% filter(Region != "World"), den_world)

pct_long <- cnt_all %>%
  left_join(den_all, by = c("Region","Species")) %>%
  mutate(
    pct = ifelse(denom > 0, 100 * n / denom, NA_real_),
    Region = factor(Region, levels = region_levels),
    Species = factor(Species, levels = species_order),
    Status  = factor(Status,  levels = status_levels)
  ) %>%
  arrange(Region, Species, Status)

# ---------------------------
# Excel: tables only (no images)
# ---------------------------
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "Long_data")
addWorksheet(wb, "Totals_by_Region_Species")

writeData(wb, "README", paste0(
  "Built from columns: Region, Answer (yes/no), Species via ", species_source, ".\n",
  "Rules:\n",
  "- Deduplicate by Region–Country–Species: any YES -> 'Policies or programmes', else if any NO -> 'None'.\n",
  "- Denominator per Region × Species = YES + NO country counts. World = sum of regions (not read from file).\n",
  "- Regions ordered: Africa, Asia, Southwest Pacific, Europe, Latin America and the Carribean, North America, Near East, World.\n",
  "- Workbook contains tables only; figures are saved separately as PNG and PDF."
))

writeData(wb, "Long_data",
          pct_long %>% select(Region, Species, Status, n, denom, pct))

totals_tab <- pct_long %>%
  select(Region, Species, Status, n, denom, pct) %>%
  pivot_wider(names_from = Status, values_from = c(n, pct), values_fill = 0) %>%
  arrange(Region, Species)
writeData(wb, "Totals_by_Region_Species", totals_tab)

# One sheet per species with 2 columns (Policies or programmes | None)
int_style <- createStyle(numFmt = "0")
for (sp in species_order) {
  sh <- sp
  addWorksheet(wb, sh)
  tab <- pct_long %>%
    filter(Species == sp) %>%
    select(Region, Status, pct) %>%
    pivot_wider(names_from = Status, values_from = pct) %>%
    arrange(factor(Region, levels = region_levels))
  writeData(wb, sh, tab)
  if (nrow(tab) > 0) {
    num_cols <- which(vapply(tab, is.numeric, TRUE))
    if (length(num_cols)) {
      addStyle(wb, sh, style = int_style,
               rows = 2:(nrow(tab)+1), cols = num_cols, gridExpand = TRUE)
    }
  }
}
saveWorkbook(wb, out_xlsx, overwrite = TRUE)

# ---------------------------
# Figures: PNG + PDF (one per species)
# ---------------------------
for (sp in species_order) {
  df_sp <- pct_long %>% filter(Species == sp)
  if (nrow(df_sp) == 0) next

  df_sp <- df_sp %>% mutate(Region = factor(Region, levels = region_levels))

  p <- ggplot(df_sp, aes(x = pct, y = Region, fill = Status)) +
    geom_col(width = 0.6) +
    scale_fill_manual(values = status_colors, breaks = status_levels, drop = FALSE) +
    scale_x_continuous(limits = c(0,100),
                       breaks = seq(0,100,10),
                       labels = function(x) paste0(x, "%")) +
    labs(x = NULL, y = NULL, fill = NULL, title = sp) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0, size = 11, face = "bold", margin = margin(b = 6)),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 9, margin = margin(t = 5))
    )

  fname <- gsub("[^A-Za-z0-9]+", "_", sp)
  ggsave(file.path(png_dir, paste0("Q16_", fname, ".png")), p, width = 7.5, height = 3.0, dpi = 300)
  ggsave(file.path(pdf_dir, paste0("Q16_", fname, ".pdf")), p, width = 7.5, height = 3.0, device = "pdf")
}

message("DONE.")
message("Tables workbook: ", out_xlsx)
message("PNG figs in: ", png_dir)
message("PDF figs in: ", pdf_dir)




Figures
# ==========================================================
# Policies or programmes (YES) vs None (NO), by Region for each species
# Figures: 7 PNGs + 7 PDFs (saved to Documents *and* Desktop, verified)
# Excel: tables only (no images)
# ==========================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr); library(tidyr)
  library(forcats); library(ggplot2); library(openxlsx)
})

# --- Paths ---
docs_base   <- "C:\\Users\\LENOVO\\Documents"
desk_base   <- "C:\\Users\\LENOVO\\Desktop"

in_path     <- file.path(docs_base, "Fig3B1_Data.xlsx")
out_xlsx    <- file.path(docs_base, "Fig3B1_Q16_Policies_tables.xlsx")

png_dir_docs <- file.path(docs_base, "Q16_Policies_Figures_PNG")
pdf_dir_docs <- file.path(docs_base, "Q16_Policies_Figures_PDF")
png_dir_desk <- file.path(desk_base, "Q16_Policies_Figures_PNG")
pdf_dir_desk <- file.path(desk_base, "Q16_Policies_Figures_PDF")

# Clean & recreate output folders (so we don't count stale files)
for (p in c(png_dir_docs, pdf_dir_docs, png_dir_desk, pdf_dir_desk)) {
  if (dir.exists(p)) unlink(p, recursive = TRUE, force = TRUE)
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

# --- Species + display names ---
main_species <- c(
  "cattle (specialized dairy)",
  "cattle (specialized beef)",
  "cattle (multipurpose)",
  "sheep","goats","pigs","chickens"
)
species_display_map <- c(
  "cattle (specialized dairy)" = "Dairy cattle",
  "cattle (specialized beef)"  = "Beef cattle",
  "cattle (multipurpose)"      = "Multipurpose cattle",
  "sheep"                      = "Sheep",
  "goats"                      = "Goats",
  "pigs"                       = "Pigs",
  "chickens"                   = "Chickens"
)
species_order <- unname(species_display_map[main_species])

# --- SpecieTag code → species (yours) ---
code_to_species <- c(
  "009" = "cattle (specialized dairy)",
  "008" = "cattle (specialized beef)",
  "007" = "cattle (multipurpose)",
  "042" = "sheep",
  "021" = "goats",
  "037" = "pigs",
  "010" = "chickens"
)

# --- Region order (2024) ---
region_levels <- c(
  "Africa","Asia","Southwest Pacific","Europe",
  "Latin America and the Carribean","North America","Near East","World"
)

# --- Categories + colors ---
status_levels <- c("Policies or programmes","None")
status_colors <- c("Policies or programmes" = "#3B6FB6", "None" = "#BFBFBF")

# ---------------------------
# Load & normalize headers
# ---------------------------
raw <- read_excel(in_path)
names(raw) <- names(raw) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+", "_") |>
  tolower()

pick_col <- function(...) {
  cands <- unlist(list(...))
  cands[cands %in% names(raw)][1]
}

region_col     <- pick_col("region")
country_col    <- pick_col("country")
answer_col     <- pick_col("answer")
tablelabel_col <- pick_col("tablelabel_2014")
specietag_col  <- pick_col("specietag","specietagnew","specietag_new","species")

stopifnot(!is.na(region_col), !is.na(answer_col))

d <- raw %>%
  transmute(
    Region_raw  = .data[[region_col]],
    Country     = if (!is.na(country_col)) .data[[country_col]] else NA_character_,
    Answer_text = str_to_lower(str_squish(as.character(.data[[answer_col]]))),
    TableLabel  = if (!is.na(tablelabel_col)) str_squish(tolower(as.character(.data[[tablelabel_col]]))) else NA_character_,
    SpecieTag   = if (!is.na(specietag_col))  str_squish(as.character(.data[[specietag_col]])) else NA_character_
  ) %>%
  mutate(
    Region = case_when(
      str_detect(Region_raw, regex("^africa$", TRUE)) ~ "Africa",
      str_detect(Region_raw, regex("^asia$", TRUE)) ~ "Asia",
      str_detect(Region_raw, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
      str_detect(Region_raw, regex("^europe", TRUE)) ~ "Europe",
      str_detect(Region_raw, regex("^latin\\s+america", TRUE)) ~ "Latin America and the Carribean",
      str_detect(Region_raw, regex("^north\\s*america", TRUE)) ~ "North America",
      str_detect(Region_raw, regex("near\\s*east", TRUE)) ~ "Near East",
      str_detect(Region_raw, regex("^world$", TRUE)) ~ "World",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Region)) %>%
  # IMPORTANT: ignore any pre-existing World rows in source; we will recompute
  filter(Region != "World")

# --- Get species (prefer TableLabel; else SpecieTag mapping) ---
d$Species_key <- NA_character_
if (!all(is.na(d$TableLabel)) && any(d$TableLabel %in% main_species, na.rm = TRUE)) {
  d$Species_key <- d$TableLabel
} else if (!all(is.na(d$SpecieTag))) {
  d$Species_key <- recode(str_pad(d$SpecieTag, width = 3, side = "left", pad = "0"), !!!code_to_species)
}
d <- d %>% filter(Species_key %in% main_species)

# If Country missing, synthesize id so de-dup works
if (all(is.na(d$Country))) d$Country <- paste0("row_", seq_len(nrow(d)))

# YES / NO normalisation
yes_set <- c("yes","y","1","true")
no_set  <- c("no","n","0","false")

# Collapse duplicates by Region–Country–Species (YES overrides NO)
d_country <- d %>%
  mutate(
    is_yes = Answer_text %in% yes_set,
    is_no  = Answer_text %in% no_set
  ) %>%
  group_by(Region, Country, Species_key) %>%
  summarise(
    Status = case_when(
      any(is_yes, na.rm = TRUE) ~ "Policies or programmes",
      any(is_no,  na.rm = TRUE) ~ "None",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  ) %>%
  filter(!is.na(Status)) %>%
  mutate(
    Species = factor(unname(species_display_map[Species_key]), levels = species_order),
    Region  = factor(Region, levels = region_levels[region_levels != "World"]),
    Status  = factor(Status, levels = status_levels)
  )

# ---------------------------
# Counts → denominators → percent (NO joins that duplicate rows)
# ---------------------------
cnt <- d_country %>%
  count(Region, Species, Status, name = "n") %>%
  complete(Region, Species, Status = factor(status_levels, levels = status_levels), fill = list(n = 0))

den_reg <- cnt %>% group_by(Region, Species) %>% summarise(denom = sum(n), .groups = "drop")

# WORLD from regions
cnt_world <- cnt %>% group_by(Species, Status) %>% summarise(n = sum(n), .groups = "drop") %>%
  mutate(Region = factor("World", levels = "World"))
den_world <- cnt_world %>% group_by(Species) %>% summarise(denom = sum(n), .groups = "drop") %>%
  mutate(Region = factor("World", levels = "World"))

cnt_all <- bind_rows(cnt, cnt_world)
den_all <- bind_rows(den_reg, den_world)

pct_long <- cnt_all %>%
  left_join(den_all, by = c("Region","Species")) %>%
  mutate(
    pct = ifelse(denom > 0, 100 * n / denom, 0),
    pct = pmax(0, pmin(100, pct)),
    Region  = fct_relevel(Region, region_levels),
    Species = factor(Species, levels = species_order),
    Status  = factor(Status, levels = status_levels)
  ) %>%
  arrange(Region, Species, Status)

# ---------------------------
# Excel (tables only)
# ---------------------------
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "Long_data")
addWorksheet(wb, "Totals_by_Region_Species")

writeData(wb, "README",
"Built from: Region, Answer (yes/no), and Species (TableLabel_2014 or SpecieTag codes).
Rules:
- De-dup by Region–Country–Species (any YES → 'Policies or programmes'; else if any NO → 'None').
- Denominator per Region × Species = YES + NO country counts.
- 'World' is recomputed from the sum of regions (source 'World' rows ignored).
- Regions ordered: Africa, Asia, Southwest Pacific, Europe, Latin America and the Carribean, North America, Near East, World.
Workbook contains tables only; figures are saved as PNG/PDF.")

writeData(wb, "Long_data", pct_long %>% select(Region, Species, Status, n, denom, pct))

totals_tab <- pct_long %>%
  select(Region, Species, Status, n, denom, pct) %>%
  pivot_wider(names_from = Status, values_from = c(n, pct), values_fill = 0) %>%
  arrange(Region, Species)
writeData(wb, "Totals_by_Region_Species", totals_tab)

# One sheet per species (2 columns)
int_style <- createStyle(numFmt = "0")
for (sp in species_order) {
  sh <- sp
  addWorksheet(wb, sh)
  tab <- pct_long %>%
    filter(Species == sp) %>%
    select(Region, Status, pct) %>%
    pivot_wider(names_from = Status, values_from = pct, values_fill = 0) %>%
    arrange(fct_relevel(Region, region_levels))
  writeData(wb, sh, tab)
  if (nrow(tab) > 0) {
    addStyle(wb, sh, int_style,
             rows = 2:(nrow(tab)+1),
             cols = which(vapply(tab, is.numeric, TRUE)),
             gridExpand = TRUE)
  }
}
saveWorkbook(wb, out_xlsx, overwrite = TRUE)

# ---------------------------
# Figure saver with verification + dual-location copy
# ---------------------------
safe_save_plot <- function(p, base_name, width = 7.5, height = 3.0, dpi = 300){
  png_docs <- file.path(png_dir_docs, paste0(base_name, ".png"))
  pdf_docs <- file.path(pdf_dir_docs, paste0(base_name, ".pdf"))
  png_desk <- file.path(png_dir_desk, paste0(base_name, ".png"))
  pdf_desk <- file.path(pdf_dir_desk, paste0(base_name, ".pdf"))

  # Save to Documents
  ggsave(png_docs, p, width = width, height = height, dpi = dpi, units = "in")
  ggsave(pdf_docs, p, width = width, height = height, units = "in")

  # Verify & copy to Desktop
  for (pair in list(
    list(src = png_docs, dst = png_desk),
    list(src = pdf_docs, dst = pdf_desk)
  )) {
    if (!file.exists(pair$src)) warning("File not found after save: ", pair$src)
    ok <- file.copy(pair$src, pair$dst, overwrite = TRUE)
    if (!ok) warning("Could not copy to Desktop: ", pair$dst)
    info <- tryCatch(file.info(pair$dst), error = function(e) NULL)
    if (is.null(info) || is.na(info$size) || info$size < 1000) {
      warning("Saved file is unexpectedly small: ", pair$dst)
    } else {
      message("Saved OK (", format(round(info$size/1024,1), nsmall=1), " KB): ", pair$dst)
    }
  }
}

# ---------------------------
# Figures: one per species
# ---------------------------
for (sp in species_order) {
  df_sp <- pct_long %>% filter(Species == sp)
  if (nrow(df_sp) == 0) next

  p <- ggplot(df_sp, aes(x = pct, y = fct_relevel(Region, region_levels), fill = Status)) +
    geom_col(width = 0.6) +
    scale_fill_manual(values = status_colors, limits = status_levels, drop = FALSE) +
    scale_x_continuous(limits = c(0,100),
                       breaks = seq(0,100,10),
                       labels = function(x) paste0(x, "%")) +
    labs(x = NULL, y = NULL, fill = NULL, title = sp) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0, size = 11, face = "bold", margin = margin(b = 6)),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 9, margin = margin(t = 5))
    )

  base_name <- paste0("Q16_", gsub("[^A-Za-z0-9]+", "_", sp))
  safe_save_plot(p, base_name)
}

# ---------------------------
# Final diagnostics
# ---------------------------
diag_list <- function(dir, pat) {
  f <- list.files(dir, pattern = pat, full.names = TRUE, ignore.case = TRUE)
  data.frame(
    file = normalizePath(f, winslash = "\\", mustWork = FALSE),
    size_kb = round(file.info(f)$size/1024, 1),
    modtime = as.character(file.info(f)$mtime),
    stringsAsFactors = FALSE
  )
}

cat("\n=== Where your figures are ===\n")
cat("PNG (Documents): ", normalizePath(png_dir_docs, winslash="\\", mustWork=FALSE), "\n")
print(diag_list(png_dir_docs, "\\.png$"))
cat("\nPDF (Documents): ", normalizePath(pdf_dir_docs, winslash="\\", mustWork=FALSE), "\n")
print(diag_list(pdf_dir_docs, "\\.pdf$"))

cat("\nPNG (Desktop):   ", normalizePath(png_dir_desk, winslash="\\", mustWork=FALSE), "\n")
print(diag_list(png_dir_desk, "\\.png$"))
cat("\nPDF (Desktop):   ", normalizePath(pdf_dir_desk, winslash="\\", mustWork=FALSE), "\n")
print(diag_list(pdf_dir_desk, "\\.pdf$"))

# Optionally open the Desktop PNG folder in Explorer (Windows only):
try(shell.exec(normalizePath(png_dir_desk, winslash="\\", mustWork=FALSE)), silent = TRUE)

message("\nDONE. Excel workbook: ", normalizePath(out_xlsx, winslash="\\", mustWork=FALSE))






Figure 3D2
# ==============================================================
# Q20 (Sec 2): Proportion of countries reporting conservation
# activities for at least one species (In situ / Ex situ in vivo / Ex situ in vitro)
# NOW ALSO: Answer-option counts per region (n/a, none, low, medium, high)
# Outputs: one Excel with BigFive / Other / All tables + QA + Answer counts
# ==============================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr); library(tidyr)
  library(forcats); library(openxlsx)
})

# ---------- Paths ----------
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3B1_Q20_Conservation_tables.xlsx"

# ---------- Regions (2024 order) ----------
region_levels <- c("Africa","Asia","Southwest Pacific","Europe",
                   "Latin America and the Carribean","North America",
                   "Near East","World")

# ---------- Species sets ----------
big5 <- c("cattle (specialized dairy)",
          "cattle (specialized beef)",
          "cattle (multipurpose)",
          "goats","sheep","pigs","chickens")

# Map SpecieTag (safety fallback)
code_to_species <- c(
  "002"="alpacas","003"="asses","004"="bactrian camels","005"="buffaloes",
  "007"="cattle (multipurpose)","008"="cattle (specialized beef)",
  "009"="cattle (specialized dairy)","010"="chickens","013"="deer",
  "015"="dromedaries","017"="ducks","021"="goats","022"="geese",
  "023"="guinea fowls","024"="guinea pigs","026"="horses","027"="llamas",
  "028"="managed bee","029"="mithun","030"="muscovy ducks","033"="ostriches",
  "037"="pigs","038"="pigeons","040"="quails","041"="rabbits",
  "042"="sheep","044"="turkeys","046"="yaks"
)

# ---------- Conservation categories (exact report headings) ----------
cat_map <- c(
  "in situ conservation"           = "In situ conservation programmes",
  "ex situ in vivo conservation"   = "Ex situ in vivo conservation programmes",
  "ex situ in vitro conservation"  = "Ex situ in vitro conservation programmes"
)
cats_report <- unname(cat_map)

# Presence vs none (for the COUNTRIES table)
present_answers <- c("low","medium","high")
none_answers    <- c("none")

# For the ANSWER COUNTS tables
answer_levels <- c("n/a","none","low","medium","high")

# ---------- Read + normalize ----------
raw <- read_excel(in_path)
names(raw) <- names(raw) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+","_") |>
  tolower()

pick_col <- function(...) {
  cands <- unlist(list(...)); hit <- cands[cands %in% names(raw)]
  if (length(hit)) hit[1] else NA_character_
}

col_region   <- pick_col("region")
col_country  <- pick_col("country")
col_section  <- pick_col("section")
col_question <- pick_col("question_2024","question")
col_answer   <- pick_col("answer")
col_subtype  <- pick_col("subquestiontype_2014")
col_breed    <- pick_col("breedtype_2014")        # prefer this for species
col_tablelab <- pick_col("tablelabel_2014","species_clean")
col_specietag<- pick_col("specietag","specietagnew","specietag_new","species")

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_answer), !is.na(col_subtype))

d0 <- tibble(
  Region_raw = raw[[col_region]],
  Country    = str_squish(as.character(raw[[col_country]])),
  Section    = raw[[col_section]],
  Question   = raw[[col_question]],
  Answer     = str_to_lower(str_squish(as.character(raw[[col_answer]]))),
  SubType0   = str_to_lower(str_squish(as.character(raw[[col_subtype]]))),
  BreedType0 = if (!is.na(col_breed))    str_to_lower(str_squish(as.character(raw[[col_breed]])))    else NA_character_,
  TableLab0  = if (!is.na(col_tablelab)) str_to_lower(str_squish(as.character(raw[[col_tablelab]]))) else NA_character_,
  SpecieTag0 = if (!is.na(col_specietag))str_squish(as.character(raw[[col_specietag]]))              else NA_character_
) %>%
  # Keep Sec 2, Q20 (accept "20" or "20.xx")
  filter(Section %in% c(2,"2"),
         grepl("^\\s*20(\\.|$)", as.character(Question))) %>%
  # Normalize region names to your list; drop any we can’t map
  mutate(
    Region = case_when(
      str_detect(Region_raw, regex("^africa$", TRUE)) ~ "Africa",
      str_detect(Region_raw, regex("^asia$", TRUE)) ~ "Asia",
      str_detect(Region_raw, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
      str_detect(Region_raw, regex("^europe", TRUE)) ~ "Europe",
      str_detect(Region_raw, regex("^latin\\s+america|caribbean|central america|south america", TRUE)) ~
        "Latin America and the Carribean",
      str_detect(Region_raw, regex("^north\\s*america", TRUE)) ~ "North America",
      str_detect(Region_raw, regex("^near\\s*east", TRUE)) ~ "Near East",
      str_detect(Region_raw, regex("^world$", TRUE)) ~ "World",
      TRUE ~ NA_character_
    ),
    SubType = recode(SubType0, !!!cat_map, .default = NA_character_)
  ) %>%
  filter(!is.na(Region), !is.na(SubType)) %>%
  # We'll reconstruct World — ignore source World rows
  filter(Region != "World") %>%
  # Species: prefer BreedType_2014; else TableLabel_2014; else SpecieTag map
  mutate(
    Species = dplyr::coalesce(na_if(BreedType0,""),
                              na_if(TableLab0,""),
                              recode(stringr::str_pad(SpecieTag0,3,pad="0"),
                                     !!!code_to_species, .default = NA_character_))
  ) %>%
  filter(!is.na(Species))

# Derive full species set and “other species”
species_all   <- sort(unique(d0$Species))
species_other <- setdiff(species_all, big5)

cat("\n[INFO] Species in Sec2/Q20:", length(species_all),
    " | BigFive in file:", sum(species_all %in% big5),
    " | Other species:", length(species_other), "\n")

# ---------- Helper: ANSWER COUNT tables (by option) ----------
make_answer_counts <- function(df_cut) {
  # Bucket the answer
  dx <- df_cut %>%
    mutate(AnsCat = case_when(
      Answer %in% c("low","medium","high") ~ Answer,
      Answer == "none"                     ~ "none",
      TRUE                                 ~ "n/a"
    )) %>%
    mutate(
      AnsCat  = factor(AnsCat, levels = answer_levels),
      SubType = factor(SubType, levels = cats_report)
    )

  # Counts per Region × SubType × AnsCat (answer rows)
  cnt_reg <- dx %>%
    count(Region, SubType, AnsCat, name = "n") %>%
    complete(Region, SubType = cats_report, AnsCat = answer_levels, fill = list(n = 0))

  # WORLD = sum of regions
  cnt_world <- cnt_reg %>%
    filter(Region != "World") %>%
    group_by(SubType, AnsCat) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    mutate(Region = factor("World", levels = region_levels)) %>%
    select(Region, SubType, AnsCat, n)

  cnt_all <- bind_rows(cnt_reg %>% filter(Region != "World"), cnt_world) %>%
    mutate(Region = factor(Region, levels = region_levels)) %>%
    arrange(Region, SubType, AnsCat)

  # Denominators (total answers) per Region × SubType
  den <- cnt_all %>%
    group_by(Region, SubType) %>%
    summarise(Total_answers = sum(n), .groups = "drop")

  # Wide matrix like screenshot + totals column
  mat_counts <- cnt_all %>%
    select(Region, SubType, AnsCat, n) %>%
    arrange(SubType, AnsCat) %>%
    mutate(RowLabel = SubType) %>%
    select(RowLabel, AnsCat, Region, n) %>%
    pivot_wider(names_from = Region, values_from = n) %>%
    # add row-wise total across regions (including World to show equality)
    rowwise() %>%
    mutate(Total = sum(c_across(all_of(region_levels)), na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(factor(RowLabel, levels = cats_report), AnsCat)

  list(
    counts_long = cnt_all,
    counts_wide = mat_counts,
    denoms      = den
  )
}

# ---------- Helper: compute one report cut (your original logic) ----------
make_cut <- function(df, keep_species, label){
  dx <- if (is.null(keep_species)) df else df %>% filter(Species %in% keep_species)

  if (nrow(dx) == 0) {
    empty_long <- tibble(Region = factor(character(), levels = region_levels),
                         SubType = character(), n_countries_present = integer(),
                         N_countries = integer(), Percent = numeric())
    empty_wide <- tibble(Region = factor(character(), levels = region_levels))
    empty_rep  <- tibble(Region = factor(character(), levels = region_levels),
                         N_countries = integer())
    # empty answer pieces too
    empty_ans_long <- tibble(Region = factor(character(), levels = region_levels),
                             SubType = character(), AnsCat = character(), n = integer())
    empty_ans_wide <- tibble()
    empty_den      <- tibble()
    return(list(
      cut = label, long = empty_long, wide = empty_wide, report = empty_rep,
      ans_long = empty_ans_long, ans_wide = empty_ans_wide, ans_den = empty_den
    ))
  }

  # Denominator per Region = distinct countries having any row in this cut
  den <- dx %>% group_by(Region) %>% summarise(N_countries = n_distinct(Country), .groups = "drop")

  # Country-level presence per category (any species present = TRUE)
  country_presence <- dx %>%
    mutate(present = Answer %in% present_answers) %>%
    group_by(Region, Country, SubType) %>%
    summarise(has_present = any(present, na.rm = TRUE), .groups = "drop")

  # Numerator per Region × SubType
  num <- country_presence %>%
    group_by(Region, SubType) %>%
    summarise(n_countries_present = sum(has_present, na.rm = TRUE), .groups = "drop") %>%
    complete(Region, SubType = cats_report, fill = list(n_countries_present = 0))

  # Merge denominators and compute percents
  reg_tbl <- num %>%
    left_join(den, by = "Region") %>%
    mutate(Percent = ifelse(N_countries > 0, 100 * n_countries_present / N_countries, NA_real_))

  # World row = sum regions
  world_row <- reg_tbl %>%
    group_by(SubType) %>%
    summarise(n_countries_present = sum(n_countries_present),
              N_countries = sum(N_countries),
              Percent = ifelse(N_countries > 0, 100 * n_countries_present / N_countries, NA_real_),
              .groups = "drop") %>%
    mutate(Region = "World")

  long <- bind_rows(reg_tbl, world_row) %>%
    mutate(Region = factor(Region, levels = region_levels)) %>%
    arrange(Region, SubType)

  wide <- long %>%
    select(Region, SubType, Percent) %>%
    pivot_wider(names_from = SubType, values_from = Percent) %>%
    arrange(Region)

  report <- bind_rows(
    den,
    tibble(Region = "World", N_countries = sum(den$N_countries, na.rm = TRUE))
  ) %>%
    mutate(Region = factor(Region, levels = region_levels)) %>%
    left_join(wide, by = "Region") %>%
    arrange(Region)

  # NEW: answer-option counts per region (rows of the dataset)
  ans <- make_answer_counts(dx)

  list(cut = label, long = long, wide = wide, report = report,
       ans_long = ans$counts_long, ans_wide = ans$counts_wide, ans_den = ans$denoms)
}

# ---------- Build the three cuts ----------
cut_big5  <- make_cut(d0, big5,          "BigFive")
cut_other <- make_cut(d0, species_other, "Other species")
cut_all   <- make_cut(d0, NULL,          "All species")

# ---------- QA sheets ----------
species_included <- tibble(
  Species = sort(unique(d0$Species)),
  Group   = ifelse(Species %in% big5, "BigFive", "Other")
)

qa_counts <- d0 %>%
  count(SubType, Answer, name = "n") %>%
  arrange(SubType, desc(n))

qa_tagmap <- raw %>%
  transmute(
    SpecieTag = if ("specietag" %in% names(raw)) as.character(specietag) else NA_character_,
    TableLabel_2014 = if ("tablelabel_2014" %in% names(raw)) as.character(tablelabel_2014) else NA_character_,
    BreedType_2014  = if ("breedtype_2014" %in% names(raw))  as.character(breedtype_2014)  else NA_character_
  ) %>% distinct()

filtered_data <- d0 %>%
  select(Region, Country, Species, SubType, Answer) %>%
  arrange(Region, Country, SubType, Species)

# ---------- Write Excel ----------
wb <- createWorkbook()

addWorksheet(wb, "README")
addWorksheet(wb, "Filtered_data")
addWorksheet(wb, "Species_included")
addWorksheet(wb, "QA_AnswerCounts")
addWorksheet(wb, "QA_TagMap")

# main (countries) tables
addWorksheet(wb, "BigFive_long")
addWorksheet(wb, "BigFive_wide")
addWorksheet(wb, "BigFive_table")

addWorksheet(wb, "Other_long")
addWorksheet(wb, "Other_wide")
addWorksheet(wb, "Other_table")

addWorksheet(wb, "All_long")
addWorksheet(wb, "All_wide")
addWorksheet(wb, "All_table")

# NEW: answer-option counts tables
addWorksheet(wb, "BigFive_AnsCounts_long")
addWorksheet(wb, "BigFive_AnsCounts_wide")
addWorksheet(wb, "BigFive_AnsDenominators")

addWorksheet(wb, "Other_AnsCounts_long")
addWorksheet(wb, "Other_AnsCounts_wide")
addWorksheet(wb, "Other_AnsDenominators")

addWorksheet(wb, "All_AnsCounts_long")
addWorksheet(wb, "All_AnsCounts_wide")
addWorksheet(wb, "All_AnsDenominators")

readme_txt <- paste(
  "Q20 (Section 2): Proportion of countries reporting conservation activities",
  "(In situ / Ex situ in vivo / Ex situ in vitro).",
  "",
  "Present = Answer in {low, medium, high}; None = 'none'.",
  "A country is counted as 'present' in a category if it reported present for",
  "  at least one species in that group (BigFive / Other / All).",
  "Denominator per region (countries tables) = # distinct countries with any row in that group.",
  "World = sum across regions (source 'World' rows ignored).",
  "",
  "NEW: Answer-option counts tables show raw ANSWER ROW counts by option",
  "  (n/a, none, low, medium, high) for each Region × Category, plus World and totals.",
  "Regions ordered: Africa, Asia, Southwest Pacific, Europe, Latin America and the Carribean, North America, Near East, World.",
  "",
  "Sheets: *_long / *_wide / *_table (countries), and *_AnsCounts_* (answer rows) + *_AnsDenominators.",
  sep = "\n"
)

writeData(wb, "README", readme_txt)
writeData(wb, "Filtered_data", filtered_data)
writeData(wb, "Species_included", species_included)
writeData(wb, "QA_AnswerCounts", qa_counts)
writeData(wb, "QA_TagMap", qa_tagmap)

write_cut <- function(cut, base){
  # Countries-based (your original) — percent table with integers
  rep_int <- cut$report %>%
    mutate(across(all_of(c("N_countries", cats_report)), ~ .x)) %>%
    mutate(across(all_of(cats_report), ~ round(.x, 0)))

  writeData(wb, paste0(base, "_long"), cut$long %>% arrange(Region, SubType))
  writeData(wb, paste0(base, "_wide"), cut$wide %>% arrange(Region))
  writeData(wb, paste0(base, "_table"), rep_int %>% arrange(Region))

  # NEW: answer-option counts
  writeData(wb, paste0(base, "_AnsCounts_long"), cut$ans_long %>% arrange(Region, SubType, AnsCat))
  writeData(wb, paste0(base, "_AnsCounts_wide"), cut$ans_wide)
  writeData(wb, paste0(base, "_AnsDenominators"),
            cut$ans_den %>% arrange(Region, SubType))
}

write_cut(cut_big5,  "BigFive")
write_cut(cut_other, "Other")
write_cut(cut_all,   "All")

# Styling
bold <- createStyle(textDecoration = "bold")
int0 <- createStyle(numFmt = "0")
for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:120, gridExpand = TRUE)
  if (grepl("_table$|AnsDenominators$", sh)) {
    df <- readWorkbook(wb, sh)
    if (!is.null(df) && nrow(df) > 0) {
      num_cols <- which(vapply(df, is.numeric, TRUE))
      if (length(num_cols)) {
        addStyle(wb, sh, int0, rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
      }
    }
  }
  setColWidths(wb, sh, cols = 1:120, widths = "auto")
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved Excel: ", out_xlsx)



TABLE 3D2
# ==============================================================
# Q20 (Sec 2): Average scores for conservation activities
#  - Categories: In situ / Ex situ in vivo / Ex situ in vitro
#  - Species: Big Five (7 rows: 3 cattle types + sheep, goats, pigs, chickens)
#  - Score map: none=0, low=1, medium=2, high=3; n/a excluded
#  - Country duplicates collapsed by max score before averaging
# Output: Excel with wide matrix like the sample + supporting sheets
# ==============================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr); library(tidyr)
  library(forcats); library(openxlsx)
})

# ---------- Paths ----------
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3B1_Q20_Conservation_AverageScores.xlsx"

# ---------- Regions (2024 order) ----------
region_levels <- c("Africa","Asia","Southwest Pacific","Europe",
                   "Latin America and the Carribean","North America",
                   "Near East","World")

# ---------- Big-five species (7 lines total) ----------
big5 <- c("cattle (specialized dairy)",
          "cattle (specialized beef)",
          "cattle (multipurpose)",
          "sheep","goats","pigs","chickens")

pretty_species <- c(
  "cattle (specialized dairy)" = "Cattle (specialized dairy)",
  "cattle (specialized beef)"  = "Cattle (specialized beef)",
  "cattle (multipurpose)"      = "Cattle (multipurpose)",
  "sheep"                      = "Sheep",
  "goats"                      = "Goats",
  "pigs"                       = "Pigs",
  "chickens"                   = "Chickens"
)

# ---------- Conservation categories (exact report headings) ----------
cat_map <- c(
  "in situ conservation"           = "In situ conservation",
  "ex situ in vivo conservation"   = "Ex situ in vivo conservation",
  "ex situ in vitro conservation"  = "Ex situ in vitro conservation"
)
cats_report <- unname(cat_map)  # keep order

# ---------- Answer → numeric score ----------
score_map <- c("none" = 0, "low" = 1, "medium" = 2, "high" = 3)

# ---------- Optional SpecieTag fallback ----------
code_to_species <- c(
  "007"="cattle (multipurpose)","008"="cattle (specialized beef)",
  "009"="cattle (specialized dairy)","010"="chickens",
  "021"="goats","037"="pigs","042"="sheep"
)

# ---------- Read + normalize ----------
raw <- read_excel(in_path)
names(raw) <- names(raw) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+","_") |>
  tolower()

pick_col <- function(...) {
  cands <- unlist(list(...)); hit <- cands[cands %in% names(raw)]
  if (length(hit)) hit[1] else NA_character_
}

col_region   <- pick_col("region")
col_country  <- pick_col("country")
col_section  <- pick_col("section")
col_question <- pick_col("question_2024","question")
col_answer   <- pick_col("answer")
col_subtype  <- pick_col("subquestiontype_2014")
col_breed    <- pick_col("breedtype_2014")
col_tablelab <- pick_col("tablelabel_2014","species_clean")
col_specietag<- pick_col("specietag","specietagnew","specietag_new","species")

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_answer), !is.na(col_subtype))

d0 <- tibble(
  Region_raw = raw[[col_region]],
  Country    = str_squish(as.character(raw[[col_country]])),
  Section    = raw[[col_section]],
  Question   = raw[[col_question]],
  Answer_txt = str_to_lower(str_squish(as.character(raw[[col_answer]]))),
  SubType0   = str_to_lower(str_squish(as.character(raw[[col_subtype]]))),
  BreedType0 = if (!is.na(col_breed))    str_to_lower(str_squish(as.character(raw[[col_breed]])))    else NA_character_,
  TableLab0  = if (!is.na(col_tablelab)) str_to_lower(str_squish(as.character(raw[[col_tablelab]]))) else NA_character_,
  SpecieTag0 = if (!is.na(col_specietag))str_squish(as.character(raw[[col_specietag]]))              else NA_character_
) %>%
  # Keep Sec 2, Q20 (accept "20" or "20.xx")
  filter(Section %in% c(2,"2"),
         grepl("^\\s*20(\\.|$)", as.character(Question))) %>%
  # Normalize Region and Category
  mutate(
    Region = case_when(
      str_detect(Region_raw, regex("^africa$", TRUE)) ~ "Africa",
      str_detect(Region_raw, regex("^asia$", TRUE)) ~ "Asia",
      str_detect(Region_raw, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
      str_detect(Region_raw, regex("^europe", TRUE)) ~ "Europe",
      str_detect(Region_raw, regex("^latin\\s+america|caribbean|central america|south america", TRUE)) ~
        "Latin America and the Carribean",
      str_detect(Region_raw, regex("^north\\s*america", TRUE)) ~ "North America",
      str_detect(Region_raw, regex("^near\\s*east", TRUE)) ~ "Near East",
      str_detect(Region_raw, regex("^world$", TRUE)) ~ "World",
      TRUE ~ NA_character_
    ),
    Conservation = recode(SubType0, !!!cat_map, .default = NA_character_)
  ) %>%
  filter(!is.na(Region), !is.na(Conservation)) %>%
  filter(Region != "World") %>%                           # we build World ourselves
  # Species: prefer BreedType_2014, else TableLabel_2014, else SpecieTag
  mutate(
    Species_key = dplyr::coalesce(na_if(BreedType0,""),
                                  na_if(TableLab0,""),
                                  recode(stringr::str_pad(SpecieTag0, 3, pad="0"),
                                         !!!code_to_species, .default = NA_character_))
  ) %>%
  filter(!is.na(Species_key),
         Species_key %in% big5) %>%
  # Map Answer → score; keep n/a as NA (dropped from averaging)
  mutate(Score = recode(Answer_txt, !!!score_map, .default = NA_real_)) %>%
  mutate(
    Region       = factor(Region, levels = region_levels[region_levels != "World"]),
    Conservation = factor(Conservation, levels = cats_report),
    Species_lab  = factor(pretty_species[Species_key], levels = pretty_species[big5])
  )

# ---------- Collapse duplicates per Country × Species × Conservation ----------
# (take MAX score for a country if multiple rows exist)
country_max <- d0 %>%
  group_by(Region, Country, Species_lab, Conservation) %>%
  summarise(Score = suppressWarnings(max(Score, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(Score = ifelse(is.infinite(Score), NA_real_, Score))  # if all NA at source

# ---------- Regional means (exclude NA scores) ----------
reg_means <- country_max %>%
  group_by(Region, Species_lab, Conservation) %>%
  summarise(AvgScore = mean(Score, na.rm = TRUE),
            N_countries_used = sum(!is.na(Score)),
            .groups = "drop")

# ---------- WORLD mean (across all countries; not mean of regions) ----------
world_means <- country_max %>%
  group_by(Species_lab, Conservation) %>%
  summarise(AvgScore = mean(Score, na.rm = TRUE),
            N_countries_used = sum(!is.na(Score)),
            .groups = "drop") %>%
  mutate(Region = factor("World", levels = "World")) %>%
  select(Region, Species_lab, Conservation, AvgScore, N_countries_used)

means_all <- bind_rows(
  reg_means,
  world_means
) %>%
  mutate(
    Region = factor(as.character(Region), levels = region_levels),
    Species_lab = factor(Species_lab, levels = pretty_species[big5]),
    Conservation = factor(Conservation, levels = cats_report)
  ) %>%
  arrange(Conservation, Species_lab, Region)

# ---------- Wide matrix like the sample ----------
wide_matrix <- means_all %>%
  select(Conservation, Species = Species_lab, Region, AvgScore) %>%
  mutate(AvgScore = round(AvgScore, 1)) %>%
  pivot_wider(names_from = Region, values_from = AvgScore) %>%
  arrange(Conservation, Species)

# ---------- Support tables (optional QA) ----------
denominators <- means_all %>%
  select(Conservation, Species = Species_lab, Region, N_countries_used) %>%
  pivot_wider(names_from = Region, values_from = N_countries_used) %>%
  arrange(Conservation, Species)

filtered_min <- d0 %>%
  select(Region, Country, Conservation, Species = Species_key,
         Answer = Answer_txt, Score)

# ---------- Write Excel ----------
wb <- createWorkbook()

addWorksheet(wb, "README")
addWorksheet(wb, "Filtered_data")
addWorksheet(wb, "Country_max")
addWorksheet(wb, "Means_long")
addWorksheet(wb, "AverageScores_matrix")
addWorksheet(wb, "N_used_byRegion")

readme_txt <- paste(
  "Q20 (Section 2) – Average scores for conservation activities.",
  "Categories: In situ / Ex situ in vivo / Ex situ in vitro.",
  "Species: Big Five set (3 cattle types + sheep, goats, pigs, chickens).",
  "",
  "Scoring: none=0, low=1, medium=2, high=3; 'n/a' excluded.",
  "Duplicates per Country × Species × Category collapsed by MAX score.",
  "Regional averages are means of country scores (excluding NA).",
  "World average is the mean across all countries (not a mean of regional means).",
  "Regions columns ordered: Africa, Asia, Southwest Pacific, Europe,",
  "  Latin America and the Carribean, North America, Near East, World.",
  "The 'AverageScores_matrix' sheet matches the layout of your sample.",
  sep = "\n"
)

writeData(wb, "README", readme_txt)
writeData(wb, "Filtered_data", filtered_min %>% arrange(Region, Country, Conservation, Species))
writeData(wb, "Country_max", country_max %>% arrange(Region, Country, Conservation, Species_lab))
writeData(wb, "Means_long", means_all %>% arrange(Conservation, Species_lab, Region))
writeData(wb, "AverageScores_matrix", wide_matrix)
writeData(wb, "N_used_byRegion", denominators)

# Styling (1 decimal like screenshot)
bold <- createStyle(textDecoration = "bold")
one_dec <- createStyle(numFmt = "0.0")

for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:120, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:120, widths = "auto")
}
# number formatting for the matrix & means
for (sh in c("AverageScores_matrix","Means_long")) {
  df <- readWorkbook(wb, sh)
  if (!is.null(df) && nrow(df) > 0) {
    num_cols <- which(vapply(df, is.numeric, TRUE))
    if (length(num_cols)) {
      addStyle(wb, sh, one_dec,
               rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
    }
  }
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved Excel: ", out_xlsx)




TABLE 3D3 - 3D5
# ==============================================================
# Q20 (Sec 2) – Proportion of countries reporting conservation
# programmes by Region × Species (Big Five), one sheet per category
# Categories: In situ / Ex situ in vivo / Ex situ in vitro
# --------------------------------------------------------------
# Output: C:/Users/LENOVO/Documents/Fig3B1_Q20_Conservation_Region_bySpecies.xlsx
# ==============================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr); library(tidyr)
  library(forcats); library(openxlsx)
})

# ---------- Paths ----------
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3B1_Q20_Conservation_Region_bySpecies.xlsx"

# ---------- Regions (2024 order) ----------
region_levels <- c("Africa","Asia","Southwest Pacific","Europe",
                   "Latin America and the Carribean","North America",
                   "Near East","World")

# ---------- Big Five species (7 columns) ----------
big5 <- c("cattle (specialized dairy)",
          "cattle (specialized beef)",
          "cattle (multipurpose)",
          "sheep","goats","pigs","chickens")

species_pretty <- c(
  "cattle (specialized dairy)" = "Dairy cattle",
  "cattle (specialized beef)"  = "Beef cattle",
  "cattle (multipurpose)"      = "Multi-purpose cattle",
  "sheep"                      = "Sheep",
  "goats"                      = "Goats",
  "pigs"                       = "Pigs",
  "chickens"                   = "Chickens"
)
species_pretty_order <- unname(species_pretty[big5])

# ---------- Conservation categories (exact header text) ----------
cat_map <- c(
  "in situ conservation"           = "In situ conservation programmes",
  "ex situ in vivo conservation"   = "Ex situ in vivo conservation programmes",
  "ex situ in vitro conservation"  = "Ex situ in vitro conservation programmes"
)

# ---------- Present vs None ----------
present_answers <- c("low","medium","high")
none_answers    <- c("none")

# ---------- SpecieTag fallback (only Big Five codes listed) ----------
code_to_species <- c(
  "007"="cattle (multipurpose)",
  "008"="cattle (specialized beef)",
  "009"="cattle (specialized dairy)",
  "010"="chickens",
  "021"="goats",
  "037"="pigs",
  "042"="sheep"
)

# ---------- Load + normalise ----------
raw <- read_excel(in_path)
names(raw) <- names(raw) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+","_") |>
  tolower()

pick_col <- function(...) {
  cands <- unlist(list(...)); hit <- cands[cands %in% names(raw)]
  if (length(hit)) hit[1] else NA_character_
}

col_region   <- pick_col("region")
col_country  <- pick_col("country")
col_section  <- pick_col("section")
col_question <- pick_col("question_2024","question")
col_answer   <- pick_col("answer")
col_subtype  <- pick_col("subquestiontype_2014")
col_breed    <- pick_col("breedtype_2014")
col_tablelab <- pick_col("tablelabel_2014","species_clean")
col_specietag<- pick_col("specietag","specietagnew","specietag_new","species")

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_answer), !is.na(col_subtype))

d0 <- tibble(
  Region_raw = raw[[col_region]],
  Country    = str_squish(as.character(raw[[col_country]])),
  Section    = raw[[col_section]],
  Question   = raw[[col_question]],
  Answer     = str_to_lower(str_squish(as.character(raw[[col_answer]]))),
  SubType0   = str_to_lower(str_squish(as.character(raw[[col_subtype]]))),
  BreedType0 = if (!is.na(col_breed))    str_to_lower(str_squish(as.character(raw[[col_breed]])))    else NA_character_,
  TableLab0  = if (!is.na(col_tablelab)) str_to_lower(str_squish(as.character(raw[[col_tablelab]]))) else NA_character_,
  SpecieTag0 = if (!is.na(col_specietag))str_squish(as.character(raw[[col_specietag]]))              else NA_character_
) %>%
  # Keep Sec 2, Q20 (accept "20" or "20.xx")
  filter(Section %in% c(2,"2"),
         grepl("^\\s*20(\\.|$)", as.character(Question))) %>%
  # Normalise region and category
  mutate(
    Region = case_when(
      str_detect(Region_raw, regex("^africa$", TRUE)) ~ "Africa",
      str_detect(Region_raw, regex("^asia$", TRUE)) ~ "Asia",
      str_detect(Region_raw, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
      str_detect(Region_raw, regex("^europe", TRUE)) ~ "Europe",
      str_detect(Region_raw, regex("^latin\\s+america|caribbean|central america|south america", TRUE)) ~
        "Latin America and the Carribean",
      str_detect(Region_raw, regex("^north\\s*america", TRUE)) ~ "North America",
      str_detect(Region_raw, regex("^near\\s*east", TRUE)) ~ "Near East",
      str_detect(Region_raw, regex("^world$", TRUE)) ~ "World",
      TRUE ~ NA_character_
    ),
    Category = recode(SubType0, !!!cat_map, .default = NA_character_)
  ) %>%
  filter(!is.na(Region), !is.na(Category)) %>%
  filter(Region != "World") %>%  # compute World ourselves
  # Species: prefer BreedType; else TableLabel; else SpecieTag
  mutate(
    Species_raw = dplyr::coalesce(na_if(BreedType0,""),
                                  na_if(TableLab0,""),
                                  recode(stringr::str_pad(SpecieTag0, 3, pad="0"),
                                         !!!code_to_species, .default = NA_character_))
  ) %>%
  filter(!is.na(Species_raw),
         Species_raw %in% big5) %>%
  mutate(
    Species = factor(species_pretty[Species_raw], levels = species_pretty_order),
    Region  = factor(Region, levels = region_levels[region_levels != "World"])
  )

# ---------- CORE helper: table for one category ----------
make_table_for_category <- function(cat_long_label) {

  dd <- d0 %>% filter(Category == cat_long_label)
  if (nrow(dd) == 0) {
    out <- tibble(
      Region = factor(region_levels, levels = region_levels),
      `Number of countries` = NA_integer_
    )
    for (sp in species_pretty_order) out[[sp]] <- NA_real_
    return(out)
  }

  # Denominator for "Number of countries" column:
  # unique countries per region with ANY Big Five row in Q20
  N_by_region <- d0 %>%
    distinct(Region, Country) %>%
    count(Region, name = "N_countries")

  # Numerator: countries reporting PRESENT for species in this category
  present_flag <- dd %>%
    mutate(is_present = Answer %in% present_answers) %>%
    group_by(Region, Country, Species) %>%
    summarise(reported = any(is_present, na.rm = TRUE), .groups = "drop")

  num_by_reg_sp <- present_flag %>%
    group_by(Region, Species) %>%
    summarise(n_reported = sum(reported, na.rm = TRUE), .groups = "drop") %>%
    complete(Region = unique(d0$Region), Species = species_pretty_order,
             fill = list(n_reported = 0))

  # WORLD rows
  world_num <- num_by_reg_sp %>%
    group_by(Species) %>%
    summarise(n_reported = sum(n_reported), .groups = "drop") %>%
    mutate(Region = factor("World", levels = "World")) %>%
    select(Region, Species, n_reported)

  world_den <- N_by_region %>%
    summarise(N_countries = sum(N_countries)) %>%
    mutate(Region = factor("World", levels = "World")) %>%
    select(Region, N_countries)

  num_all <- bind_rows(num_by_reg_sp, world_num)
  den_all <- bind_rows(N_by_region, world_den) %>%
    mutate(Region = factor(as.character(Region), levels = region_levels))

  # Percent = 100 * numerator / N(region)
  pct_long <- num_all %>%
    left_join(den_all, by = "Region") %>%
    mutate(pct = ifelse(N_countries > 0, 100 * n_reported / N_countries, NA_real_)) %>%
    mutate(
      Region  = factor(as.character(Region), levels = region_levels),
      Species = factor(Species, levels = species_pretty_order)
    ) %>%
    arrange(Region, Species)

  # Wide like the book
  wide <- pct_long %>%
    select(Region, Species, pct) %>%
    pivot_wider(names_from = Species, values_from = pct) %>%
    left_join(den_all, by = "Region") %>%
    select(Region, `Number of countries` = N_countries, all_of(species_pretty_order)) %>%
    mutate(Region = factor(as.character(Region), levels = region_levels)) %>%
    arrange(Region) %>%
    mutate(across(all_of(species_pretty_order), ~ round(.x, 0)))

  wide
}

# ---------- Build three sheets ----------
tab_in_situ   <- make_table_for_category(cat_map["in situ conservation"])
tab_ex_vivo   <- make_table_for_category(cat_map["ex situ in vivo conservation"])
tab_ex_vitro  <- make_table_for_category(cat_map["ex situ in vitro conservation"])

# ---------- Extra QA (optional) ----------
# Species-specific denominators (how many countries had any row for each species)
den_species_reg <- d0 %>%
  distinct(Region, Country, Species) %>%
  count(Region, Species, name = "N_countries_species") %>%
  complete(Region = unique(d0$Region), Species = species_pretty_order,
           fill = list(N_countries_species = 0))

den_species_world <- den_species_reg %>%
  group_by(Species) %>%
  summarise(N_countries_species = sum(N_countries_species), .groups = "drop") %>%
  mutate(Region = factor("World", levels = "World"))

den_species <- bind_rows(
  den_species_reg,
  den_species_world
) %>%
  mutate(Region = factor(as.character(Region), levels = region_levels)) %>%
  arrange(Region, Species)

qa_counts <- d0 %>%
  count(Category, Answer, name = "n") %>%
  arrange(Category, desc(n))

filtered_min <- d0 %>%
  mutate(Species = as.character(Species)) %>%
  select(Region, Country, Category, Species, Answer) %>%
  arrange(Region, Country, Category, Species)

# ---------- Write Excel ----------
wb <- createWorkbook()

addWorksheet(wb, "README")
addWorksheet(wb, "In_situ")
addWorksheet(wb, "Ex_situ_in_vivo")
addWorksheet(wb, "Ex_situ_in_vitro")
addWorksheet(wb, "Filtered_data")
addWorksheet(wb, "Denom_bySpecies")
addWorksheet(wb, "QA_AnswerCounts")

readme_txt <- paste(
  "Q20 (Section 2): Proportion of countries reporting conservation programmes.",
  "Big Five species only (3 cattle types + sheep, goats, pigs, chickens).",
  "Present = Answer in {low, medium, high}; 'none' = not present; 'n/a' ignored for presence.",
  "Number of countries = distinct countries per region with any Big Five record in Q20.",
  "World = sum of regions (numerators and denominators).",
  "Sheets 'In_situ', 'Ex_situ_in_vivo', 'Ex_situ_in_vitro' follow the book structure:",
  "  Region | Number of countries | Dairy | Beef | Multipurpose | Sheep | Goats | Pigs | Chickens.",
  sep = "\n"
)

writeData(wb, "README",           readme_txt)
writeData(wb, "In_situ",          tab_in_situ)
writeData(wb, "Ex_situ_in_vivo",  tab_ex_vivo)
writeData(wb, "Ex_situ_in_vitro", tab_ex_vitro)
writeData(wb, "Filtered_data",    filtered_min)
writeData(wb, "Denom_bySpecies",  den_species)
writeData(wb, "QA_AnswerCounts",  qa_counts)

# Styling (integers for % columns)
bold <- createStyle(textDecoration = "bold")
int0 <- createStyle(numFmt = "0")
for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:80, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:80, widths = "auto")
}
for (sh in c("In_situ","Ex_situ_in_vivo","Ex_situ_in_vitro")) {
  df <- readWorkbook(wb, sh)
  if (!is.null(df) && nrow(df) > 0) {
    num_cols <- which(vapply(df, is.numeric, TRUE))
    if (length(num_cols)) {
      addStyle(wb, sh, int0,
               rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
    }
  }
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved Excel: ", out_xlsx)



TABLE 3D7 - 3D8
# ==============================================================
# Q22 (Sec 2) – In situ conservation programme elements
# Proportion of countries (YES/NO) by element × species
# Big-five species only; global table + regional sheets
# ==============================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# -------- Paths --------
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3B1_Q22_InSitu_elements.xlsx"

# -------- Regions (2024 order) --------
region_levels <- c("Africa","Asia","Southwest Pacific","Europe",
                   "Latin America and the Carribean","North America",
                   "Near East","World")

# safe sheet names (<=31 chars)
safe_region_sheet <- function(r){
  recode(r,
    "Africa"                          = "Region_Africa",
    "Asia"                            = "Region_Asia",
    "Southwest Pacific"               = "Region_SW_Pacific",
    "Europe"                          = "Region_Europe",
    "Latin America and the Carribean" = "Region_LAC",
    "North America"                   = "Region_North_America",
    "Near East"                       = "Region_Near_East",
    .default = paste0("Region_", gsub("[^A-Za-z0-9]+","_", r))
  )
}

# -------- Big-five 7 species --------
big7 <- c("cattle (specialized dairy)",
          "cattle (specialized beef)",
          "cattle (multipurpose)",
          "sheep","goats","pigs","chickens")

# Pretty column names (keep this order)
species_pretty <- c("Dairy cattle","Beef cattle","Multipurpose cattle",
                    "Sheep","Goats","Pigs","Chickens")

# Map SpecieTag codes (fallback)
code_to_species <- c(
  "009"="cattle (specialized dairy)",
  "008"="cattle (specialized beef)",
  "007"="cattle (multipurpose)",
  "042"="sheep","021"="goats","037"="pigs","010"="chickens"
)

# -------- Programme elements (exact SubquestionType_2014 strings) --------
elements <- c(
  "Promotion of niche marketing or other market differentiation",
  "Community-based conservation programmes",
  "Incentive or subsidy payment schemes for keeping at-risk breeds",
  "Development of biocultural community protocols",
  "Recognition/award programmes for breeders",
  "Conservation breeding programmes",
  "Selection programmes for increased production or productivity in at-risk breeds",
  "Promotion of at-risk breeds as tourist attractions",
  "Use of at-risk breeds in the management of wildlife habitats and landscapes",
  "Promotion of breed-related cultural activities",
  "Extension programmes to improve the management of at-risk breeds",
  "Awareness-raising activities providing information on the potential of specific at-risk breeds"
)
row_label <- elements; names(row_label) <- elements

# -------- Read & normalize headers --------
raw <- read_excel(in_path)
names(raw) <- names(raw) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+","_") |>
  tolower()

pick_col <- function(...) {
  cands <- unlist(list(...))
  hit <- cands[cands %in% names(raw)]
  if (length(hit)) hit[1] else NA_character_
}

col_region   <- pick_col("region")
col_country  <- pick_col("country")
col_section  <- pick_col("section")
col_question <- pick_col("question_2024","question")
col_answer   <- pick_col("answer")
col_subtype  <- pick_col("subquestiontype_2014")
col_breed    <- pick_col("breedtype_2014")
col_tablelab <- pick_col("tablelabel_2014","species_clean")
col_specietag<- pick_col("specietag","specietagnew","specietag_new","species")

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_answer), !is.na(col_subtype))

# -------- Filter to Sec 2, Q22 --------
d0 <- tibble(
  Region_raw = raw[[col_region]],
  Country    = str_squish(as.character(raw[[col_country]])),
  Section    = raw[[col_section]],
  Question   = raw[[col_question]],
  Answer0    = str_to_lower(str_squish(as.character(raw[[col_answer]]))),
  Element0   = str_squish(as.character(raw[[col_subtype]])),
  BreedType0 = if (!is.na(col_breed))    str_to_lower(str_squish(as.character(raw[[col_breed]])))    else NA_character_,
  TableLab0  = if (!is.na(col_tablelab)) str_to_lower(str_squish(as.character(raw[[col_tablelab]]))) else NA_character_,
  SpecieTag0 = if (!is.na(col_specietag))str_squish(as.character(raw[[col_specietag]]))              else NA_character_
) %>%
  filter(Section %in% c(2,"2"),
         grepl("^\\s*22(\\.|$)", as.character(Question))) %>%
  mutate(
    Region = case_when(
      str_detect(Region_raw, regex("^africa$", TRUE)) ~ "Africa",
      str_detect(Region_raw, regex("^asia$", TRUE)) ~ "Asia",
      str_detect(Region_raw, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
      str_detect(Region_raw, regex("^europe", TRUE)) ~ "Europe",
      str_detect(Region_raw, regex("^latin\\s+america|caribbean|central america|south america", TRUE)) ~
        "Latin America and the Carribean",
      str_detect(Region_raw, regex("^north\\s*america", TRUE)) ~ "North America",
      str_detect(Region_raw, regex("^near\\s*east", TRUE)) ~ "Near East",
      str_detect(Region_raw, regex("^world$", TRUE)) ~ "World",
      TRUE ~ NA_character_
    ),
    Element = Element0
  ) %>%
  filter(!is.na(Region), Element %in% elements) %>%
  mutate(
    Species_key = dplyr::coalesce(na_if(BreedType0,""),
                                  na_if(TableLab0,""),
                                  recode(stringr::str_pad(SpecieTag0,3,pad="0"),
                                         !!!code_to_species, .default = NA_character_))
  ) %>%
  filter(!is.na(Species_key), Species_key %in% big7) %>%
  mutate(
    Species = factor(Species_key, levels = big7, labels = species_pretty),
    Region  = factor(Region, levels = region_levels),
    Answer  = ifelse(Answer0 %in% c("yes","y","1","true"), "yes",
                     ifelse(Answer0 %in% c("no","n","0","false"), "no", NA_character_))
  ) %>%
  filter(!is.na(Answer)) %>%
  filter(Region != "World")   # rebuild World from regions

# -------- Collapse duplicates at Country level (any YES overrides NO) --------
d_country <- d0 %>%
  mutate(is_yes = Answer == "yes") %>%
  group_by(Region, Country, Species, Element) %>%
  summarise(YES = any(is_yes, na.rm = TRUE), .groups = "drop")

# -------- Helper: build matrix (global or single region) --------
build_matrix <- function(regions = NULL) {
  if (is.null(regions)) {
    den <- d_country %>%
      group_by(Region, Country, Species) %>% summarise(.groups = "drop") %>%
      ungroup() %>%
      group_by(Species) %>% summarise(N_countries = n_distinct(paste(Region, Country)), .groups = "drop")

    num_yes <- d_country %>%
      group_by(Species, Element) %>%
      summarise(n_yes = sum(YES, na.rm = TRUE), .groups = "drop")
  } else {
    den <- d_country %>%
      filter(as.character(Region) %in% regions) %>%
      group_by(Species) %>% summarise(N_countries = n_distinct(Country), .groups = "drop")

    num_yes <- d_country %>%
      filter(as.character(Region) %in% regions) %>%
      group_by(Species, Element) %>%
      summarise(n_yes = sum(YES, na.rm = TRUE), .groups = "drop")
  }

  mat <- expand_grid(Element = elements, Species = levels(d_country$Species)) %>%
    left_join(num_yes, by = c("Element","Species")) %>%
    mutate(n_yes = replace_na(n_yes, 0)) %>%
    left_join(den, by = "Species") %>%
    mutate(pct_yes = ifelse(N_countries > 0, 100 * n_yes / N_countries, NA_real_)) %>%
    mutate(Element = factor(Element, levels = elements, labels = row_label[elements]),
           Species = factor(Species, levels = levels(d_country$Species))) %>%
    arrange(Element, Species)

  wide <- mat %>%
    select(Element, Species, pct_yes) %>%
    pivot_wider(names_from = Species, values_from = pct_yes) %>%
    mutate(`Average across species` = rowMeans(across(all_of(species_pretty)), na.rm = TRUE)) %>%
    select(Element, `Average across species`, all_of(species_pretty))

  list(wide = wide, long = mat, den = den)
}

# -------- Global & region tables --------
global <- build_matrix(NULL)

# YES & NO (global long with denominators)
global_yesno <- {
  den <- global$den
  num_no <- d_country %>%
    group_by(Species, Element) %>%
    summarise(n_no = sum(!YES, na.rm = TRUE), .groups = "drop")

  mat_no <- expand_grid(Element = elements, Species = levels(d_country$Species)) %>%
    left_join(num_no, by = c("Element","Species")) %>%
    mutate(n_no = replace_na(n_no, 0)) %>%
    left_join(den, by = "Species") %>%
    mutate(pct_no = ifelse(N_countries > 0, 100 * n_no / N_countries, NA_real_)) %>%
    arrange(Element, Species)

  global$long %>%
    select(Element, Species, N_countries, pct_yes) %>%
    left_join(mat_no %>% select(Element, Species, pct_no), by = c("Element","Species")) %>%
    mutate(across(c(pct_yes, pct_no), ~ round(.x, 0)))
}

regions_only <- region_levels[region_levels != "World"]
region_tabs <- lapply(regions_only, function(rg) {
  build_matrix(rg)$wide %>% mutate(across(-Element, ~ round(.x, 0)))
})
names(region_tabs) <- regions_only

world_tab <- build_matrix(NULL)$wide %>% mutate(across(-Element, ~ round(.x, 0)))
global_yes_sheet <- global$wide %>% mutate(across(-Element, ~ round(.x, 0)))

# Minimal filtered rows for QA
filtered_rows <- d0 %>%
  transmute(Region = as.character(Region),
            Country,
            Species = factor(Species_key, levels = big7, labels = species_pretty),
            Element,
            Answer = ifelse(Answer0 %in% c("yes","y","1","true"), "yes",
                            ifelse(Answer0 %in% c("no","n","0","false"), "no", NA_character_))) %>%
  arrange(Region, Country, Species, Element)

# -------- Write Excel --------
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "GLOBAL_YesOnly")
addWorksheet(wb, "GLOBAL_Yes_and_No")
addWorksheet(wb, "Filtered_data")

readme <- paste(
  "Q22 (Section 2) – In situ conservation programme elements.",
  "Big-five species only (3 cattle types + sheep, goats, pigs, chickens).",
  "Method:",
  "- Present = 'yes'; Absent = 'no'. Any duplicates per country/region/species/element resolved as any-YES.",
  "- Denominator per species = # countries that reported that species (any element).",
  "- GLOBAL aggregates regions (World = sum of regions).",
  sep = "\n"
)
writeData(wb, "README", readme)
writeData(wb, "GLOBAL_YesOnly", global_yes_sheet)
writeData(wb, "GLOBAL_Yes_and_No", global_yesno)
writeData(wb, "Filtered_data", filtered_rows)

# Region sheets — use SAFE short names
region_sheet_names <- setNames(vapply(regions_only, safe_region_sheet, character(1)), regions_only)
for (rg in regions_only) {
  sh <- region_sheet_names[[rg]]
  addWorksheet(wb, sh)
  writeData(wb, sh, region_tabs[[rg]])
}

# World sheet
addWorksheet(wb, "World_total")
writeData(wb, "World_total", world_tab)

# -------- Styling --------
bold <- createStyle(textDecoration = "bold")
int0 <- createStyle(numFmt = "0")

for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:100, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:100, widths = "auto")
}

# numeric columns as integers for the matrix sheets
matrix_sheets <- c("GLOBAL_YesOnly", unname(region_sheet_names), "World_total")
for (sh in matrix_sheets) {
  df <- readWorkbook(wb, sh)
  if (!is.null(df) && nrow(df) > 0) {
    num_cols <- which(vapply(df, is.numeric, TRUE))
    if (length(num_cols)) {
      addStyle(wb, sh, int0, rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
    }
  }
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved Excel: ", out_xlsx)






BUILD REGION TABLE
# ==============================================================
# Q22 (Sec 2) – In situ conservation programme elements
# Regional table: proportion of answers using each element,
# averaged across the Big-Five species (3 cattle types + sheep/goats/pigs/chickens).
# Output: a new Excel file with the single table + a README + QA sheets.
# ==============================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# -------- Paths --------
in_path   <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx  <- "C:/Users/LENOVO/Documents/Fig3B1_Q22_RegionAverages.xlsx"

# -------- Region order (2024 wording, World first in the final table) --------
region_levels <- c("Africa","Asia","Southwest Pacific","Europe",
                   "Latin America and the Carribean","North America",
                   "Near East","World")
region_display <- c("World", region_levels[region_levels != "World"])

# -------- Big-Five 7 species (internal keys) + pretty names (columns order) --------
big7 <- c("cattle (specialized dairy)",
          "cattle (specialized beef)",
          "cattle (multipurpose)",
          "sheep","goats","pigs","chickens")

species_pretty <- c("Dairy cattle","Beef cattle","Multipurpose cattle",
                    "Sheep","Goats","Pigs","Chickens")

# -------- SpecieTag fallback (safety) --------
code_to_species <- c(
  "009"="cattle (specialized dairy)",
  "008"="cattle (specialized beef)",
  "007"="cattle (multipurpose)",
  "042"="sheep","021"="goats","037"="pigs","010"="chickens"
)

# -------- Programme elements (exact SubquestionType_2014 strings) --------
elements <- c(
  "Promotion of niche marketing or other market differentiation",
  "Community-based conservation programmes",
  "Incentive or subsidy payment schemes for keeping at-risk breeds",
  "Development of biocultural community protocols",
  "Recognition/award programmes for breeders",
  "Conservation breeding programmes",
  "Selection programmes for increased production or productivity in at-risk breeds",
  "Promotion of at-risk breeds as tourist attractions",
  "Use of at-risk breeds in the management of wildlife habitats and landscapes",
  "Promotion of breed-related cultural activities",
  "Extension programmes to improve the management of at-risk breeds",
  "Awareness-raising activities providing information on the potential of specific at-risk breeds"
)

# -------- Read & normalize headers --------
raw <- read_excel(in_path)
names(raw) <- names(raw) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+","_") |>
  tolower()

pick_col <- function(...) {
  cands <- unlist(list(...))
  hit <- cands[cands %in% names(raw)]
  if (length(hit)) hit[1] else NA_character_
}

col_region   <- pick_col("region")
col_country  <- pick_col("country")
col_section  <- pick_col("section")
col_question <- pick_col("question_2024","question")
col_answer   <- pick_col("answer")
col_subtype  <- pick_col("subquestiontype_2014")
col_breed    <- pick_col("breedtype_2014")
col_tablelab <- pick_col("tablelabel_2014","species_clean")
col_specietag<- pick_col("specietag","specietagnew","specietag_new","species")

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_answer), !is.na(col_subtype))

# -------- Filter to Sec 2, Q22 and standardize --------
d0 <- tibble(
  Region_raw = raw[[col_region]],
  Country    = str_squish(as.character(raw[[col_country]])),
  Section    = raw[[col_section]],
  Question   = raw[[col_question]],
  Answer0    = str_to_lower(str_squish(as.character(raw[[col_answer]]))),
  Element0   = str_squish(as.character(raw[[col_subtype]])),
  BreedType0 = if (!is.na(col_breed))    str_to_lower(str_squish(as.character(raw[[col_breed]])))    else NA_character_,
  TableLab0  = if (!is.na(col_tablelab)) str_to_lower(str_squish(as.character(raw[[col_tablelab]]))) else NA_character_,
  SpecieTag0 = if (!is.na(col_specietag))str_squish(as.character(raw[[col_specietag]]))              else NA_character_
) %>%
  filter(Section %in% c(2,"2"),
         grepl("^\\s*22(\\.|$)", as.character(Question))) %>%
  mutate(
    Region = case_when(
      str_detect(Region_raw, regex("^africa$", TRUE)) ~ "Africa",
      str_detect(Region_raw, regex("^asia$", TRUE)) ~ "Asia",
      str_detect(Region_raw, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
      str_detect(Region_raw, regex("^europe", TRUE)) ~ "Europe",
      str_detect(Region_raw, regex("^latin\\s+america|caribbean|central america|south america", TRUE)) ~
        "Latin America and the Carribean",
      str_detect(Region_raw, regex("^north\\s*america", TRUE)) ~ "North America",
      str_detect(Region_raw, regex("^near\\s*east", TRUE)) ~ "Near East",
      str_detect(Region_raw, regex("^world$", TRUE)) ~ "World",
      TRUE ~ NA_character_
    ),
    Element = Element0
  ) %>%
  filter(!is.na(Region), Element %in% elements) %>%
  mutate(
    Species_key = dplyr::coalesce(na_if(BreedType0,""),
                                  na_if(TableLab0,""),
                                  recode(stringr::str_pad(SpecieTag0,3,pad="0"),
                                         !!!code_to_species, .default = NA_character_))
  ) %>%
  filter(!is.na(Species_key), Species_key %in% big7) %>%
  mutate(
    Species = factor(Species_key, levels = big7, labels = species_pretty),
    Region  = factor(Region, levels = region_levels),
    Answer  = ifelse(Answer0 %in% c("yes","y","1","true"), "yes",
                     ifelse(Answer0 %in% c("no","n","0","false"), "no", NA_character_))
  ) %>%
  filter(!is.na(Answer)) %>%
  filter(Region != "World")   # rebuild World from regions

# -------- Collapse duplicates at Country level (any YES overrides NO) --------
d_country <- d0 %>%
  mutate(is_yes = Answer == "yes") %>%
  group_by(Region, Country, Species, Element) %>%
  summarise(YES = any(is_yes, na.rm = TRUE), .groups = "drop")

# -------- Helper: species-by-element matrix + Average across species --------
build_matrix <- function(regions = NULL) {
  if (is.null(regions)) {
    # GLOBAL: denominators by Species = distinct countries across all regions
    den <- d_country %>%
      group_by(Species) %>% summarise(N_countries = n_distinct(Country), .groups = "drop")
    num_yes <- d_country %>%
      group_by(Species, Element) %>%
      summarise(n_yes = sum(YES, na.rm = TRUE), .groups = "drop")
  } else {
    den <- d_country %>%
      filter(as.character(Region) %in% regions) %>%
      group_by(Species) %>% summarise(N_countries = n_distinct(Country), .groups = "drop")
    num_yes <- d_country %>%
      filter(as.character(Region) %in% regions) %>%
      group_by(Species, Element) %>%
      summarise(n_yes = sum(YES, na.rm = TRUE), .groups = "drop")
  }

  mat <- expand_grid(Element = elements, Species = levels(d_country$Species)) %>%
    left_join(num_yes, by = c("Element","Species")) %>%
    mutate(n_yes = replace_na(n_yes, 0)) %>%
    left_join(den, by = "Species") %>%
    mutate(pct_yes = ifelse(N_countries > 0, 100 * n_yes / N_countries, NA_real_)) %>%
    arrange(Element, Species)

  wide <- mat %>%
    select(Element, Species, pct_yes) %>%
    pivot_wider(names_from = Species, values_from = pct_yes) %>%
    mutate(`Average across species` = rowMeans(across(all_of(species_pretty)), na.rm = TRUE)) %>%
    select(Element, `Average across species`, all_of(species_pretty))

  list(wide = wide, long = mat, den = den)
}

# -------- Build column for each region (Average across species only) --------
# World column
world_col <- build_matrix(NULL)$wide %>%
  transmute(Element, World = `Average across species`)

# Region columns
get_region_avg_col <- function(rg){
  build_matrix(rg)$wide %>% transmute(Element, !!rg := `Average across species`)
}
reg_cols <- lapply(region_levels[region_levels != "World"], get_region_avg_col)

# Combine to final table and round to integers
final_tbl <- Reduce(function(a,b) left_join(a,b, by = "Element"),
                    c(list(world_col), reg_cols)) %>%
  mutate(across(-Element, ~ round(.x, 0))) %>%
  select(Element, all_of(region_display))  # World first, then regions

# -------- QA: keep a minimal long table if you want to double-check --------
qa_long <- d_country %>%
  group_by(Region, Species, Element) %>%
  summarise(n_yes = sum(YES), n_no = sum(!YES),
            .groups = "drop")

# -------- Write Excel --------
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "Region_averages")     # main table wanted
addWorksheet(wb, "QA_long")

readme <- paste(
  "Q22 (Section 2): Proportion of answers reporting the use of each in situ element by region.",
  "Computed as the AVERAGE across the 7 Big-Five species of the species-level percentages.",
  "Species-level percentage for a region and element =",
  "  (# countries in that region with YES for the element for that species)",
  "  ÷ (# countries in that region that reported that species, any element) × 100.",
  "Duplicates at country level are resolved as any-YES per Species×Element.",
  "World column is computed analogously across all regions.",
  sep = "\n"
)

writeData(wb, "README", readme)
writeData(wb, "Region_averages", final_tbl)
writeData(wb, "QA_long", qa_long)

# Styling
bold <- createStyle(textDecoration = "bold")
int0 <- createStyle(numFmt = "0")

for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:100, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:100, widths = "auto")
}
df <- readWorkbook(wb, "Region_averages")
num_cols <- which(vapply(df, is.numeric, TRUE))
if (length(num_cols)) {
  addStyle(wb, "Region_averages", int0,
           rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved Excel: ", out_xlsx)





Figure 3D3
# ==============================================================
# Sec.2, Q22 — In situ conservation *elements* by REGION
# Using sector instead of species: Public sector / Private sector.
# We produce percentages of countries answering "yes" for each element:
#   1) Public OR Private (combined)
#   2) Public only
#   3) Private only
# Figures (PNG+PDF) are created per region (incl. World).
# Excel holds underlying tables.
# ==============================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr); library(tidyr)
  library(forcats); library(ggplot2); library(openxlsx)
})

# ----------- Paths (change if needed) -------------------------
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3B1_Q22_Sector_elements.xlsx"
png_dir  <- "C:/Users/LENOVO/Documents/Q22_Sector_Figures_PNG"
pdf_dir  <- "C:/Users/LENOVO/Documents/Q22_Sector_Figures_PDF"
dir.create(png_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(pdf_dir, showWarnings = FALSE, recursive = TRUE)

# ----------- Regions (2024 order) -----------------------------
region_levels <- c("Africa","Asia","Southwest Pacific","Europe",
                   "Latin America and the Carribean","North America",
                   "Near East","World")

# Short safe sheet names for per-region tabs
region_sheetname <- c(
  "Africa"                        = "Region_Africa",
  "Asia"                          = "Region_Asia",
  "Southwest Pacific"             = "Region_SW_Pacific",
  "Europe"                        = "Region_Europe",
  "Latin America and the Carribean" = "Region_LAC",
  "North America"                 = "Region_NA",
  "Near East"                     = "Region_Near_East",
  "World"                         = "Region_World"
)

# ----------- Elements (canonical order for plots & tables) ----
elements_order <- c(
  # Increase demand of breed products & services
  "Promotion of niche marketing or other market differentiation",
  "Promotion of at-risk breeds as tourist attractions",
  "Use of at-risk breeds in the management of wildlife habitats and landscape",
  "Promotion of breed-related cultural activities",
  # Incentivization and support of livestock keepers
  "Incentive or subsidy payment schemes for keeping at-risk breeds",
  "Recognition/award programmes for breeders",
  "Extension programmes to improve the management of at-risk breeds",
  "Awareness-raising activities providing information on the potential of specific at-risk breeds",
  # Breeding programmes
  "Conservation breeding programmes",
  "Selection programmes for increased production or productivity in at-risk breeds",
  # Community-level participation & empowerment
  "Community-based conservation programmes",
  "Development of biocultural community protocols"
)

# ----------- Grouping labels and colours ----------------------
group_levels <- c("Public or private sector","Public sector","Private sector")
group_cols   <- c("Public or private sector" = "#3B6FB6",  # blue
                  "Public sector"            = "#7FBF3F",  # green
                  "Private sector"           = "#D04F4F")  # red

# ----------- Load & normalize headers -------------------------
raw <- read_excel(in_path)
names(raw) <- names(raw) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+", "_") |>
  tolower()

pick_col <- function(...) {
  cands <- unlist(list(...))
  hit <- cands[cands %in% names(raw)]
  if (length(hit)) hit[1] else NA_character_
}

col_region  <- pick_col("region")
col_country <- pick_col("country")
col_section <- pick_col("section")
col_q       <- pick_col("question_2024","question")
col_subq    <- pick_col("subquestiontype_2014") # text of the element
col_answer  <- pick_col("answer")
col_sector  <- pick_col("breedtype_2014")       # "Public sector" / "Private sector"

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_q), !is.na(col_subq), !is.na(col_answer),
          !is.na(col_sector))

d0 <- tibble(
  Region_raw = raw[[col_region]],
  Country    = str_squish(as.character(raw[[col_country]])),
  Section    = raw[[col_section]],
  Question   = raw[[col_q]],
  Answer     = str_to_lower(str_squish(as.character(raw[[col_answer]]))),
  Sector0    = str_squish(as.character(raw[[col_sector]])),
  Element0   = str_squish(as.character(raw[[col_subq]]))
) %>%
  # Keep Section 2, Q22 (accept "22" or "22.xx")
  filter(Section %in% c(2,"2"),
         grepl("^\\s*22(\\.|$)", as.character(Question))) %>%
  # Normalize region names to your 2024 list; drop if not mapped
  mutate(
    Region = case_when(
      str_detect(Region_raw, regex("^africa$", TRUE)) ~ "Africa",
      str_detect(Region_raw, regex("^asia$", TRUE)) ~ "Asia",
      str_detect(Region_raw, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
      str_detect(Region_raw, regex("^europe", TRUE)) ~ "Europe",
      str_detect(Region_raw, regex("^latin\\s+america|caribbean|central america|south america", TRUE)) ~
        "Latin America and the Carribean",
      str_detect(Region_raw, regex("^north\\s*america", TRUE)) ~ "North America",
      str_detect(Region_raw, regex("^near\\s*east", TRUE)) ~ "Near East",
      str_detect(Region_raw, regex("^world$", TRUE)) ~ "World",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Region)) %>%
  # Keep only Public/Private sector
  mutate(Sector = case_when(
    str_detect(tolower(Sector0), "public")  ~ "Public sector",
    str_detect(tolower(Sector0), "private") ~ "Private sector",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Sector)) %>%
  # Canonicalise element text to exactly our 12 labels (tolerant matching)
  mutate(Element = case_when(
    str_detect(Element0, regex("^promotion of niche marketing", TRUE)) ~ elements_order[1],
    str_detect(Element0, regex("^promotion of at-?risk breeds as tourist", TRUE)) ~ elements_order[2],
    str_detect(Element0, regex("^use of at-?risk breeds.*wildlife habitats", TRUE)) ~ elements_order[3],
    str_detect(Element0, regex("^promotion of breed-?related cultural", TRUE)) ~ elements_order[4],
    str_detect(Element0, regex("^incentive.*keeping at-?risk breeds", TRUE)) ~ elements_order[5],
    str_detect(Element0, regex("^recognition/award programmes", TRUE)) ~ elements_order[6],
    str_detect(Element0, regex("^extension programmes.*management of at-?risk breeds", TRUE)) ~ elements_order[7],
    str_detect(Element0, regex("^awareness-raising activities", TRUE)) ~ elements_order[8],
    str_detect(Element0, regex("^conservation breeding programmes", TRUE)) ~ elements_order[9],
    str_detect(Element0, regex("^selection programmes.*productivity in at-?risk breeds", TRUE)) ~ elements_order[10],
    str_detect(Element0, regex("^community-?based conservation programmes", TRUE)) ~ elements_order[11],
    str_detect(Element0, regex("^development of biocultural community protocols", TRUE)) ~ elements_order[12],
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Element)) %>%
  mutate(
    Region  = factor(Region, levels = region_levels[region_levels != "World"]), # we'll add World later
    Element = factor(Element, levels = elements_order),
    is_yes  = Answer %in% c("yes","y","1","true")
  )

# ------------- Denominators per Region × Element --------------
den_reg_elem <- d0 %>%
  group_by(Region, Element) %>%
  summarise(N_countries = n_distinct(Country), .groups = "drop")

# ------------- Country-level "presence" for each group ---------
# For each Region × Country × Element, decide YES/NO at the *country* level:
# - Combined: any sector has_yes
# - Public:   yes in Public sector
# - Private:  yes in Private sector
by_country <- d0 %>%
  group_by(Region, Country, Element, Sector) %>%
  summarise(has_yes = any(is_yes, na.rm = TRUE), .groups = "drop") %>%
  # reshape to wide so we can compute combined easily
  tidyr::pivot_wider(names_from = Sector, values_from = has_yes, values_fill = FALSE) %>%
  mutate(`Public or private sector` = `Public sector` | `Private sector`) %>%
  tidyr::pivot_longer(cols = c(`Public or private sector`,`Public sector`,`Private sector`),
                      names_to = "Group", values_to = "has_yes") %>%
  mutate(Group = factor(Group, levels = group_levels))

# ------------- Numerators per Region × Element × Group ---------
num_reg_elem <- by_country %>%
  group_by(Region, Element, Group) %>%
  summarise(n_yes = sum(has_yes, na.rm = TRUE), .groups = "drop")

# ------------- Merge with denominators & build WORLD ----------
# (Ignore any 'World' rows from the source; we rebuild it)
pct_reg <- num_reg_elem %>%
  left_join(den_reg_elem, by = c("Region","Element")) %>%
  mutate(pct = ifelse(N_countries > 0, 100 * n_yes / N_countries, NA_real_))

# Build WORLD row from regional sums
world_den <- den_reg_elem %>%
  group_by(Element) %>% summarise(N_countries = sum(N_countries), .groups = "drop") %>%
  mutate(Region = factor("World", levels = "World"))

world_num <- num_reg_elem %>%
  group_by(Element, Group) %>% summarise(n_yes = sum(n_yes), .groups = "drop") %>%
  mutate(Region = factor("World", levels = "World"))

pct_world <- world_num %>%
  left_join(world_den, by = c("Region","Element")) %>%
  mutate(pct = ifelse(N_countries > 0, 100 * n_yes / N_countries, NA_real_))

# Final long table (Region order with World last)
pct_long <- bind_rows(
  pct_reg %>% mutate(Region = factor(as.character(Region), levels = region_levels)),
  pct_world %>% mutate(Region = factor(as.character(Region), levels = region_levels))
) %>%
  arrange(Region, Element, Group)

# ------------- WIDE matrices (one per grouping) ---------------
make_wide <- function(g){
  pct_long %>%
    filter(Group == g) %>%
    select(Element, Region, pct) %>%
    tidyr::pivot_wider(names_from = Region, values_from = pct) %>%
    arrange(factor(Element, levels = elements_order))
}
wide_combined <- make_wide("Public or private sector")
wide_public   <- make_wide("Public sector")
wide_private  <- make_wide("Private sector")

# ------------- Per-region tables (Element × three groups) -----
region_tabs <- lapply(region_levels, function(rg){
  pct_long %>%
    filter(as.character(Region) == rg) %>%
    select(Element, Group, pct, n_yes, N_countries) %>%
    tidyr::pivot_wider(
      names_from = Group, values_from = pct
    ) %>%
    arrange(factor(Element, levels = elements_order))
})
names(region_tabs) <- region_levels

# ------------- FIGURES (8) ------------------------------------
# Same bar layout for each region; bars = Combined/Public/Private
plot_one_region <- function(rg){
  df <- pct_long %>% filter(as.character(Region) == rg)
  if (nrow(df) == 0) return(NULL)

  ggplot(df, aes(x = Element, y = pct, fill = Group)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(values = group_cols, limits = group_levels, name = NULL, drop = FALSE) +
    scale_y_continuous(limits = c(0, max(60, ceiling(max(df$pct, na.rm = TRUE)/10)*10 + 10)),
                       breaks = seq(0, 100, 25),
                       labels = function(v) paste0(v, "%")) +
    labs(title = rg, x = NULL, y = "Percent of countries") +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 9),
      plot.title = element_text(face = "bold", hjust = 0),
      panel.grid.minor = element_blank()
    )
}

for (rg in region_levels) {
  p <- plot_one_region(rg)
  if (is.null(p)) next
  base <- paste0("Q22_Sector_", gsub("[^A-Za-z0-9]+","_", rg))
  ggsave(file.path(png_dir, paste0(base, ".png")), p, width = 14, height = 8, dpi = 300)
  ggsave(file.path(pdf_dir, paste0(base, ".pdf")), p, width = 14, height = 8)
}

# ------------- EXCEL (underlying tables) ----------------------
wb <- createWorkbook()

addWorksheet(wb, "README")
addWorksheet(wb, "LONG_all")
addWorksheet(wb, "WIDE_Combined")
addWorksheet(wb, "WIDE_Public")
addWorksheet(wb, "WIDE_Private")

readme <- paste(
  "Section 2 – Question 22 (in situ conservation elements) — 2024 regions.",
  "",
  "We compute, for each Region × Element, the percent of countries that answered 'yes'.",
  "Three groupings:",
  "  • Public or private sector  -> a country counts YES if either sector reported YES",
  "  • Public sector             -> only public-sector rows considered",
  "  • Private sector            -> only private-sector rows considered",
  "",
  "Denominator per Region × Element = # distinct countries that provided any row",
  "for that element in the region (across sectors).",
  "World = sum of regions (numerators and denominators).",
  "Regions ordered: Africa, Asia, Southwest Pacific, Europe, Latin America and the Carribean, North America, Near East, World.",
  sep = "\n"
)
writeData(wb, "README", readme)

# Long + three wide sheets
writeData(wb, "LONG_all",
          pct_long %>% mutate(
            Region = as.character(Region),
            Element = as.character(Element),
            Group   = as.character(Group),
            pct     = round(pct, 1)
          ) %>% arrange(Region, Element, Group))

writeData(wb, "WIDE_Combined", wide_combined %>% mutate(across(where(is.numeric), ~round(.x, 1))))
writeData(wb, "WIDE_Public",   wide_public   %>% mutate(across(where(is.numeric), ~round(.x, 1))))
writeData(wb, "WIDE_Private",  wide_private  %>% mutate(across(where(is.numeric), ~round(.x, 1))))

# Per-region tabs (short names to avoid the 31-char Excel limit)
for (rg in region_levels) {
  sh <- region_sheetname[[rg]]
  addWorksheet(wb, sh)
  writeData(wb, sh, region_tabs[[rg]] %>% mutate(across(where(is.numeric), ~round(.x, 1))))
}

# QA tabs: denominators and raw answer counts
addWorksheet(wb, "Denominators")
writeData(wb, "Denominators",
          bind_rows(
            den_reg_elem %>% mutate(Region = as.character(Region)),
            world_den %>% mutate(Region = as.character(Region))
          ) %>% arrange(Region, Element))

addWorksheet(wb, "QA_AnswerCounts")
qa_counts <- d0 %>% count(Element, Sector, Answer, name = "n") %>% arrange(Element, Sector, desc(n))
writeData(wb, "QA_AnswerCounts", qa_counts)

# Styling
bold <- createStyle(textDecoration = "bold")
num1 <- createStyle(numFmt = "0.0")
for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:120, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:120, widths = "auto")
}
for (sh in c("LONG_all","WIDE_Combined","WIDE_Public","WIDE_Private",
             unname(region_sheetname))) {
  df <- readWorkbook(wb, sh)
  if (!is.null(df) && nrow(df)) {
    num_cols <- which(vapply(df, is.numeric, TRUE))
    if (length(num_cols)) {
      addStyle(wb, sh, num1, rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
    }
  }
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)

message("Saved Excel -> ", out_xlsx)
message("Saved PNGs -> ", png_dir)
message("Saved PDFs -> ", pdf_dir)







Figure 3D3
# ==============================================================
# Q22 (Sec 2) – In-situ elements by REGION and OPERATOR
# One Excel table: Region | Number of countries | Operator | 12 elements (% yes)
# Uses "Public sector" / "Private sector" from BreedType_2014
# ==============================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr); library(tidyr)
  library(forcats); library(openxlsx)
})

# ---- paths
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3B1_Q22_Sector_byRegion_table.xlsx"

# ---- regions (2024 order)
region_levels <- c("Africa","Asia","Southwest Pacific","Europe",
                   "Latin America and the Carribean","North America",
                   "Near East","World")

# ---- elements (canonical order)
elements_order <- c(
  "Promotion of niche marketing or other market differentiation",
  "Promotion of at-risk breeds as tourist attractions",
  "Use of at-risk breeds in the management of wildlife habitats and landscape",
  "Promotion of breed-related cultural activities",
  "Incentive or subsidy payment schemes for keeping at-risk breeds",
  "Recognition/award programmes for breeders",
  "Extension programmes to improve the management of at-risk breeds",
  "Awareness-raising activities providing information on the potential of specific at-risk breeds",
  "Conservation breeding programmes",
  "Selection programmes for increased production or productivity in at-risk breeds",
  "Community-based conservation programmes",
  "Development of biocultural community protocols"
)

# ---- operators (row groups)
group_levels <- c("Public or private sector","Public sector","Private sector")

# ---- load & normalise
raw <- read_excel(in_path)
names(raw) <- names(raw) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+","_") |>
  tolower()

pick_col <- function(...) {
  cands <- unlist(list(...))
  hit <- cands[cands %in% names(raw)]
  if (length(hit)) hit[1] else NA_character_
}

col_region  <- pick_col("region")
col_country <- pick_col("country")
col_section <- pick_col("section")
col_q       <- pick_col("question_2024","question")
col_subq    <- pick_col("subquestiontype_2014")
col_answer  <- pick_col("answer")
col_sector  <- pick_col("breedtype_2014")

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_q), !is.na(col_subq), !is.na(col_answer),
          !is.na(col_sector))

d0 <- tibble(
  Region_raw = raw[[col_region]],
  Country    = str_squish(as.character(raw[[col_country]])),
  Section    = raw[[col_section]],
  Question   = raw[[col_q]],
  Answer     = str_to_lower(str_squish(as.character(raw[[col_answer]]))),
  Sector0    = str_squish(as.character(raw[[col_sector]])),
  Element0   = str_squish(as.character(raw[[col_subq]]))
) %>%
  # Section 2, Question 22
  filter(Section %in% c(2,"2"),
         grepl("^\\s*22(\\.|$)", as.character(Question))) %>%
  # Regions → 2024 names, drop if not matched; ignore any 'World' in source
  mutate(
    Region = case_when(
      str_detect(Region_raw, regex("^africa$", TRUE)) ~ "Africa",
      str_detect(Region_raw, regex("^asia$", TRUE)) ~ "Asia",
      str_detect(Region_raw, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
      str_detect(Region_raw, regex("^europe", TRUE)) ~ "Europe",
      str_detect(Region_raw, regex("^latin\\s+america|caribbean|central america|south america", TRUE)) ~
        "Latin America and the Carribean",
      str_detect(Region_raw, regex("^north\\s*america", TRUE)) ~ "North America",
      str_detect(Region_raw, regex("^near\\s*east", TRUE)) ~ "Near East",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Region)) %>%
  # Keep only public/private
  mutate(Sector = case_when(
    str_detect(tolower(Sector0), "public")  ~ "Public sector",
    str_detect(tolower(Sector0), "private") ~ "Private sector",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Sector)) %>%
  # Canonicalise element labels
  mutate(Element = case_when(
    str_detect(Element0, regex("^promotion of niche marketing", TRUE)) ~ elements_order[1],
    str_detect(Element0, regex("^promotion of at-?risk breeds as tourist", TRUE)) ~ elements_order[2],
    str_detect(Element0, regex("^use of at-?risk breeds.*wildlife habitats", TRUE)) ~ elements_order[3],
    str_detect(Element0, regex("^promotion of breed-?related cultural", TRUE)) ~ elements_order[4],
    str_detect(Element0, regex("^incentive.*keeping at-?risk breeds", TRUE)) ~ elements_order[5],
    str_detect(Element0, regex("^recognition/award programmes", TRUE)) ~ elements_order[6],
    str_detect(Element0, regex("^extension programmes.*management of at-?risk breeds", TRUE)) ~ elements_order[7],
    str_detect(Element0, regex("^awareness-raising activities", TRUE)) ~ elements_order[8],
    str_detect(Element0, regex("^conservation breeding programmes", TRUE)) ~ elements_order[9],
    str_detect(Element0, regex("^selection programmes.*productivity in at-?risk breeds", TRUE)) ~ elements_order[10],
    str_detect(Element0, regex("^community-?based conservation programmes", TRUE)) ~ elements_order[11],
    str_detect(Element0, regex("^development of biocultural community protocols", TRUE)) ~ elements_order[12],
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Element)) %>%
  mutate(
    Region  = factor(Region, levels = region_levels[region_levels != "World"]),
    Element = factor(Element, levels = elements_order),
    is_yes  = Answer %in% c("yes","y","1","true")
  )

# ---- denominators per Region × Element
den_reg_elem <- d0 %>%
  group_by(Region, Element) %>%
  summarise(N_countries = n_distinct(Country), .groups = "drop")

# ---- country-level presence for three groups
by_country <- d0 %>%
  group_by(Region, Country, Element, Sector) %>%
  summarise(has_yes = any(is_yes, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Sector, values_from = has_yes, values_fill = FALSE) %>%
  mutate(`Public or private sector` = `Public sector` | `Private sector`) %>%
  tidyr::pivot_longer(cols = c(`Public or private sector`,`Public sector`,`Private sector`),
                      names_to = "Operator", values_to = "has_yes") %>%
  mutate(Operator = factor(Operator, levels = group_levels))

# ---- numerators per Region × Element × Operator
num_reg_elem <- by_country %>%
  group_by(Region, Element, Operator) %>%
  summarise(n_yes = sum(has_yes, na.rm = TRUE), .groups = "drop")

# ---- merge; compute pct; add WORLD
pct_reg <- num_reg_elem %>%
  left_join(den_reg_elem, by = c("Region","Element")) %>%
  mutate(pct = ifelse(N_countries > 0, 100 * n_yes / N_countries, NA_real_))

world_den <- den_reg_elem %>%
  group_by(Element) %>% summarise(N_countries = sum(N_countries), .groups = "drop") %>%
  mutate(Region = factor("World", levels = "World"))

world_num <- num_reg_elem %>%
  group_by(Element, Operator) %>% summarise(n_yes = sum(n_yes), .groups = "drop") %>%
  mutate(Region = factor("World", levels = "World"))

pct_world <- world_num %>%
  left_join(world_den, by = c("Region","Element")) %>%
  mutate(pct = ifelse(N_countries > 0, 100 * n_yes / N_countries, NA_real_))

pct_long <- bind_rows(
  pct_reg %>% mutate(Region = factor(as.character(Region), levels = region_levels)),
  pct_world %>% mutate(Region = factor(as.character(Region), levels = region_levels))
) %>%
  arrange(Region, Operator, Element)

# ---- one region-level "Number of countries" (distinct countries with any Q22 row)
den_any <- d0 %>% group_by(Region) %>% summarise(Number_of_countries = n_distinct(Country), .groups = "drop")
world_any <- tibble(Region = factor("World", levels = "World"),
                    Number_of_countries = sum(den_any$Number_of_countries, na.rm = TRUE))
den_any_all <- bind_rows(
  den_any %>% mutate(Region = factor(as.character(Region), levels = region_levels)),
  world_any
)

# ---- build the single master table
master_table <- pct_long %>%
  select(Region, Operator, Element, pct) %>%
  mutate(Operator = factor(Operator, levels = group_levels)) %>%
  tidyr::pivot_wider(names_from = Element, values_from = pct) %>%
  arrange(Region, Operator) %>%
  left_join(den_any_all, by = "Region") %>%
  relocate(Number_of_countries, .after = Region) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))  # match book-style integers

# ---- extra QA: denominators per Region×Element and by Operator (numerators)
den_QA <- den_reg_elem %>% mutate(Region = as.character(Region)) %>% arrange(Region, Element)
num_QA <- pct_long %>%
  select(Region, Operator, Element, n_yes, N_countries) %>%
  mutate(Region = as.character(Region)) %>%
  arrange(Region, Operator, Element)

# ---- write Excel
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "ByRegion_Operator")
addWorksheet(wb, "Denominators_RxE")
addWorksheet(wb, "Numerators_and_Pcts")

readme <- paste(
  "Q22 (Section 2) — In-situ conservation elements.",
  "This workbook contains a single master table with rows for each 2024 region (plus World) ",
  "repeated for each operator: 'Public or private sector', 'Public sector', 'Private sector'.",
  "",
  "Cells are the PROPORTION (%) of countries that answered 'yes' for the element.",
  "Denominator for each Region × Element = number of distinct countries in that region ",
  "that reported anything for that element (across sectors).",
  "World = sum across regions (both numerators and denominators).",
  "The 'Number_of_countries' column is the count of distinct countries in the region ",
  "with any Q22 record (single number reused for all three operator rows).",
  sep = "\n"
)
writeData(wb, "README", readme)
writeData(wb, "ByRegion_Operator", master_table)
writeData(wb, "Denominators_RxE", den_QA)
writeData(wb, "Numerators_and_Pcts",
          pct_long %>% mutate(pct = round(pct, 1)) %>% arrange(Region, Operator, Element))

# Styling
bold <- createStyle(textDecoration = "bold")
int0 <- createStyle(numFmt = "0")
for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:200, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:200, widths = "auto")
}
# integer style for percentages in the master table
df <- readWorkbook(wb, "ByRegion_Operator")
if (!is.null(df) && nrow(df)) {
  num_cols <- which(vapply(df, is.numeric, TRUE))
  if (length(num_cols)) addStyle(wb, "ByRegion_Operator", int0,
                                 rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved Excel -> ", out_xlsx)









With othe figures for Fig 3D3
# ==============================================================
# Q22 (Sec 2): In situ conservation elements by operator
# Figures per region (PNG, PDF, 600-dpi JPEG, WMF) + Excel tables
# ==============================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr); library(tidyr)
  library(forcats); library(ggplot2); library(openxlsx)
})

# ----------------------- Paths -----------------------
infile   <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_root <- "C:/Users/LENOVO/Documents"

out_png  <- file.path(out_root, "Q22_Sector_Figs_PNG")
out_pdf  <- file.path(out_root, "Q22_Sector_Figs_PDF")
out_jpg  <- file.path(out_root, "Q22_Sector_Figs_JPEG")
out_wmf  <- file.path(out_root, "Q22_Sector_Figs_WMF")
dir.create(out_png, showWarnings = FALSE, recursive = TRUE)
dir.create(out_pdf, showWarnings = FALSE, recursive = TRUE)
dir.create(out_jpg, showWarnings = FALSE, recursive = TRUE)
dir.create(out_wmf, showWarnings = FALSE, recursive = TRUE)

out_xlsx <- file.path(out_root, "Fig3B1_Q22_Sector_Elements_Tables.xlsx")

# ----------------------- Constants -----------------------
region_levels <- c(
  "Africa","Asia","Southwest Pacific","Europe",
  "Latin America and the Carribean","North America","Near East","World"
)

elements_std <- c(
  "Promotion of niche marketing or other market differentiation",
  "Promotion of at-risk breeds as tourist attractions",
  "Use of at-risk breeds in the management of wildlife habitats and landscapes",
  "Promotion of breed-related cultural activities",
  "Incentive or subsidy payment schemes for keeping at-risk breeds",
  "Recognition/award programmes for breeders",
  "Extension programmes to improve the management of at-risk breeds",
  "Awareness-raising activities providing information on the potential of specific at-risk breeds",
  "Conservation breeding programmes",
  "Selection programmes for increased production or productivity in at-risk breeds",
  "Community-based conservation programmes",
  "Development of biocultural community protocols"
)

series_levels <- c("Private & Public","Private","Public")
series_colors <- c("Private & Public"="#3178C6","Private"="#C04B4B","Public"="#6BB24A")

# ----------------------- Helpers -----------------------
pick_col <- function(df, ...) {
  cands <- unlist(list(...)); nm <- names(df); lo <- tolower(nm)
  for (c in tolower(cands)) { hit <- which(lo == c); if (length(hit)) return(nm[hit[1]]) }
  NA_character_
}

norm_region <- function(x){
  y <- str_squish(as.character(x))
  y <- ifelse(grepl("^south\\s*-?west\\s*pacific$", y, TRUE), "Southwest Pacific", y)
  y <- ifelse(grepl("^latin\\s+america|caribbean|central america|south america", y, TRUE),
              "Latin America and the Carribean", y)
  y <- ifelse(grepl("^near\\s*east", y, TRUE), "Near East", y)
  y <- ifelse(grepl("^europe", y, TRUE), "Europe", y)
  y
}

canon_element <- function(x){
  a <- str_squish(as.character(x))
  case_when(
    str_detect(a, regex("^promotion of niche marketing", TRUE)) ~ elements_std[1],
    str_detect(a, regex("^promotion of at-?risk breeds.*tourist", TRUE)) ~ elements_std[2],
    str_detect(a, regex("^use of at-?risk breeds.*wildlife habitats? and landscape", TRUE)) ~ elements_std[3],
    str_detect(a, regex("^promotion of breed-?related cultural", TRUE)) ~ elements_std[4],
    str_detect(a, regex("^incentive.*keeping at-?risk breeds", TRUE)) ~ elements_std[5],
    str_detect(a, regex("^recognition/?award programmes", TRUE)) ~ elements_std[6],
    str_detect(a, regex("^extension programmes.*management of at-?risk breeds", TRUE)) ~ elements_std[7],
    str_detect(a, regex("^awareness-raising activities", TRUE)) ~ elements_std[8],
    str_detect(a, regex("^conservation breeding programmes", TRUE)) ~ elements_std[9],
    str_detect(a, regex("^selection programmes.*productivity in at-?risk breeds", TRUE)) ~ elements_std[10],
    str_detect(a, regex("^community-?based conservation programmes", TRUE)) ~ elements_std[11],
    str_detect(a, regex("^development of biocultural community protocols", TRUE)) ~ elements_std[12],
    TRUE ~ NA_character_
  )
}

short_sheet_name <- function(reg){
  recode(reg,
         "Latin America and the Carribean"="LAC",
         "Southwest Pacific"="SW_Pacific",
         .default = reg) |>
    gsub("[^A-Za-z0-9]+","_", x = _)
}

# WMF saver (Windows only)
save_wmf <- function(filename, plot, width_in=14, height_in=8){
  if (.Platform$OS.type == "windows") {
    dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
    grDevices::win.metafile(filename, width = width_in, height = height_in)
    print(plot)
    grDevices::dev.off()
  } else {
    warning("WMF export skipped (non-Windows): ", filename)
  }
}

# High-quality JPEG device for ggsave (ggsave won’t pass dpi here)
jpeg100 <- function(filename, width, height, units, ...) {
  grDevices::jpeg(filename = filename, width = width, height = height,
                  units = units, res = 600, quality = 100)
}

# ----------------------- Load & filter Q22 -----------------------
d0 <- read_excel(infile)

col_region   <- pick_col(d0, "Region")
col_country  <- pick_col(d0, "Country")
col_section  <- pick_col(d0, "Section")
col_question <- pick_col(d0, "Question_2024","Question")
col_answer   <- pick_col(d0, "Answer")
col_sector   <- pick_col(d0, "BreedType_2014")
col_element  <- pick_col(d0, "SubquestionType_2014")

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_answer),
          !is.na(col_sector),  !is.na(col_element))

d <- tibble(
  Region  = norm_region(d0[[col_region]]),
  Country = str_squish(as.character(d0[[col_country]])),
  Section = d0[[col_section]],
  Question= d0[[col_question]],
  Answer  = str_to_lower(str_squish(as.character(d0[[col_answer]]))),
  Sector0 = str_squish(as.character(d0[[col_sector]])),
  Element0= d0[[col_element]]
) |>
  filter(Section %in% c(2,"2"),
         grepl("^\\s*22(\\.|$)", as.character(Question)),
         !is.na(Region), !is.na(Country), !is.na(Element0)) |>
  mutate(
    Region  = factor(Region, levels = region_levels[region_levels != "World"]),
    Sector  = case_when(
      str_detect(tolower(Sector0), "public")  ~ "Public",
      str_detect(tolower(Sector0), "private") ~ "Private",
      TRUE ~ NA_character_
    ),
    Element = canon_element(Element0)
  ) |>
  filter(!is.na(Sector), !is.na(Element)) |>
  mutate(Element = factor(Element, levels = elements_std))

if (nrow(d) == 0) stop("No Q22 rows after filtering — check the sheet values/columns.")

# ----------------------- Percentages -----------------------
# Denominator per Region × Element = distinct countries with ANY row for that element (any sector)
den <- d |>
  distinct(Region, Country, Element) |>
  count(Region, Element, name = "N_countries")

# Country-level presence flags
country_flags <- d |>
  mutate(is_yes = Answer %in% c("yes","y","1","true")) |>
  group_by(Region, Country, Element) |>
  summarise(
    `Private & Public` = any(is_yes),                    # either sector
    `Private`          = any(is_yes & Sector == "Private"),
    `Public`           = any(is_yes & Sector == "Public"),
    .groups = "drop"
  )

reg_numer <- country_flags |>
  pivot_longer(cols = all_of(series_levels), names_to = "Series", values_to = "present") |>
  group_by(Region, Element, Series) |>
  summarise(n_present = sum(present, na.rm = TRUE), .groups = "drop")

reg_pct <- reg_numer |>
  left_join(den, by = c("Region","Element")) |>
  mutate(pct = ifelse(N_countries > 0, 100*n_present/N_countries, NA_real_))

# WORLD row (sum across regions)
world_den <- den |>
  group_by(Element) |>
  summarise(N_countries = sum(N_countries, na.rm = TRUE), .groups = "drop") |>
  mutate(Region = factor("World", levels = "World"))

world_numer <- reg_numer |>
  group_by(Element, Series) |>
  summarise(n_present = sum(n_present, na.rm = TRUE), .groups = "drop") |>
  mutate(Region = factor("World", levels = "World"))

world_pct <- world_numer |>
  left_join(world_den, by = c("Region","Element")) |>
  mutate(pct = ifelse(N_countries > 0, 100*n_present/N_countries, NA_real_))

pct_all <- bind_rows(
  reg_pct |> mutate(Region = factor(as.character(Region), levels = region_levels)),
  world_pct |> mutate(Region = factor(as.character(Region), levels = region_levels))
) |>
  mutate(
    Series  = factor(Series, levels = series_levels),
    Element = factor(Element, levels = elements_std)
  ) |>
  arrange(Region, Element, Series)

# ----------------------- Excel -----------------------
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "LONG_data")

readme <- paste(
  "Q22: In situ conservation elements by operator (Private & Public / Private / Public).",
  "Denominator per Region × Element = # distinct countries with ANY row for that element.",
  "World = sum of region numerators and denominators.",
  "Regions (2024): Africa, Asia, Southwest Pacific, Europe, Latin America and the Carribean, North America, Near East, World.",
  sep="\n"
)
writeData(wb, "README", readme)

writeData(wb, "LONG_data",
          pct_all |> select(Region, Element, Series, n_present, N_countries, pct))

for (rg in region_levels) {
  tab <- pct_all |>
    filter(as.character(Region) == rg) |>
    select(Element, Series, pct, N_countries) |>
    pivot_wider(names_from = Series, values_from = pct) |>
    relocate(N_countries, .after = Element) |>
    arrange(Element)
  sh <- paste0("Region_", short_sheet_name(rg))
  addWorksheet(wb, sh)
  writeData(wb, sh, tab)
}

bold <- createStyle(textDecoration = "bold"); int0 <- createStyle(numFmt = "0")
for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:100, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:100, widths = "auto")
  df <- tryCatch(readWorkbook(wb, sh), error = function(e) NULL)
  if (!is.null(df) && nrow(df)) {
    num_cols <- which(vapply(df, is.numeric, TRUE))
    if (length(num_cols)) addStyle(wb, sh, int0, rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
  }
}

ok <- TRUE
tryCatch(saveWorkbook(wb, out_xlsx, overwrite = TRUE),
         error = function(e){
           ok <<- FALSE
           ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
           alt <- file.path(out_root, paste0("Fig3B1_Q22_Sector_Elements_Tables_", ts, ".xlsx"))
           saveWorkbook(wb, alt, overwrite = TRUE)
           message("Primary Excel name locked; saved instead -> ", alt)
         })
if (ok) message("Excel tables written -> ", out_xlsx)

# ----------------------- Plot helper -----------------------
make_plot_for_region <- function(reg_name){
  df <- pct_all |>
    filter(as.character(Region) == reg_name) |>
    mutate(Element = factor(Element, levels = elements_std),
           Series  = factor(Series,  levels = series_levels))

  ggplot(df, aes(x = Element, y = pct, fill = Series)) +
    geom_col(position = position_dodge(width = 0.74), width = 0.68) +
    scale_fill_manual(values = series_colors, breaks = series_levels, name = NULL) +
    scale_y_continuous(
      limits = c(0, max(60, ceiling(max(df$pct, na.rm = TRUE)/10)*10)),
      breaks = seq(0, 100, 10),
      labels = function(v) paste0(v, "%"),
      expand = expansion(mult = c(0.02, 0.07))
    ) +
    labs(x = NULL, y = NULL, title = reg_name) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 9),
      panel.grid.major.x = element_line(linetype = "dotted"),
      legend.position = "bottom",
      legend.margin = margin(t = 6),
      legend.box.margin = margin(t = 2)
    )
}

# ----------------------- Save figures (PNG, PDF, JPEG, WMF) -----------------------
for (rg in region_levels) {
  p <- make_plot_for_region(rg)
  base <- paste0("Q22_Elements_bySector_", gsub("[^A-Za-z0-9]+","_", rg))

  ggsave(file.path(out_png, paste0(base, ".png")), p,
         width = 14, height = 8, dpi = 300, units = "in")

  ggsave(file.path(out_pdf, paste0(base, ".pdf")), p,
         width = 14, height = 8, units = "in")

  # 600-dpi, quality=100 JPEG (custom device ignores dpi argument)
  ggsave(file.path(out_jpg, paste0(base, ".jpg")), p,
         width = 14, height = 8, units = "in", device = jpeg100)

  # Windows Metafile (vector; Windows only)
  save_wmf(file.path(out_wmf, paste0(base, ".wmf")), p, width_in = 14, height_in = 8)

  message("Saved: ", rg)
}

message("\nAll figures saved to:\n  PNG  -> ", out_png,
        "\n  PDF  -> ", out_pdf,
        "\n  JPEG -> ", out_jpg,
        "\n  WMF  -> ", out_wmf)




WITH LEGEND INCLUDED
# ==============================================================
# Q22 (Sec 2) — In-situ elements by operator WITH BOTTOM LEGEND
# Figures per region (incl. World):
#   Legend order: "Private & public", "Private", "Public"
#   Bars show % of countries answering YES for each element
# Saves: PNG + PDF to Documents; also writes the figure data to Excel
# ==============================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr); library(tidyr)
  library(forcats); library(ggplot2); library(openxlsx)
})

# ---- paths
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_dir_png <- "C:/Users/LENOVO/Documents/Q22_Sector_Figs_PNG"
out_dir_pdf <- "C:/Users/LENOVO/Documents/Q22_Sector_Figs_PDF"
out_xlsx    <- "C:/Users/LENOVO/Documents/Fig3B1_Q22_Sector_FigData.xlsx"
dir.create(out_dir_png, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir_pdf, showWarnings = FALSE, recursive = TRUE)

# ---- regions (2024 order)
region_levels <- c("Africa","Asia","Southwest Pacific","Europe",
                   "Latin America and the Carribean","North America",
                   "Near East","World")

# ---- elements (canonical order)
elements_order <- c(
  "Promotion of niche marketing or other market differentiation",
  "Promotion of at-risk breeds as tourist attractions",
  "Use of at-risk breeds in the management of wildlife habitats and landscape",
  "Promotion of breed-related cultural activities",
  "Incentive or subsidy payment schemes for keeping at-risk breeds",
  "Recognition/award programmes for breeders",
  "Extension programmes to improve the management of at-risk breeds",
  "Awareness-raising activities providing information on the potential of specific at-risk breeds",
  "Conservation breeding programmes",
  "Selection programmes for increased production or productivity in at-risk breeds",
  "Community-based conservation programmes",
  "Development of biocultural community protocols"
)

# ---- operator display labels (in your requested order)
op_levels_display <- c("Private & public","Private","Public")
op_palette <- c("Private & public" = "#2F6FB6",   # blue
                "Private"           = "#6CBC5A",   # green
                "Public"            = "#D95F02")   # orange/red

# ---- load & normalise
raw <- read_excel(in_path)
names(raw) <- names(raw) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+","_") |>
  tolower()

pick <- function(...) { x <- unlist(list(...)); x[x %in% names(raw)][1] }

col_region  <- pick("region")
col_country <- pick("country")
col_section <- pick("section")
col_q       <- pick("question_2024","question")
col_subq    <- pick("subquestiontype_2014")
col_answer  <- pick("answer")
col_sector  <- pick("breedtype_2014")

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_q), !is.na(col_subq), !is.na(col_answer),
          !is.na(col_sector))

d0 <- tibble(
  Region_raw = raw[[col_region]],
  Country    = str_squish(as.character(raw[[col_country]])),
  Section    = raw[[col_section]],
  Question   = raw[[col_q]],
  Answer     = str_to_lower(str_squish(as.character(raw[[col_answer]]))),
  Sector0    = str_squish(as.character(raw[[col_sector]])),
  Element0   = str_squish(as.character(raw[[col_subq]]))
) %>%
  # Section 2, Question 22
  filter(Section %in% c(2,"2"),
         grepl("^\\s*22(\\.|$)", as.character(Question))) %>%
  # region names (ignore any 'World' in source; we'll rebuild World)
  mutate(
    Region = case_when(
      str_detect(Region_raw, regex("^africa$", TRUE)) ~ "Africa",
      str_detect(Region_raw, regex("^asia$", TRUE)) ~ "Asia",
      str_detect(Region_raw, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
      str_detect(Region_raw, regex("^europe", TRUE)) ~ "Europe",
      str_detect(Region_raw, regex("^latin\\s+america|caribbean|central america|south america", TRUE)) ~
        "Latin America and the Carribean",
      str_detect(Region_raw, regex("^north\\s*america", TRUE)) ~ "North America",
      str_detect(Region_raw, regex("^near\\s*east", TRUE)) ~ "Near East",
      TRUE ~ NA_character_
    )
  ) %>% filter(!is.na(Region)) %>%
  # sectors → public/private only
  mutate(Sector = case_when(
    str_detect(tolower(Sector0), "public")  ~ "Public sector",
    str_detect(tolower(Sector0), "private") ~ "Private sector",
    TRUE ~ NA_character_
  )) %>% filter(!is.na(Sector)) %>%
  # element canonicalisation
  mutate(Element = case_when(
    str_detect(Element0, regex("^promotion of niche marketing", TRUE)) ~ elements_order[1],
    str_detect(Element0, regex("^promotion of at-?risk breeds as tourist", TRUE)) ~ elements_order[2],
    str_detect(Element0, regex("^use of at-?risk breeds.*wildlife habitats", TRUE)) ~ elements_order[3],
    str_detect(Element0, regex("^promotion of breed-?related cultural", TRUE)) ~ elements_order[4],
    str_detect(Element0, regex("^incentive.*keeping at-?risk breeds", TRUE)) ~ elements_order[5],
    str_detect(Element0, regex("^recognition/award programmes", TRUE)) ~ elements_order[6],
    str_detect(Element0, regex("^extension programmes.*management of at-?risk breeds", TRUE)) ~ elements_order[7],
    str_detect(Element0, regex("^awareness-raising activities", TRUE)) ~ elements_order[8],
    str_detect(Element0, regex("^conservation breeding programmes", TRUE)) ~ elements_order[9],
    str_detect(Element0, regex("^selection programmes.*productivity in at-?risk breeds", TRUE)) ~ elements_order[10],
    str_detect(Element0, regex("^community-?based conservation programmes", TRUE)) ~ elements_order[11],
    str_detect(Element0, regex("^development of biocultural community protocols", TRUE)) ~ elements_order[12],
    TRUE ~ NA_character_
  )) %>% filter(!is.na(Element)) %>%
  mutate(
    Region  = factor(Region,  levels = region_levels[region_levels != "World"]),
    Element = factor(Element, levels = elements_order),
    is_yes  = Answer %in% c("yes","y","1","true")
  )

# ---- denominators per Region × Element
den_RxE <- d0 %>%
  group_by(Region, Element) %>%
  summarise(N_countries = n_distinct(Country), .groups = "drop")

# ---- country presence, then three operator groups
by_cty <- d0 %>%
  group_by(Region, Country, Element, Sector) %>%
  summarise(has_yes = any(is_yes, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Sector, values_from = has_yes, values_fill = FALSE) %>%
  mutate(`Private & public` = `Private sector` | `Public sector`,
         Private           = `Private sector`,
         Public            = `Public sector`) %>%
  select(Region, Country, Element, `Private & public`, Private, Public) %>%
  tidyr::pivot_longer(cols = c(`Private & public`, Private, Public),
                      names_to = "Operator_disp", values_to = "has_yes") %>%
  mutate(Operator_disp = factor(Operator_disp, levels = op_levels_display))

# ---- numerators per Region × Element × Operator
num_RxE <- by_cty %>%
  group_by(Region, Element, Operator_disp) %>%
  summarise(n_yes = sum(has_yes, na.rm = TRUE), .groups = "drop")

# ---- percentages (Region) + WORLD
pct_R <- num_RxE %>%
  left_join(den_RxE, by = c("Region","Element")) %>%
  mutate(pct = ifelse(N_countries > 0, 100 * n_yes / N_countries, NA_real_))

world_den <- den_RxE %>%
  group_by(Element) %>% summarise(N_countries = sum(N_countries), .groups = "drop") %>%
  mutate(Region = factor("World", levels = "World"))

world_num <- num_RxE %>%
  group_by(Element, Operator_disp) %>% summarise(n_yes = sum(n_yes), .groups = "drop") %>%
  mutate(Region = factor("World", levels = "World"))

pct_W <- world_num %>%
  left_join(world_den, by = c("Region","Element")) %>%
  mutate(pct = ifelse(N_countries > 0, 100 * n_yes / N_countries, NA_real_))

plot_data <- bind_rows(
  pct_R %>% mutate(Region = factor(as.character(Region), levels = region_levels)),
  pct_W %>% mutate(Region = factor(as.character(Region), levels = region_levels))
) %>%
  # Fill missing combos with zeros so the stacks are visually complete
  complete(Region, Element, Operator_disp = factor(op_levels_display, levels = op_levels_display),
           fill = list(n_yes = 0, pct = 0, N_countries = NA_real_)) %>%
  arrange(Region, Operator_disp, Element)

# ---- helper: wrapped labels for long element names
wrap_lab <- function(x, width = 26) stringr::str_wrap(x, width = width)

# ---- make & save one figure per region
make_one <- function(rg){
  df <- plot_data %>% filter(as.character(Region) == rg)
  if (nrow(df) == 0) return(invisible(NULL))

  p <- ggplot(df, aes(x = Element, y = pct, fill = Operator_disp)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.72) +
    scale_fill_manual(values = op_palette, breaks = op_levels_display, name = NULL) +
    scale_y_continuous(limits = c(0, 100),
                       breaks = c(0, 25, 50, 75, 100),
                       labels = function(v) paste0(v, "%")) +
    labs(x = NULL, y = "Percent of countries", title = rg) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      panel.grid.major.x = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.margin = margin(t = 8),
      legend.box.margin = margin(t = 2)
    ) +
    scale_x_discrete(labels = wrap_lab)

  fn_base <- paste0("Q22_Elements_bySector_", gsub("[^A-Za-z0-9]+","_", rg))
  ggsave(file.path(out_dir_png, paste0(fn_base, ".png")), p,
         width = 14, height = 8, dpi = 300, units = "in")
  ggsave(file.path(out_dir_pdf, paste0(fn_base, ".pdf")), p,
         width = 14, height = 8, units = "in")
  message("Saved: ", fn_base)
}

for (rg in region_levels) make_one(rg)

# ---- also write the plotting table (handy for QA)
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "Plot_Data")
writeData(wb, "README",
          "Figure data for Q22 in-situ elements by operator.\n\
Legend order = Private & public, Private, Public.\n\
Cells are % of countries answering YES for each element.\n\
World row is computed from regional numerators/denominators.")
writeData(wb, "Plot_Data",
          plot_data %>% mutate(pct = round(pct, 1)) %>%
            arrange(Region, Operator_disp, Element))
addStyle(wb, "Plot_Data", createStyle(textDecoration = "bold"),
         rows = 1, cols = 1:10, gridExpand = TRUE)
setColWidths(wb, "Plot_Data", cols = 1:10, widths = "auto")
saveWorkbook(wb, out_xlsx, overwrite = TRUE)

message("\nPNG dir: ", out_dir_png, "\nPDF dir: ", out_dir_pdf, "\nExcel: ", out_xlsx)







Table of Species per regions
# ==============================================================
# Q22 (Sec 2) – In situ conservation programme elements
# Proportion of countries (YES only) by Region × Element
# for ALL Big-Seven species (3 cattle types + sheep, goats, pigs, chickens)
# + QA sheets with YES/NO counts and denominators.
# ==============================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# -------- Paths --------
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3B1_Q22_InSitu_byRegion_bySpecies.xlsx"

# -------- Regions (2024 order; World appended at the end) --------
region_levels <- c(
  "Africa","Asia","Southwest Pacific","Europe",
  "Latin America and the Carribean","North America","Near East","World"
)

# -------- Programme elements (exact SubquestionType_2014 strings) --------
elements <- c(
  "Promotion of niche marketing or other market differentiation",
  "Community-based conservation programmes",
  "Incentive or subsidy payment schemes for keeping at-risk breeds",
  "Development of biocultural community protocols",
  "Recognition/award programmes for breeders",
  "Conservation breeding programmes",
  "Selection programmes for increased production or productivity in at-risk breeds",
  "Promotion of at-risk breeds as tourist attractions",
  "Use of at-risk breeds in the management of wildlife habitats and landscapes",
  "Promotion of breed-related cultural activities",
  "Extension programmes to improve the management of at-risk breeds",
  "Awareness-raising activities providing information on the potential of specific at-risk breeds"
)

# -------- Big-Seven species (internal keys + pretty labels) --------
big7_keys <- c(
  "cattle (specialized dairy)",
  "cattle (specialized beef)",
  "cattle (multipurpose)",
  "sheep","goats","pigs","chickens"
)
species_pretty <- c(
  "Dairy cattle","Beef cattle","Multipurpose cattle",
  "Sheep","Goats","Pigs","Chickens"
)
stopifnot(length(big7_keys) == length(species_pretty))

# Fallback mapping from SpecieTag
code_to_species <- c(
  "009"="cattle (specialized dairy)",
  "008"="cattle (specialized beef)",
  "007"="cattle (multipurpose)",
  "042"="sheep","021"="goats","037"="pigs","010"="chickens"
)

# -------- Load & normalize headers --------
raw <- read_excel(in_path)
names(raw) <- names(raw) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+","_") |>
  tolower()

pick_col <- function(...) {
  cands <- unlist(list(...)); hit <- cands[cands %in% names(raw)]
  if (length(hit)) hit[1] else NA_character_
}

col_region   <- pick_col("region")
col_country  <- pick_col("country")
col_section  <- pick_col("section")
col_question <- pick_col("question_2024","question")
col_answer   <- pick_col("answer")
col_subtype  <- pick_col("subquestiontype_2014")
col_breed    <- pick_col("breedtype_2014")
col_tablelab <- pick_col("tablelabel_2014","species_clean")
col_specietag<- pick_col("specietag","specietagnew","specietag_new","species")

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_answer), !is.na(col_subtype))

# -------- Filter to Sec 2, Q22 and standardize --------
d0 <- tibble(
  Region_raw = raw[[col_region]],
  Country    = str_squish(as.character(raw[[col_country]])),
  Section    = raw[[col_section]],
  Question   = raw[[col_question]],
  Answer0    = str_to_lower(str_squish(as.character(raw[[col_answer]]))),
  Element0   = str_squish(as.character(raw[[col_subtype]])),
  BreedType0 = if (!is.na(col_breed))    str_to_lower(str_squish(as.character(raw[[col_breed]])))    else NA_character_,
  TableLab0  = if (!is.na(col_tablelab)) str_to_lower(str_squish(as.character(raw[[col_tablelab]]))) else NA_character_,
  SpecieTag0 = if (!is.na(col_specietag))str_squish(as.character(raw[[col_specietag]]))              else NA_character_
) %>%
  filter(Section %in% c(2,"2"),
         grepl("^\\s*22(\\.|$)", as.character(Question))) %>%
  # Regions -> 2024 names
  mutate(
    Region = case_when(
      str_detect(Region_raw, regex("^africa$", TRUE)) ~ "Africa",
      str_detect(Region_raw, regex("^asia$", TRUE)) ~ "Asia",
      str_detect(Region_raw, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
      str_detect(Region_raw, regex("^europe", TRUE)) ~ "Europe",
      str_detect(Region_raw, regex("^latin\\s+america|caribbean|central america|south america", TRUE)) ~
        "Latin America and the Carribean",
      str_detect(Region_raw, regex("^north\\s*america", TRUE)) ~ "North America",
      str_detect(Region_raw, regex("^near\\s*east", TRUE)) ~ "Near East",
      str_detect(Region_raw, regex("^world$", TRUE)) ~ "World",
      TRUE ~ NA_character_
    ),
    Element = Element0
  ) %>%
  filter(!is.na(Region), Element %in% elements) %>%
  # Species: prefer BreedType -> TableLabel -> SpecieTag
  mutate(
    Species_key = dplyr::coalesce(na_if(BreedType0,""),
                                  na_if(TableLab0,""),
                                  recode(stringr::str_pad(SpecieTag0,3,pad="0"),
                                         !!!code_to_species, .default = NA_character_))
  ) %>%
  filter(!is.na(Species_key), Species_key %in% big7_keys) %>%
  # YES/NO mapping (numerator uses YES only)
  mutate(
    Answer = case_when(
      Answer0 %in% c("yes","y","1","true")  ~ "yes",
      Answer0 %in% c("no","n","0","false")  ~ "no",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Answer)) %>%
  filter(Region != "World") %>%  # rebuild World from regions
  mutate(
    Region  = factor(Region, levels = region_levels[region_levels != "World"]),
    Species = factor(Species_key, levels = big7_keys, labels = species_pretty)
  )

# -------- Collapse duplicates per Country×Species×Element (any YES overrides NO) --------
d_country <- d0 %>%
  mutate(is_yes = Answer == "yes") %>%
  group_by(Region, Country, Species, Element) %>%
  summarise(YES = any(is_yes, na.rm = TRUE), .groups = "drop")

# -------- Denominators: # distinct countries per Region×Species --------
den_rs <- d_country %>%
  distinct(Region, Country, Species) %>%
  count(Region, Species, name = "N_countries")

# -------- Numerators: # YES per Region×Species×Element --------
num_rse <- d_country %>%
  filter(YES) %>%
  distinct(Region, Country, Species, Element) %>%
  count(Region, Species, Element, name = "n_yes")

# Fill missing combinations with 0 YES
num_rse <- expand_grid(
  Region  = levels(d_country$Region),
  Species = levels(d_country$Species),
  Element = elements
) %>%
  left_join(num_rse, by = c("Region","Species","Element")) %>%
  mutate(n_yes = dplyr::coalesce(n_yes, 0L))

# -------- Helper: build one species table (region rows + World row) --------
build_species_table <- function(species_label){
  # pick denominators / numerators for this species
  den <- den_rs %>% filter(Species == species_label)
  num <- num_rse %>% filter(Species == species_label)

  # join, compute %YES
  reg_tbl <- num %>%
    left_join(den, by = c("Region","Species")) %>%
    mutate(
      N_countries = dplyr::coalesce(N_countries, 0L),
      pct = ifelse(N_countries > 0, 100 * n_yes / N_countries, NA_real_)
    )

  # World = sum of regional numerators; denominator = sum of regional denominators
  world_den <- sum(den$N_countries, na.rm = TRUE)
  world_row <- reg_tbl %>%
    group_by(Element) %>%
    summarise(n_yes = sum(n_yes, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      Region        = factor("World", levels = "World"),
      Species       = species_label,
      N_countries   = world_den,
      pct           = ifelse(world_den > 0, 100 * n_yes / world_den, NA_real_)
    )

  final_long <- bind_rows(reg_tbl, world_row) %>%
    mutate(Region = factor(as.character(Region), levels = region_levels)) %>%
    arrange(Region, match(Element, elements))

  # reshape to sheet-style matrix
  sheet <- final_long %>%
    select(Region, `Number of countries` = N_countries, Element, pct) %>%
    mutate(pct = round(pct, 0)) %>%
    pivot_wider(names_from = Element, values_from = pct) %>%
    arrange(factor(Region, levels = region_levels))

  list(sheet = sheet, long = final_long)
}

# -------- Build all species sheets --------
species_tables <- lapply(species_pretty, build_species_table)
names(species_tables) <- species_pretty

# -------- QA: YES/NO counts per Region × Species × Element --------
qa_yesno <- {
  # merge denominators, compute NO = N - YES
  long_all <- num_rse %>%
    left_join(den_rs, by = c("Region","Species")) %>%
    mutate(
      N_countries = dplyr::coalesce(N_countries, 0L),
      n_yes = dplyr::coalesce(n_yes, 0L),
      n_no  = pmax(N_countries - n_yes, 0L),
      pct_yes = ifelse(N_countries > 0, 100 * n_yes / N_countries, NA_real_)
    ) %>%
    arrange(Region, Species, match(Element, elements))

  # add World rows by summing across regions for each Species × Element
  world_rows <- long_all %>%
    group_by(Species, Element) %>%
    summarise(
      n_yes = sum(n_yes, na.rm = TRUE),
      N_countries = sum(N_countries, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Region  = factor("World", levels = "World"),
      n_no    = pmax(N_countries - n_yes, 0L),
      pct_yes = ifelse(N_countries > 0, 100 * n_yes / N_countries, NA_real_)
    ) %>%
    select(Region, Species, Element, n_yes, n_no, N_countries, pct_yes)

  bind_rows(
    long_all %>% mutate(Region = factor(as.character(Region), levels = region_levels[region_levels!="World"])),
    world_rows
  ) %>%
  mutate(Region = factor(as.character(Region), levels = region_levels)) %>%
  arrange(Region, Species, match(Element, elements))
}

# -------- Denominators only (Region × Species) with World --------
denominators_sheet <- {
  world_den <- den_rs %>%
    group_by(Species) %>%
    summarise(N_countries = sum(N_countries, na.rm = TRUE), .groups = "drop") %>%
    mutate(Region = factor("World", levels = "World")) %>%
    select(Region, Species, N_countries)

  bind_rows(
    den_rs %>% mutate(Region = factor(as.character(Region), levels = region_levels[region_levels!="World"])),
    world_den
  ) %>%
  mutate(Region = factor(as.character(Region), levels = region_levels)) %>%
  arrange(Region, Species)
}

# -------- Write Excel --------
wb <- createWorkbook()
addWorksheet(wb, "README")

# one sheet per species (safe names)
safe_name <- function(x) gsub("[^A-Za-z0-9]+","_", x)
for (sp in species_pretty) {
  addWorksheet(wb, safe_name(sp))
  writeData(wb, safe_name(sp), species_tables[[sp]]$sheet)
}

addWorksheet(wb, "QA_YesNo_byElement")
writeData(wb, "QA_YesNo_byElement", qa_yesno)

addWorksheet(wb, "Denominators")
writeData(wb, "Denominators", denominators_sheet)

# README
readme <- paste(
  "Q22 (Section 2) – In situ conservation programme elements.",
  "Tables show, for each species, the proportion of countries reporting **YES** for each element, by region.",
  "",
  "Method:",
  "• Numerator (per Region × Species × Element) = # distinct countries with YES (duplicates per country resolved as any-YES).",
  "• Denominator (per Region × Species)         = # distinct countries in that region that reported that species for Q22 (any element).",
  "• %YES = Numerator / Denominator × 100 (rounded to integers).",
  "• World row = sum of regional numerators and denominators (denominator is NOT repeated per element).",
  "",
  "QA sheets include the raw YES/NO counts per Region × Species × Element and the denominators.",
  sep = "\n"
)
writeData(wb, "README", readme)

# Styling
bold <- createStyle(textDecoration = "bold")
int0 <- createStyle(numFmt = "0")

for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:500, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:500, widths = "auto")
}

# integer styling on species sheets
for (sp in species_pretty) {
  sh <- safe_name(sp)
  df <- readWorkbook(wb, sh)
  if (!is.null(df) && nrow(df)) {
    num_cols <- which(vapply(df, is.numeric, TRUE))
    if (length(num_cols)) {
      addStyle(wb, sh, int0, rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
    }
  }
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved Excel -> ", out_xlsx)







TABLE 3D9
# ==============================================================
# Table 3D9 — Gene banks & stored material (Sec 2, Q23/Q24/Q26)
# ==============================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# ---- Paths ----
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3B1_Q23_24_26_GeneBank_tables.xlsx"

# ---- Regions order ----
region_levels <- c("Africa","Asia","Southwest Pacific","Europe",
                   "Latin America and the Carribean","North America","Near East","World")
regions_only <- region_levels[region_levels != "World"]

# ---- Q24 materials (canonical) ----
materials_canon <- c("Semen","Embryos","Oocytes","Somatic cells","Isolated DNA")

# ---- Load & normalize ----
raw <- read_excel(in_path)
names(raw) <- names(raw) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+","_") |>
  tolower()

pick_col <- function(...) {
  cands <- unlist(list(...)); hit <- cands[cands %in% names(raw)]
  if (length(hit)) hit[1] else NA_character_
}

col_region   <- pick_col("region")
col_country  <- pick_col("country")
col_section  <- pick_col("section")
col_question <- pick_col("question_2024","question")
col_subq     <- pick_col("subquestion_2024","subquestion")
col_answer   <- pick_col("answer")

# for materials (Q24): first try BreedType_2014, else SubquestionType_2014, else TableLabel_2014
col_material <- pick_col("breedtype_2014","subquestiontype_2014","tablelabel_2014")

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_answer))

# ---- Filter Sec 2, Q23/24/26 ----
d0 <- tibble(
  Region_raw = raw[[col_region]],
  Country    = str_squish(as.character(raw[[col_country]])),
  Section    = raw[[col_section]],
  Question   = as.character(raw[[col_question]]),
  SubQ       = if (!is.na(col_subq)) as.integer(raw[[col_subq]]) else NA_integer_,
  Answer0    = str_to_lower(str_squish(as.character(raw[[col_answer]]))),
  MaterialRaw= if (!is.na(col_material)) str_squish(as.character(raw[[col_material]])) else NA_character_
) %>%
  filter(Section %in% c(2,"2"),
         str_detect(Question, "^\\s*(23|24|26)(\\.|$)")) %>%
  mutate(
    Region = case_when(
      str_detect(Region_raw, regex("^africa$", TRUE)) ~ "Africa",
      str_detect(Region_raw, regex("^asia$", TRUE)) ~ "Asia",
      str_detect(Region_raw, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
      str_detect(Region_raw, regex("^europe", TRUE)) ~ "Europe",
      str_detect(Region_raw, regex("^latin\\s+america|caribbean|central america|south america", TRUE)) ~
        "Latin America and the Carribean",
      str_detect(Region_raw, regex("^north\\s*america", TRUE)) ~ "North America",
      str_detect(Region_raw, regex("^near\\s*east", TRUE)) ~ "Near East",
      TRUE ~ NA_character_
    ),
    QuestionNum = as.integer(str_replace(Question, "\\..*$", "")),
    Answer = case_when(
      Answer0 %in% c("yes","y","1","true")  ~ "yes",
      Answer0 %in% c("no","n","0","false")  ~ "no",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Region), !is.na(Answer)) %>%
  filter(Region != "World")   # we will rebuild World

# ---- Canonicalize material names for Q24 (from BreedType_2014 etc.) ----
canon_material <- function(x){
  xl <- str_to_lower(x %||% "")
  case_when(
    str_detect(xl, "semen")                    ~ "Semen",
    str_detect(xl, "embryo")                   ~ "Embryos",
    str_detect(xl, "oocyte")                   ~ "Oocytes",
    str_detect(xl, "somatic")                  ~ "Somatic cells",
    str_detect(xl, "isolated\\s*dna|\\bdna\\b")~ "Isolated DNA",
    TRUE ~ NA_character_
  )
}
d0 <- d0 %>%
  mutate(Material = ifelse(QuestionNum == 24, canon_material(MaterialRaw), NA_character_))

# ---- Denominator (region): distinct countries with any of Q23/24/26 ----
den_region <- d0 %>%
  distinct(Region, Country) %>%
  count(Region, name = "N_countries")

# ---- Countries with a gene bank (Q23 YES) ----
gb_yes_countries <- d0 %>%
  filter(QuestionNum == 23) %>%
  group_by(Region, Country) %>%
  summarise(has_gb = any(Answer == "yes", na.rm = TRUE), .groups = "drop") %>%
  filter(has_gb) %>%
  select(Region, Country)

gb_per_region <- gb_yes_countries %>%
  count(Region, name = "n_gb_yes")

# ---- Q24 materials YES among gene-bank countries ----
mat_yes <- d0 %>%
  filter(QuestionNum == 24, Answer == "yes", !is.na(Material), Material %in% materials_canon) %>%
  distinct(Region, Country, Material) %>%
  inner_join(gb_yes_countries, by = c("Region","Country")) %>%
  count(Region, Material, name = "n_yes_mat")

# ---- Q26 collaboration YES among gene-bank countries ----
collab_yes <- d0 %>%
  filter(QuestionNum == 26, Answer == "yes") %>%
  distinct(Region, Country) %>%
  inner_join(gb_yes_countries, by = c("Region","Country")) %>%
  count(Region, name = "n_yes_collab")

# ---- Build region rows ----
region_rows <- expand_grid(Region = regions_only) %>%
  left_join(den_region,    by = "Region") %>%
  left_join(gb_per_region, by = "Region") %>%
  mutate(
    N_countries = coalesce(N_countries, 0L),
    n_gb_yes    = coalesce(n_gb_yes, 0L),
    pct_gb      = ifelse(N_countries > 0, 100 * n_gb_yes / N_countries, NA_real_)
  )

# add material percentages (each over n_gb_yes; if n_gb_yes==0 -> 0)
for (m in materials_canon){
  mm <- mat_yes %>% filter(Material == m) %>% select(Region, n_yes_mat)
  region_rows <- region_rows %>%
    left_join(mm, by = "Region") %>%
    mutate("{m}" := ifelse(n_gb_yes > 0, 100 * coalesce(n_yes_mat, 0) / n_gb_yes, 0)) %>%
    select(-n_yes_mat)
}

# add collaboration %
region_rows <- region_rows %>%
  left_join(collab_yes, by = "Region") %>%
  mutate(
    n_yes_collab = coalesce(n_yes_collab, 0L),
    `Countries planning subregional or regional collaboration` =
      ifelse(n_gb_yes > 0, 100 * n_yes_collab / n_gb_yes, 0)
  )

# ---- World row (sum numerators/denominators across regions) ----
world_den_total <- sum(region_rows$N_countries, na.rm = TRUE)
world_gb_total  <- sum(region_rows$n_gb_yes,    na.rm = TRUE)

world_row <- tibble(
  Region        = factor("World", levels = "World"),
  N_countries   = world_den_total,
  n_gb_yes      = world_gb_total,
  pct_gb        = ifelse(world_den_total > 0, 100 * world_gb_total / world_den_total, NA_real_)
)

# world materials & collaboration (sum numerators / world_gb_total)
for (m in materials_canon){
  num_m <- mat_yes %>% filter(Material == m) %>% summarise(n = sum(n_yes_mat, na.rm = TRUE)) %>% pull(n)
  world_row[[m]] <- ifelse(world_gb_total > 0, 100 * num_m / world_gb_total, 0)
}
num_collab_world <- sum(region_rows$n_yes_collab, na.rm = TRUE)
world_row[["Countries planning subregional or regional collaboration"]] <-
  ifelse(world_gb_total > 0, 100 * num_collab_world / world_gb_total, 0)

# ---- Final table (rounded) ----
final_table <- bind_rows(
  region_rows %>% mutate(Region = factor(Region, levels = regions_only)),
  world_row
) %>%
  mutate(Region = factor(as.character(Region), levels = region_levels)) %>%
  arrange(Region) %>%
  transmute(
    Region,
    `Number of countries` = N_countries,
    `Countries reporting AnGR gene bank` = round(pct_gb, 0),
    `Semen`         = round(.data[["Semen"]], 0),
    `Embryos`       = round(.data[["Embryos"]], 0),
    `Oocytes`       = round(.data[["Oocytes"]], 0),
    `Somatic cells` = round(.data[["Somatic cells"]], 0),
    `Isolated DNA`  = round(.data[["Isolated DNA"]], 0),
    `Countries planning subregional or regional collaboration` =
      round(.data[["Countries planning subregional or regional collaboration"]], 0)
  )

# ---- QA sheets ----
qa_yesno <- bind_rows(
  d0 %>% filter(QuestionNum == 23) %>%
    group_by(Region, Answer) %>% summarise(n = n_distinct(Country), .groups = "drop") %>%
    mutate(Item = "Gene bank (Q23)"),
  d0 %>% filter(QuestionNum == 24, !is.na(Material), Material %in% materials_canon) %>%
    group_by(Region, Material, Answer) %>% summarise(n = n_distinct(Country), .groups = "drop") %>%
    mutate(Item = paste0("Material: ", Material)) %>%
    select(Region, Answer, n, Item),
  d0 %>% filter(QuestionNum == 26) %>%
    group_by(Region, Answer) %>% summarise(n = n_distinct(Country), .groups = "drop") %>%
    mutate(Item = "Collaboration (Q26)")
) %>% pivot_wider(names_from = Answer, values_from = n, values_fill = 0) %>%
  arrange(Region, Item)

qa_counts_compact <- region_rows %>%
  transmute(
    Region,
    `N_countries (any of Q23/24/26)` = N_countries,
    `# with gene bank (Q23=YES)`     = n_gb_yes,
    `# YES Semen`                    = round(n_gb_yes * (`Semen`/100), 0),
    `# YES Embryos`                  = round(n_gb_yes * (`Embryos`/100), 0),
    `# YES Oocytes`                  = round(n_gb_yes * (`Oocytes`/100), 0),
    `# YES Somatic cells`            = round(n_gb_yes * (`Somatic cells`/100), 0),
    `# YES Isolated DNA`             = round(n_gb_yes * (`Isolated DNA`/100), 0),
    `# YES Collaboration (Q26)`      = n_yes_collab
  )

filtered_view <- d0 %>%
  transmute(
    Region, Country, Question = QuestionNum,
    Material = ifelse(QuestionNum == 24, Material, NA_character_),
    Answer
  ) %>% arrange(Region, Country, Question, Material)

# ---- Write Excel ----
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "Main_table")
addWorksheet(wb, "QA_counts_compact")
addWorksheet(wb, "QA_yesno_raw")
addWorksheet(wb, "Filtered_data")

readme <- paste(
  "Table 3D9 – Proportion of countries reporting animal genetic resources (AnGR) gene banks",
  "and the use of stored materials (Q23, Q24) and collaboration (Q26).",
  "",
  "• 'Number of countries' = distinct countries per region with any record in Q23/24/26.",
  "• 'Countries reporting AnGR gene bank' = Q23 YES as % of 'Number of countries'.",
  "• Material columns & collaboration are % among countries with a gene bank (Q23 YES) in that region.",
  "• World = sum of regional numerators & denominators (no source 'World' rows).",
  sep = "\n"
)

writeData(wb, "README", readme)
writeData(wb, "Main_table", final_table)
writeData(wb, "QA_counts_compact", qa_counts_compact)
writeData(wb, "QA_yesno_raw", qa_yesno)
writeData(wb, "Filtered_data", filtered_view)

bold <- createStyle(textDecoration = "bold")
int0 <- createStyle(numFmt = "0")

for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:200, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:200, widths = "auto")
}
df <- readWorkbook(wb, "Main_table")
if (!is.null(df) && nrow(df) > 0) {
  num_cols <- which(vapply(df, is.numeric, TRUE))
  if (length(num_cols)) {
    addStyle(wb, "Main_table", int0, rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
  }
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved Excel -> ", out_xlsx)



TABLE 3D9 (2)
# ==============================================================
# Q23, Q24, Q26 (Sec 2) – AnGR gene banks & materials & collaboration
# Main table + QA tables with correct denominators and World row
# --------------------------------------------------------------
# Denominators:
#  - Q23: countries that answered Q23 (yes or no)
#  - Q24 (materials): countries with Q23 == YES (i.e., have gene bank)
#  - Q26: countries that answered Q26 (yes or no)
# Percentages are YES / Denominator * 100 (rounded to integers)
# ==============================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# --------- Paths (edit if needed) ----------
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3B1_Q23_Q24_Q26_Genebank_Materials_Collab.xlsx"

# --------- Regions (2024 order) ----------
region_levels <- c("Africa","Asia","Southwest Pacific","Europe",
                   "Latin America and the Carribean","North America",
                   "Near East","World")

# --------- Material labels (normalize to these five) ----------
materials_std <- c("Semen","Embryos","Oocytes","Somatic cells","Isolated DNA")

# --------- Read & normalize headers ----------
raw <- read_excel(in_path)
names(raw) <- names(raw) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+","_") |>
  tolower()

pick_col <- function(...) {
  cands <- unlist(list(...))
  hit <- cands[cands %in% names(raw)]
  if (length(hit)) hit[1] else NA_character_
}

col_region   <- pick_col("region")
col_country  <- pick_col("country")
col_section  <- pick_col("section")
col_question <- pick_col("question_2024","question")
col_subq     <- pick_col("subquestion_2024","subquestion","subquestion_2014")
col_answer   <- pick_col("answer")

# Some files put the 5 materials in different columns; be liberal:
# (seen as BreedType_2014 or SubquestionType_2014 in your screenshots)
col_material <- pick_col("breedtype_2014","subquestiontype_2014",
                         "material","material_type","material_2014")

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_answer))

# --------- Filter to Sec 2 and Q 23/24/26 ----------
d0 <- tibble(
  Region_raw  = raw[[col_region]],
  Country     = str_squish(as.character(raw[[col_country]])),
  Section     = raw[[col_section]],
  Question    = suppressWarnings(as.integer(as.character(raw[[col_question]]))),
  Subquestion = if (!is.na(col_subq)) suppressWarnings(as.integer(as.character(raw[[col_subq]]))) else NA_integer_,
  Answer0     = str_to_lower(str_squish(as.character(raw[[col_answer]]))),
  Material0   = if (!is.na(col_material)) str_squish(as.character(raw[[col_material]])) else NA_character_
) %>%
  filter(Section %in% c(2,"2"),
         Question %in% c(23,24,26)) %>%
  mutate(
    Region = case_when(
      str_detect(Region_raw, regex("^africa$", TRUE)) ~ "Africa",
      str_detect(Region_raw, regex("^asia$", TRUE)) ~ "Asia",
      str_detect(Region_raw, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
      str_detect(Region_raw, regex("^europe", TRUE)) ~ "Europe",
      str_detect(Region_raw, regex("^latin\\s+america|caribbean|central america|south america", TRUE)) ~
        "Latin America and the Carribean",
      str_detect(Region_raw, regex("^north\\s*america", TRUE)) ~ "North America",
      str_detect(Region_raw, regex("^near\\s*east", TRUE)) ~ "Near East",
      str_detect(Region_raw, regex("^world$", TRUE)) ~ "World",
      TRUE ~ NA_character_
    ),
    Answer = case_when(
      Answer0 %in% c("yes","y","1","true") ~ "yes",
      Answer0 %in% c("no","n","0","false") ~ "no",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Region), !is.na(Answer)) %>%
  # We will rebuild World from regions
  filter(Region != "World") %>%
  mutate(Region = factor(Region, levels = region_levels[region_levels!="World"]))

# --------- Split per question ----------
d_q23 <- d0 %>% filter(Question == 23)
d_q24 <- d0 %>% filter(Question == 24)
d_q26 <- d0 %>% filter(Question == 26)

# --------- Q24: normalize materials into the 5 buckets ----------
norm_material <- function(x) {
  x1 <- str_to_lower(x)
  case_when(
    str_detect(x1, "semen") ~ "Semen",
    str_detect(x1, "embryo") ~ "Embryos",
    str_detect(x1, "oocyte") ~ "Oocytes",
    str_detect(x1, "somatic") ~ "Somatic cells",
    str_detect(x1, "isolated\\s*dna|dna") ~ "Isolated DNA",
    TRUE ~ NA_character_
  )
}

d_q24 <- d_q24 %>%
  mutate(Material = norm_material(Material0)) %>%
  filter(Material %in% materials_std)

# --------- Denominators (REGIONAL) ----------
# Any country appearing in any of 23/24/26 (coverage only; not used for %)
den_any <- d0 %>% distinct(Region, Country) %>%
  count(Region, name = "N_any")

# Q23 denominator: countries that ANSWERED Q23
den_q23 <- d_q23 %>% distinct(Region, Country) %>%
  count(Region, name = "N_q23")

# Q24 denominator: countries with Q23 == YES (have gene bank)
den_q24 <- d_q23 %>%
  filter(Answer == "yes") %>%
  distinct(Region, Country) %>%
  count(Region, name = "N_q24_gbYES")

# Q26 denominator: countries that ANSWERED Q26
den_q26 <- d_q26 %>% distinct(Region, Country) %>%
  count(Region, name = "N_q26")

# --------- Numerators (YES) ----------
# Q23 YES
num_q23_yes <- d_q23 %>%
  filter(Answer == "yes") %>%
  distinct(Region, Country) %>%
  count(Region, name = "YES_q23")

# Q24 YES per material (dedup country-material)
num_q24_yes <- d_q24 %>%
  filter(Answer == "yes") %>%
  distinct(Region, Country, Material) %>%
  count(Region, Material, name = "YES_q24")

# Q26 YES
num_q26_yes <- d_q26 %>%
  filter(Answer == "yes") %>%
  distinct(Region, Country) %>%
  count(Region, name = "YES_q26")

# --------- Build regional table (no World yet) ----------
regions <- levels(d0$Region)

# Helper to safe merge (fill missing 0)
safe_join <- function(x, y, by, name, fill0 = TRUE){
  z <- left_join(x, y, by = by)
  if (fill0 && name %in% names(z)) z[[name]] <- dplyr::coalesce(z[[name]], 0L)
  z
}

base_reg <- tibble(Region = factor(regions, levels = regions)) %>%
  safe_join(den_any,  "Region", "N_any") %>%
  safe_join(den_q23,  "Region", "N_q23") %>%
  safe_join(num_q23_yes, "Region", "YES_q23") %>%
  safe_join(den_q24,  "Region", "N_q24_gbYES") %>%
  safe_join(den_q26,  "Region", "N_q26") %>%
  safe_join(num_q26_yes, "Region", "YES_q26")

# Add material YES (wide)
mat_yes_wide <- expand_grid(Region = factor(regions, levels = regions),
                            Material = materials_std) %>%
  left_join(num_q24_yes, by = c("Region","Material")) %>%
  mutate(YES_q24 = dplyr::coalesce(YES_q24, 0L)) %>%
  pivot_wider(names_from = Material, values_from = YES_q24)

reg_counts <- base_reg %>%
  left_join(mat_yes_wide, by = "Region")

# --------- Percentages (regional) ----------
pct_reg <- reg_counts %>%
  transmute(
    Region,
    `Number of countries`                  = N_any,
    `Countries reporting AnGR gene bank`   = ifelse(N_q23 > 0, round(100 * YES_q23 / N_q23), NA_real_),
    Semen        = ifelse(N_q24_gbYES > 0, round(100 * Semen        / N_q24_gbYES), NA_real_),
    Embryos      = ifelse(N_q24_gbYES > 0, round(100 * Embryos      / N_q24_gbYES), NA_real_),
    Oocytes      = ifelse(N_q24_gbYES > 0, round(100 * Oocytes      / N_q24_gbYES), NA_real_),
    `Somatic cells` = ifelse(N_q24_gbYES > 0, round(100 * `Somatic cells` / N_q24_gbYES), NA_real_),
    `Isolated DNA` = ifelse(N_q24_gbYES > 0, round(100 * `Isolated DNA` / N_q24_gbYES), NA_real_),
    `Countries planning subregional or regional collaboration` =
      ifelse(N_q26 > 0, round(100 * YES_q26 / N_q26), NA_real_)
  )

# --------- WORLD row (sum numerators & denominators) ----------
sum_na0 <- function(x) sum(dplyr::coalesce(x, 0L), na.rm = TRUE)

world_row_counts <- tibble(
  Region        = factor("World", levels = "World"),
  N_any         = sum_na0(reg_counts$N_any),
  N_q23         = sum_na0(reg_counts$N_q23),
  YES_q23       = sum_na0(reg_counts$YES_q23),
  N_q24_gbYES   = sum_na0(reg_counts$N_q24_gbYES),
  Semen         = sum_na0(reg_counts$Semen),
  Embryos       = sum_na0(reg_counts$Embryos),
  Oocytes       = sum_na0(reg_counts$Oocytes),
  `Somatic cells` = sum_na0(reg_counts$`Somatic cells`),
  `Isolated DNA` = sum_na0(reg_counts$`Isolated DNA`),
  N_q26         = sum_na0(reg_counts$N_q26),
  YES_q26       = sum_na0(reg_counts$YES_q26)
)

world_row_pct <- world_row_counts %>%
  transmute(
    Region = Region,
    `Number of countries` = N_any,
    `Countries reporting AnGR gene bank` =
      ifelse(N_q23 > 0, round(100 * YES_q23 / N_q23), NA_real_),
    Semen        = ifelse(N_q24_gbYES > 0, round(100 * Semen        / N_q24_gbYES), NA_real_),
    Embryos      = ifelse(N_q24_gbYES > 0, round(100 * Embryos      / N_q24_gbYES), NA_real_),
    Oocytes      = ifelse(N_q24_gbYES > 0, round(100 * Oocytes      / N_q24_gbYES), NA_real_),
    `Somatic cells` = ifelse(N_q24_gbYES > 0, round(100 * `Somatic cells` / N_q24_gbYES), NA_real_),
    `Isolated DNA` = ifelse(N_q24_gbYES > 0, round(100 * `Isolated DNA` / N_q24_gbYES), NA_real_),
    `Countries planning subregional or regional collaboration` =
      ifelse(N_q26 > 0, round(100 * YES_q26 / N_q26), NA_real_)
  )

main_table <- bind_rows(
  pct_reg %>% mutate(Region = factor(as.character(Region), levels = region_levels)),
  world_row_pct %>% mutate(Region = factor(as.character(Region), levels = region_levels))
) %>% arrange(Region)

# --------- QA: yes/no raw counts by item ----------
qa_yesno_raw <- bind_rows(
  # Q23
  d_q23 %>%
    count(Region, Answer, name = "n") %>%
    pivot_wider(names_from = Answer, values_from = n, values_fill = 0) %>%
    mutate(Item = "Gene bank (Q23)"),
  # Q24 by material
  d_q24 %>%
    mutate(Material = norm_material(Material0)) %>%
    filter(Material %in% materials_std) %>%
    count(Region, Material, Answer, name = "n") %>%
    pivot_wider(names_from = Answer, values_from = n, values_fill = 0) %>%
    rename(no = `no`, yes = `yes`) %>%
    transmute(Region, Item = paste0("Material: ", Material), no, yes),
  # Q26
  d_q26 %>%
    count(Region, Answer, name = "n") %>%
    pivot_wider(names_from = Answer, values_from = n, values_fill = 0) %>%
    mutate(Item = "Collaboration (Q26)")
) %>%
  mutate(Region = factor(as.character(Region), levels = region_levels[region_levels!="World"])) %>%
  arrange(Region, Item)

# --------- QA: compact counts (denominators + numerators per region) ----------
qa_counts_compact <- reg_counts %>%
  transmute(
    Region,
    `N_countries(any of Q23/24/26)` = N_any,
    `N_Q23_responses`               = N_q23,
    `# YES gene bank (Q23)`         = YES_q23,
    `N_Q24_geneBankYES`             = N_q24_gbYES,
    `# YES Semen`                   = Semen,
    `# YES Embryos`                 = Embryos,
    `# YES Oocytes`                 = Oocytes,
    `# YES Somatic cells`           = `Somatic cells`,
    `# YES Isolated DNA`            = `Isolated DNA`,
    `N_Q26_responses`               = N_q26,
    `# YES Collaboration (Q26)`     = YES_q26
  )

# --------- Filtered minimal long (for traceability) ----------
filtered_data <- d0 %>%
  transmute(
    Region = as.character(Region),
    Country,
    Question,
    Subquestion,
    Material = ifelse(Question == 24, norm_material(Material0), NA_character_),
    Answer
  ) %>% arrange(Region, Country, Question, Subquestion, Material)

# --------- Write Excel ----------
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "Main_table")
addWorksheet(wb, "QA_counts_compact")
addWorksheet(wb, "QA_yesno_raw")
addWorksheet(wb, "Filtered_data")

readme <- paste(
  "Q23, Q24, Q26 – AnGR gene banks, stored materials, and collaboration.",
  "",
  "Denominators:",
  "  • Q23 (Gene bank): countries that answered Q23 (yes or no).",
  "  • Q24 (Materials): countries with Q23 == YES (have gene bank).",
  "  • Q26 (Collaboration): countries that answered Q26 (yes or no).",
  "",
  "Percentages = YES / Denominator × 100 (rounded to integers).",
  "World row is the sum of regional numerators and denominators.",
  sep = "\n"
)

writeData(wb, "README", readme)
writeData(wb, "Main_table", main_table)
writeData(wb, "QA_counts_compact", qa_counts_compact)
writeData(wb, "QA_yesno_raw", qa_yesno_raw)
writeData(wb, "Filtered_data", filtered_data)

# Styling
bold <- createStyle(textDecoration = "bold")
int0 <- createStyle(numFmt = "0")
for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:100, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:100, widths = "auto")
}
# Format numeric columns in main table as integers
df <- readWorkbook(wb, "Main_table")
if (!is.null(df) && nrow(df) > 0) {
  num_cols <- which(vapply(df, is.numeric, TRUE))
  if (length(num_cols)) addStyle(wb, "Main_table", int0, rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved Excel -> ", out_xlsx)






TABLE 3D10
# ==============================================================
# Q25 – Gene banks (numeric answers) -> PROPORTIONS
# 1) % of breeds  = sum(counts) / sum(total national breed pops)
# 2) % of countries (fallback) = countries with value>0 / countries reporting
# ==============================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# -------- Paths --------
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3B1_Q25_GeneBank_Proportions.xlsx"

# -------- Regions (2024) --------
region_levels <- c("Africa","Asia","Southwest Pacific","Europe",
                   "Latin America and the Carribean","North America",
                   "Near East","World")

# -------- Species mapping --------
cattle_keys <- c("cattle (specialized dairy)",
                 "cattle (specialized beef)",
                 "cattle (multipurpose)")
big5_keys   <- c(cattle_keys, "sheep","goats","pigs","chickens")
species_out <- c("Cattle","Sheep","Goats","Pigs","Chickens")

code_to_species <- c(
  "009"="cattle (specialized dairy)",
  "008"="cattle (specialized beef)",
  "007"="cattle (multipurpose)",
  "042"="sheep","021"="goats","037"="pigs","010"="chickens"
)

# -------- Q25 sub-questions (exact meanings) --------
pat_conserved <- regex("^\\s*number of breeds for which material is stored\\s*$",
                       ignore_case = TRUE)
pat_enough    <- regex("^\\s*number of breeds for which sufficient material is stored to allow them to be reconstituted\\s*$",
                       ignore_case = TRUE)

# -------- A robust pattern for total national breed populations --------
# We look for "number of national breed populations" OR a generic
# "number of breeds" that does NOT mention "for which" (to avoid Q25 items).
pat_total_strict <- regex("number of national breed (population|populations|pops|s)$",
                          ignore_case = TRUE)
pat_total_loose  <- function(x) {
  str_detect(x, regex("number of breeds", TRUE)) &
    !str_detect(x, regex("for which", TRUE))
}

# -------- Read & normalize --------
raw <- read_excel(in_path)
names(raw) <- names(raw) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+","_") |>
  tolower()

pick_col <- function(...) {
  cands <- unlist(list(...)); hit <- cands[cands %in% names(raw)]
  if (length(hit)) hit[1] else NA_character_
}

col_region   <- pick_col("region")
col_country  <- pick_col("country")
col_section  <- pick_col("section")
col_question <- pick_col("question_2024","question")
col_subq     <- pick_col("subquestiontype_2014")
col_answer   <- pick_col("answer")
col_breed    <- pick_col("breedtype_2014")
col_tablelab <- pick_col("tablelabel_2014","species_clean")
col_specietag<- pick_col("specietag","specietagnew","specietag_new","species")

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_subq), !is.na(col_answer))

# ---------- Common region + species normalization ----------
norm_region <- function(x){
  case_when(
    str_detect(x, regex("^africa$", TRUE)) ~ "Africa",
    str_detect(x, regex("^asia$", TRUE)) ~ "Asia",
    str_detect(x, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
    str_detect(x, regex("^europe", TRUE)) ~ "Europe",
    str_detect(x, regex("^latin\\s+america|caribbean|central america|south america", TRUE)) ~
      "Latin America and the Carribean",
    str_detect(x, regex("^north\\s*america", TRUE)) ~ "North America",
    str_detect(x, regex("^near\\s*east", TRUE)) ~ "Near East",
    str_detect(x, regex("^world$", TRUE)) ~ "World",
    TRUE ~ NA_character_
  )
}

map_species <- function(bt, tl, st){
  key <- dplyr::coalesce(
    na_if(str_to_lower(str_squish(bt)),""),
    na_if(str_to_lower(str_squish(tl)),""),
    recode(stringr::str_pad(st, 3, pad = "0"), !!!code_to_species, .default = NA_character_)
  )
  case_when(
    key %in% cattle_keys ~ "Cattle",
    key == "sheep"       ~ "Sheep",
    key == "goats"       ~ "Goats",
    key == "pigs"        ~ "Pigs",
    key == "chickens"    ~ "Chickens",
    TRUE                 ~ NA_character_
  )
}

# ---------- Q25 data (numerators) ----------
d25 <- tibble(
  Region    = norm_region(raw[[col_region]]),
  Country   = str_squish(as.character(raw[[col_country]])),
  Section   = raw[[col_section]],
  Question  = raw[[col_question]],
  Subq0     = as.character(raw[[col_subq]]),
  Answer0   = as.character(raw[[col_answer]]),
  Species   = map_species(raw[[col_breed]], raw[[col_tablelab]], raw[[col_specietag]])
) %>%
  filter(Section %in% c(2,"2"),
         grepl("^\\s*25(\\.|$)", as.character(Question)),
         !is.na(Region), !is.na(Species)) %>%
  mutate(
    Status = case_when(
      str_detect(Subq0, pat_conserved) ~ "Conserved",
      str_detect(Subq0, pat_enough)    ~ "Enough material",
      TRUE ~ NA_character_
    ),
    AnswerNum = suppressWarnings(as.numeric(Answer0))
  ) %>%
  filter(!is.na(Status), !is.na(AnswerNum)) %>%
  filter(Region != "World")

# SUMS per Region×Species×Status (like before)
sum_reg <- d25 %>%
  group_by(Region, Species, Status) %>%
  summarise(total_breeds = sum(AnswerNum, na.rm = TRUE), .groups = "drop")

# ---------- Denominators for % of BREEDS ----------
# Try to find totals anywhere in the file
d_totals_raw <- tibble(
  Region  = norm_region(raw[[col_region]]),
  Country = str_squish(as.character(raw[[col_country]])),
  Section = raw[[col_section]],
  Question= raw[[col_question]],
  Subq0   = as.character(raw[[col_subq]]),
  Answer0 = as.character(raw[[col_answer]]),
  Species = map_species(raw[[col_breed]], raw[[col_tablelab]], raw[[col_specietag]])
) %>%
  filter(!is.na(Region), !is.na(Species)) %>%
  mutate(
    subq_l = str_to_lower(str_squish(Subq0)),
    is_total = str_detect(subq_l, pat_total_strict) | pat_total_loose(subq_l),
    TotalNum = suppressWarnings(as.numeric(Answer0))
  ) %>%
  filter(is_total, !is.na(TotalNum)) %>%
  filter(Region != "World")

# Sum totals per Region×Species
den_reg <- d_totals_raw %>%
  group_by(Region, Species) %>%
  summarise(total_national_breeds = sum(TotalNum, na.rm = TRUE), .groups = "drop")

# ---------- Build % of breeds ----------
pct_breeds_reg <- sum_reg %>%
  left_join(den_reg, by = c("Region","Species")) %>%
  mutate(
    pct = ifelse(total_national_breeds > 0,
                 100 * total_breeds / total_national_breeds, NA_real_)
  )

# World (sum numerators & denominators)
world_breeds <- pct_breeds_reg %>%
  group_by(Species, Status) %>%
  summarise(total_breeds = sum(total_breeds, na.rm = TRUE),
            total_national_breeds = sum(total_national_breeds, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(
    Region = "World",
    pct = ifelse(total_national_breeds > 0,
                 100 * total_breeds / total_national_breeds, NA_real_)
  )

pct_breeds_all <- bind_rows(
  pct_breeds_reg,
  world_breeds
) %>%
  mutate(Region = factor(Region, levels = region_levels),
         Species = factor(Species, levels = species_out),
         Status  = factor(Status,  levels = c("Conserved","Enough material"))) %>%
  arrange(Region, Status, Species)

# ---------- % of COUNTRIES (fallback) ----------
# Countries reporting a species (denominator)
den_countries <- d25 %>%
  group_by(Region, Species) %>%
  summarise(N_countries = n_distinct(Country), .groups = "drop")

# Countries with value > 0 for a status (numerator)
num_countries <- d25 %>%
  group_by(Region, Species, Status, Country) %>%
  summarise(x = sum(AnswerNum, na.rm = TRUE), .groups = "drop") %>%
  mutate(has_any = x > 0) %>%
  group_by(Region, Species, Status) %>%
  summarise(n_countries = sum(has_any, na.rm = TRUE), .groups = "drop")

pct_countries_reg <- num_countries %>%
  left_join(den_countries, by = c("Region","Species")) %>%
  mutate(pct = ifelse(N_countries > 0, 100 * n_countries / N_countries, NA_real_))

world_countries <- pct_countries_reg %>%
  group_by(Species, Status) %>%
  summarise(n_countries = sum(n_countries, na.rm = TRUE),
            N_countries = sum(N_countries, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Region = "World",
    pct = ifelse(N_countries > 0, 100 * n_countries / N_countries, NA_real_)
  )

pct_countries_all <- bind_rows(
  pct_countries_reg,
  world_countries
) %>%
  mutate(Region = factor(Region, levels = region_levels),
         Species = factor(Species, levels = species_out),
         Status  = factor(Status,  levels = c("Conserved","Enough material"))) %>%
  arrange(Region, Status, Species)

# ---------- Wide tables ----------
mk_wide <- function(df, value_col = "pct"){
  df %>%
    select(Region, Status, Species, !!sym(value_col)) %>%
    pivot_wider(names_from = Species, values_from = !!sym(value_col)) %>%
    {
      miss <- setdiff(species_out, names(.))
      if (length(miss)) for (m in miss) .[[m]] <- NA_real_
      .
    } %>%
    arrange(Region, Status) %>%
    mutate(across(all_of(species_out), ~ round(.x, 0))) %>%
    select(Region, Status, all_of(species_out))
}

main_table_breedsPct    <- mk_wide(pct_breeds_all, "pct")
main_table_countriesPct <- mk_wide(pct_countries_all, "pct")

# Keep the SUMS too
main_table_sums <- sum_reg %>%
  bind_rows(
    sum_reg %>% group_by(Species, Status) %>%
      summarise(total_breeds = sum(total_breeds), .groups = "drop") %>%
      mutate(Region = "World")
  ) %>%
  mutate(Region = factor(Region, levels = region_levels),
         Species = factor(Species, levels = species_out),
         Status  = factor(Status, levels = c("Conserved","Enough material"))) %>%
  arrange(Region, Status, Species) %>%
  pivot_wider(names_from = Species, values_from = total_breeds) %>%
  select(Region, Status, all_of(species_out))

# ---------- Filtered rows (audit) ----------
filtered_q25 <- d25 %>%
  select(Region, Country, Species, Status, Answer = AnswerNum) %>%
  arrange(Region, Country, Species, Status)

# ---------- Write Excel ----------
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "Main_table_breedsPct")
addWorksheet(wb, "Main_table_countriesPct")
addWorksheet(wb, "Main_table_sums")
addWorksheet(wb, "QA_breedsPct_long")
addWorksheet(wb, "QA_countriesPct_long")
addWorksheet(wb, "QA_totals_found")
addWorksheet(wb, "Filtered_Q25")

readme <- paste(
  "Q25 (Section 2) – Gene banks (numeric).",
  "",
  "Two proportion definitions:",
  "1) Main_table_breedsPct:  % of breeds = SUM(Counts) / SUM(Total national breed populations).",
  "   Totals are detected from any item whose sub-question resembles",
  '   "Number of national breed populations" OR a generic "Number of breeds" that does not include "for which".',
  "   If totals are missing for a Region×Species, that cell is NA.",
  "",
  "2) Main_table_countriesPct: % of countries = Countries with value > 0 / Countries that reported that species in Q25.",
  "",
  "Species: Cattle (aggregates dairy, beef, multipurpose), Sheep, Goats, Pigs, Chickens.",
  "World row = sum of regional numerators and denominators.",
  sep = "\n"
)

writeData(wb, "README", readme)
writeData(wb, "Main_table_breedsPct",    main_table_breedsPct)
writeData(wb, "Main_table_countriesPct", main_table_countriesPct)
writeData(wb, "Main_table_sums",         main_table_sums)
writeData(wb, "QA_breedsPct_long",       pct_breeds_all)
writeData(wb, "QA_countriesPct_long",    pct_countries_all)
writeData(wb, "QA_totals_found",         d_totals_raw %>% select(Region, Country, Species, Subq0, TotalNum))
writeData(wb, "Filtered_Q25",            filtered_q25)

# Styling
bold <- createStyle(textDecoration = "bold")
int0 <- createStyle(numFmt = "0")
for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:200, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:200, widths = "auto")
}
for (sh in c("Main_table_breedsPct","Main_table_countriesPct","Main_table_sums")){
  df <- readWorkbook(wb, sh)
  if (!is.null(df) && nrow(df)) {
    num_cols <- which(vapply(df, is.numeric, TRUE))
    if (length(num_cols)) {
      addStyle(wb, sh, int0, rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
    }
  }
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved Excel -> ", out_xlsx)








TABLE 3D12
# ==============================================================
# Q25 – Table 3D12 (YES/NO) for big-five species
# Build counts and proportions by region for 5 purposes
# ==============================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# -------- Paths --------
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3B1_Q25_3D12_GeneBank_Uses.xlsx"

# -------- Regions (order you’ve been using) --------
region_levels <- c("Africa","Asia","Southwest Pacific","Europe",
                   "Latin America and the Carribean","North America",
                   "Near East","World")

# -------- Big-five species mapping (keep cattle split, then aggregate to 'Cattle') --------
cattle_keys <- c("cattle (specialized dairy)", "cattle (specialized beef)", "cattle (multipurpose)")
big5_text   <- c(cattle_keys, "chickens", "goats", "pigs", "sheep")

code_to_species <- c(
  "009"="cattle (specialized dairy)",
  "008"="cattle (specialized beef)",
  "007"="cattle (multipurpose)",
  "010"="chickens",
  "021"="goats",
  "037"="pigs",
  "042"="sheep"
)

# collapse any cattle key to 'Cattle'; keep others as title-cased labels
map_big5 <- function(bt, tl, st){
  key <- dplyr::coalesce(
    na_if(str_to_lower(str_squish(bt)),""),
    na_if(str_to_lower(str_squish(tl)),""),
    recode(stringr::str_pad(st, 3, pad = "0"), !!!code_to_species, .default = NA_character_)
  )
  key <- str_to_lower(key)
  case_when(
    key %in% cattle_keys        ~ "Cattle",
    key == "sheep"              ~ "Sheep",
    key == "goats"              ~ "Goats",
    key == "pigs"               ~ "Pigs",
    key == "chickens"           ~ "Chickens",
    TRUE                        ~ NA_character_
  )
}

# -------- Subquestion labels (exact matches from SubquestionType_2014) --------
LBL_STORAGE      <- "Does the collection include material from not-at-risk breeds?"
LBL_RECON        <- "Have any extinct populations been reconstituted using material from the gene bank?"
LBL_EX_SITU      <- "Have the gene bank collections been used to introduce genetic variability into an ex situ population?"
LBL_IN_SITU      <- "Have the gene bank collections been used to introduce genetic variability into an in situ population?"
LBL_PARTICIP     <- "Do livestock keepers or breeders’ associations participate in the planning of the gene banking activities?"

# nice column names in the same order as your picture (storage, participation, ex situ, in situ, reconstitution)
nice_cols <- c(
  "Storage of not-at-risk breeds",
  "Participation of livestock keepers / breeders' association",
  "Increase genetic variability in ex situ population",
  "Increase genetic variability in in situ population",
  "Reconstitution of extinct breeds"
)

subq_to_nice <- c(
  `Does the collection include material from not-at-risk breeds?` =
    "Storage of not-at-risk breeds",
  `Do livestock keepers or breeders’ associations participate in the planning of the gene banking activities?` =
    "Participation of livestock keepers / breeders' association",
  `Have the gene bank collections been used to introduce genetic variability into an ex situ population?` =
    "Increase genetic variability in ex situ population",
  `Have the gene bank collections been used to introduce genetic variability into an in situ population?` =
    "Increase genetic variability in in situ population",
  `Have any extinct populations been reconstituted using material from the gene bank?` =
    "Reconstitution of extinct breeds"
)

# -------- Read & normalize --------
raw <- read_excel(in_path)
names(raw) <- names(raw) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+","_") |>
  tolower()

pick_col <- function(...) {
  cands <- unlist(list(...)); hit <- cands[cands %in% names(raw)]
  if (length(hit)) hit[1] else NA_character_
}

col_region   <- pick_col("region")
col_country  <- pick_col("country")
col_section  <- pick_col("section")
col_question <- pick_col("question_2024","question")
col_subq     <- pick_col("subquestiontype_2014")
col_answer   <- pick_col("answer")
col_breed    <- pick_col("breedtype_2014")
col_tablelab <- pick_col("tablelabel_2014","species_clean")
col_specietag<- pick_col("specietag","specietagnew","specietag_new","species")

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_subq), !is.na(col_answer))

# Region normalization (same as before)
norm_region <- function(x){
  case_when(
    str_detect(x, regex("^africa$", TRUE)) ~ "Africa",
    str_detect(x, regex("^asia$", TRUE)) ~ "Asia",
    str_detect(x, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
    str_detect(x, regex("^europe", TRUE)) ~ "Europe",
    str_detect(x, regex("^latin\\s+america|caribbean|central america|south america", TRUE)) ~
      "Latin America and the Carribean",
    str_detect(x, regex("^north\\s*america", TRUE)) ~ "North America",
    str_detect(x, regex("^near\\s*east", TRUE)) ~ "Near East",
    str_detect(x, regex("^world$", TRUE)) ~ "World",
    TRUE ~ NA_character_
  )
}

# ---------- Filter Q25 (YES/NO) for big-five ----------
d_q25 <- tibble(
  Region   = norm_region(raw[[col_region]]),
  Country  = str_squish(as.character(raw[[col_country]])),
  Section  = raw[[col_section]],
  Question = raw[[col_question]],
  Subq0    = as.character(raw[[col_subq]]),
  Answer0  = as.character(raw[[col_answer]]),
  Species  = map_big5(raw[[col_breed]], raw[[col_tablelab]], raw[[col_specietag]])
) %>%
  filter(Section %in% c(2,"2"),
         grepl("^\\s*25(\\.|$)", as.character(Question)),
         !is.na(Region), !is.na(Species),
         Subq0 %in% names(subq_to_nice)) %>%
  mutate(
    Purpose = recode(Subq0, !!!subq_to_nice),
    ans_l   = str_to_lower(str_squish(Answer0)),
    AnsYN   = case_when(
      ans_l %in% c("yes","y","true","1") ~ "YES",
      ans_l %in% c("no","n","false","0") ~ "NO",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(AnsYN)) %>%
  filter(Region != "World")

# ---------- Country counts per region (for the “Number of countries” column) ----------
# unique countries that have at least one usable row for these purposes and big-five
n_countries_region <- d_q25 %>%
  distinct(Region, Country) %>%
  count(Region, name = "Number_of_countries")

# ---------- YES/NO counts by Region × Purpose (aggregated over big-five species) ----------
counts_region_purpose <- d_q25 %>%
  count(Region, Purpose, AnsYN, name = "n") %>%
  tidyr::pivot_wider(names_from = AnsYN, values_from = n, values_fill = 0) %>%
  mutate(TOTAL = YES + NO)

# ---------- Proportions (YES / (YES+NO)) ----------
prop_region_purpose <- counts_region_purpose %>%
  mutate(pct_yes = ifelse(TOTAL > 0, 100 * YES / TOTAL, NA_real_))

# ---------- Main table (wide) ----------
Main_table_prop <- prop_region_purpose %>%
  select(Region, Purpose, pct_yes) %>%
  tidyr::pivot_wider(names_from = Purpose, values_from = pct_yes) %>%
  {
    # ensure all 5 columns exist in the requested order
    miss <- setdiff(nice_cols, names(.))
    if (length(miss)) for (m in miss) .[[m]] <- NA_real_
    .
  } %>%
  left_join(n_countries_region, by = "Region") %>%
  mutate(Region = factor(Region, levels = region_levels)) %>%
  arrange(Region) %>%
  relocate(Region, `Number_of_countries`) %>%
  # pretty header for the number-of-countries column
  rename(`Number of countries` = Number_of_countries) %>%
  # order purpose columns to match the picture
  select(Region, `Number of countries`, all_of(nice_cols)) %>%
  mutate(across(all_of(nice_cols), ~ round(.x, 0)))

# ---------- World row (sum over regions) ----------
world_counts <- counts_region_purpose %>%
  group_by(Purpose) %>%
  summarise(YES = sum(YES, na.rm = TRUE),
            NO  = sum(NO,  na.rm = TRUE),
            TOTAL = sum(TOTAL, na.rm = TRUE), .groups = "drop") %>%
  mutate(pct_yes = ifelse(TOTAL > 0, 100 * YES / TOTAL, NA_real_)) %>%
  select(Purpose, pct_yes) %>%
  tidyr::pivot_wider(names_from = Purpose, values_from = pct_yes) %>%
  {
    miss <- setdiff(nice_cols, names(.))
    if (length(miss)) for (m in miss) .[[m]] <- NA_real_
    .
  } %>%
  mutate(Region = "World") %>%
  left_join(
    d_q25 %>% distinct(Region, Country) %>% summarise(`Number of countries` = n_distinct(Country)),
    by = character()
  ) %>%
  select(Region, `Number of countries`, all_of(nice_cols)) %>%
  mutate(across(all_of(nice_cols), ~ round(.x, 0)))

Main_table_prop <- bind_rows(
  Main_table_prop,
  world_counts
) %>%
  mutate(Region = factor(Region, levels = region_levels)) %>%
  arrange(Region)

# ---------- QA: counts by Region × Purpose ----------
Counts_YesNo_byRegion <- counts_region_purpose %>%
  left_join(n_countries_region, by = "Region") %>%
  select(Region, Purpose, YES, NO, TOTAL, `Number_of_countries`) %>%
  arrange(factor(Region, levels = region_levels), match(Purpose, nice_cols))

# ---------- QA: counts by Region × Species × Purpose ----------
Counts_YesNo_byRegionSpecies <- d_q25 %>%
  count(Region, Species, Purpose, AnsYN, name = "n") %>%
  tidyr::pivot_wider(names_from = AnsYN, values_from = n, values_fill = 0) %>%
  mutate(TOTAL = YES + NO) %>%
  arrange(factor(Region, levels = region_levels), Species, match(Purpose, nice_cols))

# ---------- Filtered rows (audit) ----------
Filtered_Q25 <- d_q25 %>%
  select(Region, Country, Species, Purpose, Answer = AnsYN) %>%
  arrange(factor(Region, levels = region_levels), Country, Species, Purpose)

# ---------- Write Excel ----------
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "Main_table_prop")
addWorksheet(wb, "Counts_YesNo_byRegion")
addWorksheet(wb, "Counts_YesNo_byRegionSpecies")
addWorksheet(wb, "Country_counts")
addWorksheet(wb, "Filtered_Q25")

readme <- paste(
  "Table 3D12 – Proportion of countries using AnGR gene bank for each purpose (Q25, Section 2).",
  "",
  "Scope: Big-five species only (cattle: multipurpose + specialized beef + specialized dairy; chickens; goats; pigs; sheep).",
  "",
  "Method:",
  "• Keep rows where SubquestionType_2014 is one of:",
  "   1) Does the collection include material from not-at-risk breeds?",
  "   2) Do livestock keepers or breeders’ associations participate in the planning of the gene banking activities?",
  "   3) Have the gene bank collections been used to introduce genetic variability into an ex situ population?",
  "   4) Have the gene bank collections been used to introduce genetic variability into an in situ population?",
  "   5) Have any extinct populations been reconstituted using material from the gene bank?",
  "• YES/NO taken from the Answer column (case-insensitive).",
  "• For each Region × Purpose, count YES and NO across all big-five species rows and compute %",
  "     %YES = 100 × YES / (YES + NO).",
  "• 'Number of countries' = unique countries in that region contributing any of the above items.",
  "• A 'World' row is added by summing regional numerators/denominators.",
  sep = "\n"
)

writeData(wb, "README", readme)
writeData(wb, "Main_table_prop",             Main_table_prop)
writeData(wb, "Counts_YesNo_byRegion",       Counts_YesNo_byRegion)
writeData(wb, "Counts_YesNo_byRegionSpecies",Counts_YesNo_byRegionSpecies)
writeData(wb, "Country_counts",              n_countries_region %>% rename(`Number of countries` = Number_of_countries))
writeData(wb, "Filtered_Q25",                Filtered_Q25)

# Styling
bold <- createStyle(textDecoration = "bold")
int0 <- createStyle(numFmt = "0")

for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:200, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:200, widths = "auto")
}
# integers on % columns in the main table
df <- openxlsx::readWorkbook(wb, "Main_table_prop")
if (!is.null(df) && nrow(df)) {
  num_cols <- which(vapply(df, is.numeric, TRUE))
  if (length(num_cols)) addStyle(wb, "Main_table_prop", int0, rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved -> ", out_xlsx)





Figure 3D5
# ==============================================================
# SP7 – Policies & programmes (Section 3, Question 32)
# Count countries per region by policy status (a–h) and compute %.
# No species used here. One answer per country.
# ==============================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# -------- Paths --------
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/FigSP7_Policies.xlsx"

# -------- Regions (2024) --------
region_levels <- c("Africa","Asia","Southwest Pacific","Europe",
                   "Latin America and the Carribean","North America",
                   "Near East","World")

# -------- Exact SP7 answer options (use THESE — not the 2007 labels) --------
opt_a <- "a. Country requires no policies and programmes because all locally adapted breeds are secure"
opt_b <- "b. Yes, comprehensive policies and programmes have been in place since before the adoption of the GPA"
opt_c <- "c. Yes, comprehensive policies and programmes exist because of progress made since the adoption of the GPA"
opt_d <- "d. For some species and breeds (coverage expanded since the adoption of the GPA)"
opt_e <- "e. For some species and breeds (coverage not expanded since the adoption of the GPA)"
opt_f <- "f. No, but action is planned and funding identified"
opt_g <- "g. No, but action is planned and funding is sought"
opt_h <- "h. No"

# we will match by the leading letter ("a.","b.",...) but keep YOUR full text in outputs
labels_full <- c(opt_a,opt_b,opt_c,opt_d,opt_e,opt_f,opt_g,opt_h)
lead_letters <- letters[1:8]  # a..h

# -------- Read & normalize --------
raw <- read_excel(in_path)
names(raw) <- names(raw) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+","_") |>
  tolower()

pick_col <- function(...) {
  cands <- unlist(list(...)); hit <- cands[cands %in% names(raw)]
  if (length(hit)) hit[1] else NA_character_
}

col_region   <- pick_col("region")
col_country  <- pick_col("country")
col_section  <- pick_col("section")
col_question <- pick_col("question_2024","question")
col_answer   <- pick_col("answer")

stopifnot(!is.na(col_region), !is.na(col_country),
          !is.na(col_section), !is.na(col_question), !is.na(col_answer))

# Region normalization (same style as before)
norm_region <- function(x){
  case_when(
    str_detect(x, regex("^africa$", TRUE)) ~ "Africa",
    str_detect(x, regex("^asia$", TRUE)) ~ "Asia",
    str_detect(x, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
    str_detect(x, regex("^europe", TRUE)) ~ "Europe",
    str_detect(x, regex("^latin\\s+america|caribbean|central america|south america", TRUE)) ~
      "Latin America and the Carribean",
    str_detect(x, regex("^north\\s*america", TRUE)) ~ "North America",
    str_detect(x, regex("^near\\s*east", TRUE)) ~ "Near East",
    str_detect(x, regex("^world$", TRUE)) ~ "World",
    TRUE ~ NA_character_
  )
}

# -------- Filter to SP7 (Section 3, Question 32) and keep ONE answer per country --------
d_sp7_raw <- tibble(
  Region   = norm_region(raw[[col_region]]),
  Country  = str_squish(as.character(raw[[col_country]])),
  Section  = raw[[col_section]],
  Question = raw[[col_question]],
  Answer0  = as.character(raw[[col_answer]])
) %>%
  filter(Section %in% c(3,"3"),
         grepl("^\\s*32(\\.|$)", as.character(Question)),
         !is.na(Region), !is.na(Country)) %>%
  mutate(
    ans_trim = str_squish(Answer0),
    lead     = str_to_lower(str_sub(ans_trim, 1, 2))      # "a.", "b.", ...
  ) %>%
  # keep only rows whose answer clearly begins with a..h followed by "."
  filter(lead %in% paste0(lead_letters, "."))

# Deduplicate: one answer per Country — if duplicates, keep the first non-missing
d_sp7 <- d_sp7_raw %>%
  group_by(Region, Country) %>%
  slice(1) %>%
  ungroup() %>%
  # Map to YOUR full labels for output tables (so headers match exactly)
  mutate(
    Purpose = case_when(
      str_starts(str_to_lower(ans_trim), "a.") ~ opt_a,
      str_starts(str_to_lower(ans_trim), "b.") ~ opt_b,
      str_starts(str_to_lower(ans_trim), "c.") ~ opt_c,
      str_starts(str_to_lower(ans_trim), "d.") ~ opt_d,
      str_starts(str_to_lower(ans_trim), "e.") ~ opt_e,
      str_starts(str_to_lower(ans_trim), "f.") ~ opt_f,
      str_starts(str_to_lower(ans_trim), "g.") ~ opt_g,
      str_starts(str_to_lower(ans_trim), "h.") ~ opt_h,
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Purpose))

# -------- Counts per region (countries per option) --------
counts_long <- d_sp7 %>%
  count(Region, Purpose, name = "n") %>%
  # ensure all 8 options appear for each region
  complete(Region, Purpose = labels_full, fill = list(n = 0)) %>%
  arrange(Region, match(Purpose, labels_full))

# # of reporting countries per region (after de-duplication)
country_counts <- d_sp7 %>%
  summarise(Number_of_countries = n_distinct(Country), .by = Region)

# Wide counts (underlying table for the figure)
counts_wide <- counts_long %>%
  pivot_wider(names_from = Purpose, values_from = n) %>%
  left_join(country_counts, by = "Region") %>%
  arrange(factor(Region, levels = region_levels[region_levels != "World"]))

# Add World row (sum over regions)
world_counts <- counts_long %>%
  summarise(n = sum(n), .by = Purpose) %>%
  pivot_wider(names_from = Purpose, values_from = n) %>%
  mutate(Region = "World") %>%
  relocate(Region)

world_countries <- tibble(
  Region = "World",
  Number_of_countries = d_sp7 %>% summarise(n = n_distinct(Country)) %>% pull(n)
)

counts_wide_all <- bind_rows(
  counts_wide,
  world_counts %>% left_join(world_countries, by = "Region")
) %>%
  mutate(Region = factor(Region, levels = region_levels)) %>%
  arrange(Region)

# -------- Percentages per region (based on reporting countries) --------
props_wide <- counts_wide_all %>%
  mutate(Total_reporting = Number_of_countries) %>%
  {
    num_cols <- which(names(.) %in% labels_full)
    out <- .
    out[num_cols] <- lapply(out[num_cols], function(x)
      ifelse(out$Total_reporting > 0, 100 * x / out$Total_reporting, NA_real_))
    out
  } %>%
  # round to whole % for the figure/table
  mutate(across(all_of(labels_full), ~ round(.x, 0)))

# -------- Tidy long tables (QA & plotting) --------
props_long <- counts_long %>%
  left_join(country_counts, by = "Region") %>%
  mutate(pct = ifelse(Number_of_countries > 0, 100 * n / Number_of_countries, NA_real_)) %>%
  arrange(Region, match(Purpose, labels_full))

filtered_dump <- d_sp7 %>%
  select(Region, Country, Answer = ans_trim) %>%
  arrange(Region, Country)

# -------- Write Excel --------
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "Counts_by_region")
addWorksheet(wb, "Percents_by_region")
addWorksheet(wb, "Counts_long_QA")
addWorksheet(wb, "Country_counts")
addWorksheet(wb, "Filtered_SP7")

readme <- paste(
  "SP7 (Section 3, Question 32) — Policies & programmes.",
  "",
  "We count ONE answer per country (deduplicated) and aggregate by region.",
  "Options used (exactly as provided by the user):",
  paste0("  • ", labels_full, collapse = "\n"),
  "",
  "Sheets:",
  " - Counts_by_region: country counts per option (plus World).",
  " - Percents_by_region: percentages by region (denominator = reporting countries).",
  " - Country_counts: # reporting countries per region (after de-dup).",
  " - Counts_long_QA: tidy long table with counts and %.",
  " - Filtered_SP7: region, country, kept answer (audit).",
  sep = "\n"
)

writeData(wb, "README",             readme)
writeData(wb, "Counts_by_region",   counts_wide_all)
writeData(wb, "Percents_by_region", props_wide)
writeData(wb, "Counts_long_QA",     props_long)
writeData(wb, "Country_counts",     country_counts %>%
           bind_rows(tibble(Region="World", Number_of_countries = world_countries$Number_of_countries)))
writeData(wb, "Filtered_SP7",       filtered_dump)

# Styling
bold <- createStyle(textDecoration = "bold")
int0 <- createStyle(numFmt = "0")
for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:1000, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:1000, widths = "auto")
}
# Integer format for counts; 0-dp % for percentages
df_counts <- openxlsx::readWorkbook(wb, "Counts_by_region")
if (!is.null(df_counts) && nrow(df_counts)) {
  num_cols <- which(vapply(df_counts, is.numeric, TRUE))
  if (length(num_cols)) addStyle(wb, "Counts_by_region", int0, rows = 2:(nrow(df_counts)+1), cols = num_cols, gridExpand = TRUE)
}
df_props <- openxlsx::readWorkbook(wb, "Percents_by_region")
if (!is.null(df_props) && nrow(df_props)) {
  num_cols <- which(vapply(df_props, is.numeric, TRUE))
  if (length(num_cols)) addStyle(wb, "Percents_by_region", int0, rows = 2:(nrow(df_props)+1), cols = num_cols, gridExpand = TRUE)
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved Excel -> ", out_xlsx)




Table 3E1-2
# =====================================================================
# Q28 – Technologies (None/Low/Medium/High)
# ROW-LEVEL COUNTS (no country collapsing): how many "none/low/medium/high"
# per Region × Technology, summed across ALL species rows.
# Also computes Using = Low + Medium + High and percentages.
# Outputs for big-five only and for ALL species.
# =====================================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# ---- Paths
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3E_Technologies_ROW_COUNTS.xlsx"

# ---- Regions order (+ World)
region_levels <- c("Africa","Asia","Southwest Pacific","Europe",
                   "Latin America and the Carribean","North America",
                   "Near East","World")

# ---- Big five (to filter when needed)
big5 <- c("cattle (multipurpose)",
          "cattle (specialized beef)",
          "cattle (specialized dairy)",
          "chickens","goats","pigs","sheep")

# ---- SpecieTag map (robust species id)
code_to_species <- c(
  "007"="cattle (multipurpose)", "008"="cattle (specialized beef)", "009"="cattle (specialized dairy)",
  "010"="chickens", "021"="goats", "037"="pigs", "042"="sheep",
  "002"="alpacas","003"="asses","004"="bactrian camels","005"="buffaloes","013"="deer",
  "015"="dromedaries","017"="ducks","022"="geese","023"="guinea fowls","024"="guinea pigs",
  "026"="horses","027"="llamas","028"="managed bee","029"="mithun","030"="muscovy ducks",
  "033"="ostriches","038"="pigeons","040"="quails","044"="turkeys","046"="yaks"
)

# ---- Technology groups (exact subquestion text)
G3E1 <- c("Artificial insemination",
          "Embryo transfer",
          "Molecular genetic or genomic information",
          "Multiple ovulation and embryo transfer")

G3E2 <- c("Semen sexing",
          "In vitro fertilization",
          "Cloning",
          "Genetic modification",
          "Transplantation of gonadal tissue")

# ---------- helpers ----------
norm_region <- function(x){
  case_when(
    str_detect(x, regex("^africa$", TRUE)) ~ "Africa",
    str_detect(x, regex("^asia$", TRUE)) ~ "Asia",
    str_detect(x, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
    str_detect(x, regex("^europe", TRUE)) ~ "Europe",
    str_detect(x, regex("^latin\\s+america|caribbean|central america|south america", TRUE)) ~
      "Latin America and the Carribean",
    str_detect(x, regex("^north\\s*america", TRUE)) ~ "North America",
    str_detect(x, regex("^near\\s*east", TRUE)) ~ "Near East",
    str_detect(x, regex("^world$", TRUE)) ~ "World",
    TRUE ~ NA_character_
  )
}

species_from_cols <- function(bt, tl, st){
  dplyr::coalesce(
    na_if(str_to_lower(str_squish(bt)),""),
    na_if(str_to_lower(str_squish(tl)),""),
    recode(stringr::str_pad(st,3,pad="0"), !!!code_to_species, .default = NA_character_)
  )
}

clean_tech <- function(x){
  z <- str_to_lower(str_squish(x))
  recode(z,
    "artificial insemination" = "Artificial insemination",
    "embryo transfer"         = "Embryo transfer",
    "molecular genetic or genomic information" = "Molecular genetic or genomic information",
    "multiple ovulation and embryo transfer"   = "Multiple ovulation and embryo transfer",
    "semen sexing"             = "Semen sexing",
    "in vitro fertilization"   = "In vitro fertilization",
    "cloning"                  = "Cloning",
    "genetic modification"     = "Genetic modification",
    "transplantation of gonadal tissue" = "Transplantation of gonadal tissue",
    .default = NA_character_
  )
}

lvl_levels <- c("none","low","medium","high")   # for row-level counts
pretty_lvl <- c("None","Low","Medium","High")

# ---------- read ----------
raw <- read_excel(in_path)
names(raw) <- names(raw) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+","_") |>
  tolower()

pick <- function(...) { c <- unlist(list(...)); h <- c[c %in% names(raw)]; if (length(h)) h[1] else NA_character_ }

col_region   <- pick("region")
col_country  <- pick("country")
col_section  <- pick("section")
col_question <- pick("question_2024","question")
col_subqtxt  <- pick("subquestiontype_2014")
col_answer   <- pick("answer")
col_breed    <- pick("breedtype_2014")
col_tablelab <- pick("tablelabel_2014","species_clean")
col_specietag<- pick("specietag","specietagnew","specietag_new","species")

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_subqtxt), !is.na(col_answer))

d28_rows <- tibble(
  Region0  = raw[[col_region]],
  Country  = str_squish(as.character(raw[[col_country]])),
  Section  = raw[[col_section]],
  Question = raw[[col_question]],
  Tech0    = as.character(raw[[col_subqtxt]]),
  Answer0  = as.character(raw[[col_answer]]),
  Species0 = species_from_cols(raw[[col_breed]], raw[[col_tablelab]], raw[[col_specietag]])
) %>%
  mutate(
    Region  = norm_region(Region0),
    Tech    = clean_tech(Tech0),
    Answer  = str_to_lower(str_squish(Answer0)),
    Species = str_to_lower(str_squish(Species0))
  ) %>%
  filter(Section %in% c(2,"2"),
         grepl("^\\s*28(\\.|$)", as.character(Question)),
         !is.na(Region), Region != "World",
         !is.na(Tech),
         Answer %in% lvl_levels)

# ---------- ROW-LEVEL counts (no collapsing) ----------
# Utility to make counts + percentages, with optional species filter (big5 vs all)
make_row_outputs <- function(D, species_filter = NULL){
  X <- if (is.null(species_filter)) D else D %>% filter(Species %in% species_filter)

  # regional row-level counts per level
  counts_reg <- X %>%
    count(Region, Tech, Answer) %>%
    mutate(Answer = factor(Answer, levels = lvl_levels, labels = pretty_lvl, ordered = TRUE)) %>%
    tidyr::pivot_wider(names_from = Answer, values_from = n, values_fill = 0) %>%
    mutate(Using = Low + Medium + High,
           Total = None + Using)

  # world (sum across regions)
  counts_world <- counts_reg %>%
    group_by(Tech) %>%
    summarise(across(c(all_of(pretty_lvl), Using, Total), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(Region = "World", .before = 1)

  counts_all <- bind_rows(counts_reg, counts_world) %>%
    arrange(factor(Region, levels = region_levels), Tech)

  # row-level percentages per region (Using% and level shares)
  pct_all <- counts_all %>%
    mutate(
      `Percent Using` = ifelse(Total > 0, 100 * Using / Total, NA_real_),
      `Percent None`  = ifelse(Total > 0, 100 * None  / Total, NA_real_),
      `Percent Low`   = ifelse(Total > 0, 100 * Low   / Total, NA_real_),
      `Percent Medium`= ifelse(Total > 0, 100 * Medium/ Total, NA_real_),
      `Percent High`  = ifelse(Total > 0, 100 * High  / Total, NA_real_)
    )

  # Two main tables (percent Using) in the requested layouts
  mk_main_table <- function(techs){
    pct_all %>%
      filter(Tech %in% techs) %>%
      select(Region, Tech, `Percent Using`) %>%
      tidyr::pivot_wider(names_from = Tech, values_from = `Percent Using`) %>%
      mutate(across(where(is.numeric), ~ round(.x, 0))) %>%
      arrange(factor(Region, levels = region_levels))
  }

  list(
    counts_levels = counts_all,
    pct_levels    = pct_all,
    Table3E1_pct  = mk_main_table(G3E1),
    Table3E2_pct  = mk_main_table(G3E2)
  )
}

# ---- Build outputs
OUT_row_big5 <- make_row_outputs(d28_rows, species_filter = big5)
OUT_row_all  <- make_row_outputs(d28_rows, species_filter = NULL)

# ---------- Write Excel ----------
wb <- createWorkbook()
addWorksheet(wb, "README")

# Main tables (Percent Using) – ROW level
addWorksheet(wb, "3E1_big5_pct_ROW")
addWorksheet(wb, "3E2_big5_pct_ROW")
addWorksheet(wb, "3E1_all_pct_ROW")
addWorksheet(wb, "3E2_all_pct_ROW")

# Counts per level (ROW level)
addWorksheet(wb, "Counts_big5_levels_ROW")
addWorksheet(wb, "Counts_all_levels_ROW")

# Percent per level (ROW level, including None/Low/Medium/High & Using)
addWorksheet(wb, "Percents_big5_levels_ROW")
addWorksheet(wb, "Percents_all_levels_ROW")

# Filter dump for audit
addWorksheet(wb, "Filtered_Q28_rows")

readme <- paste(
  "Q28 – ROW-LEVEL method (no country collapsing):",
  "• Every row in the data is counted. Levels are the Answer values: none/low/medium/high.",
  "• Counts per Region×Technology are raw response counts across all species (\"all breeds together\").",
  "• Using = Low + Medium + High; Total = None + Using.",
  "• Percent Using = 100 * Using / Total, computed on row counts.",
  "• Sheets are provided for BIG-FIVE species only and for ALL species.",
  "• A World row is included (sum of regions).",
  sep = "\n"
)

writeData(wb, "README", readme)

writeData(wb, "3E1_big5_pct_ROW", OUT_row_big5$Table3E1_pct)
writeData(wb, "3E2_big5_pct_ROW", OUT_row_big5$Table3E2_pct)
writeData(wb, "3E1_all_pct_ROW",  OUT_row_all$Table3E1_pct)
writeData(wb, "3E2_all_pct_ROW",  OUT_row_all$Table3E2_pct)

writeData(wb, "Counts_big5_levels_ROW", OUT_row_big5$counts_levels)
writeData(wb, "Counts_all_levels_ROW",  OUT_row_all$counts_levels)

writeData(wb, "Percents_big5_levels_ROW",
          OUT_row_big5$pct_levels %>%
            mutate(across(where(is.numeric), ~ ifelse(!is.na(.x), round(.x, 1), NA_real_))) %>%
            arrange(factor(Region, levels = region_levels), Tech))

writeData(wb, "Percents_all_levels_ROW",
          OUT_row_all$pct_levels %>%
            mutate(across(where(is.numeric), ~ ifelse(!is.na(.x), round(.x, 1), NA_real_))) %>%
            arrange(factor(Region, levels = region_levels), Tech))

writeData(wb, "Filtered_Q28_rows",
          d28_rows %>% select(Region, Country, Species, Tech, Answer) %>%
            arrange(factor(Region, levels = region_levels), Country, Tech))

# Basic styling
bold <- createStyle(textDecoration = "bold")
int0 <- createStyle(numFmt = "0")
for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:1000, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:1000, widths = "auto")
}
for (sh in c("3E1_big5_pct_ROW","3E2_big5_pct_ROW","3E1_all_pct_ROW","3E2_all_pct_ROW",
             "Counts_big5_levels_ROW","Counts_all_levels_ROW")){
  df <- openxlsx::readWorkbook(wb, sh)
  if (!is.null(df) && nrow(df)) {
    num_cols <- which(vapply(df, is.numeric, TRUE))
    if (length(num_cols)) addStyle(wb, sh, int0, rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
  }
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved -> ", out_xlsx)





Table 3E3
# =====================================================================
# TABLE 3E3 — Global availability of technologies by species (Q28)
# n  = # countries that reported the species (any tech)
# t  = # countries using a tech (Low/Medium/High)
# Score = mean(0..3) across those n countries (None=0, Low=1, Medium=2, High=3)
# Missing tech answers for a country that reported the species are treated as 0 ("none")
# =====================================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# ---- Paths
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3E3_Technologies_by_Species.xlsx"

# ---- Species (exact column headers & order)
species_cols <- c("Dairy cattle","Beef cattle","Multi-purpose cattle","Sheep","Goats","Pigs","Chickens")

# ---- Map SpecieTag -> species text (so we're robust to where the species text comes from)
code_to_species <- c(
  "009"="Dairy cattle",          # cattle (specialized dairy)
  "008"="Beef cattle",           # cattle (specialized beef)
  "007"="Multi-purpose cattle",  # cattle (multipurpose)
  "042"="Sheep",
  "021"="Goats",
  "037"="Pigs",
  "010"="Chickens"
)

# ---- Technology list (row order must match the publication)
tech_order <- c(
  "Artificial insemination",
  "Embryo transfer",
  "Molecular genetic or genomic information",
  "Multiple ovulation and embryo transfer",
  "Semen sexing",
  "In vitro fertilization",
  "Cloning",
  "Genetic modification",
  "Transplantation of gonadal tissue"
)

# ---- Answer → score
lvl_map  <- c("none"=0,"low"=1,"medium"=2,"high"=3)

# ---------- helpers ----------
norm_region <- function(x){
  case_when(
    str_detect(x, regex("^africa$", TRUE)) ~ "Africa",
    str_detect(x, regex("^asia$", TRUE)) ~ "Asia",
    str_detect(x, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
    str_detect(x, regex("^europe", TRUE)) ~ "Europe",
    str_detect(x, regex("^latin\\s+america|caribbean|central america|south america", TRUE)) ~
      "Latin America and the Carribean",
    str_detect(x, regex("^north\\s*america", TRUE)) ~ "North America",
    str_detect(x, regex("^near\\s*east", TRUE)) ~ "Near East",
    str_detect(x, regex("^world$", TRUE)) ~ "World",
    TRUE ~ NA_character_
  )
}

species_from_cols <- function(bt, tl, st){
  # Try breedtype_2014 or tablelabel_2014 text first; then SpecieTag code
  key_text <- dplyr::coalesce(
    na_if(str_squish(str_to_lower(bt)),""),
    na_if(str_squish(str_to_lower(tl)),"")
  )
  # normalize key_text to our 7 species labels
  norm <- case_when(
    key_text %in% c("cattle (specialized dairy)", "dairy cattle")          ~ "Dairy cattle",
    key_text %in% c("cattle (specialized beef)",  "beef cattle")           ~ "Beef cattle",
    key_text %in% c("cattle (multipurpose)",      "multi-purpose cattle")  ~ "Multi-purpose cattle",
    key_text == "sheep"   ~ "Sheep",
    key_text == "goats"   ~ "Goats",
    key_text == "pigs"    ~ "Pigs",
    key_text == "chickens"~ "Chickens",
    TRUE ~ NA_character_
  )
  out <- ifelse(is.na(norm),
                recode(stringr::str_pad(st, 3, pad = "0"), !!!code_to_species, .default = NA_character_),
                norm)
  out
}

clean_tech <- function(x){
  z <- str_to_lower(str_squish(x))
  recode(z,
    "artificial insemination" = "Artificial insemination",
    "embryo transfer"         = "Embryo transfer",
    "molecular genetic or genomic information" = "Molecular genetic or genomic information",
    "multiple ovulation and embryo transfer"   = "Multiple ovulation and embryo transfer",
    "semen sexing"             = "Semen sexing",
    "in vitro fertilization"   = "In vitro fertilization",
    "cloning"                  = "Cloning",
    "genetic modification"     = "Genetic modification",
    "transplantation of gonadal tissue" = "Transplantation of gonadal tissue",
    .default = NA_character_
  )
}

# ---------- read ----------
raw <- read_excel(in_path)
names(raw) <- names(raw) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+","_") |>
  tolower()

pick <- function(...) { c <- unlist(list(...)); h <- c[c %in% names(raw)]; if (length(h)) h[1] else NA_character_ }

col_region   <- pick("region")
col_country  <- pick("country")
col_section  <- pick("section")
col_question <- pick("question_2024","question")
col_subqtxt  <- pick("subquestiontype_2014")
col_answer   <- pick("answer")
col_breed    <- pick("breedtype_2014")
col_tablelab <- pick("tablelabel_2014","species_clean")
col_specietag<- pick("specietag","specietagnew","specietag_new","species")

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_subqtxt), !is.na(col_answer))

# ---- Q28 only; keep 7 species and required techs/answers; drop World region
d28 <- tibble(
  Region   = norm_region(raw[[col_region]]),
  Country  = str_squish(as.character(raw[[col_country]])),
  Section  = raw[[col_section]],
  Question = raw[[col_question]],
  Tech0    = as.character(raw[[col_subqtxt]]),
  Answer0  = as.character(raw[[col_answer]]),
  Species0 = species_from_cols(raw[[col_breed]], raw[[col_tablelab]], raw[[col_specietag]])
) %>%
  filter(Section %in% c(2,"2"),
         grepl("^\\s*28(\\.|$)", as.character(Question)),
         !is.na(Region), Region != "World",
         !is.na(Species0), Species0 %in% species_cols) %>%
  mutate(
    Tech   = clean_tech(Tech0),
    Answer = str_to_lower(str_squish(Answer0)),
    Score  = recode(Answer, !!!lvl_map, .default = NA_real_)
  ) %>%
  filter(!is.na(Tech), Tech %in% tech_order,
         !is.na(Score)) %>%
  select(Region, Country, Species = Species0, Tech, Answer, Score)

# ---- Collapse to Country × Species × Tech: take the HIGHEST level a country reported
cst <- d28 %>%
  group_by(Country, Species, Tech) %>%
  summarise(Score = max(Score, na.rm = TRUE), .groups = "drop")

# ---- For each Species, determine its country set n (any tech reported)
species_country <- cst %>% distinct(Country, Species)

# ---- Build full grid Country×Species×Tech for those (Country,Species); fill missing tech as 0
full_grid <- species_country %>%
  crossing(Tech = tech_order) %>%
  left_join(cst, by = c("Country","Species","Tech")) %>%
  mutate(Score = coalesce(Score, 0))

# ---- t and Score per Species × Tech (GLOBAL)
t_scores <- full_grid %>%
  group_by(Species, Tech) %>%
  summarise(
    n_countries = n_distinct(Country),
    t_using     = sum(Score > 0, na.rm = TRUE),
    Score_avg   = mean(Score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Score = round(Score_avg, 1)) %>%
  select(Species, Tech, n = n_countries, t = t_using, Score)

# ---------- Make wide table: two sub-columns per species (t & Score)
make_wide <- function(df){
  # we want a single header row with technologies (rows) and for each species two cols: t and Score
  # Pivot twice then interleave
  wide_t <- df %>%
    select(Tech, Species, t) %>%
    pivot_wider(names_from = Species, values_from = t)

  wide_s <- df %>%
    select(Tech, Species, Score) %>%
    pivot_wider(names_from = Species, values_from = Score)

  # interleave t/Score columns in species_cols order
  out <- wide_t %>% arrange(match(Tech, tech_order))
  for (sp in species_cols) {
    if (!sp %in% names(wide_t)) out[[sp]] <- NA_integer_
  }
  out <- out %>% select(Tech, all_of(species_cols))

  out2 <- wide_s %>% arrange(match(Tech, tech_order))
  for (sp in species_cols) {
    if (!sp %in% names(out2)) out2[[sp]] <- NA_real_
  }
  out2 <- out2 %>% select(Tech, all_of(species_cols))

  # build final data frame with t/Score pairs
  final <- tibble(Technology = out$Tech)
  for (sp in species_cols) {
    final[[paste0(sp," t")]]     <- out[[sp]]
    final[[paste0(sp," Score")]] <- out2[[sp]]
  }
  final
}

Table_3E3 <- make_wide(t_scores)

# ---- Build an 'n=' header row (one row, showing n per species)
n_by_species <- t_scores %>%
  distinct(Species, n) %>%
  complete(Species = species_cols, fill = list(n = NA_integer_)) %>%
  arrange(match(Species, species_cols))

header_n <- tibble(Technology = paste0("n ="))  # first cell label
for (sp in species_cols) {
  nn <- n_by_species$n[n_by_species$Species == sp]
  header_n[[paste0(sp," t")]]     <- ifelse(length(nn) && !is.na(nn), as.integer(nn), NA_integer_)
  header_n[[paste0(sp," Score")]] <- ""  # blank in the Score column for the header row
}

# Prepend the header row above the main table (purely cosmetic for Excel)
Table_3E3_export <- bind_rows(header_n, Table_3E3)

# ---------- QA sheets (so you can audit)
QA_counts_levels <- full_grid %>%
  mutate(Level = factor(case_when(
    Score == 0 ~ "None",
    Score == 1 ~ "Low",
    Score == 2 ~ "Medium",
    Score == 3 ~ "High"
  ), levels = c("None","Low","Medium","High"))) %>%
  count(Species, Tech, Level, name = "n_rows") %>%
  arrange(Species, match(Tech, tech_order), Level)

QA_country_level <- full_grid %>%
  mutate(Level = factor(case_when(
    Score == 0 ~ "None",
    Score == 1 ~ "Low",
    Score == 2 ~ "Medium",
    Score == 3 ~ "High"
  ), levels = c("None","Low","Medium","High"))) %>%
  arrange(Species, Country, match(Tech, tech_order))

# ---------- Write Excel
wb <- createWorkbook()

addWorksheet(wb, "README")
addWorksheet(wb, "Table_3E3")
addWorksheet(wb, "Counts_by_level")
addWorksheet(wb, "Country_Species_Tech")

readme <- paste(
  "TABLE 3E3 — Global availability of technologies by species (Q28).",
  "",
  "Method:",
  "• Species considered: Dairy cattle, Beef cattle, Multi-purpose cattle, Sheep, Goats, Pigs, Chickens.",
  "• For each Country×Species×Technology, we take the highest level reported (None=0, Low=1, Medium=2, High=3).",
  "• For each Species:",
  "    n  = # distinct countries that reported that species (any technology).",
  "• For each Technology within the species:",
  "    t      = # countries with level > 0 (Low/Medium/High).",
  "    Score  = average 0..3 across the n countries (missing tech answers treated as 0).",
  "",
  "A header row with “n =” is included; Score cells in that header are left blank.",
  sep = "\n"
)

writeData(wb, "README", readme)
writeData(wb, "Table_3E3", Table_3E3_export)
writeData(wb, "Counts_by_level", QA_counts_levels)
writeData(wb, "Country_Species_Tech", QA_country_level)

# Styling
bold <- createStyle(textDecoration = "bold")
int0 <- createStyle(numFmt = "0")
num1 <- createStyle(numFmt = "0.0")

for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:500, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:500, widths = "auto")
}
# numbers in main table
df <- openxlsx::readWorkbook(wb, "Table_3E3")
if (!is.null(df) && nrow(df)) {
  # integer columns end with " t"
  t_cols <- grep(" t$", names(df))
  if (length(t_cols)) addStyle(wb, "Table_3E3", int0, rows = 2:(nrow(df)+1), cols = t_cols+1, gridExpand = TRUE)
  # score columns end with " Score"
  s_cols <- grep(" Score$", names(df))
  if (length(s_cols)) addStyle(wb, "Table_3E3", num1, rows = 2:(nrow(df)+1), cols = s_cols+1, gridExpand = TRUE)
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved -> ", out_xlsx)





ERROR CORRECTION
# --- FIX: build a type-safe header row and export ---

# n per species (how many countries reported that species at all)
n_by_species <- t_scores %>%
  distinct(Species, n) %>%
  tidyr::complete(Species = species_cols, fill = list(n = NA_integer_)) %>%
  arrange(match(Species, species_cols))

# Start from the existing wide table to inherit column types
header_n <- Table_3E3[0, ]          # zero-row data frame with correct types
header_n[1, ] <- NA                  # add one empty row (keeps types)
header_n$Technology[1] <- "n ="

# Fill the t and Score columns, keeping types (t = integer, Score = numeric NA)
for (sp in species_cols) {
  t_col <- paste0(sp, " t")
  s_col <- paste0(sp, " Score")
  nn <- n_by_species$n[n_by_species$Species == sp]
  header_n[[t_col]][1]  <- ifelse(length(nn) && !is.na(nn), as.integer(nn), NA_integer_)
  header_n[[s_col]][1]  <- NA_real_    # leave Score blank but numeric
}

# Prepend header row
Table_3E3_export <- dplyr::bind_rows(header_n, Table_3E3)

# ---------- Write Excel (re-run safely) ----------
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "README")
openxlsx::addWorksheet(wb, "Table_3E3")
openxlsx::addWorksheet(wb, "Counts_by_level")
openxlsx::addWorksheet(wb, "Country_Species_Tech")

readme <- paste(
  "TABLE 3E3 — Global availability of technologies by species (Q28).",
  "",
  "Method:",
  "• For each Country×Species×Technology, keep the highest level (None=0, Low=1, Medium=2, High=3).",
  "• For each Species: n = # distinct countries that reported that species (any technology).",
  "• For each Technology within the species:",
  "    t     = # countries with level > 0 (Low/Medium/High).",
  "    Score = average 0..3 across the n countries (missing tech for a country treated as 0).",
  "",
  "The first row shows 'n =' per species; Score cells in that row are left blank.",
  sep = "\n"
)

openxlsx::writeData(wb, "README", readme)
openxlsx::writeData(wb, "Table_3E3", Table_3E3_export)
openxlsx::writeData(wb, "Counts_by_level", QA_counts_levels)
openxlsx::writeData(wb, "Country_Species_Tech", QA_country_level)

# Styling
bold <- openxlsx::createStyle(textDecoration = "bold")
int0 <- openxlsx::createStyle(numFmt = "0")
num1 <- openxlsx::createStyle(numFmt = "0.0")

for (sh in openxlsx::sheets(wb)) {
  openxlsx::addStyle(wb, sh, bold, rows = 1, cols = 1:500, gridExpand = TRUE)
  openxlsx::setColWidths(wb, sh, cols = 1:500, widths = "auto")
}

df <- openxlsx::readWorkbook(wb, "Table_3E3")
if (!is.null(df) && nrow(df)) {
  t_cols <- grep(" t$", names(df))
  if (length(t_cols)) openxlsx::addStyle(wb, "Table_3E3", int0,
                                         rows = 2:(nrow(df)+1), cols = t_cols+1, gridExpand = TRUE)
  s_cols <- grep(" Score$", names(df))
  if (length(s_cols)) openxlsx::addStyle(wb, "Table_3E3", num1,
                                         rows = 2:(nrow(df)+1), cols = s_cols+1, gridExpand = TRUE)
}

out_xlsx <- "C:/Users/LENOVO/Documents/Fig3E3_Technologies_by_Species.xlsx"
openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved -> ", out_xlsx)





TABLE 3E4
# =========================
# TABLE 3E3 — Minor species
# =========================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# ---------- Paths ----------
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3E3_Technologies_by_MinorSpecies.xlsx"

# ---------- Species ----------
minor_species <- c(
  "alpacas","asses","bactrian camels","buffaloes","deer","dromedaries","ducks","geese",
  "guinea fowls","guinea pigs","horses","llamas","managed bee","mithun","muscovy ducks",
  "ostriches","pigeons","quails","rabbits","turkeys","yaks"
)

big7 <- c("cattle (specialized dairy)","cattle (specialized beef)","cattle (multipurpose)",
          "sheep","goats","pigs","chickens")

# SpecieTag -> species text (lower-case)
code_to_species <- c(
  "002"="alpacas", "003"="asses", "004"="bactrian camels", "005"="buffaloes",
  "007"="cattle (multipurpose)", "008"="cattle (specialized beef)", "009"="cattle (specialized dairy)",
  "010"="chickens", "013"="deer", "015"="dromedaries", "017"="ducks", "021"="goats", "022"="geese",
  "023"="guinea fowls", "024"="guinea pigs", "026"="horses", "027"="llamas", "028"="managed bee",
  "029"="mithun", "030"="muscovy ducks", "033"="ostriches", "037"="pigs", "038"="pigeons",
  "040"="quails", "041"="rabbits", "042"="sheep", "044"="turkeys", "046"="yaks"
)

to_title <- function(x){
  z <- str_to_title(x)
  z <- gsub(" Of ", " of ", z, fixed = TRUE)
  z <- gsub(" And ", " and ", z, fixed = TRUE)
  z
}
species_cols <- to_title(minor_species)

# ---------- Technologies (exact labels for rows, in order) ----------
tech_order <- c(
  "Artificial insemination",
  "Embryo transfer",
  "Molecular genetic or genomic information",
  "Multiple ovulation and embryo transfer",
  "Semen sexing",
  "In vitro fertilization",
  "Cloning",
  "Genetic modification",
  "Transplantation of gonadal tissue"
)

# Normalize tech text coming from SubquestionType_2014
normalize_tech <- function(x){
  key <- str_to_lower(str_squish(x))
  map <- c(
    "artificial insemination" = "Artificial insemination",
    "embryo transfer" = "Embryo transfer",
    "molecular genetic or genomic information" = "Molecular genetic or genomic information",
    "multiple ovulation and embryo transfer" = "Multiple ovulation and embryo transfer",
    "semen sexing" = "Semen sexing",
    "in vitro fertilization" = "In vitro fertilization",
    "cloning" = "Cloning",
    "genetic modification" = "Genetic modification",
    "transplantation of gonadal tissue" = "Transplantation of gonadal tissue"
  )
  unname(map[key])
}

# Answer -> score (0..3)
ans_to_score <- function(x){
  key <- str_to_lower(str_squish(x))
  recode(key, "none"=0, "low"=1, "medium"=2, "high"=3, .default = NA_real_)
}

# Robust column picker
pick_col <- function(raw, ...) {
  cands <- unlist(list(...))
  hit <- cands[cands %in% names(raw)]
  if (length(hit)) hit[1] else NA_character_
}

# Map species from any of: BreedType_2014, TableLabel_2014/Species_clean, SpecieTag
map_species_any <- function(bt, tl, st){
  dplyr::coalesce(
    na_if(str_to_lower(str_squish(bt)),""),
    na_if(str_to_lower(str_squish(tl)),""),
    recode(stringr::str_pad(st, 3, pad="0"), !!!code_to_species, .default = NA_character_)
  )
}

# ---------- Read + normalize ----------
raw <- read_excel(in_path)
names(raw) <- names(raw) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+","_") |>
  tolower()

col_year     <- pick_col(raw, "year")
col_region   <- pick_col(raw, "region")
col_country  <- pick_col(raw, "country")
col_section  <- pick_col(raw, "section")
col_question <- pick_col(raw, "question_2024","question")
col_subq     <- pick_col(raw, "subquestiontype_2014")
col_answer   <- pick_col(raw, "answer")
col_breed    <- pick_col(raw, "breedtype_2014")
col_tablelab <- pick_col(raw, "tablelabel_2014","species_clean")
col_specietag<- pick_col(raw, "specietag","specietagnew","specietag_new","species")

stopifnot(!is.na(col_country), !is.na(col_section), !is.na(col_question),
          !is.na(col_subq), !is.na(col_answer))

d28_raw <- tibble(
  Region   = as.character(raw[[col_region]]),
  Country  = str_squish(as.character(raw[[col_country]])),
  Section  = raw[[col_section]],
  Question = raw[[col_question]],
  Tech0    = as.character(raw[[col_subq]]),
  Answer0  = as.character(raw[[col_answer]]),
  Species0 = map_species_any(raw[[col_breed]], raw[[col_tablelab]], raw[[col_specietag]])
) %>%
  filter(Section %in% c(2,"2"),
         grepl("^\\s*28(\\.|$)", as.character(Question)),
         !is.na(Species0)) %>%
  mutate(
    species = str_to_lower(str_squish(Species0)),
    tech    = normalize_tech(Tech0),
    score   = ans_to_score(Answer0)
  )

# Keep only recognized minor species + recognized tech + valid answers
d28 <- d28_raw %>%
  filter(species %in% minor_species,
         !is.na(tech), tech %in% tech_order,
         !is.na(score))

# Countries that reported each species (n)
countries_by_species <- d28 %>%
  distinct(species, Country)

# Build full grid: Country x Species x Tech (for the species that country reported)
full_grid <- countries_by_species %>%
  tidyr::expand_grid(tech = tech_order) %>%
  left_join(d28 %>% select(species, Country, tech, score),
            by = c("species","Country","tech")) %>%
  group_by(species, Country, tech) %>%
  summarise(Score = max(score, na.rm = TRUE), .groups = "drop") %>%
  mutate(Score = ifelse(is.infinite(Score), 0, Score))  # if all NA -> 0

# n per species
n_by_species <- countries_by_species %>%
  count(species, name = "n")

# t and Score per species-tech
t_scores <- full_grid %>%
  left_join(n_by_species, by = "species") %>%
  group_by(species, tech, n) %>%
  summarise(
    t     = sum(Score > 0, na.rm = TRUE),
    Score = mean(Score, na.rm = TRUE),   # mean across the n countries (0 for missing already)
    .groups = "drop"
  )

# ---------- Make wide table (rows = Technology, columns = Species t / Species Score) ----------
make_wide_cols <- function(df, species_vec){
  # Ensure all species/tech present
  df2 <- df %>%
    tidyr::complete(species = species_vec, tech = tech_order, fill = list(n = NA_integer_, t = 0, Score = 0)) %>%
    arrange(match(tech, tech_order), match(species, species_vec))
  
  # wide for t
  w_t <- df2 %>%
    select(tech, species, t) %>%
    tidyr::pivot_wider(names_from = species, values_from = t) %>%
    rename(Technology = tech)
  
  # wide for Score
  w_s <- df2 %>%
    select(tech, species, Score) %>%
    tidyr::pivot_wider(names_from = species, values_from = Score) %>%
    rename(Technology = tech)
  
  # Interleave columns as "Species t" and "Species Score"
  out <- tibble(Technology = w_t$Technology)
  for (sp in species_vec) {
    out[[paste0(to_title(sp), " t")]]     <- as.integer(w_t[[sp]])
    out[[paste0(to_title(sp), " Score")]] <- as.numeric(w_s[[sp]])
  }
  out
}

Table_3E3 <- make_wide_cols(t_scores, minor_species)

# ---------- Build a type-safe "n =" header and prepend ----------
# Create zero-row slice to inherit column types
header_n <- Table_3E3[0, ]
header_n[1, ] <- NA
header_n$Technology[1] <- "n ="

# fill per-species n (t columns get n; Score columns remain numeric NA)
for (sp in minor_species) {
  nn <- n_by_species$n[n_by_species$species == sp]
  t_col <- paste0(to_title(sp), " t")
  s_col <- paste0(to_title(sp), " Score")
  header_n[[t_col]][1] <- ifelse(length(nn) && !is.na(nn), as.integer(nn), NA_integer_)
  header_n[[s_col]][1] <- NA_real_
}

Table_3E3_export <- dplyr::bind_rows(header_n, Table_3E3)

# ---------- QA sheets ----------
QA_counts_levels <- full_grid %>%
  mutate(Level = factor(case_when(
    Score == 0 ~ "None",
    Score == 1 ~ "Low",
    Score == 2 ~ "Medium",
    Score == 3 ~ "High"
  ), levels = c("None","Low","Medium","High"))) %>%
  count(Species = to_title(species), Technology = tech, Level, name = "n_rows") %>%
  arrange(Species, match(Technology, tech_order), Level)

QA_country_level <- full_grid %>%
  transmute(
    Species = to_title(species),
    Country,
    Technology = tech,
    Score
  ) %>%
  arrange(Species, Country, match(Technology, tech_order))

# ---------- Write Excel ----------
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "Table_3E3_minor")
addWorksheet(wb, "Counts_by_level")
addWorksheet(wb, "Country_Species_Tech")

readme <- paste(
  "TABLE 3E3 — Global availability of technologies by species (Q28), MINOR species only.",
  "",
  "Method:",
  "• Country×Species×Technology: keep the highest of none/low/medium/high (0–3).",
  "• For each Species (column pair):",
  "   n shown in the header row (equals # distinct countries reporting that species).",
  "• For each Technology (row) within the species:",
  "   t     = # countries with level > 0 (Low/Medium/High).",
  "   Score = mean 0..3 across the n countries (missing tech treated as 0).",
  "",
  "Species included:",
  paste0("   ", paste(to_title(minor_species), collapse = ", ")),
  sep = "\n"
)

writeData(wb, "README", readme)
writeData(wb, "Table_3E3_minor", Table_3E3_export)
writeData(wb, "Counts_by_level", QA_counts_levels)
writeData(wb, "Country_Species_Tech", QA_country_level)

# Styling
bold <- createStyle(textDecoration = "bold")
int0 <- createStyle(numFmt = "0")
num1 <- createStyle(numFmt = "0.0")

for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:1000, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:1000, widths = "auto")
}

# Numeric formats in the main table
df <- openxlsx::readWorkbook(wb, "Table_3E3_minor")
if (!is.null(df) && nrow(df)) {
  t_cols <- grep(" t$", names(df))
  if (length(t_cols)) addStyle(wb, "Table_3E3_minor", int0,
                               rows = 2:(nrow(df)+1), cols = t_cols+1, gridExpand = TRUE)
  s_cols <- grep(" Score$", names(df))
  if (length(s_cols)) addStyle(wb, "Table_3E3_minor", num1,
                               rows = 2:(nrow(df)+1), cols = s_cols+1, gridExpand = TRUE)
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved -> ", out_xlsx)








Table 3E4
# ==============================
# Q31 – Production systems x items (GLOBAL scores 0–3)
# + underlying count sheets
# ==============================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# ---- Paths
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig31_ProductionSystems_bySpecies.xlsx"

# ---- Helpers
norm_names <- function(x){
  x |> str_squish() |> str_replace_all("[^A-Za-z0-9]+","_") |> tolower()
}
pick_col <- function(df, ...) {
  cands <- unlist(list(...))
  hit <- cands[cands %in% names(df)]
  if (length(hit)) hit[1] else NA_character_
}

# ---- Read
raw <- read_excel(in_path)
names(raw) <- norm_names(names(raw))

# ---- Locate columns
col_region    <- pick_col(raw, "region")
col_country   <- pick_col(raw, "country")
col_section   <- pick_col(raw, "section")
col_question  <- pick_col(raw, "question_2024","question")
col_subqtxt   <- pick_col(raw, "subquestiontype_2014")   # Production system
col_breedtype <- pick_col(raw, "breedtype_2014")         # Item (AI variants, natural mating)
col_answer    <- pick_col(raw, "answer")
col_specietag <- pick_col(raw, "specietag","specietagnew","specietag_new","species")

stopifnot(!is.na(col_country), !is.na(col_section), !is.na(col_question),
          !is.na(col_subqtxt), !is.na(col_breedtype), !is.na(col_answer),
          !is.na(col_specietag))

# ---- Species mapping (big five split cattle; chickens excluded)
code_to_species <- c(
  "009"="Dairy cattle",
  "008"="Beef cattle",
  "007"="Multipurpose cattle",
  "042"="Sheep",
  "021"="Goats",
  "037"="Pigs",
  "010"="Chickens"
)
map_species <- function(st){
  recode(stringr::str_pad(st, 3, pad = "0"), !!!code_to_species, .default = NA_character_)
}
species_keep <- c("Dairy cattle","Beef cattle","Multipurpose cattle","Sheep","Goats","Pigs")

# ---- Production systems (row order)
prod_levels <- c(
  "Pastoralist systems",
  "Ranching or similar grassland-based production systems",
  "Mixed farming systems (rural areas)",
  "Small-scale urban or peri-urban systems",
  "Industrial systems"
)

# ---- Items (column order)
ai_items <- c(
  "Artificial insemination using imported semen from exotic breeds",
  "Artificial insemination using nationally produced semen from exotic breeds",
  "Artificial insemination using semen from locally adapted breeds",
  "Natural mating"
)

# ---- Map level text -> numeric score
level_map <- c("n/a"=0, "na"=0, "n.a."=0, "none"=0, "low"=1, "medium"=2, "high"=3)

# ---------- Normalize Q31 rows ----------
d31_raw <- tibble(
  Region      = if (!is.na(col_region)) raw[[col_region]] else NA_character_,
  Country     = str_squish(as.character(raw[[col_country]])),
  Section     = raw[[col_section]],
  Question    = raw[[col_question]],
  ProdSystem0 = as.character(raw[[col_subqtxt]]),
  Item0       = as.character(raw[[col_breedtype]]),
  Answer0     = as.character(raw[[col_answer]]),
  Species0    = map_species(raw[[col_specietag]])
) %>%
  filter(Section %in% c(2,"2"),
         grepl("^\\s*31(\\.|$)", as.character(Question)),
         !is.na(Species0),
         Species0 %in% species_keep) %>%
  mutate(
    ProdSystem = str_squish(ProdSystem0),
    Item       = str_squish(Item0),
    Answer_l   = str_to_lower(str_squish(Answer0)),
    Score      = unname(level_map[Answer_l]),
    Species    = Species0
  ) %>%
  filter(ProdSystem %in% prod_levels, Item %in% ai_items) %>%
  select(Region, Country, Species, ProdSystem, Item, Answer_l, Score)

# Collapse to ONE row per Country×Species×System×Item:
# - numeric score = max(score)
# - if max score is 0, keep "none" over "n/a" if present (so we can count them separately)
collapse_ctsi <- d31_raw %>%
  group_by(Species, Country, ProdSystem, Item) %>%
  summarise(
    Score = max(Score, na.rm = TRUE),
    had_none = any(Answer_l == "none", na.rm = TRUE),
    had_na   = any(Answer_l %in% c("n/a","na","n.a."), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Level_collapsed = case_when(
    Score == 3 ~ "high",
    Score == 2 ~ "medium",
    Score == 1 ~ "low",
    Score == 0 & had_none ~ "none",
    Score == 0 & !had_none & had_na ~ "n/a",
    Score == 0 ~ "none"   # fallback
  ))

# Universe of countries per species
species_countries <- collapse_ctsi %>% distinct(Species, Country)

# Expanded grid so missing combos are explicit and counted as 0 for averages
full_grid <- tidyr::expand_grid(
  Species    = unique(species_countries$Species),
  Country    = unique(species_countries$Country),
  ProdSystem = prod_levels,
  Item       = ai_items
) %>%
  inner_join(species_countries, by = c("Species","Country")) %>%   # keep only countries that reported that species
  left_join(collapse_ctsi, by = c("Species","Country","ProdSystem","Item")) %>%
  mutate(
    Missing_imputed = is.na(Score),
    Score = coalesce(Score, 0L),
    Level_for_counts = ifelse(Missing_imputed, "missing", Level_collapsed)
  )

# ---------- Main average table (0–3) ----------
avg_scores <- full_grid %>%
  group_by(Species, ProdSystem, Item) %>%
  summarise(Score = mean(Score, na.rm = TRUE), .groups = "drop")

Table_31 <- avg_scores %>%
  mutate(
    Species    = factor(Species, levels = c("Dairy cattle","Beef cattle","Multipurpose cattle","Sheep","Goats","Pigs")),
    ProdSystem = factor(ProdSystem, levels = prod_levels),
    Item       = factor(Item, levels = ai_items)
  ) %>%
  arrange(Species, ProdSystem, Item) %>%
  mutate(Score = round(Score, 1)) %>%
  tidyr::pivot_wider(names_from = Item, values_from = Score) %>%
  rename(
    `Imported semen from exotic breeds`            = `Artificial insemination using imported semen from exotic breeds`,
    `Nationally produced semen from exotic breeds` = `Artificial insemination using nationally produced semen from exotic breeds`,
    `Semen from locally adapted breeds`            = `Artificial insemination using semen from locally adapted breeds`,
    `Natural mating`                               = `Natural mating`
  ) %>%
  arrange(Species, ProdSystem) %>%
  rename(`Production system` = ProdSystem) %>%
  select(Species, `Production system`,
         `Imported semen from exotic breeds`,
         `Nationally produced semen from exotic breeds`,
         `Semen from locally adapted breeds`,
         `Natural mating`)

# ---------- UNDERLYING COUNT SHEETS ----------

# (A) Counts of REPORTED levels (no imputation)
Counts_reported_levels <- collapse_ctsi %>%
  mutate(Level = factor(Level_collapsed, levels = c("n/a","none","low","medium","high"))) %>%
  count(Species, ProdSystem, Item, Level, name = "n") %>%
  tidyr::pivot_wider(names_from = Level, values_from = n, values_fill = 0) %>%
  arrange(Species, match(ProdSystem, prod_levels), match(Item, ai_items))

# (B) Counts INCLUDING missing (explicit “missing” column)
Counts_with_missing <- full_grid %>%
  mutate(Level = factor(Level_for_counts, levels = c("n/a","none","low","medium","high","missing"))) %>%
  count(Species, ProdSystem, Item, Level, name = "n") %>%
  tidyr::pivot_wider(names_from = Level, values_from = n, values_fill = 0) %>%
  arrange(Species, match(ProdSystem, prod_levels), match(Item, ai_items))

# (C) Country-level grid (what the averages were computed from)
Country_level_expanded <- full_grid %>%
  mutate(Level_collapsed = ifelse(Level_for_counts == "missing", NA_character_, Level_for_counts)) %>%
  select(Species, Country, ProdSystem, Item, Score, Level_collapsed, Missing_imputed) %>%
  arrange(Species, Country, match(ProdSystem, prod_levels), match(Item, ai_items))

# (D) n per species
n_by_species <- species_countries %>% count(Species, name = "n_countries") %>% arrange(Species)

# ---------- Write Excel ----------
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "Table_31")
addWorksheet(wb, "Counts_reported_levels")
addWorksheet(wb, "Counts_with_missing")
addWorksheet(wb, "Country_level_expanded")
addWorksheet(wb, "n_by_species")
addWorksheet(wb, "Source_rows")

readme <- paste(
  "Q31 – Production systems by species (GLOBAL).",
  "",
  "Species: Dairy cattle, Beef cattle, Multipurpose cattle, Sheep, Goats, Pigs (Chickens excluded).",
  "Production systems: Pastoralist; Ranching/grassland; Mixed (rural); Small-scale urban/peri-urban; Industrial.",
  "Items: imported exotic semen; nationally produced exotic semen; semen from locally adapted breeds; natural mating.",
  "",
  "Scoring for averages: N/A/None=0, Low=1, Medium=2, High=3.",
  "If multiple rows exist for a Country×Species×System×Item, the highest level is used.",
  "If max level = 0, we keep 'none' over 'n/a' when counting levels; missing combinations are imputed as 0 for averages and",
  "are labelled 'missing' in the counts-with-missing sheet.",
  sep = "\n"
)

writeData(wb, "README", readme)
writeData(wb, "Table_31",               Table_31)
writeData(wb, "Counts_reported_levels", Counts_reported_levels)
writeData(wb, "Counts_with_missing",    Counts_with_missing)
writeData(wb, "Country_level_expanded", Country_level_expanded)
writeData(wb, "n_by_species",           n_by_species)
writeData(wb, "Source_rows",            d31_raw %>% arrange(Species, Country, ProdSystem, Item))

# styling
bold <- createStyle(textDecoration = "bold")
num1 <- createStyle(numFmt = "0.0")
int0 <- createStyle(numFmt = "0")

for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:300, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:300, widths = "auto")
}

# numeric formatting for main table
df <- openxlsx::readWorkbook(wb, "Table_31")
if (!is.null(df) && nrow(df)) {
  sc <- which(vapply(df, is.numeric, TRUE))
  if (length(sc)) addStyle(wb, "Table_31", num1, rows = 2:(nrow(df)+1), cols = sc+1, gridExpand = TRUE)
}

# ints for count sheets
for (sh in c("Counts_reported_levels","Counts_with_missing")) {
  dfc <- openxlsx::readWorkbook(wb, sh)
  if (!is.null(dfc) && nrow(dfc)) {
    num_cols <- which(vapply(dfc, is.numeric, TRUE))
    if (length(num_cols)) addStyle(wb, sh, int0, rows = 2:(nrow(dfc)+1), cols = num_cols+1, gridExpand = TRUE)
  }
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved -> ", out_xlsx)






Table 3E6
# ==============================================================
# Q29 – Stakeholders involved in AI / ET (YES/NO)  ->  % of countries
# Output workbook layout:
#   - Main_table           (your figure/table layout)
#   - Counts_YesNo_byRegionTech
#   - Percents_byRegionTech
#   - Country_counts
#   - Country_Tech_Stakeholder (collapsed YES/NO per country)
#   - Filtered_Q29           (auditable raw slice)
#   - README
# ==============================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# ---- Paths
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3B1_Q29_Stakeholders_AI_ET.xlsx"

# ---- Regions (display order)
region_levels <- c("Africa","Asia","Southwest Pacific","Europe",
                   "Latin America and the Carribean","North America",
                   "Near East","World")

# ---- Column helpers
norm_names <- function(x) {
  x |> str_squish() |> str_replace_all("[^A-Za-z0-9]+","_") |> tolower()
}
pick_col <- function(df, ...) {
  cands <- unlist(list(...)); hit <- intersect(cands, names(df))
  if (length(hit)) hit[1] else NA_character_
}
norm_region <- function(x){
  case_when(
    str_detect(x, regex("^africa$", TRUE)) ~ "Africa",
    str_detect(x, regex("^asia$", TRUE)) ~ "Asia",
    str_detect(x, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
    str_detect(x, regex("^europe", TRUE)) ~ "Europe",
    str_detect(x, regex("^latin\\s+america|caribbean|central america|south america", TRUE)) ~
      "Latin America and the Carribean",
    str_detect(x, regex("^north\\s*america", TRUE)) ~ "North America",
    str_detect(x, regex("^near\\s*east", TRUE)) ~ "Near East",
    str_detect(x, regex("^world$", TRUE)) ~ "World",
    TRUE ~ NA_character_
  )
}

# ---- Read once & locate columns
raw <- read_excel(in_path)
names(raw) <- norm_names(names(raw))

col_region  <- pick_col(raw, "region")
col_country <- pick_col(raw, "country")
col_section <- pick_col(raw, "section")
col_question<- pick_col(raw, "question_2024","question")
col_answer  <- pick_col(raw, "answer")
col_btype   <- pick_col(raw, "breedtype_2014")         # technology (AI / ET)
col_subqt   <- pick_col(raw, "subquestiontype_2014")   # stakeholder

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_answer),
          !is.na(col_btype), !is.na(col_subqt))

# ---- Canonical forms for tech & stakeholders
canon <- function(x){
  x |>
    str_replace_all("[\u2018\u2019]", "'") |>   # curly → straight apostrophes
    str_squish()
}

tech_map <- c(
  "artificial insemination" = "AI",
  "embryo transfer"         = "ET"
)

stakeholders <- c(
  "Breeders' associations or cooperatives",
  "Donors and development agencies",
  "External commercial companies",
  "National commercial companies",
  "National non-governmental organizations",
  "Public sector"
)

# Build regexes that match the stakeholders robustly
stake_re <- list(
  breeders   = regex("^breeders'?\\s+associations?\\s+or\\s+cooperatives$", TRUE),
  donors     = regex("^donors?\\s+and\\s+development\\s+agencies$", TRUE),
  external   = regex("^external\\s+commercial\\s+companies$", TRUE),
  national_c = regex("^national\\s+commercial\\s+companies$", TRUE),
  nngo       = regex("^national\\s+non-?governmental\\s+organizations$", TRUE),
  public     = regex("^public\\s+sector$", TRUE)
)

map_stake <- function(x){
  x <- canon(x)
  case_when(
    str_detect(x, stake_re$breeders)   ~ "Breeders' associations or cooperatives",
    str_detect(x, stake_re$donors)     ~ "Donors and development agencies",
    str_detect(x, stake_re$external)   ~ "External commercial companies",
    str_detect(x, stake_re$national_c) ~ "National commercial companies",
    str_detect(x, stake_re$nngo)       ~ "National non-governmental organizations",
    str_detect(x, stake_re$public)     ~ "Public sector",
    TRUE ~ NA_character_
  )
}

map_tech <- function(x){
  x0 <- str_to_lower(canon(x))
  recode(x0, !!!tech_map, .default = NA_character_)
}

# ---- Slice the data for Q29
d29_raw <- tibble(
  Region0   = raw[[col_region]],
  Country   = str_squish(as.character(raw[[col_country]])),
  Section   = raw[[col_section]],
  Question  = raw[[col_question]],
  Answer0   = as.character(raw[[col_answer]]),
  Tech0     = as.character(raw[[col_btype]]),
  Stake0    = as.character(raw[[col_subqt]])
)

d29 <- d29_raw %>%
  mutate(
    Region = norm_region(Region0),
    Tech   = map_tech(Tech0),
    Stake  = map_stake(Stake0),
    Ans    = str_to_lower(str_squish(Answer0))
  ) %>%
  filter(Section %in% c(2,"2"),
         grepl("^\\s*29(\\.|$)", as.character(Question)),
         !is.na(Region), Region != "World",
         !is.na(Tech), !is.na(Stake),
         Ans %in% c("yes","no")) %>%
  select(Region, Country, Tech, Stake, Ans)

# ---- Collapse to Country × Tech × Stake (YES if ANY yes among rows)
country_stake <- d29 %>%
  group_by(Region, Country, Tech, Stake) %>%
  summarise(Yes = any(Ans == "yes"),
            Reported = n()>0, .groups = "drop")

# ---- # Countries that reported the technology (denominator per Region×Tech)
country_counts <- d29 %>%
  distinct(Region, Country, Tech) %>%
  count(Region, Tech, name = "Number_of_countries")

# ---- YES & NO counts per Region×Tech×Stake
yes_counts <- country_stake %>%
  group_by(Region, Tech, Stake) %>%
  summarise(YES = sum(Yes), .groups = "drop") %>%
  left_join(country_counts, by = c("Region","Tech")) %>%
  mutate(NO = pmax(Number_of_countries - YES, 0L),
         pct_yes = ifelse(Number_of_countries > 0,
                          100 * YES / Number_of_countries, NA_real_))

# ---- Add WORLD by summing numerators and denominators (not averaging %)
world_counts <- yes_counts %>%
  group_by(Tech, Stake) %>%
  summarise(YES = sum(YES, na.rm = TRUE),
            Number_of_countries = sum(Number_of_countries, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(Region = "World",
         NO = pmax(Number_of_countries - YES, 0L),
         pct_yes = ifelse(Number_of_countries > 0,
                          100 * YES / Number_of_countries, NA_real_)) %>%
  select(Region, everything())

yes_counts_all <- bind_rows(
  yes_counts,
  world_counts
) %>%
  mutate(
    Region = factor(Region, levels = region_levels),
    Stake  = factor(Stake,  levels = stakeholders),
    Tech   = factor(Tech,   levels = c("AI","ET"))
  ) %>%
  arrange(Region, Tech, Stake)

# ---- Main table (your layout)
Main_table <- yes_counts_all %>%
  select(Region, Tech, Stake, Number_of_countries, pct_yes) %>%
  mutate(pct_yes = round(pct_yes, 0)) %>%
  # one row per Region×Tech, stakeholder % as columns
  pivot_wider(
    id_cols = c(Region, Tech, Number_of_countries),
    names_from = Stake, values_from = pct_yes
  ) %>%
  arrange(Region, Tech) %>%
  # move Number_of_countries right after Tech with your header name
  rename(`Number of countries` = Number_of_countries)

# ---- Percent table (long → wide, keeps decimals)
Percents_byRegionTech <- yes_counts_all %>%
  select(Region, Tech, Stake, pct_yes) %>%
  mutate(pct_yes = round(pct_yes, 1)) %>%
  pivot_wider(names_from = Stake, values_from = pct_yes) %>%
  arrange(Region, Tech)

# ---- YES/NO counts (for audit)
Counts_YesNo_byRegionTech <- yes_counts_all %>%
  select(Region, Tech, Stake, YES, NO, Number_of_countries) %>%
  arrange(Region, Tech, Stake)

# ---- Country×Tech presence (denominators)
Country_counts <- country_counts %>%
  arrange(factor(Region, levels = region_levels), Tech)

# ---- Country-level collapsed YES/NO (audit)
Country_Tech_Stakeholder <- country_stake %>%
  arrange(factor(Region, levels = region_levels), Country, Tech, Stake)

# ---- Keep the filtered slice (raw-ish)
Filtered_Q29 <- d29 %>%
  arrange(factor(Region, levels = region_levels), Country, Tech, Stake, Ans)

# ==============================================================
# Write Excel
# ==============================================================
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "Main_table")
addWorksheet(wb, "Percents_byRegionTech")
addWorksheet(wb, "Counts_YesNo_byRegionTech")
addWorksheet(wb, "Country_counts")
addWorksheet(wb, "Country_Tech_Stakeholder")
addWorksheet(wb, "Filtered_Q29")

readme <- paste(
  "Q29 – Stakeholders involved in AI / ET (YES/NO).",
  "",
  "Definitions:",
  "  • Technology (BreedType_2014): Artificial insemination (AI) or Embryo transfer (ET).",
  "  • Stakeholders (SubquestionType_2014):",
  "      - Breeders' associations or cooperatives",
  "      - Donors and development agencies",
  "      - External commercial companies",
  "      - National commercial companies",
  "      - National non-governmental organizations",
  "      - Public sector",
  "",
  "Method:",
  "  1) Collapse to Country×Technology×Stakeholder; YES if any row = 'yes'.",
  "  2) Number of countries (denominator) per Region×Technology =",
  "     count of distinct countries that reported that technology (any stakeholder, yes or no).",
  "  3) Percent shown for each stakeholder = 100 × (# countries with YES) / (Number of countries).",
  "  4) World row is built by summing regional numerators & denominators (not averaging %).",
  sep = "\n"
)

writeData(wb, "README",                    readme)
writeData(wb, "Main_table",                Main_table)
writeData(wb, "Percents_byRegionTech",     Percents_byRegionTech)
writeData(wb, "Counts_YesNo_byRegionTech", Counts_YesNo_byRegionTech)
writeData(wb, "Country_counts",            Country_counts)
writeData(wb, "Country_Tech_Stakeholder",  Country_Tech_Stakeholder)
writeData(wb, "Filtered_Q29",              Filtered_Q29)

# Styling
bold <- createStyle(textDecoration = "bold")
int0 <- createStyle(numFmt = "0")
for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:300, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:300, widths = "auto")
}
# make percent columns in Main_table integers
df <- openxlsx::readWorkbook(wb, "Main_table")
if (!is.null(df) && nrow(df)) {
  num_cols <- which(vapply(df, is.numeric, TRUE))
  if (length(num_cols)) addStyle(wb, "Main_table", int0,
                                 rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved -> ", out_xlsx)




WITH OTHER TECHNOLOGY
# ==============================================================
# Q29 – Stakeholders by technology (custom tech list, NOT AI/ET)
# We compute, for each Region × Technology:
#   • Number of countries reporting that technology
#   • % of countries with YES for each stakeholder
# Plus audit sheets: counts, percents, denominators, country-level.
# ==============================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# ---- Paths
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3B1_Q29_Stakeholders_BY_TECH.xlsx"

# ---- Region utilities (same as before)
norm_names <- function(x) x |> str_squish() |> str_replace_all("[^A-Za-z0-9]+","_") |> tolower()
pick_col <- function(df, ...) {
  cands <- unlist(list(...)); hit <- intersect(cands, names(df))
  if (length(hit)) hit[1] else NA_character_
}
norm_region <- function(x){
  case_when(
    str_detect(x, regex("^africa$", TRUE)) ~ "Africa",
    str_detect(x, regex("^asia$", TRUE)) ~ "Asia",
    str_detect(x, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
    str_detect(x, regex("^europe", TRUE)) ~ "Europe",
    str_detect(x, regex("^latin\\s+america|caribbean|central america|south america", TRUE)) ~
      "Latin America and the Carribean",
    str_detect(x, regex("^north\\s*america", TRUE)) ~ "North America",
    str_detect(x, regex("^near\\s*east", TRUE)) ~ "Near East",
    str_detect(x, regex("^world$", TRUE)) ~ "World",
    TRUE ~ NA_character_
  )
}
region_levels <- c("Africa","Asia","Southwest Pacific","Europe",
                   "Latin America and the Carribean","North America",
                   "Near East","World")

# ---- Load once
raw <- read_excel(in_path)
names(raw) <- norm_names(names(raw))

col_region  <- pick_col(raw, "region")
col_country <- pick_col(raw, "country")
col_section <- pick_col(raw, "section")
col_question<- pick_col(raw, "question_2024","question")
col_answer  <- pick_col(raw, "answer")
col_btype   <- pick_col(raw, "breedtype_2014")        # TECHNOLOGY names
col_subqt   <- pick_col(raw, "subquestiontype_2014")  # STAKEHOLDER names

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_answer),
          !is.na(col_btype), !is.na(col_subqt))

# ---- Stakeholders (same set as earlier)
stakeholders <- c(
  "Breeders' associations or cooperatives",
  "Donors and development agencies",
  "External commercial companies",
  "National commercial companies",
  "National non-governmental organizations",
  "Public sector"
)
stake_re <- list(
  breeders   = regex("^breeders'?\\s+associations?\\s+or\\s+cooperatives$", TRUE),
  donors     = regex("^donors?\\s+and\\s+development\\s+agencies$", TRUE),
  external   = regex("^external\\s+commercial\\s+companies$", TRUE),
  national_c = regex("^national\\s+commercial\\s+companies$", TRUE),
  nngo       = regex("^national\\s+non-?governmental\\s+organizations$", TRUE),
  public     = regex("^public\\s+sector$", TRUE)
)
map_stake <- function(x){
  x <- x |>
    str_replace_all("[\u2018\u2019]", "'") |>
    str_squish()
  case_when(
    str_detect(x, stake_re$breeders)   ~ "Breeders' associations or cooperatives",
    str_detect(x, stake_re$donors)     ~ "Donors and development agencies",
    str_detect(x, stake_re$external)   ~ "External commercial companies",
    str_detect(x, stake_re$national_c) ~ "National commercial companies",
    str_detect(x, stake_re$nngo)       ~ "National non-governmental organizations",
    str_detect(x, stake_re$public)     ~ "Public sector",
    TRUE ~ NA_character_
  )
}

# ---- EXACT output technologies (order preserved)
tech_targets <- c(
  "Cloning",
  "DNA paternity testing",
  "MOET",
  "Semen sexing",
  "In vitro fertilization",
  "Molecular genetic or genomic information",
  "Molecular genetic or genomic information/Genética molecular or Información genómica",
  "Use of molecular genetic or genomic information for prediction of breeding values",
  "Sexado de semen",
  "Superovulación y transferencia de embriones",
  "Heat induction and Synchronization",
  "Semen cryopreservation",
  "Use of genomic data / Utilisation de données génomiques pour l'évaluation génétique ou d'autres applications",
  "Semen sexing / Sexage de la semence",
  "Fecundación in vitro"
)

# Robust mapper from BreedType_2014 to the EXACT labels above
norm_text <- function(x){
  x |>
    str_replace_all("[\u2018\u2019]", "'") |>
    str_to_lower() |>
    str_squish()
}
map_tech_custom <- function(x){
  z <- norm_text(x)
  case_when(
    str_detect(z, "^cloning$") ~ "Cloning",
    str_detect(z, "^dna paternity testing$") ~ "DNA paternity testing",
    str_detect(z, "^(moet|multiple ovulation and embryo transfer)$") ~ "MOET",
    str_detect(z, "^semen sexing$") ~ "Semen sexing",
    str_detect(z, "^in vitro fertilization$") ~ "In vitro fertilization",
    str_detect(z, "^molecular genetic or genomic information$") ~ "Molecular genetic or genomic information",
    str_detect(z, "^molecular genetic or genomic information/ ?gen(e|é)tica molecular (or|o) informaci(o|ó)n gen(o|ó)mica$") ~
      "Molecular genetic or genomic information/Genética molecular or Información genómica",
    str_detect(z, "^use of molecular genetic or genomic information for prediction of breeding values$") ~
      "Use of molecular genetic or genomic information for prediction of breeding values",
    str_detect(z, "^sexado de semen$") ~ "Sexado de semen",
    str_detect(z, "^superovulaci(o|ó)n y transferencia de embriones$") ~
      "Superovulación y transferencia de embriones",
    str_detect(z, "^heat induction and (synchronization|synchronisation)$") ~
      "Heat induction and Synchronization",
    str_detect(z, "^semen cryopreservation$") ~ "Semen cryopreservation",
    str_detect(z, "^use of genomic data( / utilisation de donn(é|e)es g(é|e)nomiques pour l'(é|e)valuation g(é|e)n(é|e)tique( ou d'autres applications)?)?$") ~
      "Use of genomic data / Utilisation de données génomiques pour l'évaluation génétique ou d'autres applications",
    str_detect(z, "^(semen sexing / sexage de la semence|sexage de la semence)$") ~
      "Semen sexing / Sexage de la semence",
    str_detect(z, "^fecundaci(o|ó)n in vitro$") ~ "Fecundación in vitro",
    TRUE ~ NA_character_
  )
}

# ---- Slice Q29 YES/NO
d29 <- tibble(
  Region0  = raw[[col_region]],
  Country  = str_squish(as.character(raw[[col_country]])),
  Section  = raw[[col_section]],
  Question = raw[[col_question]],
  Ans0     = as.character(raw[[col_answer]]),
  Tech0    = as.character(raw[[col_btype]]),
  Stake0   = as.character(raw[[col_subqt]])
) %>%
  mutate(
    Region = norm_region(Region0),
    Tech   = map_tech_custom(Tech0),
    Stake  = map_stake(Stake0),
    Ans    = str_to_lower(str_squish(Ans0))
  ) %>%
  filter(Section %in% c(2,"2"),
         grepl("^\\s*29(\\.|$)", as.character(Question)),
         !is.na(Region), Region != "World",
         !is.na(Tech), Tech %in% tech_targets,
         !is.na(Stake),
         Ans %in% c("yes","no")) %>%
  select(Region, Country, Tech, Stake, Ans)

# ---- Collapse to Country × Tech × Stake (YES if any yes among rows)
country_stake <- d29 %>%
  group_by(Region, Country, Tech, Stake) %>%
  summarise(Yes = any(Ans == "yes"),
            Reported = n()>0, .groups = "drop")

# ---- Denominators: # countries that reported that technology in the region
country_counts <- d29 %>%
  distinct(Region, Country, Tech) %>%
  count(Region, Tech, name = "Number_of_countries")

# ---- YES/NO counts and % YES
yes_counts <- country_stake %>%
  group_by(Region, Tech, Stake) %>%
  summarise(YES = sum(Yes), .groups = "drop") %>%
  left_join(country_counts, by = c("Region","Tech")) %>%
  mutate(NO = pmax(Number_of_countries - YES, 0L),
         pct_yes = ifelse(Number_of_countries > 0, 100 * YES / Number_of_countries, NA_real_))

# ---- WORLD aggregation (sum numerators & denominators)
world_counts <- yes_counts %>%
  group_by(Tech, Stake) %>%
  summarise(YES = sum(YES, na.rm = TRUE),
            Number_of_countries = sum(Number_of_countries, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(Region = "World",
         NO = pmax(Number_of_countries - YES, 0L),
         pct_yes = ifelse(Number_of_countries > 0, 100 * YES / Number_of_countries, NA_real_)) %>%
  select(Region, everything())

yes_counts_all <- bind_rows(yes_counts, world_counts) %>%
  mutate(
    Region = factor(Region, levels = region_levels),
    Tech   = factor(Tech, levels = tech_targets),
    Stake  = factor(Stake, levels = stakeholders)
  ) %>%
  arrange(Region, Tech, Stake)

# ---- Main table (Region × Technology with stakeholder % columns)
Main_table <- yes_counts_all %>%
  select(Region, Tech, Stake, Number_of_countries, pct_yes) %>%
  mutate(pct_yes = round(pct_yes, 0)) %>%
  pivot_wider(
    id_cols = c(Region, Tech, Number_of_countries),
    names_from = Stake, values_from = pct_yes
  ) %>%
  arrange(Region, Tech) %>%
  rename(`Number of countries` = Number_of_countries)

# ---- Percent and counts (audit)
Percents_byRegionTech <- yes_counts_all %>%
  select(Region, Tech, Stake, pct_yes) %>%
  mutate(pct_yes = round(pct_yes, 1)) %>%
  pivot_wider(names_from = Stake, values_from = pct_yes) %>%
  arrange(Region, Tech)

Counts_YesNo_byRegionTech <- yes_counts_all %>%
  select(Region, Tech, Stake, YES, NO, Number_of_countries) %>%
  arrange(Region, Tech, Stake)

Country_counts <- country_counts %>%
  arrange(factor(Region, levels = region_levels), factor(Tech, levels = tech_targets))

Country_Tech_Stakeholder <- country_stake %>%
  arrange(factor(Region, levels = region_levels), Country,
          factor(Tech, levels = tech_targets), factor(Stake, levels = stakeholders))

Filtered_Q29 <- d29 %>%
  arrange(factor(Region, levels = region_levels), Country,
          factor(Tech, levels = tech_targets), factor(Stake, levels = stakeholders), Ans)

# ==============================================================
# Write Excel
# ==============================================================
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "Main_table")
addWorksheet(wb, "Percents_byRegionTech")
addWorksheet(wb, "Counts_YesNo_byRegionTech")
addWorksheet(wb, "Country_counts")
addWorksheet(wb, "Country_Tech_Stakeholder")
addWorksheet(wb, "Filtered_Q29")

readme <- paste(
  "Q29 – Stakeholders involved, by technology (custom list; AI/ET excluded).",
  "",
  "Technologies included (exact labels as requested, order preserved):",
  paste0("  • ", paste(tech_targets, collapse = "\n  • ")),
  "",
  "Stakeholders:",
  "  • Breeders' associations or cooperatives",
  "  • Donors and development agencies",
  "  • External commercial companies",
  "  • National commercial companies",
  "  • National non-governmental organizations",
  "  • Public sector",
  "",
  "Method:",
  "  1) Collapse to Country×Technology×Stakeholder; YES if any row is 'yes'.",
  "  2) Denominator per Region×Technology = # distinct countries that reported that technology.",
  "  3) % YES per stakeholder = 100 × YES / Number of countries.",
  "  4) World row sums regional numerators & denominators.",
  sep = "\n"
)

writeData(wb, "README",                    readme)
writeData(wb, "Main_table",                Main_table)
writeData(wb, "Percents_byRegionTech",     Percents_byRegionTech)
writeData(wb, "Counts_YesNo_byRegionTech", Counts_YesNo_byRegionTech)
writeData(wb, "Country_counts",            Country_counts)
writeData(wb, "Country_Tech_Stakeholder",  Country_Tech_Stakeholder)
writeData(wb, "Filtered_Q29",              Filtered_Q29)

# Styling
bold <- createStyle(textDecoration = "bold")
int0  <- createStyle(numFmt = "0")

for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:400, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:400, widths = "auto")
}
# make stakeholder % in Main_table integers
df <- openxlsx::readWorkbook(wb, "Main_table")
if (!is.null(df) && nrow(df)) {
  num_cols <- which(vapply(df, is.numeric, TRUE))
  if (length(num_cols)) addStyle(wb, "Main_table", int0,
                                 rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved -> ", out_xlsx)








TABLE 3E7
# ==============================
# Q30 – Research (National vs International) by Region
# Technologies: AI, Embryo transfer or MOET, Semen sexing, In vitro fertilization, Cloning
# Stakeholders: Public or private research at national level (National),
#               Research undertaken as part of international collaboration (International)
# Output: one main table (percents), plus counts/percents/QA sheets
# ==============================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# ---- Paths
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/FigQ30_Research_ByRegion.xlsx"

# ---- Region order (2024)
region_levels <- c("Africa","Asia","Southwest Pacific","Europe",
                   "Latin America and the Carribean","North America",
                   "Near East","World")

# ---- Helpers
norm_names <- function(x) x |> str_squish() |> str_replace_all("[^A-Za-z0-9]+","_") |> tolower()
pick_col <- function(df, ...) { c <- unlist(list(...)); h <- c[c %in% names(df)]; if (length(h)) h[1] else NA_character_ }

# ---- Read & standardize names
raw <- read_excel(in_path)
names(raw) <- norm_names(names(raw))

# ---- Find columns
col_region  <- pick_col(raw, "region")
col_country <- pick_col(raw, "country")
col_section <- pick_col(raw, "section")
col_question<- pick_col(raw, "question_2024","question")
col_answer  <- pick_col(raw, "answer")
col_subq    <- pick_col(raw, "subquestiontype_2014")  # stakeholder
col_breed   <- pick_col(raw, "breedtype_2014")        # technology

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_answer), !is.na(col_subq), !is.na(col_breed))

# ---- Map stakeholders (exact phrases you asked for)
pat_nat <- regex("^\\s*public or private research at national level\\s*$", ignore_case = TRUE)
pat_int <- regex("^\\s*research undertaken as part of international collaboration\\s*$", TRUE)

map_stakeholder <- function(x){
  case_when(
    str_detect(x, pat_nat) ~ "National",
    str_detect(x, pat_int) ~ "International",
    TRUE ~ NA_character_
  )
}

# ---- Technologies to keep (map ET variants -> Embryo transfer or MOET)
tech_keep <- c("Artificial insemination",
               "Embryo transfer or MOET",
               "Semen sexing",
               "In vitro fertilization",
               "Cloning")

map_tech <- function(x){
  x0 <- str_squish(as.character(x))
  case_when(
    str_detect(x0, regex("^embryo transfer", TRUE)) ~ "Embryo transfer or MOET",
    str_detect(x0, regex("^artificial insemination$", TRUE)) ~ "Artificial insemination",
    str_detect(x0, regex("^semen sexing$", TRUE)) ~ "Semen sexing",
    str_detect(x0, regex("^in vitro fertilization$", TRUE)) ~ "In vitro fertilization",
    str_detect(x0, regex("^cloning$", TRUE)) ~ "Cloning",
    TRUE ~ NA_character_
  )
}

# ---- Filter to Q30 and normalize
d30_raw <- tibble(
  Region   = str_squish(as.character(raw[[col_region]])),
  Country  = str_squish(as.character(raw[[col_country]])),
  Section  = raw[[col_section]],
  Question = raw[[col_question]],
  Stake0   = as.character(raw[[col_subq]]),
  Tech0    = as.character(raw[[col_breed]]),
  Answer0  = as.character(raw[[col_answer]])
) %>%
  filter(Section %in% c(2,"2"),
         grepl("^\\s*30(\\.|$)", as.character(Question)),
         !is.na(Region), !is.na(Country)) %>%
  mutate(
    Stake = map_stakeholder(Stake0),
    Tech  = map_tech(Tech0),
    ans   = str_to_lower(str_squish(Answer0))
  ) %>%
  filter(!is.na(Stake), !is.na(Tech),
         ans %in% c("yes","no"))

# ---- Number of countries per region (denominator for percents)
country_counts <- d30_raw %>%
  distinct(Region, Country) %>%
  count(Region, name = "Number_of_countries") %>%
  filter(Region != "World")

# ---- Collapse duplicates at Country×Tech×Stake (YES dominates NO)
d30_country <- d30_raw %>%
  mutate(is_yes = ans == "yes") %>%
  group_by(Region, Country, Tech, Stake) %>%
  summarise(YES = as.integer(any(is_yes)), .groups = "drop") %>%
  filter(Region != "World")

# ---- Counts of YES/NO by Region×Tech×Stake
counts_yes_no <- d30_country %>%
  group_by(Region, Tech, Stake) %>%
  summarise(YES = sum(YES, na.rm = TRUE),
            NO  = n() - YES,
            .groups = "drop") %>%
  left_join(country_counts, by = "Region")

# ---- Percents (YES / #countries in region)
percents <- counts_yes_no %>%
  mutate(pct = ifelse(Number_of_countries > 0, 100 * YES / Number_of_countries, NA_real_))

# ---- World row (sum numerators & denominators)
world_counts <- counts_yes_no %>%
  group_by(Tech, Stake) %>%
  summarise(YES = sum(YES), NO = sum(NO), Number_of_countries = sum(Number_of_countries), .groups = "drop") %>%
  mutate(Region = "World")

world_perc <- world_counts %>%
  mutate(pct = ifelse(Number_of_countries > 0, 100 * YES / Number_of_countries, NA_real_))

# ---- Assemble long table with percents (+ world)
perc_all <- bind_rows(
  percents %>% mutate(Region = factor(Region, levels = region_levels)),
  world_perc %>% mutate(Region = factor(Region, levels = region_levels))
) %>%
  mutate(
    Tech  = factor(Tech, levels = c("Artificial insemination","Embryo transfer or MOET",
                                    "Semen sexing","In vitro fertilization","Cloning")),
    Stake = factor(Stake, levels = c("National","International"))
  )

# ---- MAIN TABLE (wide, layout like your figure)
mk_colname <- function(tech, stake) paste0(tech, " – ", stake)

main_table <- perc_all %>%
  select(Region, Tech, Stake, Number_of_countries, pct) %>%
  mutate(col = mk_colname(as.character(Tech), as.character(Stake))) %>%
  select(Region, Number_of_countries, col, pct) %>%
  pivot_wider(names_from = col, values_from = pct) %>%
  arrange(Region) %>%
  mutate(across(-Region, ~ round(.x, 0))) %>%
  # ensure column order
  relocate(
    Region, Number_of_countries,
    `Artificial insemination – National`,
    `Artificial insemination – International`,
    `Embryo transfer or MOET – National`,
    `Embryo transfer or MOET – International`,
    `Semen sexing – National`,
    `Semen sexing – International`,
    `In vitro fertilization – National`,
    `In vitro fertilization – International`,
    `Cloning – National`,
    `Cloning – International`
  )

# ---- Also provide long sheets for counts and percents, plus the filtered rows used
counts_long <- bind_rows(
  counts_yes_no,
  world_counts
) %>%
  arrange(factor(Region, levels = region_levels), Tech, Stake)

percents_long <- perc_all %>%
  select(Region, Tech, Stake, Number_of_countries, Percent = pct) %>%
  arrange(Region, Tech, Stake)

filtered_rows <- d30_raw %>%
  transmute(
    Region, Country,
    Technology = map_tech(Tech0),
    Stakeholder = map_stakeholder(Stake0),
    Answer = str_to_lower(str_squish(Answer0))
  ) %>%
  filter(!is.na(Technology), !is.na(Stakeholder), Answer %in% c("yes","no")) %>%
  arrange(Region, Country, Technology, Stakeholder)

# ---- Write Excel
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "Main_table")
addWorksheet(wb, "Counts_yes_no")
addWorksheet(wb, "Percents_long")
addWorksheet(wb, "Country_counts")
addWorksheet(wb, "Filtered_Q30")

readme <- paste(
  "Q30 — Research at national vs international level for reproduction/biotech (by region).",
  "",
  "Stakeholders considered:",
  "  • Public or private research at national level  → “National”",
  "  • Research undertaken as part of international collaboration  → “International”",
  "Technologies considered:",
  "  • Artificial insemination; Embryo transfer or MOET; Semen sexing; In vitro fertilization; Cloning.",
  "",
  "Method:",
  "  • For each Region×Country×Technology×Stakeholder, YES dominates NO if duplicates exist.",
  "  • Number of countries = distinct countries with ANY Q30 record in the region.",
  "  • Percent in the main table = 100 × (# countries with YES for that Technology×Stakeholder) / (Number of countries in region).",
  "  • World row sums numerators and denominators across regions (not an average of %).",
  sep = "\n"
)

writeData(wb, "README",         readme)
writeData(wb, "Main_table",     main_table)
writeData(wb, "Counts_yes_no",  counts_long)
writeData(wb, "Percents_long",  percents_long)
writeData(wb, "Country_counts", country_counts %>%
            bind_rows(world_counts %>% distinct(Region, Number_of_countries)) %>%
            arrange(factor(Region, levels = region_levels)))
writeData(wb, "Filtered_Q30",   filtered_rows)

# Basic styling
bold <- createStyle(textDecoration = "bold")
int0 <- createStyle(numFmt = "0")
for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:300, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:300, widths = "auto")
}
# integers on % columns
df <- openxlsx::readWorkbook(wb, "Main_table")
if (!is.null(df) && nrow(df)) {
  num_cols <- which(vapply(df, is.numeric, TRUE))
  if (length(num_cols))
    addStyle(wb, "Main_table", int0, rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved -> ", out_xlsx)






TABLE 3E9
# ==============================
# 3D9 (2024 only) — AI & ET by region from Q28
# Using = Low/Medium/High; Not using = None
# ==============================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(openxlsx)
})

# ---- Paths
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3D9_2024_AI_ET_byRegion.xlsx"

# ---- Region order (keep EXACT labels you want to see)
region_levels <- c(
  "Africa","Asia","Southwest Pacific","Europe",
  "Latin America and the Carribean","North America",
  "Near East","World"
)

# Normalise whatever is in the file onto the above labels
norm_region <- function(x){
  x0 <- str_squish(as.character(x))
  case_when(
    str_detect(x0, regex("^africa$", TRUE)) ~ "Africa",
    str_detect(x0, regex("^asia$", TRUE)) ~ "Asia",
    str_detect(x0, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
    str_detect(x0, regex("^europe", TRUE)) ~ "Europe",
    # map ANY 'Latin America ... Caribbean/Carribean' variant to your display label
    str_detect(x0, regex("^latin\\s*america.*carib", TRUE)) ~ "Latin America and the Carribean",
    str_detect(x0, regex("^north\\s*america", TRUE)) ~ "North America",
    str_detect(x0, regex("^near\\s*east", TRUE)) ~ "Near East",
    str_detect(x0, regex("^world$", TRUE)) ~ "World",
    TRUE ~ x0
  )
}

# ---- Helpers
norm_names <- function(x) x |> str_squish() |> str_replace_all("[^A-Za-z0-9]+","_") |> tolower()
pick <- function(df, ...) { c <- unlist(list(...)); h <- c[c %in% names(df)]; if (length(h)) h[1] else NA_character_ }

# ---- Read
raw <- read_excel(in_path)
names(raw) <- norm_names(names(raw))

col_region   <- pick(raw, "region")
col_country  <- pick(raw, "country")
col_section  <- pick(raw, "section")
col_question <- pick(raw, "question_2024","question")
col_subq     <- pick(raw, "subquestiontype_2014")  # <-- AI / ET here per your request
col_answer   <- pick(raw, "answer")

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_subq), !is.na(col_answer))

# ---- Map tech from SubquestionType_2014
map_tech <- function(x){
  x0 <- str_squish(as.character(x))
  case_when(
    str_detect(x0, regex("^artificial\\s+insemination\\b", TRUE)) ~ "Artificial insemination",
    str_detect(x0, regex("^embryo\\s+transfer\\b", TRUE))         ~ "Embryo transfer",
    TRUE ~ NA_character_
  )
}

# ---- Map level to Using flag
level_to_num <- c("none"=0, "low"=1, "medium"=2, "high"=3)

d28 <- tibble(
  Region   = norm_region(raw[[col_region]]),          # <-- normalize region names
  Country  = str_squish(as.character(raw[[col_country]])),
  Section  = raw[[col_section]],
  Question = raw[[col_question]],
  Tech0    = as.character(raw[[col_subq]]),
  Answer0  = as.character(raw[[col_answer]])
) %>%
  filter(Section %in% c(2,"2"),
         grepl("^\\s*28(\\.|$)", as.character(Question)),
         !is.na(Region), !is.na(Country)) %>%
  mutate(
    Tech     = map_tech(Tech0),
    level_l  = tolower(str_squish(Answer0)),
    LevelNum = unname(level_to_num[level_l])
  ) %>%
  filter(!is.na(Tech), !is.na(LevelNum))

# ---- Collapse to one record per Region × Country × Tech (highest level)
ct <- d28 %>%
  group_by(Region, Country, Tech) %>%
  summarise(Level = max(LevelNum, na.rm = TRUE), .groups = "drop") %>%
  mutate(Using = Level > 0)

# ---- Regional stats
reg_stats <- ct %>%
  group_by(Region, Tech) %>%
  summarise(
    n_countries = n_distinct(Country),
    using_count = sum(Using),
    pct_using   = ifelse(n_countries > 0, 100 * using_count / n_countries, NA_real_),
    .groups = "drop"
  ) %>%
  filter(Region != "World")

# ---- World row
world_stats <- reg_stats %>%
  group_by(Tech) %>%
  summarise(
    n_countries = sum(n_countries),
    using_count = sum(using_count),
    pct_using   = ifelse(n_countries > 0, 100 * using_count / n_countries, NA_real_),
    .groups = "drop"
  ) %>%
  mutate(Region = "World")

stats_all <- bind_rows(reg_stats, world_stats)

# ---- Ensure ALL regions appear (and in your exact order), fill missing with 0
tech_levels <- c("Artificial insemination","Embryo transfer")
scaffold <- tidyr::expand_grid(Region = region_levels, Tech = tech_levels)

stats_filled <- scaffold %>%
  left_join(stats_all, by = c("Region","Tech")) %>%
  mutate(
    n_countries = coalesce(as.integer(n_countries), 0L),
    using_count = coalesce(as.integer(using_count), 0L),
    pct_using   = coalesce(round(pct_using, 0), 0)
  )

# ---- Main table (wide)
main <- stats_filled %>%
  select(Region, Tech, n_countries, pct_using) %>%
  mutate(
    Region = factor(Region, levels = region_levels),
    Tech   = factor(Tech,   levels = tech_levels)
  ) %>%
  arrange(Region, Tech) %>%
  pivot_wider(
    names_from = Tech, values_from = c(n_countries, pct_using)
  ) %>%
  rename(
    `n (AI)`  = `n_countries_Artificial insemination`,
    `AI %`    = `pct_using_Artificial insemination`,
    `n (ET)`  = `n_countries_Embryo transfer`,
    `ET %`    = `pct_using_Embryo transfer`
  ) %>%
  arrange(Region)

# ---- QA sheets
country_level <- ct %>%
  arrange(factor(Region, levels = region_levels), Country, Tech)

counts_yesno <- ct %>%
  group_by(Region, Tech) %>%
  summarise(YES = sum(Using), NO = n_distinct(Country) - YES, .groups = "drop") %>%
  complete(Region = region_levels, Tech = tech_levels, fill = list(YES = 0, NO = 0)) %>%
  arrange(factor(Region, levels = region_levels), Tech)

# ---- Write Excel
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "3D9_2024_main")
addWorksheet(wb, "Counts_YesNo_byRegionTech")
addWorksheet(wb, "Country_Tech_Level")

readme <- paste(
  "3D9 (2024 only) — Use of Artificial insemination and Embryo transfer by region (Q28).",
  "Using = Low/Medium/High; Not using = None.",
  "n (per tech) = number of countries in the region that reported that technology.",
  "% = 100 × Using / n. World row sums regional numerators and denominators.",
  "Regions are normalised and shown in the order:",
  paste(region_levels, collapse = "  ·  "),
  sep = "\n"
)

writeData(wb, "README", readme)
writeData(wb, "3D9_2024_main", main)
writeData(wb, "Counts_YesNo_byRegionTech", counts_yesno)
writeData(wb, "Country_Tech_Level", country_level)

bold <- createStyle(textDecoration = "bold")
int0  <- createStyle(numFmt = "0")
for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:200, gridExpand = TRUE)
  setColWidths(wb, sh, 1:200, "auto")
}
df <- openxlsx::readWorkbook(wb, "3D9_2024_main")
if (!is.null(df) && nrow(df)) {
  num_cols <- which(vapply(df, is.numeric, TRUE))
  if (length(num_cols)) addStyle(wb, "3D9_2024_main", int0, rows = 2:(nrow(df)+1), cols = num_cols+1, gridExpand = TRUE)
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved -> ", out_xlsx)

Table 3E1-2
# =====================================================================
# Q28 – Technologies (None/Low/Medium/High)
# ROW-LEVEL COUNTS (no country collapsing): how many "none/low/medium/high"
# per Region × Technology, summed across ALL species rows.
# Also computes Using = Low + Medium + High and percentages.
# Outputs for big-five only and for ALL species.
# =====================================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# ---- Paths
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3E_Technologies_ROW_COUNTS.xlsx"

# ---- Regions order (+ World)
region_levels <- c("Africa","Asia","Southwest Pacific","Europe",
                   "Latin America and the Carribean","North America",
                   "Near East","World")

# ---- Big five (to filter when needed)
big5 <- c("cattle (multipurpose)",
          "cattle (specialized beef)",
          "cattle (specialized dairy)",
          "chickens","goats","pigs","sheep")

# ---- SpecieTag map (robust species id)
code_to_species <- c(
  "007"="cattle (multipurpose)", "008"="cattle (specialized beef)", "009"="cattle (specialized dairy)",
  "010"="chickens", "021"="goats", "037"="pigs", "042"="sheep",
  "002"="alpacas","003"="asses","004"="bactrian camels","005"="buffaloes","013"="deer",
  "015"="dromedaries","017"="ducks","022"="geese","023"="guinea fowls","024"="guinea pigs",
  "026"="horses","027"="llamas","028"="managed bee","029"="mithun","030"="muscovy ducks",
  "033"="ostriches","038"="pigeons","040"="quails","044"="turkeys","046"="yaks"
)

# ---- Technology groups (exact subquestion text)
G3E1 <- c("Artificial insemination",
          "Embryo transfer",
          "Molecular genetic or genomic information",
          "Multiple ovulation and embryo transfer")

G3E2 <- c("Semen sexing",
          "In vitro fertilization",
          "Cloning",
          "Genetic modification",
          "Transplantation of gonadal tissue")

# ---------- helpers ----------
norm_region <- function(x){
  case_when(
    str_detect(x, regex("^africa$", TRUE)) ~ "Africa",
    str_detect(x, regex("^asia$", TRUE)) ~ "Asia",
    str_detect(x, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
    str_detect(x, regex("^europe", TRUE)) ~ "Europe",
    str_detect(x, regex("^latin\\s+america|caribbean|central america|south america", TRUE)) ~
      "Latin America and the Carribean",
    str_detect(x, regex("^north\\s*america", TRUE)) ~ "North America",
    str_detect(x, regex("^near\\s*east", TRUE)) ~ "Near East",
    str_detect(x, regex("^world$", TRUE)) ~ "World",
    TRUE ~ NA_character_
  )
}

species_from_cols <- function(bt, tl, st){
  dplyr::coalesce(
    na_if(str_to_lower(str_squish(bt)),""),
    na_if(str_to_lower(str_squish(tl)),""),
    recode(stringr::str_pad(st,3,pad="0"), !!!code_to_species, .default = NA_character_)
  )
}

clean_tech <- function(x){
  z <- str_to_lower(str_squish(x))
  recode(z,
    "artificial insemination" = "Artificial insemination",
    "embryo transfer"         = "Embryo transfer",
    "molecular genetic or genomic information" = "Molecular genetic or genomic information",
    "multiple ovulation and embryo transfer"   = "Multiple ovulation and embryo transfer",
    "semen sexing"             = "Semen sexing",
    "in vitro fertilization"   = "In vitro fertilization",
    "cloning"                  = "Cloning",
    "genetic modification"     = "Genetic modification",
    "transplantation of gonadal tissue" = "Transplantation of gonadal tissue",
    .default = NA_character_
  )
}

lvl_levels <- c("none","low","medium","high")   # for row-level counts
pretty_lvl <- c("None","Low","Medium","High")

# ---------- read ----------
raw <- read_excel(in_path)
names(raw) <- names(raw) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+","_") |>
  tolower()

pick <- function(...) { c <- unlist(list(...)); h <- c[c %in% names(raw)]; if (length(h)) h[1] else NA_character_ }

col_region   <- pick("region")
col_country  <- pick("country")
col_section  <- pick("section")
col_question <- pick("question_2024","question")
col_subqtxt  <- pick("subquestiontype_2014")
col_answer   <- pick("answer")
col_breed    <- pick("breedtype_2014")
col_tablelab <- pick("tablelabel_2014","species_clean")
col_specietag<- pick("specietag","specietagnew","specietag_new","species")

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_subqtxt), !is.na(col_answer))

d28_rows <- tibble(
  Region0  = raw[[col_region]],
  Country  = str_squish(as.character(raw[[col_country]])),
  Section  = raw[[col_section]],
  Question = raw[[col_question]],
  Tech0    = as.character(raw[[col_subqtxt]]),
  Answer0  = as.character(raw[[col_answer]]),
  Species0 = species_from_cols(raw[[col_breed]], raw[[col_tablelab]], raw[[col_specietag]])
) %>%
  mutate(
    Region  = norm_region(Region0),
    Tech    = clean_tech(Tech0),
    Answer  = str_to_lower(str_squish(Answer0)),
    Species = str_to_lower(str_squish(Species0))
  ) %>%
  filter(Section %in% c(2,"2"),
         grepl("^\\s*28(\\.|$)", as.character(Question)),
         !is.na(Region), Region != "World",
         !is.na(Tech),
         Answer %in% lvl_levels)

# ---------- ROW-LEVEL counts (no collapsing) ----------
# Utility to make counts + percentages, with optional species filter (big5 vs all)
make_row_outputs <- function(D, species_filter = NULL){
  X <- if (is.null(species_filter)) D else D %>% filter(Species %in% species_filter)

  # regional row-level counts per level
  counts_reg <- X %>%
    count(Region, Tech, Answer) %>%
    mutate(Answer = factor(Answer, levels = lvl_levels, labels = pretty_lvl, ordered = TRUE)) %>%
    tidyr::pivot_wider(names_from = Answer, values_from = n, values_fill = 0) %>%
    mutate(Using = Low + Medium + High,
           Total = None + Using)

  # world (sum across regions)
  counts_world <- counts_reg %>%
    group_by(Tech) %>%
    summarise(across(c(all_of(pretty_lvl), Using, Total), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(Region = "World", .before = 1)

  counts_all <- bind_rows(counts_reg, counts_world) %>%
    arrange(factor(Region, levels = region_levels), Tech)

  # row-level percentages per region (Using% and level shares)
  pct_all <- counts_all %>%
    mutate(
      `Percent Using` = ifelse(Total > 0, 100 * Using / Total, NA_real_),
      `Percent None`  = ifelse(Total > 0, 100 * None  / Total, NA_real_),
      `Percent Low`   = ifelse(Total > 0, 100 * Low   / Total, NA_real_),
      `Percent Medium`= ifelse(Total > 0, 100 * Medium/ Total, NA_real_),
      `Percent High`  = ifelse(Total > 0, 100 * High  / Total, NA_real_)
    )

  # Two main tables (percent Using) in the requested layouts
  mk_main_table <- function(techs){
    pct_all %>%
      filter(Tech %in% techs) %>%
      select(Region, Tech, `Percent Using`) %>%
      tidyr::pivot_wider(names_from = Tech, values_from = `Percent Using`) %>%
      mutate(across(where(is.numeric), ~ round(.x, 0))) %>%
      arrange(factor(Region, levels = region_levels))
  }

  list(
    counts_levels = counts_all,
    pct_levels    = pct_all,
    Table3E1_pct  = mk_main_table(G3E1),
    Table3E2_pct  = mk_main_table(G3E2)
  )
}

# ---- Build outputs
OUT_row_big5 <- make_row_outputs(d28_rows, species_filter = big5)
OUT_row_all  <- make_row_outputs(d28_rows, species_filter = NULL)

# ---------- Write Excel ----------
wb <- createWorkbook()
addWorksheet(wb, "README")

# Main tables (Percent Using) – ROW level
addWorksheet(wb, "3E1_big5_pct_ROW")
addWorksheet(wb, "3E2_big5_pct_ROW")
addWorksheet(wb, "3E1_all_pct_ROW")
addWorksheet(wb, "3E2_all_pct_ROW")

# Counts per level (ROW level)
addWorksheet(wb, "Counts_big5_levels_ROW")
addWorksheet(wb, "Counts_all_levels_ROW")

# Percent per level (ROW level, including None/Low/Medium/High & Using)
addWorksheet(wb, "Percents_big5_levels_ROW")
addWorksheet(wb, "Percents_all_levels_ROW")

# Filter dump for audit
addWorksheet(wb, "Filtered_Q28_rows")

readme <- paste(
  "Q28 – ROW-LEVEL method (no country collapsing):",
  "• Every row in the data is counted. Levels are the Answer values: none/low/medium/high.",
  "• Counts per Region×Technology are raw response counts across all species (\"all breeds together\").",
  "• Using = Low + Medium + High; Total = None + Using.",
  "• Percent Using = 100 * Using / Total, computed on row counts.",
  "• Sheets are provided for BIG-FIVE species only and for ALL species.",
  "• A World row is included (sum of regions).",
  sep = "\n"
)

writeData(wb, "README", readme)

writeData(wb, "3E1_big5_pct_ROW", OUT_row_big5$Table3E1_pct)
writeData(wb, "3E2_big5_pct_ROW", OUT_row_big5$Table3E2_pct)
writeData(wb, "3E1_all_pct_ROW",  OUT_row_all$Table3E1_pct)
writeData(wb, "3E2_all_pct_ROW",  OUT_row_all$Table3E2_pct)

writeData(wb, "Counts_big5_levels_ROW", OUT_row_big5$counts_levels)
writeData(wb, "Counts_all_levels_ROW",  OUT_row_all$counts_levels)

writeData(wb, "Percents_big5_levels_ROW",
          OUT_row_big5$pct_levels %>%
            mutate(across(where(is.numeric), ~ ifelse(!is.na(.x), round(.x, 1), NA_real_))) %>%
            arrange(factor(Region, levels = region_levels), Tech))

writeData(wb, "Percents_all_levels_ROW",
          OUT_row_all$pct_levels %>%
            mutate(across(where(is.numeric), ~ ifelse(!is.na(.x), round(.x, 1), NA_real_))) %>%
            arrange(factor(Region, levels = region_levels), Tech))

writeData(wb, "Filtered_Q28_rows",
          d28_rows %>% select(Region, Country, Species, Tech, Answer) %>%
            arrange(factor(Region, levels = region_levels), Country, Tech))

# Basic styling
bold <- createStyle(textDecoration = "bold")
int0 <- createStyle(numFmt = "0")
for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:1000, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:1000, widths = "auto")
}
for (sh in c("3E1_big5_pct_ROW","3E2_big5_pct_ROW","3E1_all_pct_ROW","3E2_all_pct_ROW",
             "Counts_big5_levels_ROW","Counts_all_levels_ROW")){
  df <- openxlsx::readWorkbook(wb, sh)
  if (!is.null(df) && nrow(df)) {
    num_cols <- which(vapply(df, is.numeric, TRUE))
    if (length(num_cols)) addStyle(wb, sh, int0, rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
  }
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved -> ", out_xlsx)





Table 3E3
# =====================================================================
# TABLE 3E3 — Global availability of technologies by species (Q28)
# n  = # countries that reported the species (any tech)
# t  = # countries using a tech (Low/Medium/High)
# Score = mean(0..3) across those n countries (None=0, Low=1, Medium=2, High=3)
# Missing tech answers for a country that reported the species are treated as 0 ("none")
# =====================================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# ---- Paths
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3E3_Technologies_by_Species.xlsx"

# ---- Species (exact column headers & order)
species_cols <- c("Dairy cattle","Beef cattle","Multi-purpose cattle","Sheep","Goats","Pigs","Chickens")

# ---- Map SpecieTag -> species text (so we're robust to where the species text comes from)
code_to_species <- c(
  "009"="Dairy cattle",          # cattle (specialized dairy)
  "008"="Beef cattle",           # cattle (specialized beef)
  "007"="Multi-purpose cattle",  # cattle (multipurpose)
  "042"="Sheep",
  "021"="Goats",
  "037"="Pigs",
  "010"="Chickens"
)

# ---- Technology list (row order must match the publication)
tech_order <- c(
  "Artificial insemination",
  "Embryo transfer",
  "Molecular genetic or genomic information",
  "Multiple ovulation and embryo transfer",
  "Semen sexing",
  "In vitro fertilization",
  "Cloning",
  "Genetic modification",
  "Transplantation of gonadal tissue"
)

# ---- Answer → score
lvl_map  <- c("none"=0,"low"=1,"medium"=2,"high"=3)

# ---------- helpers ----------
norm_region <- function(x){
  case_when(
    str_detect(x, regex("^africa$", TRUE)) ~ "Africa",
    str_detect(x, regex("^asia$", TRUE)) ~ "Asia",
    str_detect(x, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
    str_detect(x, regex("^europe", TRUE)) ~ "Europe",
    str_detect(x, regex("^latin\\s+america|caribbean|central america|south america", TRUE)) ~
      "Latin America and the Carribean",
    str_detect(x, regex("^north\\s*america", TRUE)) ~ "North America",
    str_detect(x, regex("^near\\s*east", TRUE)) ~ "Near East",
    str_detect(x, regex("^world$", TRUE)) ~ "World",
    TRUE ~ NA_character_
  )
}

species_from_cols <- function(bt, tl, st){
  # Try breedtype_2014 or tablelabel_2014 text first; then SpecieTag code
  key_text <- dplyr::coalesce(
    na_if(str_squish(str_to_lower(bt)),""),
    na_if(str_squish(str_to_lower(tl)),"")
  )
  # normalize key_text to our 7 species labels
  norm <- case_when(
    key_text %in% c("cattle (specialized dairy)", "dairy cattle")          ~ "Dairy cattle",
    key_text %in% c("cattle (specialized beef)",  "beef cattle")           ~ "Beef cattle",
    key_text %in% c("cattle (multipurpose)",      "multi-purpose cattle")  ~ "Multi-purpose cattle",
    key_text == "sheep"   ~ "Sheep",
    key_text == "goats"   ~ "Goats",
    key_text == "pigs"    ~ "Pigs",
    key_text == "chickens"~ "Chickens",
    TRUE ~ NA_character_
  )
  out <- ifelse(is.na(norm),
                recode(stringr::str_pad(st, 3, pad = "0"), !!!code_to_species, .default = NA_character_),
                norm)
  out
}

clean_tech <- function(x){
  z <- str_to_lower(str_squish(x))
  recode(z,
    "artificial insemination" = "Artificial insemination",
    "embryo transfer"         = "Embryo transfer",
    "molecular genetic or genomic information" = "Molecular genetic or genomic information",
    "multiple ovulation and embryo transfer"   = "Multiple ovulation and embryo transfer",
    "semen sexing"             = "Semen sexing",
    "in vitro fertilization"   = "In vitro fertilization",
    "cloning"                  = "Cloning",
    "genetic modification"     = "Genetic modification",
    "transplantation of gonadal tissue" = "Transplantation of gonadal tissue",
    .default = NA_character_
  )
}

# ---------- read ----------
raw <- read_excel(in_path)
names(raw) <- names(raw) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+","_") |>
  tolower()

pick <- function(...) { c <- unlist(list(...)); h <- c[c %in% names(raw)]; if (length(h)) h[1] else NA_character_ }

col_region   <- pick("region")
col_country  <- pick("country")
col_section  <- pick("section")
col_question <- pick("question_2024","question")
col_subqtxt  <- pick("subquestiontype_2014")
col_answer   <- pick("answer")
col_breed    <- pick("breedtype_2014")
col_tablelab <- pick("tablelabel_2014","species_clean")
col_specietag<- pick("specietag","specietagnew","specietag_new","species")

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_subqtxt), !is.na(col_answer))

# ---- Q28 only; keep 7 species and required techs/answers; drop World region
d28 <- tibble(
  Region   = norm_region(raw[[col_region]]),
  Country  = str_squish(as.character(raw[[col_country]])),
  Section  = raw[[col_section]],
  Question = raw[[col_question]],
  Tech0    = as.character(raw[[col_subqtxt]]),
  Answer0  = as.character(raw[[col_answer]]),
  Species0 = species_from_cols(raw[[col_breed]], raw[[col_tablelab]], raw[[col_specietag]])
) %>%
  filter(Section %in% c(2,"2"),
         grepl("^\\s*28(\\.|$)", as.character(Question)),
         !is.na(Region), Region != "World",
         !is.na(Species0), Species0 %in% species_cols) %>%
  mutate(
    Tech   = clean_tech(Tech0),
    Answer = str_to_lower(str_squish(Answer0)),
    Score  = recode(Answer, !!!lvl_map, .default = NA_real_)
  ) %>%
  filter(!is.na(Tech), Tech %in% tech_order,
         !is.na(Score)) %>%
  select(Region, Country, Species = Species0, Tech, Answer, Score)

# ---- Collapse to Country × Species × Tech: take the HIGHEST level a country reported
cst <- d28 %>%
  group_by(Country, Species, Tech) %>%
  summarise(Score = max(Score, na.rm = TRUE), .groups = "drop")

# ---- For each Species, determine its country set n (any tech reported)
species_country <- cst %>% distinct(Country, Species)

# ---- Build full grid Country×Species×Tech for those (Country,Species); fill missing tech as 0
full_grid <- species_country %>%
  crossing(Tech = tech_order) %>%
  left_join(cst, by = c("Country","Species","Tech")) %>%
  mutate(Score = coalesce(Score, 0))

# ---- t and Score per Species × Tech (GLOBAL)
t_scores <- full_grid %>%
  group_by(Species, Tech) %>%
  summarise(
    n_countries = n_distinct(Country),
    t_using     = sum(Score > 0, na.rm = TRUE),
    Score_avg   = mean(Score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Score = round(Score_avg, 1)) %>%
  select(Species, Tech, n = n_countries, t = t_using, Score)

# ---------- Make wide table: two sub-columns per species (t & Score)
make_wide <- function(df){
  # we want a single header row with technologies (rows) and for each species two cols: t and Score
  # Pivot twice then interleave
  wide_t <- df %>%
    select(Tech, Species, t) %>%
    pivot_wider(names_from = Species, values_from = t)

  wide_s <- df %>%
    select(Tech, Species, Score) %>%
    pivot_wider(names_from = Species, values_from = Score)

  # interleave t/Score columns in species_cols order
  out <- wide_t %>% arrange(match(Tech, tech_order))
  for (sp in species_cols) {
    if (!sp %in% names(wide_t)) out[[sp]] <- NA_integer_
  }
  out <- out %>% select(Tech, all_of(species_cols))

  out2 <- wide_s %>% arrange(match(Tech, tech_order))
  for (sp in species_cols) {
    if (!sp %in% names(out2)) out2[[sp]] <- NA_real_
  }
  out2 <- out2 %>% select(Tech, all_of(species_cols))

  # build final data frame with t/Score pairs
  final <- tibble(Technology = out$Tech)
  for (sp in species_cols) {
    final[[paste0(sp," t")]]     <- out[[sp]]
    final[[paste0(sp," Score")]] <- out2[[sp]]
  }
  final
}

Table_3E3 <- make_wide(t_scores)

# ---- Build an 'n=' header row (one row, showing n per species)
n_by_species <- t_scores %>%
  distinct(Species, n) %>%
  complete(Species = species_cols, fill = list(n = NA_integer_)) %>%
  arrange(match(Species, species_cols))

header_n <- tibble(Technology = paste0("n ="))  # first cell label
for (sp in species_cols) {
  nn <- n_by_species$n[n_by_species$Species == sp]
  header_n[[paste0(sp," t")]]     <- ifelse(length(nn) && !is.na(nn), as.integer(nn), NA_integer_)
  header_n[[paste0(sp," Score")]] <- ""  # blank in the Score column for the header row
}

# Prepend the header row above the main table (purely cosmetic for Excel)
Table_3E3_export <- bind_rows(header_n, Table_3E3)

# ---------- QA sheets (so you can audit)
QA_counts_levels <- full_grid %>%
  mutate(Level = factor(case_when(
    Score == 0 ~ "None",
    Score == 1 ~ "Low",
    Score == 2 ~ "Medium",
    Score == 3 ~ "High"
  ), levels = c("None","Low","Medium","High"))) %>%
  count(Species, Tech, Level, name = "n_rows") %>%
  arrange(Species, match(Tech, tech_order), Level)

QA_country_level <- full_grid %>%
  mutate(Level = factor(case_when(
    Score == 0 ~ "None",
    Score == 1 ~ "Low",
    Score == 2 ~ "Medium",
    Score == 3 ~ "High"
  ), levels = c("None","Low","Medium","High"))) %>%
  arrange(Species, Country, match(Tech, tech_order))

# ---------- Write Excel
wb <- createWorkbook()

addWorksheet(wb, "README")
addWorksheet(wb, "Table_3E3")
addWorksheet(wb, "Counts_by_level")
addWorksheet(wb, "Country_Species_Tech")

readme <- paste(
  "TABLE 3E3 — Global availability of technologies by species (Q28).",
  "",
  "Method:",
  "• Species considered: Dairy cattle, Beef cattle, Multi-purpose cattle, Sheep, Goats, Pigs, Chickens.",
  "• For each Country×Species×Technology, we take the highest level reported (None=0, Low=1, Medium=2, High=3).",
  "• For each Species:",
  "    n  = # distinct countries that reported that species (any technology).",
  "• For each Technology within the species:",
  "    t      = # countries with level > 0 (Low/Medium/High).",
  "    Score  = average 0..3 across the n countries (missing tech answers treated as 0).",
  "",
  "A header row with “n =” is included; Score cells in that header are left blank.",
  sep = "\n"
)

writeData(wb, "README", readme)
writeData(wb, "Table_3E3", Table_3E3_export)
writeData(wb, "Counts_by_level", QA_counts_levels)
writeData(wb, "Country_Species_Tech", QA_country_level)

# Styling
bold <- createStyle(textDecoration = "bold")
int0 <- createStyle(numFmt = "0")
num1 <- createStyle(numFmt = "0.0")

for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:500, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:500, widths = "auto")
}
# numbers in main table
df <- openxlsx::readWorkbook(wb, "Table_3E3")
if (!is.null(df) && nrow(df)) {
  # integer columns end with " t"
  t_cols <- grep(" t$", names(df))
  if (length(t_cols)) addStyle(wb, "Table_3E3", int0, rows = 2:(nrow(df)+1), cols = t_cols+1, gridExpand = TRUE)
  # score columns end with " Score"
  s_cols <- grep(" Score$", names(df))
  if (length(s_cols)) addStyle(wb, "Table_3E3", num1, rows = 2:(nrow(df)+1), cols = s_cols+1, gridExpand = TRUE)
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved -> ", out_xlsx)





ERROR CORRECTION
# --- FIX: build a type-safe header row and export ---

# n per species (how many countries reported that species at all)
n_by_species <- t_scores %>%
  distinct(Species, n) %>%
  tidyr::complete(Species = species_cols, fill = list(n = NA_integer_)) %>%
  arrange(match(Species, species_cols))

# Start from the existing wide table to inherit column types
header_n <- Table_3E3[0, ]          # zero-row data frame with correct types
header_n[1, ] <- NA                  # add one empty row (keeps types)
header_n$Technology[1] <- "n ="

# Fill the t and Score columns, keeping types (t = integer, Score = numeric NA)
for (sp in species_cols) {
  t_col <- paste0(sp, " t")
  s_col <- paste0(sp, " Score")
  nn <- n_by_species$n[n_by_species$Species == sp]
  header_n[[t_col]][1]  <- ifelse(length(nn) && !is.na(nn), as.integer(nn), NA_integer_)
  header_n[[s_col]][1]  <- NA_real_    # leave Score blank but numeric
}

# Prepend header row
Table_3E3_export <- dplyr::bind_rows(header_n, Table_3E3)

# ---------- Write Excel (re-run safely) ----------
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "README")
openxlsx::addWorksheet(wb, "Table_3E3")
openxlsx::addWorksheet(wb, "Counts_by_level")
openxlsx::addWorksheet(wb, "Country_Species_Tech")

readme <- paste(
  "TABLE 3E3 — Global availability of technologies by species (Q28).",
  "",
  "Method:",
  "• For each Country×Species×Technology, keep the highest level (None=0, Low=1, Medium=2, High=3).",
  "• For each Species: n = # distinct countries that reported that species (any technology).",
  "• For each Technology within the species:",
  "    t     = # countries with level > 0 (Low/Medium/High).",
  "    Score = average 0..3 across the n countries (missing tech for a country treated as 0).",
  "",
  "The first row shows 'n =' per species; Score cells in that row are left blank.",
  sep = "\n"
)

openxlsx::writeData(wb, "README", readme)
openxlsx::writeData(wb, "Table_3E3", Table_3E3_export)
openxlsx::writeData(wb, "Counts_by_level", QA_counts_levels)
openxlsx::writeData(wb, "Country_Species_Tech", QA_country_level)

# Styling
bold <- openxlsx::createStyle(textDecoration = "bold")
int0 <- openxlsx::createStyle(numFmt = "0")
num1 <- openxlsx::createStyle(numFmt = "0.0")

for (sh in openxlsx::sheets(wb)) {
  openxlsx::addStyle(wb, sh, bold, rows = 1, cols = 1:500, gridExpand = TRUE)
  openxlsx::setColWidths(wb, sh, cols = 1:500, widths = "auto")
}

df <- openxlsx::readWorkbook(wb, "Table_3E3")
if (!is.null(df) && nrow(df)) {
  t_cols <- grep(" t$", names(df))
  if (length(t_cols)) openxlsx::addStyle(wb, "Table_3E3", int0,
                                         rows = 2:(nrow(df)+1), cols = t_cols+1, gridExpand = TRUE)
  s_cols <- grep(" Score$", names(df))
  if (length(s_cols)) openxlsx::addStyle(wb, "Table_3E3", num1,
                                         rows = 2:(nrow(df)+1), cols = s_cols+1, gridExpand = TRUE)
}

out_xlsx <- "C:/Users/LENOVO/Documents/Fig3E3_Technologies_by_Species.xlsx"
openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved -> ", out_xlsx)





TABLE 3E4
# =========================
# TABLE 3E3 — Minor species
# =========================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# ---------- Paths ----------
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3E3_Technologies_by_MinorSpecies.xlsx"

# ---------- Species ----------
minor_species <- c(
  "alpacas","asses","bactrian camels","buffaloes","deer","dromedaries","ducks","geese",
  "guinea fowls","guinea pigs","horses","llamas","managed bee","mithun","muscovy ducks",
  "ostriches","pigeons","quails","rabbits","turkeys","yaks"
)

big7 <- c("cattle (specialized dairy)","cattle (specialized beef)","cattle (multipurpose)",
          "sheep","goats","pigs","chickens")

# SpecieTag -> species text (lower-case)
code_to_species <- c(
  "002"="alpacas", "003"="asses", "004"="bactrian camels", "005"="buffaloes",
  "007"="cattle (multipurpose)", "008"="cattle (specialized beef)", "009"="cattle (specialized dairy)",
  "010"="chickens", "013"="deer", "015"="dromedaries", "017"="ducks", "021"="goats", "022"="geese",
  "023"="guinea fowls", "024"="guinea pigs", "026"="horses", "027"="llamas", "028"="managed bee",
  "029"="mithun", "030"="muscovy ducks", "033"="ostriches", "037"="pigs", "038"="pigeons",
  "040"="quails", "041"="rabbits", "042"="sheep", "044"="turkeys", "046"="yaks"
)

to_title <- function(x){
  z <- str_to_title(x)
  z <- gsub(" Of ", " of ", z, fixed = TRUE)
  z <- gsub(" And ", " and ", z, fixed = TRUE)
  z
}
species_cols <- to_title(minor_species)

# ---------- Technologies (exact labels for rows, in order) ----------
tech_order <- c(
  "Artificial insemination",
  "Embryo transfer",
  "Molecular genetic or genomic information",
  "Multiple ovulation and embryo transfer",
  "Semen sexing",
  "In vitro fertilization",
  "Cloning",
  "Genetic modification",
  "Transplantation of gonadal tissue"
)

# Normalize tech text coming from SubquestionType_2014
normalize_tech <- function(x){
  key <- str_to_lower(str_squish(x))
  map <- c(
    "artificial insemination" = "Artificial insemination",
    "embryo transfer" = "Embryo transfer",
    "molecular genetic or genomic information" = "Molecular genetic or genomic information",
    "multiple ovulation and embryo transfer" = "Multiple ovulation and embryo transfer",
    "semen sexing" = "Semen sexing",
    "in vitro fertilization" = "In vitro fertilization",
    "cloning" = "Cloning",
    "genetic modification" = "Genetic modification",
    "transplantation of gonadal tissue" = "Transplantation of gonadal tissue"
  )
  unname(map[key])
}

# Answer -> score (0..3)
ans_to_score <- function(x){
  key <- str_to_lower(str_squish(x))
  recode(key, "none"=0, "low"=1, "medium"=2, "high"=3, .default = NA_real_)
}

# Robust column picker
pick_col <- function(raw, ...) {
  cands <- unlist(list(...))
  hit <- cands[cands %in% names(raw)]
  if (length(hit)) hit[1] else NA_character_
}

# Map species from any of: BreedType_2014, TableLabel_2014/Species_clean, SpecieTag
map_species_any <- function(bt, tl, st){
  dplyr::coalesce(
    na_if(str_to_lower(str_squish(bt)),""),
    na_if(str_to_lower(str_squish(tl)),""),
    recode(stringr::str_pad(st, 3, pad="0"), !!!code_to_species, .default = NA_character_)
  )
}

# ---------- Read + normalize ----------
raw <- read_excel(in_path)
names(raw) <- names(raw) |>
  str_squish() |>
  str_replace_all("[^A-Za-z0-9]+","_") |>
  tolower()

col_year     <- pick_col(raw, "year")
col_region   <- pick_col(raw, "region")
col_country  <- pick_col(raw, "country")
col_section  <- pick_col(raw, "section")
col_question <- pick_col(raw, "question_2024","question")
col_subq     <- pick_col(raw, "subquestiontype_2014")
col_answer   <- pick_col(raw, "answer")
col_breed    <- pick_col(raw, "breedtype_2014")
col_tablelab <- pick_col(raw, "tablelabel_2014","species_clean")
col_specietag<- pick_col(raw, "specietag","specietagnew","specietag_new","species")

stopifnot(!is.na(col_country), !is.na(col_section), !is.na(col_question),
          !is.na(col_subq), !is.na(col_answer))

d28_raw <- tibble(
  Region   = as.character(raw[[col_region]]),
  Country  = str_squish(as.character(raw[[col_country]])),
  Section  = raw[[col_section]],
  Question = raw[[col_question]],
  Tech0    = as.character(raw[[col_subq]]),
  Answer0  = as.character(raw[[col_answer]]),
  Species0 = map_species_any(raw[[col_breed]], raw[[col_tablelab]], raw[[col_specietag]])
) %>%
  filter(Section %in% c(2,"2"),
         grepl("^\\s*28(\\.|$)", as.character(Question)),
         !is.na(Species0)) %>%
  mutate(
    species = str_to_lower(str_squish(Species0)),
    tech    = normalize_tech(Tech0),
    score   = ans_to_score(Answer0)
  )

# Keep only recognized minor species + recognized tech + valid answers
d28 <- d28_raw %>%
  filter(species %in% minor_species,
         !is.na(tech), tech %in% tech_order,
         !is.na(score))

# Countries that reported each species (n)
countries_by_species <- d28 %>%
  distinct(species, Country)

# Build full grid: Country x Species x Tech (for the species that country reported)
full_grid <- countries_by_species %>%
  tidyr::expand_grid(tech = tech_order) %>%
  left_join(d28 %>% select(species, Country, tech, score),
            by = c("species","Country","tech")) %>%
  group_by(species, Country, tech) %>%
  summarise(Score = max(score, na.rm = TRUE), .groups = "drop") %>%
  mutate(Score = ifelse(is.infinite(Score), 0, Score))  # if all NA -> 0

# n per species
n_by_species <- countries_by_species %>%
  count(species, name = "n")

# t and Score per species-tech
t_scores <- full_grid %>%
  left_join(n_by_species, by = "species") %>%
  group_by(species, tech, n) %>%
  summarise(
    t     = sum(Score > 0, na.rm = TRUE),
    Score = mean(Score, na.rm = TRUE),   # mean across the n countries (0 for missing already)
    .groups = "drop"
  )

# ---------- Make wide table (rows = Technology, columns = Species t / Species Score) ----------
make_wide_cols <- function(df, species_vec){
  # Ensure all species/tech present
  df2 <- df %>%
    tidyr::complete(species = species_vec, tech = tech_order, fill = list(n = NA_integer_, t = 0, Score = 0)) %>%
    arrange(match(tech, tech_order), match(species, species_vec))
  
  # wide for t
  w_t <- df2 %>%
    select(tech, species, t) %>%
    tidyr::pivot_wider(names_from = species, values_from = t) %>%
    rename(Technology = tech)
  
  # wide for Score
  w_s <- df2 %>%
    select(tech, species, Score) %>%
    tidyr::pivot_wider(names_from = species, values_from = Score) %>%
    rename(Technology = tech)
  
  # Interleave columns as "Species t" and "Species Score"
  out <- tibble(Technology = w_t$Technology)
  for (sp in species_vec) {
    out[[paste0(to_title(sp), " t")]]     <- as.integer(w_t[[sp]])
    out[[paste0(to_title(sp), " Score")]] <- as.numeric(w_s[[sp]])
  }
  out
}

Table_3E3 <- make_wide_cols(t_scores, minor_species)

# ---------- Build a type-safe "n =" header and prepend ----------
# Create zero-row slice to inherit column types
header_n <- Table_3E3[0, ]
header_n[1, ] <- NA
header_n$Technology[1] <- "n ="

# fill per-species n (t columns get n; Score columns remain numeric NA)
for (sp in minor_species) {
  nn <- n_by_species$n[n_by_species$species == sp]
  t_col <- paste0(to_title(sp), " t")
  s_col <- paste0(to_title(sp), " Score")
  header_n[[t_col]][1] <- ifelse(length(nn) && !is.na(nn), as.integer(nn), NA_integer_)
  header_n[[s_col]][1] <- NA_real_
}

Table_3E3_export <- dplyr::bind_rows(header_n, Table_3E3)

# ---------- QA sheets ----------
QA_counts_levels <- full_grid %>%
  mutate(Level = factor(case_when(
    Score == 0 ~ "None",
    Score == 1 ~ "Low",
    Score == 2 ~ "Medium",
    Score == 3 ~ "High"
  ), levels = c("None","Low","Medium","High"))) %>%
  count(Species = to_title(species), Technology = tech, Level, name = "n_rows") %>%
  arrange(Species, match(Technology, tech_order), Level)

QA_country_level <- full_grid %>%
  transmute(
    Species = to_title(species),
    Country,
    Technology = tech,
    Score
  ) %>%
  arrange(Species, Country, match(Technology, tech_order))

# ---------- Write Excel ----------
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "Table_3E3_minor")
addWorksheet(wb, "Counts_by_level")
addWorksheet(wb, "Country_Species_Tech")

readme <- paste(
  "TABLE 3E3 — Global availability of technologies by species (Q28), MINOR species only.",
  "",
  "Method:",
  "• Country×Species×Technology: keep the highest of none/low/medium/high (0–3).",
  "• For each Species (column pair):",
  "   n shown in the header row (equals # distinct countries reporting that species).",
  "• For each Technology (row) within the species:",
  "   t     = # countries with level > 0 (Low/Medium/High).",
  "   Score = mean 0..3 across the n countries (missing tech treated as 0).",
  "",
  "Species included:",
  paste0("   ", paste(to_title(minor_species), collapse = ", ")),
  sep = "\n"
)

writeData(wb, "README", readme)
writeData(wb, "Table_3E3_minor", Table_3E3_export)
writeData(wb, "Counts_by_level", QA_counts_levels)
writeData(wb, "Country_Species_Tech", QA_country_level)

# Styling
bold <- createStyle(textDecoration = "bold")
int0 <- createStyle(numFmt = "0")
num1 <- createStyle(numFmt = "0.0")

for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:1000, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:1000, widths = "auto")
}

# Numeric formats in the main table
df <- openxlsx::readWorkbook(wb, "Table_3E3_minor")
if (!is.null(df) && nrow(df)) {
  t_cols <- grep(" t$", names(df))
  if (length(t_cols)) addStyle(wb, "Table_3E3_minor", int0,
                               rows = 2:(nrow(df)+1), cols = t_cols+1, gridExpand = TRUE)
  s_cols <- grep(" Score$", names(df))
  if (length(s_cols)) addStyle(wb, "Table_3E3_minor", num1,
                               rows = 2:(nrow(df)+1), cols = s_cols+1, gridExpand = TRUE)
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved -> ", out_xlsx)








Table 3E4
# ==============================
# Q31 – Production systems x items (GLOBAL scores 0–3)
# + underlying count sheets
# ==============================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# ---- Paths
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig31_ProductionSystems_bySpecies.xlsx"

# ---- Helpers
norm_names <- function(x){
  x |> str_squish() |> str_replace_all("[^A-Za-z0-9]+","_") |> tolower()
}
pick_col <- function(df, ...) {
  cands <- unlist(list(...))
  hit <- cands[cands %in% names(df)]
  if (length(hit)) hit[1] else NA_character_
}

# ---- Read
raw <- read_excel(in_path)
names(raw) <- norm_names(names(raw))

# ---- Locate columns
col_region    <- pick_col(raw, "region")
col_country   <- pick_col(raw, "country")
col_section   <- pick_col(raw, "section")
col_question  <- pick_col(raw, "question_2024","question")
col_subqtxt   <- pick_col(raw, "subquestiontype_2014")   # Production system
col_breedtype <- pick_col(raw, "breedtype_2014")         # Item (AI variants, natural mating)
col_answer    <- pick_col(raw, "answer")
col_specietag <- pick_col(raw, "specietag","specietagnew","specietag_new","species")

stopifnot(!is.na(col_country), !is.na(col_section), !is.na(col_question),
          !is.na(col_subqtxt), !is.na(col_breedtype), !is.na(col_answer),
          !is.na(col_specietag))

# ---- Species mapping (big five split cattle; chickens excluded)
code_to_species <- c(
  "009"="Dairy cattle",
  "008"="Beef cattle",
  "007"="Multipurpose cattle",
  "042"="Sheep",
  "021"="Goats",
  "037"="Pigs",
  "010"="Chickens"
)
map_species <- function(st){
  recode(stringr::str_pad(st, 3, pad = "0"), !!!code_to_species, .default = NA_character_)
}
species_keep <- c("Dairy cattle","Beef cattle","Multipurpose cattle","Sheep","Goats","Pigs")

# ---- Production systems (row order)
prod_levels <- c(
  "Pastoralist systems",
  "Ranching or similar grassland-based production systems",
  "Mixed farming systems (rural areas)",
  "Small-scale urban or peri-urban systems",
  "Industrial systems"
)

# ---- Items (column order)
ai_items <- c(
  "Artificial insemination using imported semen from exotic breeds",
  "Artificial insemination using nationally produced semen from exotic breeds",
  "Artificial insemination using semen from locally adapted breeds",
  "Natural mating"
)

# ---- Map level text -> numeric score
level_map <- c("n/a"=0, "na"=0, "n.a."=0, "none"=0, "low"=1, "medium"=2, "high"=3)

# ---------- Normalize Q31 rows ----------
d31_raw <- tibble(
  Region      = if (!is.na(col_region)) raw[[col_region]] else NA_character_,
  Country     = str_squish(as.character(raw[[col_country]])),
  Section     = raw[[col_section]],
  Question    = raw[[col_question]],
  ProdSystem0 = as.character(raw[[col_subqtxt]]),
  Item0       = as.character(raw[[col_breedtype]]),
  Answer0     = as.character(raw[[col_answer]]),
  Species0    = map_species(raw[[col_specietag]])
) %>%
  filter(Section %in% c(2,"2"),
         grepl("^\\s*31(\\.|$)", as.character(Question)),
         !is.na(Species0),
         Species0 %in% species_keep) %>%
  mutate(
    ProdSystem = str_squish(ProdSystem0),
    Item       = str_squish(Item0),
    Answer_l   = str_to_lower(str_squish(Answer0)),
    Score      = unname(level_map[Answer_l]),
    Species    = Species0
  ) %>%
  filter(ProdSystem %in% prod_levels, Item %in% ai_items) %>%
  select(Region, Country, Species, ProdSystem, Item, Answer_l, Score)

# Collapse to ONE row per Country×Species×System×Item:
# - numeric score = max(score)
# - if max score is 0, keep "none" over "n/a" if present (so we can count them separately)
collapse_ctsi <- d31_raw %>%
  group_by(Species, Country, ProdSystem, Item) %>%
  summarise(
    Score = max(Score, na.rm = TRUE),
    had_none = any(Answer_l == "none", na.rm = TRUE),
    had_na   = any(Answer_l %in% c("n/a","na","n.a."), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Level_collapsed = case_when(
    Score == 3 ~ "high",
    Score == 2 ~ "medium",
    Score == 1 ~ "low",
    Score == 0 & had_none ~ "none",
    Score == 0 & !had_none & had_na ~ "n/a",
    Score == 0 ~ "none"   # fallback
  ))

# Universe of countries per species
species_countries <- collapse_ctsi %>% distinct(Species, Country)

# Expanded grid so missing combos are explicit and counted as 0 for averages
full_grid <- tidyr::expand_grid(
  Species    = unique(species_countries$Species),
  Country    = unique(species_countries$Country),
  ProdSystem = prod_levels,
  Item       = ai_items
) %>%
  inner_join(species_countries, by = c("Species","Country")) %>%   # keep only countries that reported that species
  left_join(collapse_ctsi, by = c("Species","Country","ProdSystem","Item")) %>%
  mutate(
    Missing_imputed = is.na(Score),
    Score = coalesce(Score, 0L),
    Level_for_counts = ifelse(Missing_imputed, "missing", Level_collapsed)
  )

# ---------- Main average table (0–3) ----------
avg_scores <- full_grid %>%
  group_by(Species, ProdSystem, Item) %>%
  summarise(Score = mean(Score, na.rm = TRUE), .groups = "drop")

Table_31 <- avg_scores %>%
  mutate(
    Species    = factor(Species, levels = c("Dairy cattle","Beef cattle","Multipurpose cattle","Sheep","Goats","Pigs")),
    ProdSystem = factor(ProdSystem, levels = prod_levels),
    Item       = factor(Item, levels = ai_items)
  ) %>%
  arrange(Species, ProdSystem, Item) %>%
  mutate(Score = round(Score, 1)) %>%
  tidyr::pivot_wider(names_from = Item, values_from = Score) %>%
  rename(
    `Imported semen from exotic breeds`            = `Artificial insemination using imported semen from exotic breeds`,
    `Nationally produced semen from exotic breeds` = `Artificial insemination using nationally produced semen from exotic breeds`,
    `Semen from locally adapted breeds`            = `Artificial insemination using semen from locally adapted breeds`,
    `Natural mating`                               = `Natural mating`
  ) %>%
  arrange(Species, ProdSystem) %>%
  rename(`Production system` = ProdSystem) %>%
  select(Species, `Production system`,
         `Imported semen from exotic breeds`,
         `Nationally produced semen from exotic breeds`,
         `Semen from locally adapted breeds`,
         `Natural mating`)

# ---------- UNDERLYING COUNT SHEETS ----------

# (A) Counts of REPORTED levels (no imputation)
Counts_reported_levels <- collapse_ctsi %>%
  mutate(Level = factor(Level_collapsed, levels = c("n/a","none","low","medium","high"))) %>%
  count(Species, ProdSystem, Item, Level, name = "n") %>%
  tidyr::pivot_wider(names_from = Level, values_from = n, values_fill = 0) %>%
  arrange(Species, match(ProdSystem, prod_levels), match(Item, ai_items))

# (B) Counts INCLUDING missing (explicit “missing” column)
Counts_with_missing <- full_grid %>%
  mutate(Level = factor(Level_for_counts, levels = c("n/a","none","low","medium","high","missing"))) %>%
  count(Species, ProdSystem, Item, Level, name = "n") %>%
  tidyr::pivot_wider(names_from = Level, values_from = n, values_fill = 0) %>%
  arrange(Species, match(ProdSystem, prod_levels), match(Item, ai_items))

# (C) Country-level grid (what the averages were computed from)
Country_level_expanded <- full_grid %>%
  mutate(Level_collapsed = ifelse(Level_for_counts == "missing", NA_character_, Level_for_counts)) %>%
  select(Species, Country, ProdSystem, Item, Score, Level_collapsed, Missing_imputed) %>%
  arrange(Species, Country, match(ProdSystem, prod_levels), match(Item, ai_items))

# (D) n per species
n_by_species <- species_countries %>% count(Species, name = "n_countries") %>% arrange(Species)

# ---------- Write Excel ----------
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "Table_31")
addWorksheet(wb, "Counts_reported_levels")
addWorksheet(wb, "Counts_with_missing")
addWorksheet(wb, "Country_level_expanded")
addWorksheet(wb, "n_by_species")
addWorksheet(wb, "Source_rows")

readme <- paste(
  "Q31 – Production systems by species (GLOBAL).",
  "",
  "Species: Dairy cattle, Beef cattle, Multipurpose cattle, Sheep, Goats, Pigs (Chickens excluded).",
  "Production systems: Pastoralist; Ranching/grassland; Mixed (rural); Small-scale urban/peri-urban; Industrial.",
  "Items: imported exotic semen; nationally produced exotic semen; semen from locally adapted breeds; natural mating.",
  "",
  "Scoring for averages: N/A/None=0, Low=1, Medium=2, High=3.",
  "If multiple rows exist for a Country×Species×System×Item, the highest level is used.",
  "If max level = 0, we keep 'none' over 'n/a' when counting levels; missing combinations are imputed as 0 for averages and",
  "are labelled 'missing' in the counts-with-missing sheet.",
  sep = "\n"
)

writeData(wb, "README", readme)
writeData(wb, "Table_31",               Table_31)
writeData(wb, "Counts_reported_levels", Counts_reported_levels)
writeData(wb, "Counts_with_missing",    Counts_with_missing)
writeData(wb, "Country_level_expanded", Country_level_expanded)
writeData(wb, "n_by_species",           n_by_species)
writeData(wb, "Source_rows",            d31_raw %>% arrange(Species, Country, ProdSystem, Item))

# styling
bold <- createStyle(textDecoration = "bold")
num1 <- createStyle(numFmt = "0.0")
int0 <- createStyle(numFmt = "0")

for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:300, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:300, widths = "auto")
}

# numeric formatting for main table
df <- openxlsx::readWorkbook(wb, "Table_31")
if (!is.null(df) && nrow(df)) {
  sc <- which(vapply(df, is.numeric, TRUE))
  if (length(sc)) addStyle(wb, "Table_31", num1, rows = 2:(nrow(df)+1), cols = sc+1, gridExpand = TRUE)
}

# ints for count sheets
for (sh in c("Counts_reported_levels","Counts_with_missing")) {
  dfc <- openxlsx::readWorkbook(wb, sh)
  if (!is.null(dfc) && nrow(dfc)) {
    num_cols <- which(vapply(dfc, is.numeric, TRUE))
    if (length(num_cols)) addStyle(wb, sh, int0, rows = 2:(nrow(dfc)+1), cols = num_cols+1, gridExpand = TRUE)
  }
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved -> ", out_xlsx)






Table 3E6
# ==============================================================
# Q29 – Stakeholders involved in AI / ET (YES/NO)  ->  % of countries
# Output workbook layout:
#   - Main_table           (your figure/table layout)
#   - Counts_YesNo_byRegionTech
#   - Percents_byRegionTech
#   - Country_counts
#   - Country_Tech_Stakeholder (collapsed YES/NO per country)
#   - Filtered_Q29           (auditable raw slice)
#   - README
# ==============================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# ---- Paths
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3B1_Q29_Stakeholders_AI_ET.xlsx"

# ---- Regions (display order)
region_levels <- c("Africa","Asia","Southwest Pacific","Europe",
                   "Latin America and the Carribean","North America",
                   "Near East","World")

# ---- Column helpers
norm_names <- function(x) {
  x |> str_squish() |> str_replace_all("[^A-Za-z0-9]+","_") |> tolower()
}
pick_col <- function(df, ...) {
  cands <- unlist(list(...)); hit <- intersect(cands, names(df))
  if (length(hit)) hit[1] else NA_character_
}
norm_region <- function(x){
  case_when(
    str_detect(x, regex("^africa$", TRUE)) ~ "Africa",
    str_detect(x, regex("^asia$", TRUE)) ~ "Asia",
    str_detect(x, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
    str_detect(x, regex("^europe", TRUE)) ~ "Europe",
    str_detect(x, regex("^latin\\s+america|caribbean|central america|south america", TRUE)) ~
      "Latin America and the Carribean",
    str_detect(x, regex("^north\\s*america", TRUE)) ~ "North America",
    str_detect(x, regex("^near\\s*east", TRUE)) ~ "Near East",
    str_detect(x, regex("^world$", TRUE)) ~ "World",
    TRUE ~ NA_character_
  )
}

# ---- Read once & locate columns
raw <- read_excel(in_path)
names(raw) <- norm_names(names(raw))

col_region  <- pick_col(raw, "region")
col_country <- pick_col(raw, "country")
col_section <- pick_col(raw, "section")
col_question<- pick_col(raw, "question_2024","question")
col_answer  <- pick_col(raw, "answer")
col_btype   <- pick_col(raw, "breedtype_2014")         # technology (AI / ET)
col_subqt   <- pick_col(raw, "subquestiontype_2014")   # stakeholder

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_answer),
          !is.na(col_btype), !is.na(col_subqt))

# ---- Canonical forms for tech & stakeholders
canon <- function(x){
  x |>
    str_replace_all("[\u2018\u2019]", "'") |>   # curly → straight apostrophes
    str_squish()
}

tech_map <- c(
  "artificial insemination" = "AI",
  "embryo transfer"         = "ET"
)

stakeholders <- c(
  "Breeders' associations or cooperatives",
  "Donors and development agencies",
  "External commercial companies",
  "National commercial companies",
  "National non-governmental organizations",
  "Public sector"
)

# Build regexes that match the stakeholders robustly
stake_re <- list(
  breeders   = regex("^breeders'?\\s+associations?\\s+or\\s+cooperatives$", TRUE),
  donors     = regex("^donors?\\s+and\\s+development\\s+agencies$", TRUE),
  external   = regex("^external\\s+commercial\\s+companies$", TRUE),
  national_c = regex("^national\\s+commercial\\s+companies$", TRUE),
  nngo       = regex("^national\\s+non-?governmental\\s+organizations$", TRUE),
  public     = regex("^public\\s+sector$", TRUE)
)

map_stake <- function(x){
  x <- canon(x)
  case_when(
    str_detect(x, stake_re$breeders)   ~ "Breeders' associations or cooperatives",
    str_detect(x, stake_re$donors)     ~ "Donors and development agencies",
    str_detect(x, stake_re$external)   ~ "External commercial companies",
    str_detect(x, stake_re$national_c) ~ "National commercial companies",
    str_detect(x, stake_re$nngo)       ~ "National non-governmental organizations",
    str_detect(x, stake_re$public)     ~ "Public sector",
    TRUE ~ NA_character_
  )
}

map_tech <- function(x){
  x0 <- str_to_lower(canon(x))
  recode(x0, !!!tech_map, .default = NA_character_)
}

# ---- Slice the data for Q29
d29_raw <- tibble(
  Region0   = raw[[col_region]],
  Country   = str_squish(as.character(raw[[col_country]])),
  Section   = raw[[col_section]],
  Question  = raw[[col_question]],
  Answer0   = as.character(raw[[col_answer]]),
  Tech0     = as.character(raw[[col_btype]]),
  Stake0    = as.character(raw[[col_subqt]])
)

d29 <- d29_raw %>%
  mutate(
    Region = norm_region(Region0),
    Tech   = map_tech(Tech0),
    Stake  = map_stake(Stake0),
    Ans    = str_to_lower(str_squish(Answer0))
  ) %>%
  filter(Section %in% c(2,"2"),
         grepl("^\\s*29(\\.|$)", as.character(Question)),
         !is.na(Region), Region != "World",
         !is.na(Tech), !is.na(Stake),
         Ans %in% c("yes","no")) %>%
  select(Region, Country, Tech, Stake, Ans)

# ---- Collapse to Country × Tech × Stake (YES if ANY yes among rows)
country_stake <- d29 %>%
  group_by(Region, Country, Tech, Stake) %>%
  summarise(Yes = any(Ans == "yes"),
            Reported = n()>0, .groups = "drop")

# ---- # Countries that reported the technology (denominator per Region×Tech)
country_counts <- d29 %>%
  distinct(Region, Country, Tech) %>%
  count(Region, Tech, name = "Number_of_countries")

# ---- YES & NO counts per Region×Tech×Stake
yes_counts <- country_stake %>%
  group_by(Region, Tech, Stake) %>%
  summarise(YES = sum(Yes), .groups = "drop") %>%
  left_join(country_counts, by = c("Region","Tech")) %>%
  mutate(NO = pmax(Number_of_countries - YES, 0L),
         pct_yes = ifelse(Number_of_countries > 0,
                          100 * YES / Number_of_countries, NA_real_))

# ---- Add WORLD by summing numerators and denominators (not averaging %)
world_counts <- yes_counts %>%
  group_by(Tech, Stake) %>%
  summarise(YES = sum(YES, na.rm = TRUE),
            Number_of_countries = sum(Number_of_countries, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(Region = "World",
         NO = pmax(Number_of_countries - YES, 0L),
         pct_yes = ifelse(Number_of_countries > 0,
                          100 * YES / Number_of_countries, NA_real_)) %>%
  select(Region, everything())

yes_counts_all <- bind_rows(
  yes_counts,
  world_counts
) %>%
  mutate(
    Region = factor(Region, levels = region_levels),
    Stake  = factor(Stake,  levels = stakeholders),
    Tech   = factor(Tech,   levels = c("AI","ET"))
  ) %>%
  arrange(Region, Tech, Stake)

# ---- Main table (your layout)
Main_table <- yes_counts_all %>%
  select(Region, Tech, Stake, Number_of_countries, pct_yes) %>%
  mutate(pct_yes = round(pct_yes, 0)) %>%
  # one row per Region×Tech, stakeholder % as columns
  pivot_wider(
    id_cols = c(Region, Tech, Number_of_countries),
    names_from = Stake, values_from = pct_yes
  ) %>%
  arrange(Region, Tech) %>%
  # move Number_of_countries right after Tech with your header name
  rename(`Number of countries` = Number_of_countries)

# ---- Percent table (long → wide, keeps decimals)
Percents_byRegionTech <- yes_counts_all %>%
  select(Region, Tech, Stake, pct_yes) %>%
  mutate(pct_yes = round(pct_yes, 1)) %>%
  pivot_wider(names_from = Stake, values_from = pct_yes) %>%
  arrange(Region, Tech)

# ---- YES/NO counts (for audit)
Counts_YesNo_byRegionTech <- yes_counts_all %>%
  select(Region, Tech, Stake, YES, NO, Number_of_countries) %>%
  arrange(Region, Tech, Stake)

# ---- Country×Tech presence (denominators)
Country_counts <- country_counts %>%
  arrange(factor(Region, levels = region_levels), Tech)

# ---- Country-level collapsed YES/NO (audit)
Country_Tech_Stakeholder <- country_stake %>%
  arrange(factor(Region, levels = region_levels), Country, Tech, Stake)

# ---- Keep the filtered slice (raw-ish)
Filtered_Q29 <- d29 %>%
  arrange(factor(Region, levels = region_levels), Country, Tech, Stake, Ans)

# ==============================================================
# Write Excel
# ==============================================================
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "Main_table")
addWorksheet(wb, "Percents_byRegionTech")
addWorksheet(wb, "Counts_YesNo_byRegionTech")
addWorksheet(wb, "Country_counts")
addWorksheet(wb, "Country_Tech_Stakeholder")
addWorksheet(wb, "Filtered_Q29")

readme <- paste(
  "Q29 – Stakeholders involved in AI / ET (YES/NO).",
  "",
  "Definitions:",
  "  • Technology (BreedType_2014): Artificial insemination (AI) or Embryo transfer (ET).",
  "  • Stakeholders (SubquestionType_2014):",
  "      - Breeders' associations or cooperatives",
  "      - Donors and development agencies",
  "      - External commercial companies",
  "      - National commercial companies",
  "      - National non-governmental organizations",
  "      - Public sector",
  "",
  "Method:",
  "  1) Collapse to Country×Technology×Stakeholder; YES if any row = 'yes'.",
  "  2) Number of countries (denominator) per Region×Technology =",
  "     count of distinct countries that reported that technology (any stakeholder, yes or no).",
  "  3) Percent shown for each stakeholder = 100 × (# countries with YES) / (Number of countries).",
  "  4) World row is built by summing regional numerators & denominators (not averaging %).",
  sep = "\n"
)

writeData(wb, "README",                    readme)
writeData(wb, "Main_table",                Main_table)
writeData(wb, "Percents_byRegionTech",     Percents_byRegionTech)
writeData(wb, "Counts_YesNo_byRegionTech", Counts_YesNo_byRegionTech)
writeData(wb, "Country_counts",            Country_counts)
writeData(wb, "Country_Tech_Stakeholder",  Country_Tech_Stakeholder)
writeData(wb, "Filtered_Q29",              Filtered_Q29)

# Styling
bold <- createStyle(textDecoration = "bold")
int0 <- createStyle(numFmt = "0")
for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:300, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:300, widths = "auto")
}
# make percent columns in Main_table integers
df <- openxlsx::readWorkbook(wb, "Main_table")
if (!is.null(df) && nrow(df)) {
  num_cols <- which(vapply(df, is.numeric, TRUE))
  if (length(num_cols)) addStyle(wb, "Main_table", int0,
                                 rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved -> ", out_xlsx)




WITH OTHER TECHNOLOGY
# ==============================================================
# Q29 – Stakeholders by technology (custom tech list, NOT AI/ET)
# We compute, for each Region × Technology:
#   • Number of countries reporting that technology
#   • % of countries with YES for each stakeholder
# Plus audit sheets: counts, percents, denominators, country-level.
# ==============================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# ---- Paths
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3B1_Q29_Stakeholders_BY_TECH.xlsx"

# ---- Region utilities (same as before)
norm_names <- function(x) x |> str_squish() |> str_replace_all("[^A-Za-z0-9]+","_") |> tolower()
pick_col <- function(df, ...) {
  cands <- unlist(list(...)); hit <- intersect(cands, names(df))
  if (length(hit)) hit[1] else NA_character_
}
norm_region <- function(x){
  case_when(
    str_detect(x, regex("^africa$", TRUE)) ~ "Africa",
    str_detect(x, regex("^asia$", TRUE)) ~ "Asia",
    str_detect(x, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
    str_detect(x, regex("^europe", TRUE)) ~ "Europe",
    str_detect(x, regex("^latin\\s+america|caribbean|central america|south america", TRUE)) ~
      "Latin America and the Carribean",
    str_detect(x, regex("^north\\s*america", TRUE)) ~ "North America",
    str_detect(x, regex("^near\\s*east", TRUE)) ~ "Near East",
    str_detect(x, regex("^world$", TRUE)) ~ "World",
    TRUE ~ NA_character_
  )
}
region_levels <- c("Africa","Asia","Southwest Pacific","Europe",
                   "Latin America and the Carribean","North America",
                   "Near East","World")

# ---- Load once
raw <- read_excel(in_path)
names(raw) <- norm_names(names(raw))

col_region  <- pick_col(raw, "region")
col_country <- pick_col(raw, "country")
col_section <- pick_col(raw, "section")
col_question<- pick_col(raw, "question_2024","question")
col_answer  <- pick_col(raw, "answer")
col_btype   <- pick_col(raw, "breedtype_2014")        # TECHNOLOGY names
col_subqt   <- pick_col(raw, "subquestiontype_2014")  # STAKEHOLDER names

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_answer),
          !is.na(col_btype), !is.na(col_subqt))

# ---- Stakeholders (same set as earlier)
stakeholders <- c(
  "Breeders' associations or cooperatives",
  "Donors and development agencies",
  "External commercial companies",
  "National commercial companies",
  "National non-governmental organizations",
  "Public sector"
)
stake_re <- list(
  breeders   = regex("^breeders'?\\s+associations?\\s+or\\s+cooperatives$", TRUE),
  donors     = regex("^donors?\\s+and\\s+development\\s+agencies$", TRUE),
  external   = regex("^external\\s+commercial\\s+companies$", TRUE),
  national_c = regex("^national\\s+commercial\\s+companies$", TRUE),
  nngo       = regex("^national\\s+non-?governmental\\s+organizations$", TRUE),
  public     = regex("^public\\s+sector$", TRUE)
)
map_stake <- function(x){
  x <- x |>
    str_replace_all("[\u2018\u2019]", "'") |>
    str_squish()
  case_when(
    str_detect(x, stake_re$breeders)   ~ "Breeders' associations or cooperatives",
    str_detect(x, stake_re$donors)     ~ "Donors and development agencies",
    str_detect(x, stake_re$external)   ~ "External commercial companies",
    str_detect(x, stake_re$national_c) ~ "National commercial companies",
    str_detect(x, stake_re$nngo)       ~ "National non-governmental organizations",
    str_detect(x, stake_re$public)     ~ "Public sector",
    TRUE ~ NA_character_
  )
}

# ---- EXACT output technologies (order preserved)
tech_targets <- c(
  "Cloning",
  "DNA paternity testing",
  "MOET",
  "Semen sexing",
  "In vitro fertilization",
  "Molecular genetic or genomic information",
  "Molecular genetic or genomic information/Genética molecular or Información genómica",
  "Use of molecular genetic or genomic information for prediction of breeding values",
  "Sexado de semen",
  "Superovulación y transferencia de embriones",
  "Heat induction and Synchronization",
  "Semen cryopreservation",
  "Use of genomic data / Utilisation de données génomiques pour l'évaluation génétique ou d'autres applications",
  "Semen sexing / Sexage de la semence",
  "Fecundación in vitro"
)

# Robust mapper from BreedType_2014 to the EXACT labels above
norm_text <- function(x){
  x |>
    str_replace_all("[\u2018\u2019]", "'") |>
    str_to_lower() |>
    str_squish()
}
map_tech_custom <- function(x){
  z <- norm_text(x)
  case_when(
    str_detect(z, "^cloning$") ~ "Cloning",
    str_detect(z, "^dna paternity testing$") ~ "DNA paternity testing",
    str_detect(z, "^(moet|multiple ovulation and embryo transfer)$") ~ "MOET",
    str_detect(z, "^semen sexing$") ~ "Semen sexing",
    str_detect(z, "^in vitro fertilization$") ~ "In vitro fertilization",
    str_detect(z, "^molecular genetic or genomic information$") ~ "Molecular genetic or genomic information",
    str_detect(z, "^molecular genetic or genomic information/ ?gen(e|é)tica molecular (or|o) informaci(o|ó)n gen(o|ó)mica$") ~
      "Molecular genetic or genomic information/Genética molecular or Información genómica",
    str_detect(z, "^use of molecular genetic or genomic information for prediction of breeding values$") ~
      "Use of molecular genetic or genomic information for prediction of breeding values",
    str_detect(z, "^sexado de semen$") ~ "Sexado de semen",
    str_detect(z, "^superovulaci(o|ó)n y transferencia de embriones$") ~
      "Superovulación y transferencia de embriones",
    str_detect(z, "^heat induction and (synchronization|synchronisation)$") ~
      "Heat induction and Synchronization",
    str_detect(z, "^semen cryopreservation$") ~ "Semen cryopreservation",
    str_detect(z, "^use of genomic data( / utilisation de donn(é|e)es g(é|e)nomiques pour l'(é|e)valuation g(é|e)n(é|e)tique( ou d'autres applications)?)?$") ~
      "Use of genomic data / Utilisation de données génomiques pour l'évaluation génétique ou d'autres applications",
    str_detect(z, "^(semen sexing / sexage de la semence|sexage de la semence)$") ~
      "Semen sexing / Sexage de la semence",
    str_detect(z, "^fecundaci(o|ó)n in vitro$") ~ "Fecundación in vitro",
    TRUE ~ NA_character_
  )
}

# ---- Slice Q29 YES/NO
d29 <- tibble(
  Region0  = raw[[col_region]],
  Country  = str_squish(as.character(raw[[col_country]])),
  Section  = raw[[col_section]],
  Question = raw[[col_question]],
  Ans0     = as.character(raw[[col_answer]]),
  Tech0    = as.character(raw[[col_btype]]),
  Stake0   = as.character(raw[[col_subqt]])
) %>%
  mutate(
    Region = norm_region(Region0),
    Tech   = map_tech_custom(Tech0),
    Stake  = map_stake(Stake0),
    Ans    = str_to_lower(str_squish(Ans0))
  ) %>%
  filter(Section %in% c(2,"2"),
         grepl("^\\s*29(\\.|$)", as.character(Question)),
         !is.na(Region), Region != "World",
         !is.na(Tech), Tech %in% tech_targets,
         !is.na(Stake),
         Ans %in% c("yes","no")) %>%
  select(Region, Country, Tech, Stake, Ans)

# ---- Collapse to Country × Tech × Stake (YES if any yes among rows)
country_stake <- d29 %>%
  group_by(Region, Country, Tech, Stake) %>%
  summarise(Yes = any(Ans == "yes"),
            Reported = n()>0, .groups = "drop")

# ---- Denominators: # countries that reported that technology in the region
country_counts <- d29 %>%
  distinct(Region, Country, Tech) %>%
  count(Region, Tech, name = "Number_of_countries")

# ---- YES/NO counts and % YES
yes_counts <- country_stake %>%
  group_by(Region, Tech, Stake) %>%
  summarise(YES = sum(Yes), .groups = "drop") %>%
  left_join(country_counts, by = c("Region","Tech")) %>%
  mutate(NO = pmax(Number_of_countries - YES, 0L),
         pct_yes = ifelse(Number_of_countries > 0, 100 * YES / Number_of_countries, NA_real_))

# ---- WORLD aggregation (sum numerators & denominators)
world_counts <- yes_counts %>%
  group_by(Tech, Stake) %>%
  summarise(YES = sum(YES, na.rm = TRUE),
            Number_of_countries = sum(Number_of_countries, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(Region = "World",
         NO = pmax(Number_of_countries - YES, 0L),
         pct_yes = ifelse(Number_of_countries > 0, 100 * YES / Number_of_countries, NA_real_)) %>%
  select(Region, everything())

yes_counts_all <- bind_rows(yes_counts, world_counts) %>%
  mutate(
    Region = factor(Region, levels = region_levels),
    Tech   = factor(Tech, levels = tech_targets),
    Stake  = factor(Stake, levels = stakeholders)
  ) %>%
  arrange(Region, Tech, Stake)

# ---- Main table (Region × Technology with stakeholder % columns)
Main_table <- yes_counts_all %>%
  select(Region, Tech, Stake, Number_of_countries, pct_yes) %>%
  mutate(pct_yes = round(pct_yes, 0)) %>%
  pivot_wider(
    id_cols = c(Region, Tech, Number_of_countries),
    names_from = Stake, values_from = pct_yes
  ) %>%
  arrange(Region, Tech) %>%
  rename(`Number of countries` = Number_of_countries)

# ---- Percent and counts (audit)
Percents_byRegionTech <- yes_counts_all %>%
  select(Region, Tech, Stake, pct_yes) %>%
  mutate(pct_yes = round(pct_yes, 1)) %>%
  pivot_wider(names_from = Stake, values_from = pct_yes) %>%
  arrange(Region, Tech)

Counts_YesNo_byRegionTech <- yes_counts_all %>%
  select(Region, Tech, Stake, YES, NO, Number_of_countries) %>%
  arrange(Region, Tech, Stake)

Country_counts <- country_counts %>%
  arrange(factor(Region, levels = region_levels), factor(Tech, levels = tech_targets))

Country_Tech_Stakeholder <- country_stake %>%
  arrange(factor(Region, levels = region_levels), Country,
          factor(Tech, levels = tech_targets), factor(Stake, levels = stakeholders))

Filtered_Q29 <- d29 %>%
  arrange(factor(Region, levels = region_levels), Country,
          factor(Tech, levels = tech_targets), factor(Stake, levels = stakeholders), Ans)

# ==============================================================
# Write Excel
# ==============================================================
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "Main_table")
addWorksheet(wb, "Percents_byRegionTech")
addWorksheet(wb, "Counts_YesNo_byRegionTech")
addWorksheet(wb, "Country_counts")
addWorksheet(wb, "Country_Tech_Stakeholder")
addWorksheet(wb, "Filtered_Q29")

readme <- paste(
  "Q29 – Stakeholders involved, by technology (custom list; AI/ET excluded).",
  "",
  "Technologies included (exact labels as requested, order preserved):",
  paste0("  • ", paste(tech_targets, collapse = "\n  • ")),
  "",
  "Stakeholders:",
  "  • Breeders' associations or cooperatives",
  "  • Donors and development agencies",
  "  • External commercial companies",
  "  • National commercial companies",
  "  • National non-governmental organizations",
  "  • Public sector",
  "",
  "Method:",
  "  1) Collapse to Country×Technology×Stakeholder; YES if any row is 'yes'.",
  "  2) Denominator per Region×Technology = # distinct countries that reported that technology.",
  "  3) % YES per stakeholder = 100 × YES / Number of countries.",
  "  4) World row sums regional numerators & denominators.",
  sep = "\n"
)

writeData(wb, "README",                    readme)
writeData(wb, "Main_table",                Main_table)
writeData(wb, "Percents_byRegionTech",     Percents_byRegionTech)
writeData(wb, "Counts_YesNo_byRegionTech", Counts_YesNo_byRegionTech)
writeData(wb, "Country_counts",            Country_counts)
writeData(wb, "Country_Tech_Stakeholder",  Country_Tech_Stakeholder)
writeData(wb, "Filtered_Q29",              Filtered_Q29)

# Styling
bold <- createStyle(textDecoration = "bold")
int0  <- createStyle(numFmt = "0")

for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:400, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:400, widths = "auto")
}
# make stakeholder % in Main_table integers
df <- openxlsx::readWorkbook(wb, "Main_table")
if (!is.null(df) && nrow(df)) {
  num_cols <- which(vapply(df, is.numeric, TRUE))
  if (length(num_cols)) addStyle(wb, "Main_table", int0,
                                 rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved -> ", out_xlsx)








TABLE 3E7
# ==============================
# Q30 – Research (National vs International) by Region
# Technologies: AI, Embryo transfer or MOET, Semen sexing, In vitro fertilization, Cloning
# Stakeholders: Public or private research at national level (National),
#               Research undertaken as part of international collaboration (International)
# Output: one main table (percents), plus counts/percents/QA sheets
# ==============================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# ---- Paths
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/FigQ30_Research_ByRegion.xlsx"

# ---- Region order (2024)
region_levels <- c("Africa","Asia","Southwest Pacific","Europe",
                   "Latin America and the Carribean","North America",
                   "Near East","World")

# ---- Helpers
norm_names <- function(x) x |> str_squish() |> str_replace_all("[^A-Za-z0-9]+","_") |> tolower()
pick_col <- function(df, ...) { c <- unlist(list(...)); h <- c[c %in% names(df)]; if (length(h)) h[1] else NA_character_ }

# ---- Read & standardize names
raw <- read_excel(in_path)
names(raw) <- norm_names(names(raw))

# ---- Find columns
col_region  <- pick_col(raw, "region")
col_country <- pick_col(raw, "country")
col_section <- pick_col(raw, "section")
col_question<- pick_col(raw, "question_2024","question")
col_answer  <- pick_col(raw, "answer")
col_subq    <- pick_col(raw, "subquestiontype_2014")  # stakeholder
col_breed   <- pick_col(raw, "breedtype_2014")        # technology

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_answer), !is.na(col_subq), !is.na(col_breed))

# ---- Map stakeholders (exact phrases you asked for)
pat_nat <- regex("^\\s*public or private research at national level\\s*$", ignore_case = TRUE)
pat_int <- regex("^\\s*research undertaken as part of international collaboration\\s*$", TRUE)

map_stakeholder <- function(x){
  case_when(
    str_detect(x, pat_nat) ~ "National",
    str_detect(x, pat_int) ~ "International",
    TRUE ~ NA_character_
  )
}

# ---- Technologies to keep (map ET variants -> Embryo transfer or MOET)
tech_keep <- c("Artificial insemination",
               "Embryo transfer or MOET",
               "Semen sexing",
               "In vitro fertilization",
               "Cloning")

map_tech <- function(x){
  x0 <- str_squish(as.character(x))
  case_when(
    str_detect(x0, regex("^embryo transfer", TRUE)) ~ "Embryo transfer or MOET",
    str_detect(x0, regex("^artificial insemination$", TRUE)) ~ "Artificial insemination",
    str_detect(x0, regex("^semen sexing$", TRUE)) ~ "Semen sexing",
    str_detect(x0, regex("^in vitro fertilization$", TRUE)) ~ "In vitro fertilization",
    str_detect(x0, regex("^cloning$", TRUE)) ~ "Cloning",
    TRUE ~ NA_character_
  )
}

# ---- Filter to Q30 and normalize
d30_raw <- tibble(
  Region   = str_squish(as.character(raw[[col_region]])),
  Country  = str_squish(as.character(raw[[col_country]])),
  Section  = raw[[col_section]],
  Question = raw[[col_question]],
  Stake0   = as.character(raw[[col_subq]]),
  Tech0    = as.character(raw[[col_breed]]),
  Answer0  = as.character(raw[[col_answer]])
) %>%
  filter(Section %in% c(2,"2"),
         grepl("^\\s*30(\\.|$)", as.character(Question)),
         !is.na(Region), !is.na(Country)) %>%
  mutate(
    Stake = map_stakeholder(Stake0),
    Tech  = map_tech(Tech0),
    ans   = str_to_lower(str_squish(Answer0))
  ) %>%
  filter(!is.na(Stake), !is.na(Tech),
         ans %in% c("yes","no"))

# ---- Number of countries per region (denominator for percents)
country_counts <- d30_raw %>%
  distinct(Region, Country) %>%
  count(Region, name = "Number_of_countries") %>%
  filter(Region != "World")

# ---- Collapse duplicates at Country×Tech×Stake (YES dominates NO)
d30_country <- d30_raw %>%
  mutate(is_yes = ans == "yes") %>%
  group_by(Region, Country, Tech, Stake) %>%
  summarise(YES = as.integer(any(is_yes)), .groups = "drop") %>%
  filter(Region != "World")

# ---- Counts of YES/NO by Region×Tech×Stake
counts_yes_no <- d30_country %>%
  group_by(Region, Tech, Stake) %>%
  summarise(YES = sum(YES, na.rm = TRUE),
            NO  = n() - YES,
            .groups = "drop") %>%
  left_join(country_counts, by = "Region")

# ---- Percents (YES / #countries in region)
percents <- counts_yes_no %>%
  mutate(pct = ifelse(Number_of_countries > 0, 100 * YES / Number_of_countries, NA_real_))

# ---- World row (sum numerators & denominators)
world_counts <- counts_yes_no %>%
  group_by(Tech, Stake) %>%
  summarise(YES = sum(YES), NO = sum(NO), Number_of_countries = sum(Number_of_countries), .groups = "drop") %>%
  mutate(Region = "World")

world_perc <- world_counts %>%
  mutate(pct = ifelse(Number_of_countries > 0, 100 * YES / Number_of_countries, NA_real_))

# ---- Assemble long table with percents (+ world)
perc_all <- bind_rows(
  percents %>% mutate(Region = factor(Region, levels = region_levels)),
  world_perc %>% mutate(Region = factor(Region, levels = region_levels))
) %>%
  mutate(
    Tech  = factor(Tech, levels = c("Artificial insemination","Embryo transfer or MOET",
                                    "Semen sexing","In vitro fertilization","Cloning")),
    Stake = factor(Stake, levels = c("National","International"))
  )

# ---- MAIN TABLE (wide, layout like your figure)
mk_colname <- function(tech, stake) paste0(tech, " – ", stake)

main_table <- perc_all %>%
  select(Region, Tech, Stake, Number_of_countries, pct) %>%
  mutate(col = mk_colname(as.character(Tech), as.character(Stake))) %>%
  select(Region, Number_of_countries, col, pct) %>%
  pivot_wider(names_from = col, values_from = pct) %>%
  arrange(Region) %>%
  mutate(across(-Region, ~ round(.x, 0))) %>%
  # ensure column order
  relocate(
    Region, Number_of_countries,
    `Artificial insemination – National`,
    `Artificial insemination – International`,
    `Embryo transfer or MOET – National`,
    `Embryo transfer or MOET – International`,
    `Semen sexing – National`,
    `Semen sexing – International`,
    `In vitro fertilization – National`,
    `In vitro fertilization – International`,
    `Cloning – National`,
    `Cloning – International`
  )

# ---- Also provide long sheets for counts and percents, plus the filtered rows used
counts_long <- bind_rows(
  counts_yes_no,
  world_counts
) %>%
  arrange(factor(Region, levels = region_levels), Tech, Stake)

percents_long <- perc_all %>%
  select(Region, Tech, Stake, Number_of_countries, Percent = pct) %>%
  arrange(Region, Tech, Stake)

filtered_rows <- d30_raw %>%
  transmute(
    Region, Country,
    Technology = map_tech(Tech0),
    Stakeholder = map_stakeholder(Stake0),
    Answer = str_to_lower(str_squish(Answer0))
  ) %>%
  filter(!is.na(Technology), !is.na(Stakeholder), Answer %in% c("yes","no")) %>%
  arrange(Region, Country, Technology, Stakeholder)

# ---- Write Excel
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "Main_table")
addWorksheet(wb, "Counts_yes_no")
addWorksheet(wb, "Percents_long")
addWorksheet(wb, "Country_counts")
addWorksheet(wb, "Filtered_Q30")

readme <- paste(
  "Q30 — Research at national vs international level for reproduction/biotech (by region).",
  "",
  "Stakeholders considered:",
  "  • Public or private research at national level  → “National”",
  "  • Research undertaken as part of international collaboration  → “International”",
  "Technologies considered:",
  "  • Artificial insemination; Embryo transfer or MOET; Semen sexing; In vitro fertilization; Cloning.",
  "",
  "Method:",
  "  • For each Region×Country×Technology×Stakeholder, YES dominates NO if duplicates exist.",
  "  • Number of countries = distinct countries with ANY Q30 record in the region.",
  "  • Percent in the main table = 100 × (# countries with YES for that Technology×Stakeholder) / (Number of countries in region).",
  "  • World row sums numerators and denominators across regions (not an average of %).",
  sep = "\n"
)

writeData(wb, "README",         readme)
writeData(wb, "Main_table",     main_table)
writeData(wb, "Counts_yes_no",  counts_long)
writeData(wb, "Percents_long",  percents_long)
writeData(wb, "Country_counts", country_counts %>%
            bind_rows(world_counts %>% distinct(Region, Number_of_countries)) %>%
            arrange(factor(Region, levels = region_levels)))
writeData(wb, "Filtered_Q30",   filtered_rows)

# Basic styling
bold <- createStyle(textDecoration = "bold")
int0 <- createStyle(numFmt = "0")
for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:300, gridExpand = TRUE)
  setColWidths(wb, sh, cols = 1:300, widths = "auto")
}
# integers on % columns
df <- openxlsx::readWorkbook(wb, "Main_table")
if (!is.null(df) && nrow(df)) {
  num_cols <- which(vapply(df, is.numeric, TRUE))
  if (length(num_cols))
    addStyle(wb, "Main_table", int0, rows = 2:(nrow(df)+1), cols = num_cols, gridExpand = TRUE)
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved -> ", out_xlsx)






TABLE 3E9
# ==============================
# 3D9 (2024 only) — AI & ET by region from Q28
# Using = Low/Medium/High; Not using = None
# ==============================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(openxlsx)
})

# ---- Paths
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3D9_2024_AI_ET_byRegion.xlsx"

# ---- Region order (keep EXACT labels you want to see)
region_levels <- c(
  "Africa","Asia","Southwest Pacific","Europe",
  "Latin America and the Carribean","North America",
  "Near East","World"
)

# Normalise whatever is in the file onto the above labels
norm_region <- function(x){
  x0 <- str_squish(as.character(x))
  case_when(
    str_detect(x0, regex("^africa$", TRUE)) ~ "Africa",
    str_detect(x0, regex("^asia$", TRUE)) ~ "Asia",
    str_detect(x0, regex("^south.?west\\s*pacific", TRUE)) ~ "Southwest Pacific",
    str_detect(x0, regex("^europe", TRUE)) ~ "Europe",
    # map ANY 'Latin America ... Caribbean/Carribean' variant to your display label
    str_detect(x0, regex("^latin\\s*america.*carib", TRUE)) ~ "Latin America and the Carribean",
    str_detect(x0, regex("^north\\s*america", TRUE)) ~ "North America",
    str_detect(x0, regex("^near\\s*east", TRUE)) ~ "Near East",
    str_detect(x0, regex("^world$", TRUE)) ~ "World",
    TRUE ~ x0
  )
}

# ---- Helpers
norm_names <- function(x) x |> str_squish() |> str_replace_all("[^A-Za-z0-9]+","_") |> tolower()
pick <- function(df, ...) { c <- unlist(list(...)); h <- c[c %in% names(df)]; if (length(h)) h[1] else NA_character_ }

# ---- Read
raw <- read_excel(in_path)
names(raw) <- norm_names(names(raw))

col_region   <- pick(raw, "region")
col_country  <- pick(raw, "country")
col_section  <- pick(raw, "section")
col_question <- pick(raw, "question_2024","question")
col_subq     <- pick(raw, "subquestiontype_2014")  # <-- AI / ET here per your request
col_answer   <- pick(raw, "answer")

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_subq), !is.na(col_answer))

# ---- Map tech from SubquestionType_2014
map_tech <- function(x){
  x0 <- str_squish(as.character(x))
  case_when(
    str_detect(x0, regex("^artificial\\s+insemination\\b", TRUE)) ~ "Artificial insemination",
    str_detect(x0, regex("^embryo\\s+transfer\\b", TRUE))         ~ "Embryo transfer",
    TRUE ~ NA_character_
  )
}

# ---- Map level to Using flag
level_to_num <- c("none"=0, "low"=1, "medium"=2, "high"=3)

d28 <- tibble(
  Region   = norm_region(raw[[col_region]]),          # <-- normalize region names
  Country  = str_squish(as.character(raw[[col_country]])),
  Section  = raw[[col_section]],
  Question = raw[[col_question]],
  Tech0    = as.character(raw[[col_subq]]),
  Answer0  = as.character(raw[[col_answer]])
) %>%
  filter(Section %in% c(2,"2"),
         grepl("^\\s*28(\\.|$)", as.character(Question)),
         !is.na(Region), !is.na(Country)) %>%
  mutate(
    Tech     = map_tech(Tech0),
    level_l  = tolower(str_squish(Answer0)),
    LevelNum = unname(level_to_num[level_l])
  ) %>%
  filter(!is.na(Tech), !is.na(LevelNum))

# ---- Collapse to one record per Region × Country × Tech (highest level)
ct <- d28 %>%
  group_by(Region, Country, Tech) %>%
  summarise(Level = max(LevelNum, na.rm = TRUE), .groups = "drop") %>%
  mutate(Using = Level > 0)

# ---- Regional stats
reg_stats <- ct %>%
  group_by(Region, Tech) %>%
  summarise(
    n_countries = n_distinct(Country),
    using_count = sum(Using),
    pct_using   = ifelse(n_countries > 0, 100 * using_count / n_countries, NA_real_),
    .groups = "drop"
  ) %>%
  filter(Region != "World")

# ---- World row
world_stats <- reg_stats %>%
  group_by(Tech) %>%
  summarise(
    n_countries = sum(n_countries),
    using_count = sum(using_count),
    pct_using   = ifelse(n_countries > 0, 100 * using_count / n_countries, NA_real_),
    .groups = "drop"
  ) %>%
  mutate(Region = "World")

stats_all <- bind_rows(reg_stats, world_stats)

# ---- Ensure ALL regions appear (and in your exact order), fill missing with 0
tech_levels <- c("Artificial insemination","Embryo transfer")
scaffold <- tidyr::expand_grid(Region = region_levels, Tech = tech_levels)

stats_filled <- scaffold %>%
  left_join(stats_all, by = c("Region","Tech")) %>%
  mutate(
    n_countries = coalesce(as.integer(n_countries), 0L),
    using_count = coalesce(as.integer(using_count), 0L),
    pct_using   = coalesce(round(pct_using, 0), 0)
  )

# ---- Main table (wide)
main <- stats_filled %>%
  select(Region, Tech, n_countries, pct_using) %>%
  mutate(
    Region = factor(Region, levels = region_levels),
    Tech   = factor(Tech,   levels = tech_levels)
  ) %>%
  arrange(Region, Tech) %>%
  pivot_wider(
    names_from = Tech, values_from = c(n_countries, pct_using)
  ) %>%
  rename(
    `n (AI)`  = `n_countries_Artificial insemination`,
    `AI %`    = `pct_using_Artificial insemination`,
    `n (ET)`  = `n_countries_Embryo transfer`,
    `ET %`    = `pct_using_Embryo transfer`
  ) %>%
  arrange(Region)

# ---- QA sheets
country_level <- ct %>%
  arrange(factor(Region, levels = region_levels), Country, Tech)

counts_yesno <- ct %>%
  group_by(Region, Tech) %>%
  summarise(YES = sum(Using), NO = n_distinct(Country) - YES, .groups = "drop") %>%
  complete(Region = region_levels, Tech = tech_levels, fill = list(YES = 0, NO = 0)) %>%
  arrange(factor(Region, levels = region_levels), Tech)

# ---- Write Excel
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "3D9_2024_main")
addWorksheet(wb, "Counts_YesNo_byRegionTech")
addWorksheet(wb, "Country_Tech_Level")

readme <- paste(
  "3D9 (2024 only) — Use of Artificial insemination and Embryo transfer by region (Q28).",
  "Using = Low/Medium/High; Not using = None.",
  "n (per tech) = number of countries in the region that reported that technology.",
  "% = 100 × Using / n. World row sums regional numerators and denominators.",
  "Regions are normalised and shown in the order:",
  paste(region_levels, collapse = "  ·  "),
  sep = "\n"
)

writeData(wb, "README", readme)
writeData(wb, "3D9_2024_main", main)
writeData(wb, "Counts_YesNo_byRegionTech", counts_yesno)
writeData(wb, "Country_Tech_Level", country_level)

bold <- createStyle(textDecoration = "bold")
int0  <- createStyle(numFmt = "0")
for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:200, gridExpand = TRUE)
  setColWidths(wb, sh, 1:200, "auto")
}
df <- openxlsx::readWorkbook(wb, "3D9_2024_main")
if (!is.null(df) && nrow(df)) {
  num_cols <- which(vapply(df, is.numeric, TRUE))
  if (length(num_cols)) addStyle(wb, "3D9_2024_main", int0, rows = 2:(nrow(df)+1), cols = num_cols+1, gridExpand = TRUE)
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved -> ", out_xlsx)


