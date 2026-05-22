MAPS
FIGURE 3C2
# ==============================
# Breeders’ associations map (Figure 3C2 style)
# + ISO list + legend-group sheets
# ==============================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# ---- Paths
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3C2_BreedersAssociations_MapTables.xlsx"

# ---- Region order (keep your spelling)
region_levels <- c("Africa","Asia","Southwest Pacific","Europe",
                   "Latin America and the Carribean","North America","Near East","World")

# ---- Helpers
norm_names <- function(x) x |>
  str_squish() |> str_replace_all("[^A-Za-z0-9]+","_") |> tolower()
pick <- function(df, ...) { c <- unlist(list(...)); h <- c[c %in% names(df)]; if (length(h)) h[1] else NA_character_ }

# ---- Read & normalise
raw <- read_excel(in_path)
names(raw) <- norm_names(names(raw))

col_region   <- pick(raw, "region")
col_country  <- pick(raw, "country")
col_isocode  <- pick(raw, "isocode","iso_code","iso3","iso3code","iso3_code")
col_section  <- pick(raw, "section")
col_question <- pick(raw, "question_2024","question")
col_subqt    <- pick(raw, "subquestiontype_2014")
col_tablelab <- pick(raw, "tablelabel_2014")
col_answer   <- pick(raw, "answer")

stopifnot(!is.na(col_region), !is.na(col_country),
          !is.na(col_section), !is.na(col_question),
          !is.na(col_subqt),  !is.na(col_tablelab),
          !is.na(col_answer))

# ---- Filter to Q15 (breeders’ associations/cooperatives) rows
df_ba <- tibble(
  Region     = str_squish(as.character(raw[[col_region]])),
  Country    = str_squish(as.character(raw[[col_country]])),
  ISOCode    = if (!is.na(col_isocode)) str_squish(as.character(raw[[col_isocode]])) else NA_character_,
  Section    = raw[[col_section]],
  Question   = raw[[col_question]],
  SubqTxt    = as.character(raw[[col_subqt]]),
  TableLabel = as.character(raw[[col_tablelab]]),
  Answer0    = as.character(raw[[col_answer]])
) %>%
  filter(Section %in% c(2, "2"),
         grepl("^\\s*15(\\.|$)", as.character(Question))) %>%
  filter(str_detect(str_to_lower(SubqTxt),
                    "breeders.?\\s*(associations|cooperatives)")) %>%
  mutate(
    Answer = str_to_lower(str_squish(Answer0)),
    Answer = ifelse(Answer %in% c("none","low","medium","high"), Answer, NA_character_),
    # BA runs breeding programmes: "Setting breeding ..." element
    setting_breeding = str_detect(str_to_lower(TableLabel), "setting\\s+breeding")
  ) %>%
  filter(!is.na(Answer))

# ---- Per-country logical flags across ALL species/elements
country_flags <- df_ba %>%
  group_by(Region, Country) %>%
  summarise(
    ISOCode           = suppressWarnings(first(na.omit(ISOCode))),
    has_high_setting  = any(setting_breeding & Answer == "high"),
    has_low_or_medium = any(Answer %in% c("low","medium")),
    all_none          = all(Answer == "none"),
    .groups = "drop"
  )

# ---- Universe of countries (for "Did not report")
all_countries <- raw %>%
  transmute(
    Region  = str_squish(as.character(.data[[col_region]])),
    Country = str_squish(as.character(.data[[col_country]])),
    ISOCode = if (!is.na(col_isocode)) str_squish(as.character(.data[[col_isocode]])) else NA_character_
  ) %>%
  distinct() %>%
  filter(!is.na(Region), !is.na(Country))

# ---- Final class per country (vectorised; NAs → FALSE)
class_levels <- c("Operate BA breeding programmes (≥1 species)",
                  "Involved in some element (≥1 species)",
                  "No BA involvement reported",
                  "Did not report")

country_classes <- all_countries %>%
  left_join(country_flags, by = c("Region","Country"), suffix = c("", "_flag")) %>%
  mutate(
    ISOCode = coalesce(ISOCode_flag, ISOCode),
    has_high_setting  = coalesce(has_high_setting,  FALSE),
    has_low_or_medium = coalesce(has_low_or_medium, FALSE),
    all_none          = coalesce(all_none,          FALSE),
    Class = case_when(
      has_high_setting                      ~ class_levels[1],
      !has_high_setting & has_low_or_medium ~ class_levels[2],
      !has_high_setting & !has_low_or_medium & all_none ~ class_levels[3],
      TRUE                                  ~ class_levels[4]
    )
  ) %>%
  select(Region, Country, ISOCode, Class) %>%
  arrange(factor(Region, levels = region_levels), Country)

# ---- Region × Class counts (+ World)
region_counts <- country_classes %>%
  mutate(Class = factor(Class, levels = class_levels)) %>%
  count(Region, Class, name = "Countries") %>%
  arrange(factor(Region, levels = region_levels), Class)

world_counts <- region_counts %>%
  filter(Region != "World") %>%
  group_by(Class) %>%
  summarise(Countries = sum(Countries), .groups = "drop") %>%
  mutate(Region = "World")

region_counts <- bind_rows(region_counts %>% filter(Region != "World"),
                           world_counts) %>%
  arrange(factor(Region, levels = region_levels), Class)

# ---- ISO listings for legend groups
iso_by_class <- country_classes %>%
  arrange(factor(Region, levels = region_levels), Country)

legend_groups <- iso_by_class %>%
  mutate(Class = factor(Class, levels = class_levels)) %>%
  group_by(Class) %>%
  summarise(
    Count     = n_distinct(Country),
    ISOCodes  = paste(sort(unique(ISOCode[!is.na(ISOCode) & ISOCode != ""])), collapse = ", "),
    Countries = paste(sort(unique(Country)), collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(Class)

legend_groups_byRegion <- iso_by_class %>%
  mutate(Class = factor(Class, levels = class_levels)) %>%
  group_by(Region, Class) %>%
  summarise(
    Count    = n_distinct(Country),
    ISOCodes = paste(sort(unique(ISOCode[!is.na(ISOCode) & ISOCode != ""])), collapse = ", "),
    .groups  = "drop"
  ) %>%
  arrange(factor(Region, levels = region_levels), Class)

# ---- Rows used (audit)
rows_used <- df_ba %>%
  select(Region, Country, ISOCode, TableLabel, Answer) %>%
  arrange(factor(Region, levels = region_levels), Country, TableLabel)

# ---- Write Excel
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "Country_classes")          # main map table
addWorksheet(wb, "Counts_byRegion_Class")    # counts table
addWorksheet(wb, "Country_ISO_Class")        # ISO per country (for GIS joins)
addWorksheet(wb, "Legend_groups")            # one row per legend class
addWorksheet(wb, "Legend_groups_byRegion")   # legend class by region
addWorksheet(wb, "Rows_used")                # audit rows

readme <- paste(
  "Breeders’ associations (Q15) – underlying tables for Figure 3C2 style map.",
  "",
  "Class rules:",
  "  • Operate BA breeding programmes (≥1 species): any 'high' on the 'Setting breeding ...' element.",
  "  • Involved in some element (≥1 species): no green, but any 'low' or 'medium' on any element.",
  "  • No BA involvement reported: reported for BA, but all responses are 'none'.",
  "  • Did not report: no BA rows found for the country.",
  "",
  "Sheets:",
  "  • Country_classes              — Region, Country, ISOCode, Class (use this for the map).",
  "  • Counts_byRegion_Class        — counts by Region × Class (+ World).",
  "  • Country_ISO_Class            — same as Country_classes, ordered for GIS.",
  "  • Legend_groups                — class totals + ISO lists.",
  "  • Legend_groups_byRegion       — per-region class totals + ISO lists.",
  "  • Rows_used                    — the Q15 rows used (TableLabel & Answer) for audit.",
  sep = "\n"
)

writeData(wb, "README",                 readme)
writeData(wb, "Country_classes",        country_classes)
writeData(wb, "Counts_byRegion_Class",  region_counts)
writeData(wb, "Country_ISO_Class",      iso_by_class)
writeData(wb, "Legend_groups",          legend_groups)
writeData(wb, "Legend_groups_byRegion", legend_groups_byRegion)
writeData(wb, "Rows_used",              rows_used)

bold <- createStyle(textDecoration = "bold")
for (sh in sheets(wb)) { addStyle(wb, sh, bold, rows = 1, cols = 1:200, gridExpand = TRUE)
                         setColWidths(wb, sh, 1:200, "auto") }

# If the file is open in Excel you'll get "Permission denied".
# Close it or change the filename here:
saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved -> ", out_xlsx)





FIGURE 3D4
# ==============================
# Figure 3D4 – State of development of in vitro gene banks
# Classes: Established / Planned / Not established / No data
# ==============================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# ---- Paths
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3D4_GeneBank_MapTables.xlsx"

# ---- Region order (use your book order/spelling)
region_levels <- c(
  "Africa","Asia","Southwest Pacific","Europe",
  "Latin America and the Carribean","North America",
  "Near East","World"
)

# ---- Small helpers
norm_names <- function(x) x |>
  str_squish() |> str_replace_all("[^A-Za-z0-9]+","_") |> tolower()

pick <- function(df, ...) {
  cands <- unlist(list(...))
  hit   <- cands[cands %in% names(df)]
  if (length(hit)) hit[1] else NA_character_
}

yes_like <- function(x) {
  # robust yes detector
  xl <- tolower(str_squish(as.character(x)))
  xl %in% c("yes","y","true","1")
}

# ---- Read and normalise
raw <- read_excel(in_path)
names(raw) <- norm_names(names(raw))

col_region   <- pick(raw, "region")
col_country  <- pick(raw, "country")
col_isocode  <- pick(raw, "isocode","iso_code","iso3","iso3code","iso3_code")
col_section  <- pick(raw, "section")
col_question <- pick(raw, "question_2024","question")
col_answer   <- pick(raw, "answer")

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_answer))

# Restrict to the survey section we use
raw2 <- raw %>%
  transmute(
    Region   = str_squish(as.character(.data[[col_region]])),
    Country  = str_squish(as.character(.data[[col_country]])),
    ISOCode  = if (!is.na(col_isocode)) str_squish(as.character(.data[[col_isocode]])) else NA_character_,
    Section  = .data[[col_section]],
    Question = .data[[col_question]],
    Answer   = .data[[col_answer]]
  ) %>%
  filter(Section %in% c(2, "2")) %>%
  filter(!is.na(Region), !is.na(Country))

# ---- Parse Q23 (operational in vitro genebank?)
q23 <- raw2 %>%
  filter(grepl("^\\s*23(\\.|$)", as.character(Question))) %>%
  mutate(q23_yes = yes_like(Answer)) %>%
  group_by(Region, Country) %>%
  summarise(
    q23_any      = any(!is.na(q23_yes)),     # did they answer at all?
    q23_yes_any  = any(q23_yes, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Parse Q26 (plans for regional/subregional genebank?)
q26 <- raw2 %>%
  filter(grepl("^\\s*26(\\.|$)", as.character(Question))) %>%
  mutate(q26_yes = yes_like(Answer)) %>%
  group_by(Region, Country) %>%
  summarise(
    q26_any     = any(!is.na(q26_yes)),
    q26_yes_any = any(q26_yes, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Universe of reporting countries (to flag "No data" properly)
universe <- raw2 %>%
  distinct(Region, Country, ISOCode)

# ---- Join & classify
class_levels <- c("Established","Planned","Not established","No data")

country_classes <- universe %>%
  left_join(q23, by = c("Region","Country")) %>%
  left_join(q26, by = c("Region","Country")) %>%
  mutate(
    q23_any      = coalesce(q23_any, FALSE),
    q23_yes_any  = coalesce(q23_yes_any, FALSE),
    q26_any      = coalesce(q26_any, FALSE),
    q26_yes_any  = coalesce(q26_yes_any, FALSE),

    Class = case_when(
      q23_yes_any ~ "Established",
      (!q23_yes_any & q26_yes_any) ~ "Planned",
      (q23_any & !q23_yes_any & !q26_yes_any) ~ "Not established",
      TRUE ~ "No data"
    ),
    Class = factor(Class, levels = class_levels)
  ) %>%
  arrange(factor(Region, levels = region_levels), Country)

# ---- Region x Class counts (+ World)
region_counts <- country_classes %>%
  count(Region, Class, name = "Countries") %>%
  arrange(factor(Region, levels = region_levels), Class)

world_counts <- region_counts %>%
  filter(Region != "World") %>%
  group_by(Class) %>%
  summarise(Countries = sum(Countries), .groups = "drop") %>%
  mutate(Region = "World")

region_counts <- bind_rows(
  region_counts %>% filter(Region != "World"),
  world_counts
) %>% arrange(factor(Region, levels = region_levels), Class)

# ---- ISO listing sheet (handy for GIS) and legend groupings
iso_by_class <- country_classes %>%
  select(Region, Country, ISOCode, Class) %>%
  arrange(factor(Region, levels = region_levels), Country)

legend_groups <- iso_by_class %>%
  group_by(Class) %>%
  summarise(
    Count     = n_distinct(Country),
    ISOCodes  = paste(sort(unique(ISOCode[!is.na(ISOCode) & ISOCode != ""])), collapse = ", "),
    Countries = paste(sort(unique(Country)), collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(Class)

legend_groups_byRegion <- iso_by_class %>%
  group_by(Region, Class) %>%
  summarise(
    Count    = n_distinct(Country),
    ISOCodes = paste(sort(unique(ISOCode[!is.na(ISOCode) & ISOCode != ""])), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(factor(Region, levels = region_levels), Class)

# ---- QA: show the flags used for the classification
qa_flags <- universe %>%
  left_join(q23, by = c("Region","Country")) %>%
  left_join(q26, by = c("Region","Country")) %>%
  mutate(
    q23_any      = coalesce(q23_any, FALSE),
    q23_yes_any  = coalesce(q23_yes_any, FALSE),
    q26_any      = coalesce(q26_any, FALSE),
    q26_yes_any  = coalesce(q26_yes_any, FALSE)
  ) %>%
  left_join(country_classes %>% select(Region, Country, Class), by = c("Region","Country")) %>%
  arrange(factor(Region, levels = region_levels), Country)

# ---- Export workbook
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "Country_classes")         # main map table
addWorksheet(wb, "Counts_byRegion_Class")   # regional counts (+ world)
addWorksheet(wb, "Country_ISO_Class")       # ISOCode listing
addWorksheet(wb, "Legend_groups")           # overall legend groups
addWorksheet(wb, "Legend_groups_byRegion")  # legend groups per region
addWorksheet(wb, "QA_Q23_Q26_flags")        # audit of inputs to the class

readme <- paste(
  "Figure 3D4 – State of development of in vitro gene banks (Q23, Q26).",
  "",
  "Classification logic:",
  " • Established: any YES to Q23 (operational in vitro genebank).",
  " • Planned: Q23 not YES, but YES to Q26 (plans for a regional/subregional genebank).",
  " • Not established: country answered Q23 but it was not YES, and Q26 not YES.",
  " • No data: no records for both Q23 and Q26.",
  "",
  "Sheets:",
  " • Country_classes – Region, Country, ISOCode, Class.",
  " • Counts_byRegion_Class – regional tallies + World totals.",
  " • Country_ISO_Class – convenient feed for GIS.",
  " • Legend_groups / Legend_groups_byRegion – comma-separated ISO lists per legend colour.",
  " • QA_Q23_Q26_flags – the exact flags used (q23_any/q23_yes_any/q26_any/q26_yes_any).",
  sep = "\n"
)

writeData(wb, "README",                 readme)
writeData(wb, "Country_classes",        country_classes)
writeData(wb, "Counts_byRegion_Class",  region_counts)
writeData(wb, "Country_ISO_Class",      iso_by_class)
writeData(wb, "Legend_groups",          legend_groups)
writeData(wb, "Legend_groups_byRegion", legend_groups_byRegion)
writeData(wb, "QA_Q23_Q26_flags",       qa_flags)

bold <- createStyle(textDecoration = "bold")
for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:200, gridExpand = TRUE)
  setColWidths(wb, sh, 1:200, "auto")
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved -> ", out_xlsx)









FIGURE 3D4
# ==============================
# Figure 3D4 – State of development of in vitro gene banks
# Classes: Established / Planned / Not established / No data
# ==============================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(stringr); library(forcats); library(openxlsx)
})

# ---- Paths
in_path  <- "C:/Users/LENOVO/Documents/Fig3B1_Data.xlsx"
out_xlsx <- "C:/Users/LENOVO/Documents/Fig3D4_GeneBank_MapTables.xlsx"

# ---- Region order (use your book order/spelling)
region_levels <- c(
  "Africa","Asia","Southwest Pacific","Europe",
  "Latin America and the Carribean","North America",
  "Near East","World"
)

# ---- Small helpers
norm_names <- function(x) x |>
  str_squish() |> str_replace_all("[^A-Za-z0-9]+","_") |> tolower()

pick <- function(df, ...) {
  cands <- unlist(list(...))
  hit   <- cands[cands %in% names(df)]
  if (length(hit)) hit[1] else NA_character_
}

yes_like <- function(x) {
  # robust yes detector
  xl <- tolower(str_squish(as.character(x)))
  xl %in% c("yes","y","true","1")
}

# ---- Read and normalise
raw <- read_excel(in_path)
names(raw) <- norm_names(names(raw))

col_region   <- pick(raw, "region")
col_country  <- pick(raw, "country")
col_isocode  <- pick(raw, "isocode","iso_code","iso3","iso3code","iso3_code")
col_section  <- pick(raw, "section")
col_question <- pick(raw, "question_2024","question")
col_answer   <- pick(raw, "answer")

stopifnot(!is.na(col_region), !is.na(col_country), !is.na(col_section),
          !is.na(col_question), !is.na(col_answer))

# Restrict to the survey section we use
raw2 <- raw %>%
  transmute(
    Region   = str_squish(as.character(.data[[col_region]])),
    Country  = str_squish(as.character(.data[[col_country]])),
    ISOCode  = if (!is.na(col_isocode)) str_squish(as.character(.data[[col_isocode]])) else NA_character_,
    Section  = .data[[col_section]],
    Question = .data[[col_question]],
    Answer   = .data[[col_answer]]
  ) %>%
  filter(Section %in% c(2, "2")) %>%
  filter(!is.na(Region), !is.na(Country))

# ---- Parse Q23 (operational in vitro genebank?)
q23 <- raw2 %>%
  filter(grepl("^\\s*23(\\.|$)", as.character(Question))) %>%
  mutate(q23_yes = yes_like(Answer)) %>%
  group_by(Region, Country) %>%
  summarise(
    q23_any      = any(!is.na(q23_yes)),     # did they answer at all?
    q23_yes_any  = any(q23_yes, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Parse Q26 (plans for regional/subregional genebank?)
q26 <- raw2 %>%
  filter(grepl("^\\s*26(\\.|$)", as.character(Question))) %>%
  mutate(q26_yes = yes_like(Answer)) %>%
  group_by(Region, Country) %>%
  summarise(
    q26_any     = any(!is.na(q26_yes)),
    q26_yes_any = any(q26_yes, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Universe of reporting countries (to flag "No data" properly)
universe <- raw2 %>%
  distinct(Region, Country, ISOCode)

# ---- Join & classify
class_levels <- c("Established","Planned","Not established","No data")

country_classes <- universe %>%
  left_join(q23, by = c("Region","Country")) %>%
  left_join(q26, by = c("Region","Country")) %>%
  mutate(
    q23_any      = coalesce(q23_any, FALSE),
    q23_yes_any  = coalesce(q23_yes_any, FALSE),
    q26_any      = coalesce(q26_any, FALSE),
    q26_yes_any  = coalesce(q26_yes_any, FALSE),

    Class = case_when(
      q23_yes_any ~ "Established",
      (!q23_yes_any & q26_yes_any) ~ "Planned",
      (q23_any & !q23_yes_any & !q26_yes_any) ~ "Not established",
      TRUE ~ "No data"
    ),
    Class = factor(Class, levels = class_levels)
  ) %>%
  arrange(factor(Region, levels = region_levels), Country)

# ---- Region x Class counts (+ World)
region_counts <- country_classes %>%
  count(Region, Class, name = "Countries") %>%
  arrange(factor(Region, levels = region_levels), Class)

world_counts <- region_counts %>%
  filter(Region != "World") %>%
  group_by(Class) %>%
  summarise(Countries = sum(Countries), .groups = "drop") %>%
  mutate(Region = "World")

region_counts <- bind_rows(
  region_counts %>% filter(Region != "World"),
  world_counts
) %>% arrange(factor(Region, levels = region_levels), Class)

# ---- ISO listing sheet (handy for GIS) and legend groupings
iso_by_class <- country_classes %>%
  select(Region, Country, ISOCode, Class) %>%
  arrange(factor(Region, levels = region_levels), Country)

legend_groups <- iso_by_class %>%
  group_by(Class) %>%
  summarise(
    Count     = n_distinct(Country),
    ISOCodes  = paste(sort(unique(ISOCode[!is.na(ISOCode) & ISOCode != ""])), collapse = ", "),
    Countries = paste(sort(unique(Country)), collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(Class)

legend_groups_byRegion <- iso_by_class %>%
  group_by(Region, Class) %>%
  summarise(
    Count    = n_distinct(Country),
    ISOCodes = paste(sort(unique(ISOCode[!is.na(ISOCode) & ISOCode != ""])), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(factor(Region, levels = region_levels), Class)

# ---- QA: show the flags used for the classification
qa_flags <- universe %>%
  left_join(q23, by = c("Region","Country")) %>%
  left_join(q26, by = c("Region","Country")) %>%
  mutate(
    q23_any      = coalesce(q23_any, FALSE),
    q23_yes_any  = coalesce(q23_yes_any, FALSE),
    q26_any      = coalesce(q26_any, FALSE),
    q26_yes_any  = coalesce(q26_yes_any, FALSE)
  ) %>%
  left_join(country_classes %>% select(Region, Country, Class), by = c("Region","Country")) %>%
  arrange(factor(Region, levels = region_levels), Country)

# ---- Export workbook
wb <- createWorkbook()
addWorksheet(wb, "README")
addWorksheet(wb, "Country_classes")         # main map table
addWorksheet(wb, "Counts_byRegion_Class")   # regional counts (+ world)
addWorksheet(wb, "Country_ISO_Class")       # ISOCode listing
addWorksheet(wb, "Legend_groups")           # overall legend groups
addWorksheet(wb, "Legend_groups_byRegion")  # legend groups per region
addWorksheet(wb, "QA_Q23_Q26_flags")        # audit of inputs to the class

readme <- paste(
  "Figure 3D4 – State of development of in vitro gene banks (Q23, Q26).",
  "",
  "Classification logic:",
  " • Established: any YES to Q23 (operational in vitro genebank).",
  " • Planned: Q23 not YES, but YES to Q26 (plans for a regional/subregional genebank).",
  " • Not established: country answered Q23 but it was not YES, and Q26 not YES.",
  " • No data: no records for both Q23 and Q26.",
  "",
  "Sheets:",
  " • Country_classes – Region, Country, ISOCode, Class.",
  " • Counts_byRegion_Class – regional tallies + World totals.",
  " • Country_ISO_Class – convenient feed for GIS.",
  " • Legend_groups / Legend_groups_byRegion – comma-separated ISO lists per legend colour.",
  " • QA_Q23_Q26_flags – the exact flags used (q23_any/q23_yes_any/q26_any/q26_yes_any).",
  sep = "\n"
)

writeData(wb, "README",                 readme)
writeData(wb, "Country_classes",        country_classes)
writeData(wb, "Counts_byRegion_Class",  region_counts)
writeData(wb, "Country_ISO_Class",      iso_by_class)
writeData(wb, "Legend_groups",          legend_groups)
writeData(wb, "Legend_groups_byRegion", legend_groups_byRegion)
writeData(wb, "QA_Q23_Q26_flags",       qa_flags)

bold <- createStyle(textDecoration = "bold")
for (sh in sheets(wb)) {
  addStyle(wb, sh, bold, rows = 1, cols = 1:200, gridExpand = TRUE)
  setColWidths(wb, sh, 1:200, "auto")
}

saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Saved -> ", out_xlsx)




What questions are used

Q23: “Does your country have an operational in vitro gene bank…?”
Expected answers: Yes/No (in your file they appear as yes/no).

Q26: “Does your country have plans to enter into collaboration… to set up a regional/sub-regional in vitro gene bank?”
Expected answers: Yes/No.

In code, anything that looks like yes (case-insensitive; also accepts y, true, 1) is treated as Yes. Everything else (e.g., no, blank, NA) is not-Yes.

Classification rules

Established → Q23 = Yes (country reports an operational in-vitro genebank).
(Q26 can be anything; once Q23 is Yes, it’s “Established”.)

Planned → Q23 ≠ Yes AND Q26 = Yes
(no operational bank, but they report plans to set one up, usually via collaboration).

Not established → country answered Q23 but it wasn’t Yes, and Q26 ≠ Yes
(i.e., they reported on Q23 but don’t have a bank and didn’t say they’re planning one).

No data → no records for both Q23 and Q26 for that country
(we can’t tell either way).

Quick decision table
Q23 (operational?)	Q26 (plans?)	Class
Yes	Yes/No/blank	Established
No/blank	Yes	Planned
No	No/blank	Not established
blank	blank	No data
What we did not use (by default)

Q24/Q25 (what material is stored; number of breeds; use of material, etc.) are not required for the map classification. If you want, we can treat any evidence in Q24/25 as “Established” too—easy to flip on.

If you want me to include Q24/25 as additional proof of “Established” (e.g., any stored material ⇒ Established), say the word and I’ll give you that variant.