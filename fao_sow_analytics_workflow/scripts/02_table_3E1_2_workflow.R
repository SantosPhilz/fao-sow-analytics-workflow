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

