library(tidyverse)
library(sjPlot)
library(janitor)
library(readxl)
library(skimr)
library(gtsummary)
library(bmisc)
library(gt)

all <- read_csv("data/2020_to_2024_10172024.csv") |>
  mutate(
    gender = factor(gender),
    zip_code = as.character(zip_code),
    case_number = as.character(case_number),
    year_month = format(start_date, "%Y-%m"),
    year = year(start_date),
    age_cat = case_when(
      age %in% 0:4 ~ "<5",
      age %in% 5:12 ~ "5-12",
      age %in% 13:19 ~ "13-19",
      age >= 20 ~ "20+"
    ),
    age_cat = factor(age_cat, levels = c("<5", "5-12", "13-19", "20+"))
  )

all$can_flag <- ifelse(str_detect(
  all$subst_combined,
  "CBD|Marijuana|THC|Cannabinoid|Cannabinoids|Delta 8|Delta 10|Cannabidiol"
), TRUE, FALSE)

all$cminor_s <- ifelse(str_detect(
  all$subst_combined,
  "Minor Cannabinoids|Synthetic Cannabinoids"
), TRUE, FALSE)


all$hhc <- ifelse(str_detect(
  all$subst_combined,
  "HHC"
), TRUE, FALSE)


all$d8 <- ifelse(str_detect(
  all$subst_combined,
  "D8|Delta 11|Delta 8"
), TRUE, FALSE)

all$canflag2 <- ifelse(str_detect(
  all$subst_combined,
  "Minor Cannabinoids|Cannabidiol|eCigarettes: Marijuana Device Flavor Unknown|eCigarettes: Marijuana Device With Added Flavors|eCigarettes: Marijuana Device Without Added Flavors|eCigarettes: Marijuana Liquid Flavor Unknown|eCigarettes: Marijuana Liquid With Added Flavors|eCigarettes: Marijuana Liquid Without Added Flavor|Marijuana: Concentrated Extract|Marijuana: Dried Plant|Marijuana: Edible Preparation|Marijuana: Oral Capsule or Pill Preparation|Marijuana: Other or Unknown Preparation|Marijuana: Pharmaceutical Preparation|Marijuana: Topical Preparation|Marijuana: Undried Plant"
), TRUE, FALSE)



all$synflag <- ifelse(str_detect(
  all$subst_combined,
  "Synthetic Cannabinoids"
), TRUE, FALSE)


all |>
  count(year, cminor_s) |>
  filter(cminor_s) |>
  ggplot(aes(year, n)) +
  geom_col()




clinical_long <- all |> separate_rows(clinical_effect,
  sep = "(?<=\\))\\s*,"
)

clinical_long$clinical_effect <- trimws(clinical_long$clinical_effect)

clinical_long |>
  filter(cminor_s) |>
  count(clinical_effect, sort = T) |>
  View()

# all |>
#   mutate(oc = ifelse(str_detect(all$subst_combined,
#                                 "Delta 8|Delta 10|Cannabidiol"), TRUE, FALSE)) |># View()
#  filter(year == 2024) |>
#   # filter(oc == "TRUE")# |>
#  # count(medical_outcome)
#   count(oc)



all$vape_flag <- ifelse(str_detect(
  all$subst_combined,
  "Vaping|Vape|eCigarettes"
), TRUE, FALSE)

all |> count(vape_flag)
all |>
  #  group_by(year) |>
  count(vape_flag)

all$bflag <- ifelse(all$can_flag & all$vape_flag, TRUE, FALSE)
all |> count(bflag)




can_vape <- all |>
  filter(bflag) |>
  select(year, age_cat, medical_outcome) |>
  tbl_summary( # by = bflag,
    percent = "column",
    label = list(
      # bflag ~ "Cannabis/Vaping Related Case",
      year ~ "Year",
      age_cat ~ "Age Group", medical_outcome ~ "Medical Outcome"
    )
  )

can <- all |>
  filter(can_flag) |>
  select(year, age_cat, medical_outcome) |>
  tbl_summary( # by = bflag,
    percent = "column",
    label = list(
      # bflag ~ "Cannabis/Vaping Related Case",
      year ~ "Year",
      age_cat ~ "Age Group", medical_outcome ~ "Medical Outcome"
    )
  )

all_case <- all |>
  select(year, age_cat, medical_outcome) |>
  tbl_summary( # by = bflag,
    percent = "column",
    label = list(
      # bflag ~ "Cannabis/Vaping Related Case",
      year ~ "Year",
      age_cat ~ "Age Group",
      medical_outcome ~ "Medical Outcome"
    )
  )


tbl_merge(
  tbls = list(can_vape, can, all_case),
  tab_spanner = c(
    "Cannabis and Vape Cases", "Cannabis Cases",
    "All Cases"
  )
)

#|>
# modify_header("**Variable**")# |>

# as_gt() |>
# gt::gtsave("output/cannabis_vape_table.docx")
# modify_footnote(everything()~"An important footnote.")











canflag2_d <- filter(all, canflag2)
canflag_d <- filter(all, can_flag)
all |>
  filter(case_number %in% setdiff(canflag_d$case_number, canflag2_d$case_number)) |>
  View()
