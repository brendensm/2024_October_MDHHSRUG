library(dplyr)
library(purrr)
library(quarto)
library(lubridate)
library(fs)
library(palmerpenguins)


# Data frame of arguments -------------------------------------------------


exp <- penguins |>
  distinct(species) |>
  mutate(output_format = "html",
         output_file = paste(
           Sys.Date(),
           tolower(species),
           "report.html",
           sep = "_"
         ),
         execute_params = map(
           species,
           \(species) list(species = species)
         )
  ) |>
  select(output_file, output_format, execute_params)

View(exp)


# Iterate over the data frame ---------------------------------------------

pwalk(
  exp, # our data
  quarto_render, # function to render a quarto document
  input = "Quarto_report.qmd", # the source template report
  .progress = TRUE
)


# Moving our reports to a new folder --------------------------------------

output_dir <- paste0("reports/", month(Sys.Date(), label = TRUE), year(Sys.Date()))

dir_create(output_dir)

files <- dir_ls(regexp = ".html$")

file_move(files, output_dir)
