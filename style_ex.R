# install.packages("styler")
# install.packages("lintr")

library(styler)
library(lintr)
library(tidyverse)
library(palmerpenguins)


# messy code with lintr

lint("messy_files/two.R")
lint_dir("messy_files")


# styler

penguins

penguins |>
  group_by(species) |>
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))

style_text("penguins|>group_by(species)|>summarise(mean_bill_length=mean(bill_length_mm,na.rm=TRUE))")

style_file("style_ex.R")

style_file("messy_files/two.R")

style_dir("messy_files")
