################################################################
# Name: ATU_table_cleaning.R
# Purpose: Joining fairy books stories to their
# country of origin and AT number
# Creator: N. M. Shahir
# Github: nmshahir
# Data source: University of Missouri Libraries
# https://libraryguides.missouri.edu/c.php?g=1078942&p=7861219
# Data Obtained: 2026.03.10
#################################################################
library(datapasta)
library(dplyr)
library(tidyverse)

# Import table found on reddit
# Source: https://www.reddit.com/r/folklore/comments/1lu402g/atu_index_spreadsheet_for_writers_folklorists/
# Date: 2026.03.16
atu_table_pre_clean <- read.csv2(
  "C:/Users/nmshahir/Documents/Data_Science_Practice/ATU_reddit_pre_cleaning.txt",
  sep = "\t",
  stringsAsFactors = FALSE
)

#How Many Unique Labels Are There? 2237...which is one less than the number of rows....not helpful
length(unique(atu_table_pre_clean$AT))

#How many categories? 7
length(unique(atu_table_pre_clean$Category))

#How many subcategories? 56
length(unique(atu_table_pre_clean$Subcategory))

#Just removing that weird "-" from some of the lines
atu_table <- atu_table_pre_clean |>
  mutate(AT = str_replace_all(AT, "[^[:alnum:][:space:]*]", " ")) |>
  mutate(AT = str_squish(AT))

saveRDS(atu_table, "cleaned_atu_index.rds")
