## Installer packages analyse
library(readr)
library(tidyverse)
library(modelsummary)
library(dplyr)
library(readxl)
library(polyglotr)
library(googleLanguageR)

## Setting up a path to the file directory: [UNCOMMENT TO RUN]
# path <- "YOUR PATH HERE"

### ------Clean data - Filter to Select relevant parties-------- ###

UK_speech <- readRDS(paste(path, "Corp_HouseOfCommons_V2.rds", sep = "")) |>
  mutate(
    date = as.Date(date, format = "%Y-%m-%d")
  ) |>
  filter(party %in% c("UKIP", "Con", "Lab", "LibDem") &
           date > "1999-01-01")

DNK_speech <- readRDS(paste(path, "Corp_Folketing_V2.rds", sep = "")) |>
  mutate(
    date = as.Date(date, format = "%Y-%m-%d")
  ) |>
  filter(party %in% c("FP", "DF", "V", "RV", "LA", "KF", "KRF", "EL", "CD", "S") &
           date > "1999-01-01")

NL_speech <- readRDS(paste(path, "Corp_TweedeKamer_V2.rds", sep = "")) |>
  mutate(
    date = as.Date(date, format = "%Y-%m-%d")
  ) |>
  filter(party %in% c("CDA", "PvdA", "VVD", "D66", "CU", "SGP", "PVV", "FvD", "LPF", "RPF") &
           date > "1999-01-01")

AUS_speech <- readRDS(paste(path, "Corp_Nationalrat_V2.rds", sep = "")) |>
  mutate(
    date = as.Date(date, format = "%Y-%m-%d")
  ) |>
  filter(party %in% c("SPÖ", "FPÖ", "ÖVP", "BZÖ", "NEOS") &
           date > "1999-01-01")

CZE_speech <- readRDS(paste(path, "Corp_PSP_V2.rds", sep = "")) |>
  mutate(
    date = as.Date(date, format = "%Y-%m-%d")
  ) |>
  filter(party %in% c("ODS", "KDU-ČSL", "ANO", "TOP09", "ČSSD", "SPR-RSČ") &
           date > "1999-01-01")

SWE_speech <- readRDS(paste(path, "Corp_Riksdagen_V2.rds", sep = "")) |>
  mutate(
    date = as.Date(date, format = "%Y-%m-%d")
  ) |>
  filter(party %in% c("SD", "C", "M", "KD", "FP", "S", "L") &
           date > "1999-01-01")

ES_speech <- readRDS(paste(path, "Corp_Congreso_V2.rds", sep = "")) |>
  mutate(
    date = as.Date(date, format = "%Y-%m-%d")
  ) |>
  filter(party %in% c("GPSOE", "GPP") &
           date > "1999-01-01")

DEU_speech <- readRDS(paste(path, "Corp_Bundestag_V2.rds", seop = "")) |>
  mutate(
    date = as.Date(date, format = "%Y-%m-%d")
  ) |>
  filter(party %in% c("CDU/CSU", "SPD", "FDP", "AfD") &
           date > "1999-01-01")

### ------Save Cleaned DataSet ----###

write_csv(DEU_speech,"DEU_speech.csv")
write_csv(ES_speech,"ES_speech.csv")
write_csv(SWE_speech,"SWE_speech.csv")
write_csv(CZE_speech,"CZE_speech.csv")
write_csv(AUS_speech,"AUS_speech.csv")
write_csv(NL_speech,"NL_speech.csv")
write_csv(UK_speech,"UK_speech.csv")
write_csv(DNK_speech,"DNK_speech.csv")




