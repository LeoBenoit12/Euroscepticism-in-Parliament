## Installer packages analyse
library(readr)
library(tidyverse)
library(modelsummary)
library(dplyr)
library(readxl)
library(polyglotr)
library(googleLanguageR)

### ------Clean data - Filter to Select relevant parties-------- ###

UK_speech <- Corp_HouseOfCommons_V2 %>% 
  filter(party %in% c("UKIP", "Con", "Lab", "LibDem"))

DNK_speech <- Corp_Folketing_V2 %>% 
  filter(party %in% c("FP", "DF", "V", "RV", "LA", "KF", "KRF", "EL", "CD", "S"))

NL_speech <- Corp_TweedeKamer_V2 %>% 
  filter(party %in% c("CDA", "PvdA", "VVD", "D66", "CU", "SGP", "PVV", "FvD", "LPF", "RPF"))

AUS_speech <- Corp_Nationalrat_V2 %>% 
  filter(party %in% c("SPÖ", "FPÖ", "ÖVP", "BZÖ", "NEOS"))

CZE_speech <- Corp_PSP_V2 %>% 
  filter(party %in% c("ODS", "KDU-ČSL", "ANO", "TOP09", "ČSSD", "SPR-RSČ"))

SWE_speech <- Corp_Riksdagen_V2 %>% 
  filter(party %in% c("SD", "C", "M", "KD", "FP", "S", "L"))

ES_speech <- Corp_Congreso_V2 %>% 
  filter(party %in% c("GPSOE", "GPP"))

DEU_speech <- Corp_Bundestag_V2 %>% 
  filter(party %in% c("CDU/CSU", "SPD", "FDP", "AfD"))

### ------Save Cleaned DataSet ----###

write_csv(DEU_speech,"DEU_speech.csv")
write_csv(ES_speech,"ES_speech.csv")
write_csv(SWE_speech,"SWE_speech.csv")
write_csv(CZE_speech,"CZE_speech.csv")
write_csv(AUS_speech,"AUS_speech.csv")
write_csv(NL_speech,"NL_speech.csv")
write_csv(UK_speech,"UK_speech.csv")
write_csv(DNK_speech,"DNK_speech.csv")




