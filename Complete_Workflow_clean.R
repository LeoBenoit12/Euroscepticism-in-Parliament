### ============================================================= ###
###        COMPLETE WORKFLOW — EU STANCE CLASSIFICATION           ###
### ============================================================= ###
### Pipeline overview:
###   1–4   : Load, clean and tokenise raw speeches
###   5–6   : Translate and flag EU-related sentences (dictionary)
###   7–9   : First annotation round → standardise labels
###  10–16  : EU Model (EUroBerta-xlm) — two training rounds
###  17–22  : Opposition Model — iterative annotation (rounds 2–6)
###  23–25  : Support Model — iterative annotation (rounds 3–5)
###  26     : Merge all annotated data → final classified corpus
### ============================================================= ###


### ---- Load Required Packages ---- ###

library(readr)
library(tidyverse)
library(readxl)
library(stringr)
library(tidytext)
library(writexl)


### ===============================================================
### STEP 1. LOAD RAW SPEECHES
### ===============================================================

speech_files <- list(
  DEU = "Full speech/DEU_speech.csv",
  ES  = "Full speech/ES_speech.csv",
  SWE = "Full speech/SWE_speech.csv",
  CZE = "Full speech/CZE_speech.csv",
  AUS = "Full speech/AUS_speech.csv",
  NL  = "Full speech/NL_speech.csv",
  DNK = "Full speech/DNK_speech.csv",
  UK  = "Full speech/UK_speech.csv"
)

speeches <- lapply(speech_files, read_csv)


### ===============================================================
### STEP 2. CLEAN SPEECH TEXT
### ===============================================================

clean_text <- function(df) {
  df$text <- gsub("https?://[^\\.]+\\.|[âÂ]|\\b\\d+\\s*\\.", "", df$text)
  df$text <- gsub("\\s+", " ", df$text)
  return(df)
}

speeches <- lapply(speeches, clean_text)

invisible(Map(function(data, name) {
  write.csv(data, paste0(name, "_speech.csv"), row.names = FALSE)
}, speeches, names(speeches)))


### ===============================================================
### STEP 3. SPLIT SPEECHES INTO SENTENCES
### ===============================================================

split_sentences <- function(df) {
  df %>%
    mutate(speech_ID = row_number()) %>%
    unnest_tokens(output = sentence, input = text, token = "sentences")
}

sentences <- lapply(speeches, split_sentences)

invisible(Map(function(data, name) {
  write.csv(data, paste0(name, "_sentence.csv"), row.names = FALSE)
}, sentences, names(sentences)))


### ===============================================================
### STEP 4. SAMPLE SENTENCES FOR TRANSLATION
### ===============================================================
### Keeps only sentences with more than 5 tokens to exclude
### headers, footnotes, and artefacts. Samples 12 000 per country.

sample_sentences <- function(df, n = 12000) {
  df %>%
    filter(str_count(sentence, "\\S+") > 5) %>%
    sample_n(n)
}

set.seed(123)
samples <- lapply(sentences, sample_sentences)

invisible(Map(function(data, name) {
  write_csv(data, paste0(name, "_subset.csv"))
}, samples, names(samples)))


### ===============================================================
### STEP 5. LOAD TRANSLATED SPEECHES AND EU DICTIONARY
### ===============================================================

translated_files <- list(
  DEU = "Translated Speech/DEU_speech_translated.csv",
  AUS = "Translated Speech/AUS_speech_translated.csv",
  ES  = "Translated Speech/ES_speech_translated.csv",
  NL  = "Translated Speech/NL_speech_translated.csv",
  SWE = "Translated Speech/SWE_speech_translated.csv",
  CZE = "Translated Speech/CZE_speech_translated.csv",
  DNK = "Translated Speech/DNK_speech_translated.csv"
)

translated_speeches <- lapply(translated_files, read_csv)
EU_Dictionary       <- read_excel("EU_Dictionary.xlsx") 


### ===============================================================
### STEP 6. FLAG EU-RELATED SENTENCES (DICTIONARY APPROACH)
### ===============================================================
### Build a regex pattern from the EU Dictionary (polity / politics
### / policy columns). "eu" and "ep" are wrapped in word boundaries
### to avoid spurious matches (e.g. "europe").

keyword_pattern <- EU_Dictionary %>%
  select(polity, politics, policy) %>%
  unlist() %>%
  na.omit() %>%
  gsub('["\n]', "", .) %>%
  ifelse(. %in% c("eu", "ep"), paste0("\\b", ., "\\b"), .) %>%
  paste(collapse = "|")

flag_eu_sentences <- function(df) {
  df %>%
    mutate(eu = if_else(
      str_detect(translated_sentence, regex(keyword_pattern, ignore_case = TRUE)),
      1L, 0L
    ))
}

translated_flagged <- lapply(translated_speeches, flag_eu_sentences)

invisible(Map(function(data, name) {
  write.csv(data, paste0(name, "_speech_translated.csv"), row.names = FALSE)
}, translated_flagged, names(translated_flagged)))


### ===============================================================
### STEP 7. CREATE FIRST-ROUND ANNOTATION SAMPLES (STRATIFIED)
### ===============================================================
### Sample 600 sentences per country (360 non-EU / 240 EU) and
### assign coders. The "Both" slice is double-coded for reliability.

create_stratified_sample <- function(df) {
  bind_rows(
    df %>% filter(eu == 0) %>% slice_sample(n = 360),
    df %>% filter(eu == 1) %>% slice_sample(n = 240)
  ) %>%
    slice_sample(n = 600) %>%
    mutate(Coder = case_when(
      row_number() <= 250              ~ "Leo",
      row_number() <= 350              ~ "Both",
      TRUE                             ~ "Duarte"
    ))
}

set.seed(1234)
samples_annotated <- lapply(translated_flagged, create_stratified_sample)

invisible(Map(function(data, name) {
  write_xlsx(data, paste0(name, "_sample.xlsx"))
}, samples_annotated, names(samples_annotated)))


### ===============================================================
### STEP 8. LOAD AND COMBINE FIRST-ROUND ANNOTATED SAMPLES
### ===============================================================

read_and_clean <- function(path) {
  read_excel(path) %>%
    select(-starts_with("..."))  # Remove unnamed index columns
}

sample_files <- list(
  UK  = "PJ 1 - Euroscepticism /Samples for Coding/UK_sample.xlsx",
  SWE = "PJ 1 - Euroscepticism /Samples for Coding/SWE_sample.xlsx",
  NL  = "PJ 1 - Euroscepticism /Samples for Coding/NL_sample.xlsx",
  ES  = "PJ 1 - Euroscepticism /Samples for Coding/ES_sample.xlsx",
  DNK = "PJ 1 - Euroscepticism /Samples for Coding/DNK_sample.xlsx",
  DEU = "PJ 1 - Euroscepticism /Samples for Coding/DEU_sample.xlsx",
  AUS = "PJ 1 - Euroscepticism /Samples for Coding/AUS_sample.xlsx"
)

annotated_samples  <- lapply(sample_files, read_and_clean)
Annotated_speech   <- bind_rows(annotated_samples)


### ===============================================================
### STEP 9. STANDARDISE LABELS AND CREATE LABEL_ID
### ===============================================================
### Label schema:
###   0 = No EU  |  1 = Pro EU  |  2 = Neutral
###   3 = Soft Eurosceptic  |  4 = Hard Eurosceptic

Annotated_speech_V1 <- Annotated_speech %>%
  mutate(Label = str_trim(tolower(Label))) %>%
  mutate(Label = case_when(
    Label %in% c("no eu", "no") ~ "No EU",
    Label %in% c("pro eu", "pro") ~ "Pro EU",
    Label == "neutral"           ~ "Neutral",
    Label == "soft"              ~ "Soft",
    Label == "hard"              ~ "Hard",
    TRUE                         ~ NA_character_
  )) %>%
  mutate(Label_ID = case_when(
    Label == "No EU"   ~ 0,
    Label == "Pro EU"  ~ 1,
    Label == "Neutral" ~ 2,
    Label == "Soft"    ~ 3,
    Label == "Hard"    ~ 4,
    TRUE               ~ NA_integer_
  ))

write_csv(Annotated_speech_V1, "Annotated_speech_V1.csv")


### ===============================================================
### STEP 10. EU CLASSIFIER — FIRST TRAINING ROUND
### ===============================================================
### Collapse the five original labels into binary EU / Non-EU:
###   0 = No EU  |  1 = EU (Pro, Neutral, Soft, Hard)

FirstRound_EUclassification <- Annotated_speech_V1 %>%
  mutate(eu = case_when(
    Label_ID == 0     ~ 0L,
    Label_ID %in% 1:4 ~ 1L,
    TRUE              ~ NA_integer_
  ))

write_csv(FirstRound_EUclassification, "FirstRound_EUclassification.csv")


### ===============================================================
### STEP 11. LOAD OUTPUT FROM EU MODEL V1 (EUroBerta-xlm)
### ===============================================================

classified_files <- list(
  AUS = "Classified EU/AUS_classified_eu.csv",
  ES  = "Classified EU/ES_classified_eu.csv",
  NL  = "Classified EU/NL_classified_eu.csv",
  DEU = "Classified EU/DEU_classified_eu.csv",
  DNK = "Classified EU/DNK_classified_eu.csv",
  SWE = "Classified EU/SWE_classified_eu.csv",
  UK  = "Classified EU/UK_classified_eu.csv"
)

classified_speeches <- lapply(classified_files, function(path) {
  read_csv(path) %>% mutate(EU = as.integer(EU))
})

list2env(classified_speeches, envir = .GlobalEnv)  # Assigns AUS, ES, ... UK


### ===============================================================
### STEP 12. FILTER EU-RELATED SENTENCES AND COMBINE
### ===============================================================

eu_speeches <- lapply(classified_speeches, function(df) filter(df, EU == 1))

Combined <- bind_rows(eu_speeches) %>%
  select(-agenda)

write_csv(Combined, "EUSpeeches_classified_V1.csv")


### ===============================================================
### STEP 13. SECOND-ROUND ANNOTATION SAMPLE
### ===============================================================
### Sample 250 EU-related sentences per country for re-annotation.
### UKIP is oversampled (~50 sentences) to improve coverage of
### radical-right oppositional discourse.

create_eu_sample <- function(df, rrp_party = NULL, rrp_n = 50, total_n = 250) {
  if (!is.null(rrp_party)) {
    rrp_sample <- df %>%
      filter(EU == 1, party == rrp_party) %>%
      slice_sample(n = rrp_n)

    random_sample <- df %>%
      filter(EU == 1) %>%
      anti_join(rrp_sample, by = "speech_ID") %>%
      slice_sample(n = total_n - rrp_n)

    bind_rows(rrp_sample, random_sample)
  } else {
    df %>%
      filter(EU == 1) %>%
      slice_sample(n = total_n)
  }
}

set.seed(123)
UK_eu_sample  <- create_eu_sample(UK,  rrp_party = "UKIP")
SWE_eu_sample <- create_eu_sample(SWE)
DNK_eu_sample <- create_eu_sample(DNK)
NL_eu_sample  <- create_eu_sample(NL)
AUS_eu_sample <- create_eu_sample(AUS)
DEU_eu_sample <- create_eu_sample(DEU)
ES_eu_sample  <- create_eu_sample(ES)

invisible(Map(function(data, name) {
  write_xlsx(data, paste0("Samples/", name, "_eu_sample.xlsx"))
}, list(UK = UK_eu_sample, SWE = SWE_eu_sample, DNK = DNK_eu_sample,
        NL = NL_eu_sample, AUS = AUS_eu_sample, DEU = DEU_eu_sample,
        ES = ES_eu_sample),
   c("UK", "SWE", "DNK", "NL", "AUS", "DEU", "ES")))


### ===============================================================
### STEP 14. LOAD AND COMBINE SECOND-ROUND ANNOTATED SAMPLES
### ===============================================================

second_round_files <- list(
  UK  = "Samples for Coding/Second round samples/UK_eu_sample.xlsx",
  SWE = "Samples for Coding/Second round samples/SWE_eu_sample.xlsx",
  DNK = "Samples for Coding/Second round samples/DNK_eu_sample.xlsx",
  NL  = "Samples for Coding/Second round samples/NL_eu_sample.xlsx",
  AUS = "Samples for Coding/Second round samples/AUS_eu_sample.xlsx",
  DEU = "Samples for Coding/Second round samples/DEU_eu_sample.xlsx",
  ES  = "Samples for Coding/Second round samples/ES_eu_sample.xlsx"
)

second_round_samples <- lapply(second_round_files, read_excel)

Annotated_speech_V2 <- bind_rows(second_round_samples) %>%
  mutate(Label_ID = case_when(
    Label == "No EU"   ~ 0,
    Label == "Pro"     ~ 1,
    Label == "Neutral" ~ 2,
    Label == "Soft"    ~ 3,
    Label == "Hard"    ~ 4,
    TRUE               ~ NA_integer_
  ))

write_csv(Annotated_speech_V2, "Annotated_speech_V2.csv")


### ===============================================================
### STEP 15. PREPARE BINARY EU LABEL FOR EU MODEL V2
### ===============================================================

EU_classification_second <- Annotated_speech_V2 %>%
  mutate(eu = if_else(Label_ID == 0, 0, 1)) %>%
  select(-EU)


### ===============================================================
### STEP 16. MERGE ROUNDS 1 & 2 → FINAL TRAINING SET FOR EU MODEL
### ===============================================================
### Downsample the non-EU majority class from Round 1 to balance
### the combined training set before retraining EUroBerta-xlm.

FirstRound_data <- read_csv("FirstRound_EUclassification.csv") %>%
  select(-translated_sentence) %>%
  rename(sentence = text) %>%
  select(-1)

set.seed(123)
balanced_first_round <- bind_rows(
  FirstRound_data %>% filter(eu == 0) %>% slice_sample(n = 1048),
  FirstRound_data %>% filter(eu == 1) %>% slice_sample(n = 202)
)

Training_set_EU_Classifier <- bind_rows(balanced_first_round, EU_classification_second)

write_csv(Training_set_EU_Classifier, "SecondRound_EuClassification.csv")

### → Train EUroBerta-xlm V2 on SecondRound_EuClassification.csv


### ===============================================================
### STEP 17. LOAD OUTPUT FROM EU MODEL V2 AND FILTER EU SENTENCES
### ===============================================================

EU_Speeches_classified <- read_csv("Data_EU sentence/EU_Speeches_classified.csv") %>%
  select(-1)

EUSpeeches_classified_V2 <- EU_Speeches_classified %>%
  filter(EU_2 == 1)

write_csv(EUSpeeches_classified_V2, "EUSpeeches_classified_V2.csv")

### Note: EUSpeeches_classified_V2 is the final set of EU-relevant
### sentences after two rounds of EU detection. For salience analysis,
### combine EU_2 == 0 sentences with EUSpeeches_classified_V2.


### ===============================================================
### STEP 18. PREPARE STANCE DATA FOR OPPOSITION MODEL (ROUND 2)
### ===============================================================
### Recode EU sentences into three stance classes:
###   0 = Pro / Neutral  |  1 = Soft  |  2 = Hard

Annotated_speech_V2 <- read_csv("Annotated_speech_V2.csv")

SecondRound_data <- Annotated_speech_V2 %>%
  select(-c(Coder, EU)) %>%
  filter(Label != "No EU") %>%
  mutate(Label_ID = case_when(
    Label == "Pro"                    ~ 0,
    Label == "Neutral"                ~ 1,
    Label %in% c("Soft", "Hard")      ~ 2,
    TRUE                              ~ NA_integer_
  )) %>%
  select(-1)

FirstRound_data_stance <- read_csv("FirstRound_EUclassification.csv") %>%
  select(-translated_sentence) %>%
  rename(sentence = text) %>%
  select(-c(Coder, eu)) %>%
  filter(Label != "No EU") %>%
  mutate(Label_ID = case_when(
    Label == "Pro EU"                 ~ 0,
    Label == "Neutral"                ~ 1,
    Label %in% c("Soft", "Hard")      ~ 2,
    TRUE                              ~ NA_integer_
  )) %>%
  select(-1)

SecondRound_Stance <- bind_rows(FirstRound_data_stance, SecondRound_data)

write_csv(SecondRound_Stance, "SecondRound_Stance.csv")


### ===============================================================
### STEP 19. COLLAPSE TO BINARY STANCE (NON-OPPOSITION vs OPPOSITION)
### ===============================================================

SecondRound_Stance_combined <- SecondRound_Stance %>%
  mutate(Label_ID = case_when(
    Label_ID %in% c(0L, 1) ~ 0,  # Pro / Neutral → Non-Opposition
    Label_ID == 2           ~ 1   # Soft / Hard   → Opposition
  ))

write_csv(SecondRound_Stance_combined, "SecondRound_Stance_combined.csv")


### ===============================================================
### STEP 20. BALANCE TRAINING SET FOR OPPOSITION MODEL
### ===============================================================
### Undersample the Non-Opposition majority class to reduce the
### imbalance before training the first Opposition model iteration.

set.seed(123)
SecondRound_Stance_balanced <- bind_rows(
  SecondRound_Stance_combined %>% filter(Label_ID == 0) %>% slice_sample(n = 1525),
  SecondRound_Stance_combined %>% filter(Label_ID == 1)
)

write_csv(SecondRound_Stance_balanced, "SecondRound_Stance_balanced.csv")

### → Train Opposition Model V1 on SecondRound_Stance_balanced.csv


### ===============================================================
### STEP 21. SAMPLE EU OPPOSITION SENTENCES FOR ROUND 3 ANNOTATION
### ===============================================================
### Select sentences classified as Opposition by Model V1,
### not already in the annotated set — 150 per country.

SecondRound_Stance <- read_csv("SecondRound_Stance_combined.csv") %>%
  select(-1)

EU_stance <- read_csv("EU_stance_classified.csv") %>%
  mutate(Stance = as.integer(Stance))

set.seed(123)
EU_Opposition_sample <- EU_stance %>%
  filter(Stance == 1) %>%
  anti_join(SecondRound_Stance, by = "speech_ID") %>%
  group_by(iso3country) %>%
  slice_sample(n = 150) %>%
  ungroup()

write_xlsx(EU_Opposition_sample, "EU_Opposition_sample.xlsx")

### → Annotate EU_Opposition_sample.xlsx (Round 3)


### ===============================================================
### STEP 22. PREPARE ROUND 4 TRAINING DATASET FOR OPPOSITION MODEL
### ===============================================================

EU_Opposition_sample <- read_excel("EU_Opposition_sample.xlsx")

SecondRound_Stance <- read_csv("Data for Classification/SecondRound_Stance.csv") %>%
  select(-1, -agenda)

ThirdRound <- EU_Opposition_sample %>%
  select(-c(EU, EU_2, Stance)) %>%
  filter(Label != "No EU") %>%
  mutate(Label_ID = case_when(
    Label %in% c("Pro", "Neutral")        ~ 0,
    Label %in% c("Soft", "soft", "Hard")  ~ 1,
    TRUE                                  ~ NA_integer_
  ))

SecondRound_Stance <- SecondRound_Stance %>%
  mutate(Label_ID = case_when(
    Label %in% c("Pro", "Pro EU", "Neutral") ~ 0,
    Label %in% c("Soft", "soft", "Hard")     ~ 1,
    TRUE                                     ~ NA_integer_
  ))

ThirdRound_Stance <- bind_rows(ThirdRound, SecondRound_Stance) %>%
  rename(Code = Label)

write_csv(ThirdRound_Stance, "ThirdRound_Stance.csv")
### ThirdRound_Stance.csv: binary Non-Opposition vs Opposition.
### Used to train Opposition Model V2.


### ===============================================================
### STEP 22b. ROUND 4 — ACTIVE LEARNING SAMPLE FROM MODEL V2 OUTPUT
### ===============================================================

OPPOSITION_CLASSIFIED <- read_csv("FInal classified data/EU_stance_classified_V2.csv")

set.seed(123)
sample_anti <- OPPOSITION_CLASSIFIED %>%
  filter(Stance == "TRUE") %>%
  slice_sample(n = 200)

write_xlsx(sample_anti, "sample_anti.xlsx")

### → Annotate sample_anti.xlsx (Round 4)

sample_anti      <- read_excel("sample_anti.xlsx")
ThirdRound_Stance <- read_csv("ThirdRound_Stance.csv") %>% select(-1)

Round4_Opposition <- sample_anti %>%
  rename(Code = Stance) %>%
  filter(Code != "No") %>%
  mutate(Label_ID = case_when(
    Code == "Neutral" ~ 0,
    Code == "TRUE"    ~ 1,
    TRUE              ~ NA_integer_
  )) %>%
  filter(!is.na(Label_ID))

FourthRound_Opposition <- bind_rows(ThirdRound_Stance, Round4_Opposition)

write_csv(FourthRound_Opposition, "FourthRound_Opposition.csv")
### FourthRound_Opposition.csv: final training set for EuOppostion_Classifier.


### ===============================================================
### STEP 23. BUILD INITIAL TRAINING SET FOR SUPPORT MODEL
### ===============================================================
### Extract Pro / Neutral sentences from ThirdRound_Stance for
### the first iteration of the Support classifier.

ThirdRound_Stance <- read_csv("ThirdRound_Stance.csv")

Class_Support <- ThirdRound_Stance %>%
  mutate(Label_ID = case_when(
    Code %in% c("Pro", "Pro EU") ~ 0,
    Code == "Neutral"            ~ 1,
    TRUE                         ~ NA_integer_
  )) %>%
  drop_na(Label_ID)

write_csv(Class_Support, "Class_Support.csv")


### ===============================================================
### STEP 24. ROUND 4 ANNOTATION — INCREASE PRO-EU COVERAGE
### ===============================================================

EU_Speeches_SecondModel <- read_csv("EU_Speeches_SecondModel.csv")

set.seed(123)
Round4_Annotation <- EU_Speeches_SecondModel %>%
  filter(EU_2 == 1) %>%
  anti_join(ThirdRound_Stance, by = "speech_ID") %>%
  group_by(iso3country) %>%
  slice_sample(n = 75) %>%
  ungroup()

write_xlsx(Round4_Annotation, "Round4_Annotation.xlsx")

### → Annotate Round4_Annotation.xlsx (Round 4 for Support model)

Class_Support     <- read_csv("Class_Support.csv")
Round4_Annotation <- read_excel("Round4_Annotation.xlsx")

Class_Support_2 <- bind_rows(
  Class_Support,
  Round4_Annotation %>%
    filter(Code == "Pro") %>%
    mutate(Label_ID = 0))

write_csv(Class_Support_2, "Class_Support_2.csv")
### Class_Support_2.csv: training data for Support Model V1.


### ===============================================================
### STEP 25. ROUND 5 ANNOTATION — ACTIVE LEARNING FROM MODEL V1
### ===============================================================

OPPOSITION_CLASSIFIED <- read_csv("FInal classified data/EU_stance_classified_V2.csv")

set.seed(123)
Neutral_Support_V1 <- OPPOSITION_CLASSIFIED %>%
  filter(Stance == "FALSE") %>%
  slice_sample(n = 50000)

write_csv(Neutral_Support_V1, "Neutral_Support_V1.csv")

### → Run Support Model V1 on Neutral_Support_V1.csv

SUPPORT_V1 <- read_csv("FInal classified data/Stance_Support_Classified.csv")

set.seed(123)
NewSample_Support <- SUPPORT_V1 %>%
  filter(Stance == "FALSE") %>%
  slice_sample(n = 350)

write_xlsx(NewSample_Support, "Annot_V5.xlsx")

### → Annotate Annot_V5.xlsx (Round 5 for Support model)

Annot_V5 <- read_excel("Annot_V5.xlsx") %>%
  mutate(Code = if_else(Code == "FALSE", "Pro", Code)) %>%
  mutate(Label_ID = case_when(
    Code == "Pro"     ~ 0,
    Code == "Neutral" ~ 1,
    TRUE              ~ NA_integer_
  )) %>%
  drop_na(Label_ID)

Class_Support_2 <- read_csv("Class_Support_2.csv") %>%
  select(-(1:2))

Class_Support_3 <- bind_rows(Class_Support_2, Annot_V5)

write_csv(Class_Support_3, "Class_Support_3.csv")
### Class_Support_3.csv: final training set for EuSupport_Classifier.


### ===============================================================
### STEP 26. OPPOSITION MODEL — ROUNDS 5 & 6 REFINEMENT
### ===============================================================

FourthRound_Opposition <- read_csv("FourthRound_Opposition.csv")

# Extract confirmed Opposition cases for human review
Opposition_for_review <- FourthRound_Opposition %>%
  filter(Label_ID == 1)

write_xlsx(Opposition_for_review, "OppositionReview.xlsx")

# Sample fresh sentences classified as Opposition for Round 5
OppositionReview <- read_excel("OppositionReview.xlsx")

Opposition_Classified <- read_csv("Classified Data/Opposition_Classified.csv") %>%
  select(-1) %>%
  distinct(sentence, .keep_all = TRUE) %>%
  select(-chair, -EU) %>%
  rename(Stance_V2 = Stance) %>%
  mutate(Stance_V1 = case_when(
    Stance_V1 == "FALSE" ~ 0,
    Stance_V1 == "TRUE"  ~ 1,
    TRUE                 ~ NA_real_
  ))

FifthRound_Opposition <- Opposition_Classified %>%
  filter(Stance_V2 == 1) %>%
  anti_join(FourthRound_Opposition, by = "sentence") %>%
  anti_join(OppositionReview,       by = "sentence") %>%
  slice_sample(n = 275)

write_xlsx(FifthRound_Opposition, "FifthRound_Opposition.xlsx")

### → Annotate FifthRound_Opposition.xlsx (Round 5)

FifthRound_Opposition <- read_excel("FifthRound_Opposition.xlsx")
Neutral_4th           <- FourthRound_Opposition %>% filter(Label_ID == 0)

FifthRound_Class <- bind_rows(OppositionReview, FifthRound_Opposition, Neutral_4th) %>%
  distinct(sentence, .keep_all = TRUE) %>%
  filter(Label_ID != 2)

write_csv(FifthRound_Class, "FifthRound_Class.csv")

# Round 6: sample additional Opposition sentences not in Round 5
SixthRound_Opposition <- Opposition_Classified %>%
  filter(Stance_V2 == 1) %>%
  anti_join(FifthRound_Class, by = "sentence") %>%
  slice_sample(n = 500)

write_xlsx(SixthRound_Opposition, "SixthRound_Opposition.xlsx")

### → Annotate SixthRound_Opposition.xlsx (Round 6)

SixthRound_Opposition <- read_excel("SixthRound_Opposition.xlsx") %>%
  filter(Label_ID %in% c(0, 1)) %>%
  mutate(Label_ID = as.numeric(Label_ID))

FifthRound_Class <- FifthRound_Class %>%
  mutate(Label_ID = as.numeric(Label_ID))

SixthRound_Class <- bind_rows(FifthRound_Class, SixthRound_Opposition)

write_csv(SixthRound_Class, "SixthRound_Class.csv")
SixthRound_Class %>% count(Label_ID)


### ===============================================================
### STEP 27. MERGE ALL ANNOTATED DATA → FINAL LABELLED CORPUS
### ===============================================================
### Harmonise Label_ID and Code across the three annotation streams,
### then deduplicate by sentence.

Support_R4    <- read_excel("Class_Support_4.xlsx")
Support_R3    <- read_csv("Class_Support_3.csv")
Opposition_R6 <- read_csv("SixthRound_Class.csv")

Support_R4 <- Support_R4 %>%
  filter(Label_ID %in% c(0, 1, 2)) %>%
  mutate(Code = case_when(
    Label_ID == 0 ~ "Neutral",
    Label_ID == 1 ~ "Support",
    Label_ID == 2 ~ "Opposition"
  ))

Support_R3 <- Support_R3 %>%
  mutate(Label_ID = if_else(Label_ID == 1, 0, 1)) %>%
  mutate(Code = case_when(
    Label_ID == 0 ~ "Neutral",
    Label_ID == 1 ~ "Support"
  ))

Opposition_R6 <- Opposition_R6 %>%
  mutate(Label_ID = case_when(
    Label_ID == 0 ~ 0,
    Label_ID == 1 ~ 2
  )) %>%
  mutate(Code = case_when(
    Label_ID == 0 ~ "Neutral",
    Label_ID == 2 ~ "Opposition"
  ))

ALL_Annotated <- bind_rows(Support_R4, Support_R3, Opposition_R6) %>%
  distinct(sentence, .keep_all = TRUE) %>%
  select(date, speaker, speechnumber, speech_ID, Code, iso3country, party, sentence) %>%
  mutate(sentence = str_replace_all(sentence, ";", ","))

write_csv(ALL_Annotated, "ALL_Annotated.csv")


### ===============================================================
### STEP 28. BUILD FINAL CLASSIFIED CORPUS (ALL SPEECHES)
### ===============================================================
### Combine human-annotated sentences with model predictions for
### the full parliament corpus, removing duplicates.

ALL_Annotated            <- read_csv("ALL_Annotated.csv")
Final_Classified_Speeches <- read_csv("FInal classified data/Final_Classified_Speeches.csv") %>%
  select(-1)

set.seed(123)

# Sample 500 new Opposition / Support sentences predicted by the model
# that are not already in the human-annotated set
New_Class <- Final_Classified_Speeches %>%
  filter(Stance %in% c("Opposition", "Support")) %>%
  anti_join(ALL_Annotated, by = "sentence") %>%
  group_by(Stance) %>%
  slice_sample(n = 500) %>%
  ungroup() %>%
  select(-Opposition, -Support, -Neutral, -Stance_V1, -Stance_V2, -EU_2) %>%
  mutate(
    Label_ID = case_when(
      Stance == "Neutral"    ~ 0,
      Stance == "Support"    ~ 1,
      Stance == "Opposition" ~ 2
    ),
    Pr = 1   # Flag: model-predicted (not human-annotated)
  )

All <- ALL_Annotated %>%
  rename(Stance = Code) %>%
  mutate(Pr = 0)  # Flag: human-annotated

write_csv(New_Class, "New_Class.csv")

Class_Final <- bind_rows(All, New_Class)

write_csv(Class_Final, "3Classification.csv")


### ===============================================================
### STEP 29. BUILD FINAL CLASSIFIED SPEECH DATASET
### ===============================================================
### Merge the three stance classes from Opposition_Classified and
### Support_Classified, and add binary dummy columns.

Support_Classified <- read_csv("Classified Data/Support_Classified_V1.csv") %>%
  mutate(Stance = ifelse(Stance == 1, 0, 1))  # Labels were inverted — correct here

Support <- Support_Classified %>%
  filter(Stance == 1) %>%
  select(-Stance_Probability) %>%
  mutate(Stance = "Support")

Neutral <- Support_Classified %>%
  filter(Stance == 0) %>%
  select(-Stance_Probability) %>%
  mutate(Stance = "Neutral")

Opposition <- Opposition_Classified %>%
  filter(Stance_V2 == 1) %>%
  select(-Stance_Probability) %>%
  mutate(Stance = "Opposition")

Final_Classified_Speeches <- bind_rows(Support, Neutral, Opposition) %>%
  mutate(
    Support    = as.integer(Stance == "Support"),
    Neutral    = as.integer(Stance == "Neutral"),
    Opposition = as.integer(Stance == "Opposition")
  )

write_csv(Final_Classified_Speeches, "Final_Classified_Speeches.csv")

# Filter Non-Opposition sentences for downstream Support classification
Neutral_4Classification <- Opposition_Classified %>%
  filter(Stance_V2 == 0)

write_csv(Neutral_4Classification, "Neutral_4Classification.csv")


### ===============================================================
### STEP 31. FINALISE EU_SPEECHES DATASET (RENAME EU_2 → EU)
### ===============================================================

EU_Speeches_Final <- read_csv("Classified Data/EU_Speeches_Final.csv") %>%
  select(-1) %>%
  select(-EU) %>%
  rename(EU = EU_2)

write_csv(EU_Speeches_Final, "EU_Speeches_Final.csv")
