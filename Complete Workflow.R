### ---- Load Required Packages ---- ###
library(readr)
library(tidyverse)
library(readxl)
library(stringr)
library(tidytext)
library(writexl)


### ===============================================================
### 1. LOAD RAW DATASETS
### ===============================================================

speech_files <- list(
  DEU = "Full speech/DEU_speech.csv",
  ES = "Full speech/ES_speech.csv",
  SWE = "Full speech/SWE_speech.csv",
  CZE = "Full speech/CZE_speech.csv",
  AUS = "Full speech/AUS_speech.csv",
  NL = "Full speech/NL_speech.csv",
  DNK = "Full speech/DNK_speech.csv",
  UK = "Full speech/UK_speech.csv"
)

speeches <- lapply(speech_files, read_csv)


### ===============================================================
### 2. CLEAN SPEECH TEXT
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
### 3. SPLIT SPEECHES INTO SENTENCES
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
### 4. SAMPLE SENTENCES FOR TRANSLATION
### ===============================================================

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
### 5. LOAD TRANSLATED SPEECHES AND EU DICTIONARY
### ===============================================================

translated_files <- list(
  DEU = "Translated Speech/DEU_speech_translated.csv",
  AUS = "Translated Speech/AUS_speech_translated.csv",
  ES = "Translated Speech/ES_speech_translated.csv",
  NL = "Translated Speech/NL_speech_translated.csv",
  SWE = "Translated Speech/SWE_speech_translated.csv",
  CZE = "Translated Speech/CZE_speech_translated.csv",
  DNK = "Translated Speech/DNK_speech_translated.csv"
)

translated_speeches <- lapply(translated_files, read_csv)
EU_Dictionary <- read_excel("EU_Dictionary.xlsx")


### ===============================================================
### 6. FLAG EU-RELATED SENTENCES
### ===============================================================

keyword_pattern <- EU_Dictionary %>%
  select(polity, politics, policy) %>%
  unlist() %>%
  na.omit() %>%
  gsub('["\n]', '', .) %>%
  ifelse(. %in% c("eu", "ep"), paste0("\\b", ., "\\b"), .) %>%
  paste(collapse = "|")

flag_eu_sentences <- function(df) {
  df %>% mutate(eu = if_else(str_detect(translated_sentence, regex(keyword_pattern, ignore_case = TRUE)), 1, 0))
}

translated_flagged <- mapply(flag_eu_sentences, translated_speeches, SIMPLIFY = FALSE)

invisible(Map(function(data, name) {
  write.csv(data, paste0(name, "_speech_translated.csv"), row.names = FALSE)
}, translated_flagged, names(translated_flagged)))


### ===============================================================
### 7. CREATE FIRST ROUND ANNOTATION SAMPLES (STRATIFIED)
### ===============================================================

create_stratified_sample <- function(df) {
  bind_rows(
    df %>% filter(eu == 0) %>% slice_sample(n = 360),
    df %>% filter(eu == 1) %>% slice_sample(n = 240)
  ) %>%
    slice_sample(n = 600) %>%
    mutate(Coder = case_when(
      row_number() <= 250 ~ "Leo",
      row_number() <= 350 ~ "Both",
      TRUE ~ "Duarte"
    ))
}

set.seed(1234)
samples_annotated <- lapply(translated_flagged, create_stratified_sample)

invisible(Map(function(data, name) {
  write_xlsx(data, paste0(name, "_sample.xlsx"))
}, samples_annotated, names(samples_annotated)))


### ===============================================================
### 8. AFTER FIRST ANNOTATION, LOAD AND CLEAN ANNOTATED SAMPLES
### ===============================================================

# Function to read and clean individual Excel samples
read_and_clean <- function(path) {
  read_excel(path) %>%
    select(-starts_with("..."))
}

# Define file paths for each country's sample
sample_files <- list(
  UK   = "PJ 1 - Euroscepticism /Samples for Coding/UK_sample.xlsx",
  SWE  = "PJ 1 - Euroscepticism /Samples for Coding/SWE_sample.xlsx",
  NL   = "PJ 1 - Euroscepticism /Samples for Coding/NL_sample.xlsx",
  ES   = "PJ 1 - Euroscepticism /Samples for Coding/ES_sample.xlsx",
  DNK  = "PJ 1 - Euroscepticism /Samples for Coding/DNK_sample.xlsx",
  DEU  = "PJ 1 - Euroscepticism /Samples for Coding/DEU_sample.xlsx",
  AUS  = "PJ 1 - Euroscepticism /Samples for Coding/AUS_sample.xlsx"
)

# Load and clean each sample
annotated_samples <- lapply(sample_files, read_and_clean)

# Combine all into one dataset
Annotated_speech <- bind_rows(annotated_samples)


### ===============================================================
### 9. STANDARDIZE LABELS AND CREATE LABEL_ID
### ===============================================================

Annotated_speech <- Annotated_speech %>%
  mutate(Label = str_trim(tolower(Label))) %>%
  mutate(Label = case_when(
    Label %in% c("no eu", "no")     ~ "No EU",
    Label %in% c("pro eu", "pro")   ~ "Pro EU",
    Label == "neutral"              ~ "Neutral",
    Label == "soft"                 ~ "Soft",
    Label == "hard"                 ~ "Hard",
    TRUE                            ~ NA_character_
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
### 10. CLASSIFICATION MODEL 1 — EU VS NON-EU
### ===============================================================

FirstRound_EUclassification <- Annotated_speech_V1 %>%
  mutate(eu = case_when(
    Label_ID == 0      ~ 0,
    Label_ID %in% 1:4  ~ 1,
    TRUE               ~ NA_integer_
  ))

write_csv(FirstRound_EUclassification, "FirstRound_EUclassification.csv")

### ===============================================================
### 11. LOAD CLASSIFIED DATA FROM EU MODEL 1 (EUROBERTA_XLM)
### ===============================================================

# Load Model 1 output (EU classification)
AUS_class <- read_csv("Classified EU/AUS_classified_eu.csv")
ES_class  <- read_csv("Classified EU/ES_classified_eu.csv")
NL_class  <- read_csv("Classified EU/NL_classified_eu.csv")
DEU_class <- read_csv("Classified EU/DEU_classified_eu.csv")
DNK_class <- read_csv("Classified EU/DNK_classified_eu.csv")
SWE_class <- read_csv("Classified EU/SWE_classified_eu.csv")
UK_class  <- read_csv("Classified EU/UK_classified_eu.csv")

# Ensure binary EU variable
UK_class <- UK_class %>%
  mutate(EU = as.integer(EU))

AUS_class <- AUS_class %>%
  mutate(EU = as.integer(EU))

ES_class <- ES_class %>%
  mutate(EU = as.integer(EU))

NL_class <- NL_class %>%
  mutate(EU = as.integer(EU))

DEU_class <- DEU_class %>%
  mutate(EU = as.integer(EU))

DNK_class <- DNK_class %>%
  mutate(EU = as.integer(EU))

SWE_class <- SWE_class %>%
  mutate(EU = as.integer(EU))

### ===============================================================
### 12. FILTER FOR EU-RELATED SPEECHES
### ===============================================================

SWE_EU <- SWE_class %>% filter(EU == 1)
AUS_EU <- AUS_class %>% filter(EU == 1)
ES_EU  <- ES_class  %>% filter(EU == 1)
NL_EU  <- NL_class  %>% filter(EU == 1)
DEU_EU <- DEU_class %>% filter(EU == 1)
DNK_EU <- DNK_class %>% filter(EU == 1)
UK_EU  <- UK_class  %>% filter(EU == 1)

Combined <- bind_rows(SWE_EU, DNK_EU, ES_EU, NL_EU, AUS_EU, DEU_EU, UK_EU) %>%
  select(-agenda)

write_csv(Combined, "EUSpeeches_classified_V1.csv")

### ====================================================================
### 13. SAMPLE 250 EU-RELATED SPEECHES FOR A SECOND CLASSIFICATION ROUND
### ====================================================================

# 50 speeches from UKIP (Oversample for RRP)
RRP_sample <- UK_class %>%
  filter(EU == 1, party == "UKIP") %>%
  slice_sample(n = 50)

# 200 random EU speeches excluding those from UKIP sample
random_sample_200 <- UK_class %>%
  filter(EU == 1) %>%
  anti_join(RRP_sample, by = "speech_ID") %>%
  slice_sample(n = 200)

# Combine and shuffle
UK_eu_sample <- bind_rows(RRP_sample, random_sample_200) %>%
  slice_sample(prop = 1) %>%
  mutate(Coder = case_when(
    row_number() <= 100              ~ "Leo",
    row_number() > 100 & row_number() <= 150 ~ "Both",
    TRUE                             ~ "Duarte"
  ))

# Save
write_xlsx(UK_eu_sample, "Samples/UK_eu_sample.xlsx")

### Repaeat for each country ###


### ======================================================================
### 14. LOAD AND COMBINE ANNOTATED SAMPLES FROM SECOND ROUND ANNOTATION
### =====================================================================

# Load Excel files with manually annotated EU-related speeches
UK_eu_sample   <- read_excel("Samples for Coding/Second round samples/UK_eu_sample.xlsx")
SWE_eu_sample  <- read_excel("Samples for Coding/Second round samples/SWE_eu_sample.xlsx")
DNK_eu_sample  <- read_excel("Samples for Coding/Second round samples/DNK_eu_sample.xlsx")
NL_eu_sample   <- read_excel("Samples for Coding/Second round samples/NL_eu_sample.xlsx")
AUS_eu_sample  <- read_excel("Samples for Coding/Second round samples/AUS_eu_sample.xlsx")
DEU_eu_sample  <- read_excel("Samples for Coding/Second round samples/DEU_eu_sample.xlsx")
ES_eu_sample   <- read_excel("Samples for Coding/Second round samples/ES_eu_sample.xlsx")

# Combine all annotated country samples
Annotated_speech_V2 <- bind_rows(
  UK_eu_sample,
  SWE_eu_sample,
  NL_eu_sample,
  ES_eu_sample,
  DNK_eu_sample,
  DEU_eu_sample,
  AUS_eu_sample
)

# Assign numerical values to stance labels
Annotated_speech_V2 <- Annotated_speech_V2 %>%
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
### 15. CONVERT TO BINARY EU/NON-EU FOR CLASSIFICATION
### ===============================================================

# Create a binary EU label for training a binary classifier
EU_classification_second <- Annotated_speech_V2 %>%
  mutate(eu = if_else(Label_ID == 0, 0, 1)) %>%
  select(-EU)  # Remove old 'EU' column if present

### ===============================================================
### 16. MERGE WITH FIRST ROUND DATA & BALANCE TRAINING SET
### ===============================================================

# Load and clean first round data
FirstRound_data <- read_csv("FirstRound_EuClassification.csv") %>% ## See Step 10
  select(-translated_sentence) %>%
  rename(sentence = text) %>%
  select(-1)  # Remove index column if needed

# Merge first and second round annotated data
SecondRound_EuClassification <- bind_rows(FirstRound_data, EU_classification_second)

# To train a more balanced classifier, downsample overrepresented class (non-EU)
# Target: up to ~1500 examples for each class
sample_0 <- FirstRound_data %>%
  filter(eu == 0) %>%
  slice_sample(n = 1048)

sample_1 <- FirstRound_data %>%
  filter(eu == 1) %>%
  slice_sample(n = 202)

balanced_first_round <- bind_rows(sample_0, sample_1)

# Final training dataset = balanced subset from Round 1 + full Round 2 manually coded data
Training_set_EU_Classifier <- bind_rows(balanced_first_round, EU_classification_second)

write_csv(Training_set_EU_Classifier, "SecondRound_EuClassification.csv")

### Train the second EU model using two rounds of annotated data


### ===============================================================
### 17. FILTER SPEECHES CLASSIFIED AS EU-RELATED by EU MODEL 2
### ===============================================================

# After training the model a second time, run the model on EUSpeeches_classified_V1.csv. 

# Load data classified by both Model 1 and Model 2
EU_Speeches_classified <- read_csv("EU_Speeches_classified.csv") %>%
  select(-1)  # Remove index column if present

# Filter to keep only EU-related speeches according to Model 2
EUSpeeches_classified_V2 <- EU_Speeches_classified %>%
  filter(EU_2 == 1)

# Save filtered dataset
write_csv(EUSpeeches_classified_V2, "EUSpeeches_classified_V2.csv")

## This is the final dataset containing all(and only) the sentences discussing the EU.
## After 2 round of classification and two XLM models trained.
## To measure EU salience, go back to step 11, instead of selecting sentences where EU = 1, 
## select EU == 0, combined with this dataset "EUSpeeches_classified_V2" and add the EU 0 from second classfication


### ===============================================================
### STEP 18. PREPARE DATA FOR STANCE CLASSIFICATION MODEL
### ===============================================================

# Load second-round coded data
Annotated_speech_V2 <- read_csv("Annotated_speech_V2.csv") ## See step 14

SecondRound_data <- Annotated_speech_V2 %>%
  select(-c(Coder, EU)) %>%
  filter(Label != "No EU") %>%
  mutate(Label_ID = case_when(
    Label == "Pro" ~ 0,
    Label == "Neutral" ~ 1,
    Label %in% c("Soft", "Hard") ~ 2,
    TRUE ~ NA_integer_
  )) %>%
  select(-1)  # Remove index column if present

# Load first-round coded data
FirstRound_data <- read_csv("FirstRound_EuClassification.csv") %>%
  select(-translated_sentence) %>%
  rename(sentence = text) %>%
  select(-c(Coder, eu)) %>%
  filter(Label != "No EU") %>%
  mutate(Label_ID = case_when(
    Label == "Pro EU" ~ 0,
    Label == "Neutral" ~ 1,
    Label %in% c("Soft", "Hard") ~ 2,
    TRUE ~ NA_integer_
  )) %>%
  select(-1)  # Remove index column if present

# Combine both rounds
SecondRound_Stance <- bind_rows(FirstRound_data, SecondRound_data)

# Save full dataset with 3-class stance coding
write_csv(SecondRound_Stance, "SecondRound_Stance.csv")

### ===============================================================
### STEP 19. PREPARE BINARY STANCE DATASET (PRO/NEUTRAL vs SOFT/HARD)
### ===============================================================

SecondRound_Stance <- read_csv("SecondRound_Stance.csv")

SecondRound_Stance_combined <- SecondRound_Stance %>%
  mutate(Label_ID = case_when(
    Label_ID %in% c(0, 1) ~ 0,  # Pro/Neutral
    Label_ID == 2 ~ 1           # Soft/Hard
  ))

write_csv(SecondRound_Stance_combined, "SecondRound_Stance_combined.csv")

### =============================================================================
### STEP 20. BALANCE DATA FOR BINARY STANCE CLASSIFICATION (Undersampling Neutral
### =============================================================================

# Undersample the majority class
set.seed(123)
majority_class <- SecondRound_Stance_combined %>%
  filter(Label_ID == 0) %>%
  sample_n(1525)

minority_class <- SecondRound_Stance_combined %>%
  filter(Label_ID == 1)

# Combine in a balance dataset of ~2000 speeches (1525 Neutral and 475 Opposing) 
SecondRound_Stance_balanced <- bind_rows(majority_class, minority_class)

write_csv(SecondRound_Stance_balanced, "SecondRound_Stance_balanced.csv")

## After mutltiples test, the Dataset best suited for training the model is
## a BINARY STANCE CLASSIFICATION Model, with undersampled Neutral stance.

# Train the model with SecondRound_Stance_balanced.csv and run on EUSpeeches_classified_V2.csv (Step 17)


### ===============================================================
### STEP 21. CREATE SAMPLE OF EU OPPOSITION SPEECHES (FOR ROUND 3)
### ===============================================================

# Load second-round stance data and classified EU stance results
SecondRound_Stance <- read_csv("SecondRound_Stance_combined.csv") %>%
  select(-1)  # Remove index column

EU_stance <- read_csv("EU_stance_classified.csv") %>%
  mutate(Stance = as.integer(Stance))  # Convert TRUE/FALSE to 1/0

# Sample 150 EU-opposing sentences per country, not already annotated
set.seed(123)
EU_Opposition_sample <- EU_stance %>%
  filter(Stance == 1) %>%
  anti_join(SecondRound_Stance, by = "speech_ID") %>%
  group_by(iso3country) %>%
  slice_sample(n = 150) %>%
  ungroup()

write_xlsx(EU_Opposition_sample, "EU_Opposition_sample.xlsx")

## Third round annotation with new sample of sentences classified as Opposition by the second Stance model

### ===============================================================
### STEP 22. PREPARE THIRD ROUND TRAINING DATASET
### ===============================================================

# Load new annotated sample and second-round data
EU_Opposition_sample <- read_excel("EU_Opposition_sample.xlsx")

SecondRound_Stance <- read_csv("Data for Classification/SecondRound_Stance.csv") %>%
  select(-1, -agenda)  # Remove index and agenda columns

# Clean and recode third-round sample
ThirdRound <- EU_Opposition_sample %>%
  select(-c(EU, EU_2, Stance)) %>%
  filter(Label != "No EU") %>%
  mutate(Label_ID = case_when(
    Label %in% c("Pro", "Neutral") ~ 0,
    Label %in% c("Soft", "soft", "Hard") ~ 1,
    TRUE ~ NA_integer_
  ))

# Harmonize Label_ID in second-round data
SecondRound_Stance <- SecondRound_Stance %>%
  mutate(Label_ID = case_when(
    Label %in% c("Pro", "Pro EU", "Neutral") ~ 0,
    Label %in% c("Soft", "soft", "Hard") ~ 1,
    TRUE ~ NA_integer_
  ))

# Combine second and third rounds
ThirdRound_Stance <- bind_rows(ThirdRound, SecondRound_Stance) %>%
  rename(Code = Label)

write_csv(ThirdRound_Stance, "ThirdRound_Stance.csv")

# Train the model with ThirdRound_Stance.csv and run on EUSpeeches_classified_V2.csv (Step 17)
## Get final results, run regressions

