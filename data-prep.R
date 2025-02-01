#Load person dataframe
dataset_person_df

# Load necessary libraries
library(dplyr)
library(lubridate)

# Person Dataframe
# Convert date_of_birth to Date format
dataset_person_df$date_of_birth <- as.Date(dataset_person_df$date_of_birth)

# Select specific columns, calculate age, and drop date_of_birth
cleaned_person_df <- dataset_person_df %>%
  filter(sex_at_birth %in% c("Male", "Female")) %>%
  select(person_id, sex_at_birth, date_of_birth) %>%
  mutate(age = as.integer(floor(interval(date_of_birth, today()) / years(1)))) %>%
  select(-date_of_birth)

# Print the final DataFrame
head(cleaned_person_df,3)

#Check cohort size
num_unique_person_ids <- cleaned_person_df %>%
  summarise(unique_person_ids = n_distinct(person_id))

# Print the result
print(num_unique_person_ids)

#Load condition dataframe
dataset_condition_df

# Create a new column 'ascvd' based on 'source_concept_code'
dataset_condition_df <- dataset_condition_df %>%
  mutate(ascvd = ifelse(grepl("410|431|432|433|434|I21|I22|I61|I62|I63", source_concept_code), 1, 0))

# Create a new column 'HF' based on 'source_concept_code'
dataset_condition_df <- dataset_condition_df %>%
  mutate(HF = ifelse(grepl("428|I50", source_concept_code), 1, 0))

# Create a new column 'diabetes' based on 'source_concept_code'
dataset_condition_df <- dataset_condition_df %>%
  mutate(diabetes = ifelse(grepl("E11|250|E10|E13|E08|O24|648|249", source_concept_code), 1, 0))

# Create a new column 'ascvd' based on 'source_concept_code'
dataset_condition_df <- dataset_condition_df %>%
  mutate(mortality = ifelse(grepl("799|R99", source_concept_code), 1, 0))

head(dataset_condition_df,5)

library(lubridate)

# Create the condition time_ascvd column
added_time_condition_df <- dataset_condition_df %>%
  mutate(time_ascvd = ifelse(
    ascvd == 1,
    # Check if condition_end_time is NA or the difference is less than one day
    ifelse(
      is.na(condition_end_datetime) | as.numeric(difftime(condition_end_datetime, condition_start_datetime, units = "hours")) < 24,
      Inf,
      # Calculate the time difference in years
      as.numeric(difftime(condition_end_datetime, condition_start_datetime, units = "days")) / 365.25
    ),
    NA  # Set NA for rows where ascvd != 1
  ))

# Print the first few rows to check the new column
head(added_time_condition_df)


# Load the lubridate package
library(lubridate)

# Define the start and end dates of the study period according to the All of Us v7 CDR
start_date <- ymd("2018-05-06")
end_date <- ymd("2022-07-01")

# Calculate the time difference in years
max_time_years <- as.numeric(difftime(end_date, start_date, units = "days")) / 365.25

# Print the max_time_years to check the result
print(max_time_years)


# Replace NA and Inf values with the calculated maximum value
added_time_condition_df <- added_time_condition_df %>%
  mutate(time_ascvd = ifelse(is.infinite(time_ascvd) | is.na(time_ascvd), max_time_years, time_ascvd))

# Print the first few rows to check the updated column
head(added_time_condition_df)


#Add time_HF as well

# Create the time_ascvd column
added_time_condition_df <- added_time_condition_df %>%
  mutate(time_HF = ifelse(
    HF == 1,
    # Check if condition_end_time is NA or the difference is less than one day
    ifelse(
      is.na(condition_end_datetime) | as.numeric(difftime(condition_end_datetime, condition_start_datetime, units = "hours")) < 24,
      Inf,
      # Calculate the time difference in years
      as.numeric(difftime(condition_end_datetime, condition_start_datetime, units = "days")) / 365.25
    ),
    NA  # Set NA for rows where HF != 1
  ))

# Print the first few rows to check the new column
head(added_time_condition_df)


# Replace NA and Inf values with the calculated maximum value
added_time_condition_df <- added_time_condition_df %>%
  mutate(time_HF = ifelse(is.infinite(time_HF) | is.na(time_HF), max_time_years, time_HF))

# Print the first few rows to check the updated column
head(added_time_condition_df)


cleaned_condition_df <- added_time_condition_df %>%
  select(person_id, ascvd, HF, diabetes, mortality, time_ascvd, time_HF)

head(cleaned_condition_df,3)

# Drop rows with NA values in all columns except 'time_ascvd' and 'time_HF'
cleaned_condition_df <- cleaned_condition_df %>%
  drop_na(-time_ascvd, -time_HF)

# Print the first few rows to check the result
head(cleaned_condition_df,3)


#Load drug dataframe
dataset_drug_df

# Create a new column 'statinmed' based on
dataset_drug_df <- dataset_drug_df %>%
  mutate(statinmed = ifelse(grepl("simvastatin|lovastatin|pravastatin|fluvastatin|atorvastatin|cerivastatin|rosuvastatin|pitavastatin|lovastatin and nicotinic acid|simvastatin and ezetimibe|pravastatin and fenofibrate|simvastatin and fenofibrate|atorvastatin and ezetimibe|rosuvastatin and ezetimibe|rosuvastatin and omega-3 fatty acids|atorvastatin and omega-3 fatty acids|rosuvastatin and fenofibrate|bempedoic acid and ezetimibe|pravastatin and ezetimibe|pravastatin, ezetimibe and fenofibrate|simvastatin and acetylsalicylic acid|pravastatin and acetylsalicylic acid|atorvastatin and amlodipine|simvastatin, acetylsalicylic acid and ramipril|rosuvastatin and acetylsalicylic acid|atorvastatin, acetylsalicylic acid and ramiprilrosuvastatin, amlodipine and lisinopril|atorvastatin and acetylsalicylic acid|rosuvastatin and amlodipine|rosuvastatin and valsartan|atorvastatin, amlodipine and perindopril|atorvastatin, acetylsalicylic acid and perindopril|	rosuvastatin, perindopril and indapamide|rosuvastatin, amlodipine and perindopril|atorvastatin and perindopril|rosuvastatin and fimasartan|rosuvastatin and ramipril|atorvastatin, amlodipine and ramipril|atorvastatin, amlodipine and candesartan|rosuvastatin and telmisartan|rosuvastatin and perindopril", standard_concept_name), 1, 0))

# Create a new column 'htnmed' based on
dataset_drug_df <- dataset_drug_df %>%
  mutate(htnmed = ifelse(grepl("BendroflumethiazideHydroflumethiazide|Hydrochlorothiazide|Chlorothiazide|Polythiazide|Trichlormethiazide|Cyclopenthiazide|Methyclothiazide|Cyclothiazide|Mebutizide|Trichlormethiazide|thiazide and potassium|Quinethazone|Clopamide|Chlortalidone|Mefruside|Clofenamide|Metolazone|Meticrane|Xipamide|Indapamide|Clorexolone|Fenquizone|Clorexolone|Mersalyl|Theobromine|Cicletanine|Furosemide|Bumetanide|Piretanide|Torasemide|Etacrynic acid|Tienilic acid|Muzolimine|Etozolin|Spironolactone|Potassium canrenoate|Canrenone|Eplerenone|Finerenone|Amiloride|Triamterene|Tolvaptan|Conivaptan|
Rescinnamine|Reserpine|Rauwolfia alkaloids|Deserpidine|Methoserpidine|Bietaserpine|Methyldopa|Clonidine|Guanfacine|Tolonidine|Moxonidine|Rilmenidine|Trimetaphan|Mecamylamine|Prazosin|Indoramin|Trimazosin|Doxazosin|Urapidil|Betanidine|Guanethidine|Guanoxan|Debrisoquine|Guanoclor|Guanazodine|Guanoxabenz|Diazoxide|Dihydralazine|Hydralazine|Endralazine|Cadralazine|Minoxidil|Nitroprusside|Pinacidil|Veratrum|Metirosine|Pargyline|Ketanserin|Aprocitentan|Bosentan|Ambrisentan|Sitaxentan|Macitentan|Riociguat|Ambrisentan and tadalafil|Macitentan and tadalafil|
alprenolol|oxprenolol|pindolol|propranolol|timolol|sotalol|nadolol|mepindolol|carteolol|tertatolol|bopindolol|bupranolol|penbutolol|cloranolol|practolol|metoprolol|atenolol|acebutolol|betaxolol|bevantolol|bisoprolol|celiprolol|esmolol|epanolol|s-atenolol|nebivolol|talinolol|landiolol|labetalol|carvedilol|oxprenolol and thiazides|propranolol and thiazides|timolol and thiazides|sotalol and thiazides|nadolol and thiazides|metipranolol and thiazides|metoprolol and thiazides|atenolol and thiazides|acebutolol and thiazides|bevantolol and thiazides|bisoprolol and thiazides|nebivolol and thiazides|metoprolol and thiazides|labetalol and thiazides|oxprenolol and other diuretics|pindolol and other diuretics|bopindolol and other diuretics|penbutolol and other diuretics|metoprolol and other diuretics|atenolol and other diuretics|labetalol and other diuretics|timolol|thiazides|metoprolol and felodipine|atenolol and nifedipine|bisoprolol and amlodipine|nebivolol and amlodipine|metoprolol and amlodipine|
amlodipine|felodipine|isradipine|nicardipine|nifedipine|nimodipine|nisoldipine|nitrendipine|lacidipine|nilvadipine|manidipine|barnidipine|lercanidipine|cilnidipine|benidipine|clevidipine|levamlodipine|amlodipine and celecoxib|nifedipine|mibefradil|verapamil|gallopamil|verapamil|diltiazem|fendiline|bepridil|lidoflazine|perhexiline|nifedipine and diuretics|amlodipine and diuretics|
captopril|enalapril|lisinopril|perindopril|ramipril|quinapril|benazepril|cilazapril|fosinopril|trandolapril|spirapril|delapril|moexipril|temocapril|zofenopril|imidapril|enalapril and lercanidipine|lisinopril and amlodipine|perindopril and amlodipine|ramipril and felodipine|enalapril and nitrendipine|ramipril and amlodipine|trandolapril and verapamil|delapril and manidipine|benazepril and amlodipine|perindopril, amlodipine and indapamide|perindopril and bisoprolol|ramipril, amlodipine and hydrochlorothiazide|perindopril, bisoprolol and amlodipine|ramipril and bisoprolol|perindopril, bisoprolol, amlodipine and indapamide|zofenopril and nebivolol|losartan|eprosartan|valsartan|irbesartan|tasosartan|candesartan|telmisartan|olmesartan medoxomil|azilsartan medoxomil|fimasartan|losartan and diuretics|eprosartan and diuretics|valsartan and diuretics|irbesartan and diuretics|candesartan and diuretics|telmisartan and diuretics|olmesartan medoxomil and diuretics|azilsartan medoxomil and diuretics|fimasartan and diuretics|valsartan and amlodipine|olmesartan medoxomil and amlodipine|telmisartan and amlodipine|irbesartan and amlodipine|losartan and amlodipine|candesartan and amlodipine|valsartan and lercanidipine|fimasartan and amlodipine|valsartan, amlodipine and hydrochlorothiazide|valsartan and aliskiren|olmesartan medoxomil, amlodipine and hydrochlorothiazide|valsartan and sacubitril|valsartan and nebivolol|candesartan, amlodipine and hydrochlorothiazide|irbesartan, amlodipine and hydrochlorothiazide|telmisartan, amlodipine and hydrochlorothiazide|remikiren|aliskiren|aliskiren and hydrochlorothiazide|aliskiren and amlodipine|aliskiren, amlodipine and hydrochlorothiazide",standard_concept_name), 1, 0))


cleaned_drug_df <- dataset_drug_df %>%
  select(person_id, statinmed, htnmed)

head(cleaned_drug_df,3) #keep only person_id, statinmed, htnmed

#Load measurement dataframe
dataset_measurement

# Create a new column 'creatinine' based on the conditions specified
dataset_measurement_df <- dataset_measurement_df %>%
  mutate(creatinine = ifelse(
    standard_concept_name %in% c(
      "Creatinine [Mass/volume] in Blood",
      "Creatinine [Mass/volume] in Arterial blood",
      "Creatinine and Glomerular filtration rate.predicted panel - Serum, Plasma or Blood",
      "Creatinine [Mass/volume] in Arterial blood", "Creatinine [Mass/volume] in Blood"),
    value_as_number,
    NA
  ))

dataset_measurement_df <- dataset_measurement_df %>%
  mutate(totalchol = ifelse(
    measurement_concept_id == 3027114 &
    unit_source_value == "mg/dL" &
    value_as_number >= 130 & value_as_number <= 320,
    value_as_number,
    NA
  ))


dataset_measurement_df <- dataset_measurement_df %>%
  mutate(hdlc = ifelse(
    measurement_concept_id == 3007070 &
    unit_source_value == "mg/dL" &
    value_as_number >= 20 & value_as_number <= 100,
    value_as_number,
    NA
  ))

dataset_measurement_df <- dataset_measurement_df %>%
  mutate(sbp = ifelse(
    measurement_concept_id == 3004249 &
    unit_source_value == "mm[Hg]" &
    value_as_number >= 90 & value_as_number <= 200,
    value_as_number,
    NA
  ))
head(dataset_measurement_df,3)

cleaned_measurement_df <- dataset_measurement_df %>%
  select(person_id, bmi, totalchol, hdlc, sbp, creatinine, enrollment_start_datetime)

head(cleaned_measurement_df,10)


# Merge rows of the same 'person_id', keeping the non-NA values (but all creatinine)
final_cleaned_measurement_df <- cleaned_measurement_df %>%
  group_by(person_id) %>%
  summarise(
    bmi = max(bmi, na.rm = TRUE),          
    creatinine = if_else(all(is.na(creatinine)), NA_real_, max(creatinine, na.rm = TRUE)), 
    totalchol = max(totalchol, na.rm = TRUE),   
    hdlc = max(hdlc, na.rm = TRUE),            
    sbp = max(sbp, na.rm = TRUE),
    enrollment_start_datetime = min(enrollment_start_datetime, na.rm = TRUE),  # Take min non-NA value
    .groups = 'drop'                          
  )

# Print the first few rows of the merged DataFrame
head(final_cleaned_measurement_df)


#Load survey dataframe
dataset_survey_df

dataset_survey_df <- dataset_survey_df %>%
  mutate(currsmoke = case_when(
    answer %in% c('Smoke Frequency: Every Day', 'Smoke Frequency: Some Days') ~ 1,
    TRUE ~ 0  # Assign 0 for unexpected values instead of NA
  ))

# Filter rows where 'smoking' is not NA
cleaned_survey_df <- dataset_survey_df %>%
  filter(!is.na(currsmoke))

# Print the first few rows of the filtered DataFrame
head(cleaned_survey_df,5)


#Join cleaned datatables together
combined_df <- cleaned_person_df %>%
  full_join(cleaned_condition_df, by = "person_id") %>%
  full_join(cleaned_drug_df, by = "person_id") %>%
  full_join(final_cleaned_measurement_df, by = "person_id") %>%
  full_join(cleaned_survey_df, by = "person_id")

# Print the first few rows of the combined DataFrame
head(combined_df)

final_combined_df <- combined_df %>%
  group_by(person_id) %>%
  summarize(
    creatinine = if (all(is.na(creatinine))) NA else max(creatinine, na.rm = TRUE),
    across(-creatinine, ~ max(., na.rm = TRUE)),
    .groups = 'drop'
  )

head(final_combined_df)


# Set NA and Inf values to 0 for specific columns
final_combined_df <- final_combined_df %>%
  mutate(across(c(ascvd, HF, diabetes, mortality, statinmed, htnmed, currsmoke), 
                ~ replace(replace_na(., 0), is.infinite(.), 0)))


# Drop rows with NA values in all columns except 'time_ascvd', 'time_HF', and 'creatinine'
final_combined_df_cleaned <- final_combined_df %>%
  filter(across(everything(), ~ !(is.na(.) & !cur_column() %in% c("time_ascvd", "time_HF", "creatinine"))))

head(final_combined_df_cleaned)


final_combined_df_cleaned <- final_combined_df_cleaned %>%
  mutate(time_ascvd = ifelse(is.infinite(time_ascvd) | is.na(time_ascvd), max_time_years, time_ascvd))

final_combined_df_cleaned <- final_combined_df_cleaned %>%
  mutate(time_HF = ifelse(is.infinite(time_HF) | is.na(time_HF), max_time_years, time_HF))

head(final_combined_df_cleaned)

#Next we will impute the eGFR values using a two-step approach that considers the covariates: age, chronic kidney disease stage, diabetes status, creatinine

# We need to pull-in data on CKD stage status for this cohort
ckd_condition_df

# Create a subset for individuals with the general "Chronic kidney disease" condition
ckd_general <- subset(ckd_condition_df, standard_concept_name == "Chronic kidney disease")

# Check if these individuals have any other specific CKD-related conditions
ckd_with_other_conditions <- subset(ckd_condition_df, 
    standard_concept_name != "Chronic kidney disease" & 
    standard_concept_name %in% c(
        "Chronic kidney disease stage 1",
        "Chronic kidney disease stage 2",
        "Chronic kidney disease stage 3",
        "Chronic kidney disease stage 3A",
        "Chronic kidney disease stage 3B",
        "Chronic kidney disease stage 4",
        "Chronic kidney disease stage 5",
        "End-stage renal disease",
        "End stage renal failure",
        "End stage renal failure on dialysis"
    ))

# Find common individuals with both the general and specific CKD conditions
common_individuals <- intersect(ckd_general$person_id, ckd_with_other_conditions$person_id)

# Count the number of such individuals
length(common_individuals)

# Initialize ckd_stage with NA
ckd_condition_df$ckd_stage <- NA

# Assign stage numbers based on the condition names, starting with the most severe stages
cdk_condition_df$ckd_stage[grepl("Chronic kidney disease stage 5|End-stage renal disease|End stage renal failure", cdk_condition_df$standard_concept_name)] <- 5
cdk_condition_df$ckd_stage[grepl("Chronic kidney disease stage 4", cdk_condition_df$standard_concept_name)] <- 4
cdk_condition_df$ckd_stage[grepl("Chronic kidney disease stage 3|Chronic kidney disease stage 3A|Chronic kidney disease stage 3B|Chronic kidney disease stage 3 due to type 2 diabetes mellitus", cdk_condition_df$standard_concept_name)] <- 3
cdk_condition_df$ckd_stage[grepl("Chronic kidney disease stage 2", cdk_condition_df$standard_concept_name)] <- 2
cdk_condition_df$ckd_stage[grepl("Chronic kidney disease stage 1", cdk_condition_df$standard_concept_name)] <- 1

# Check the updated dataframe
head(ckd_condition_df)

# Perform a left join and set NA values in ckd_stage to 0
final_combined_df_cleaned <- final_combined_df_cleaned %>%
  left_join(ckd_condition_df %>% select(person_id, ckd_stage), by = "person_id") %>%
  mutate(ckd_stage = ifelse(is.na(ckd_stage), 0, ckd_stage))

# Check the updated dataframe
head(ckd_condition_df)

#Create the egfr column based on the given logic
final_combined_df_cleaned <- final_combined_df_cleaned %>%
  mutate(egfr = case_when(
    sex_at_birth == "Female" & creatinine <= 0.7 ~ 142 * (creatinine / 0.7)^(-0.241) * 0.9938^age,
    sex_at_birth == "Female" & creatinine > 0.7 ~ 142 * (creatinine / 0.7)^(-1.200) * 0.9938^age,
    sex_at_birth == "Male" & creatinine <= 0.9 ~ 142 * (creatinine / 0.9)^(-0.302) * 0.9938^age,
    sex_at_birth == "Male" & creatinine > 0.9 ~ 142 * (creatinine / 0.9)^(-1.200) * 0.9938^age,
    TRUE ~ NA_real_ # Default case if none of the conditions are met
  ))

# Print the first few rows to check the result
head(final_combined_df_cleaned)

# Update final_combined_df_cleaned to keep only the maximum ckd_stage per individual
final_combined_df_cleaned <- final_combined_df_cleaned %>%
  group_by(person_id) %>%
  mutate(ckd_stage = max(ckd_stage, na.rm = TRUE)) %>%
  ungroup()

# Count the number of unique person_id with non-missing values for both ckd_stage and creatinine
num_with_both <- final_combined_df_cleaned %>%
  filter(!is.na(ckd_stage) & !is.na(creatinine)) %>%
  summarise(count = n_distinct(person_id))

num_with_both

library(tidyr)

# Replace Inf or NaN values in 'egfr' with NA
final_combined_df_cleaned <- final_combined_df_cleaned %>%
  mutate(egfr = ifelse(is.infinite(egfr) | is.nan(egfr), NA, egfr))

# Remove rows with NA, NaN, or Inf in any of the relevant columns
final_combined_df_cleaned_no_na <- final_combined_df_cleaned %>%
  filter(!is.na(egfr) & !is.infinite(egfr) &
         !is.na(sex_at_birth) & !is.na(age) & !is.na(totalchol) & !is.na(hdlc) &
         !is.na(sbp) & !is.na(bmi) & !is.na(diabetes) & !is.na(currsmoke) &
         !is.na(htnmed) & !is.na(statinmed) &
         !is.infinite(sex_at_birth) & !is.infinite(age) & !is.infinite(totalchol) &
         !is.infinite(hdlc) & !is.infinite(sbp) & !is.infinite(bmi) &
         !is.infinite(diabetes) & !is.infinite(currsmoke) & !is.infinite(htnmed) &
         !is.infinite(statinmed))

# Print the first few rows to check the result
head(final_combined_df_cleaned)


# Two-Step Imputation Approach

# Step 1: Impute for individuals with all predictors
missing_egfr_df <- final_combined_df_cleaned[is.na(final_combined_df_cleaned$egfr) & 
                                              !is.na(final_combined_df_cleaned$age) & 
                                              !is.na(final_combined_df_cleaned$creatinine) & 
                                              !is.na(final_combined_df_cleaned$ckd_stage) & 
                                              !is.na(final_combined_df_cleaned$diabetes), ]

# Predict missing eGFR values for these individuals
predicted_egfr <- predict(egfr_model, newdata = missing_egfr_df)

# Fill in the predicted eGFR values in the original data
final_combined_df_cleaned$egfr[is.na(final_combined_df_cleaned$egfr) & 
                                !is.na(final_combined_df_cleaned$age) & 
                                !is.na(final_combined_df_cleaned$creatinine) & 
                                !is.na(final_combined_df_cleaned$ckd_stage) & 
                                !is.na(final_combined_df_cleaned$diabetes)] <- predicted_egfr


# Step 2: Impute based on age and diabetes status
# Group by age and diabetes status and calculate the mean eGFR
mean_egfr_by_group <- final_combined_df_cleaned %>%
  group_by(age, diabetes) %>%
  summarise(mean_egfr = mean(egfr, na.rm = TRUE))

# Join the calculated means back to the original data based on age and diabetes
final_combined_df_cleaned <- left_join(final_combined_df_cleaned, mean_egfr_by_group, by = c("age", "diabetes"))

# Fill in missing eGFR values with the mean eGFR for each age and diabetes group
final_combined_df_cleaned$egfr[is.na(final_combined_df_cleaned$egfr)] <- final_combined_df_cleaned$mean_egfr[is.na(final_combined_df_cleaned$egfr)]

# Remove the temporary mean_egfr column
final_combined_df_cleaned$mean_egfr <- NULL

# Ensure acceptable range for PREVENT calculator
final_combined_df_cleaned <- final_combined_df_cleaned %>%
    filter(egfr >= 15 & egfr <= 140)
head(final_combined_df_cleaned)

#Drop creatinine
final_combined_df_cleaned <- final_combined_df_cleaned %>%
  select(-creatinine)

# Print the first few rows to check the result
head(final_combined_df_cleaned)

# Remove rows with NA, NaN, or Inf in any of the relevant columns
final_combined_df_cleaned <- final_combined_df_cleaned %>%
  filter(!is.na(egfr) & !is.infinite(egfr) &
         !is.na(sex_at_birth) & !is.na(age) & !is.na(totalchol) & !is.na(hdlc) &
         !is.na(sbp) & !is.na(bmi) & !is.na(diabetes) & !is.na(currsmoke) &
         !is.na(htnmed) & !is.na(statinmed) &
         !is.infinite(sex_at_birth) & !is.infinite(age) & !is.infinite(totalchol) &
         !is.infinite(hdlc) & !is.infinite(sbp) & !is.infinite(bmi) &
         !is.infinite(diabetes) & !is.infinite(currsmoke) & !is.infinite(htnmed) &
         !is.infinite(statinmed) & !is.infinite(egfr))

# Print the first few rows to check the result
head(final_combined_df_cleaned)

#Save dataframe to workspace bucket
library(tidyverse)
my_dataframe <- final_combined_df_cleaned

# Replace 'test.csv' with THE NAME of the file you're going to store in the bucket (don't delete the quotation marks)
destination_filename <- 'test.csv'

########################################################################
##
################# DON'T CHANGE FROM HERE ###############################
##
########################################################################

# store the dataframe in current workspace
write_excel_csv(my_dataframe, destination_filename)

# Get the bucket name
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')

# Copy the file from current workspace to the bucket
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/data/"), intern=T)

# Check if file is in the bucket
system(paste0("gsutil ls ", my_bucket, "/data/*.csv"), intern=T)


