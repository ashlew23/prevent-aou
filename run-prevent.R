# This snippet assumes that you run setup first

# This code copies a file from your Google Bucket into a dataframe

# replace 'test.csv' with the name of the file in your google bucket (don't delete the quotation marks)
name_of_file_in_bucket <- 'test.csv'

########################################################################
##
################# DON'T CHANGE FROM HERE ###############################
##
########################################################################

# Get the bucket name
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')

# Copy the file from current workspace to the bucket
system(paste0("gsutil cp ", my_bucket, "/data/", name_of_file_in_bucket, " ."), intern=T)

# Load the file into a dataframe
my_dataframe  <- read_csv(name_of_file_in_bucket)
head(my_dataframe)

## Step 0: Load in Relevant Packages for Cleaning

#Loading Packages
if (!require("pacman")) install.packages("pacman")


pacman::p_load("tidyverse",     # For all of the data cleaning
               "here",          # To establish working directories
               "haven",         # to import SAS files
               )      

here::here() #double check where our working directory is.


## Step 1: Ensure Proper Units, Center Relevant Variables

#Creating "nonhdlc"
prevent_data <- prevent_data %>% mutate(
  nonhdlc = totalchol-hdlc
) #Calculating Non-HDL-C in mg/dL from total cholesterol and HDL-C

#Conducting Conversion
prevent_data <- prevent_data %>% mutate(
  age55_cen = (age-55)/10,               #Age centered at 55, per 10 units
  nonhdlc_cen = (nonhdlc*0.02586) - 3.5, #Non-HDL-C centered at 3.5
  hdlc_cen = ((hdlc*0.02586) - 1.3)/0.3  #HDL-C centered at 1.3, per 0.3 units


## Step 2: Create Knots for SBP, BMI, and eGFR

#Creating Knots First
prevent_data <- prevent_data %>% mutate(
  sbp_less110 = ifelse(sbp >=110 & sbp < Inf, 0, sbp), #Knot for SBP< 110
  sbp_110plus = ifelse(sbp < 110, 0, sbp), #Knot for SBP 110+
  bmi_less30 = ifelse(bmi >=30 & bmi < Inf, 0, bmi), #Knot for BMI <30
  bmi_30plus = ifelse(bmi <30, 0, bmi), #Knot for BMI 30+
  egfr_less60 = ifelse(egfr >=60 & egfr < Inf, 0, egfr), #Knot for eGFR < 60
  egfr_60plus = ifelse (egfr <60, 0, egfr) #Knot for eGFR 60+
)

# Now Standardizing and Centering the Knots
prevent_data <- prevent_data %>% mutate(
  sbp_less110_cen = ifelse (sbp_less110 ==0, 0, (sbp_less110-110)/20), # SBP < 110 knot centered at 110 mmHg, per 20 units
  sbp_110plus_cen = ifelse(sbp_110plus ==0, 0, (sbp_110plus-130)/20), # SBP 110+ knot centered at 130 mmHg, per 20 units
  bmi_less30_cen = ifelse(bmi_less30 ==0, 0, (bmi_less30-25)/5), #BMI < 30 knot, centered at 25, per 5 units
  bmi_30plus_cen = ifelse(bmi_30plus ==0, 0, (bmi_30plus-30)/5), #BMI 30+ Knot, centered at 30, per 5 units
  egfr_less60_cen = ifelse(egfr_less60 ==0, 0, (egfr_less60 - 60)/-15), #eGFR < 60 knot, per -15 units
  egfr_60plus_cen = ifelse(egfr_60plus==0, 0, (egfr_60plus - 90)/-15) #eGFR 60+ knot, per -15 units
)

## Step 3: Creating Splines for Treated SBP and Treated Non-HDL-C

#First, ensure that "htnmed" and "statinmed" are binary 0=No, 1=yes

#Calculating spline terms
prevent_data <- prevent_data %>% mutate(
  sbp_110plus_centx = sbp_110plus_cen*htnmed, # Treated SBP 110+, Centered
  nonhdlc_centx = nonhdlc_cen*statinmed #Treated Non-HDL-C, Centered
)

## Step 4: Create Interaction Terms with age for non-HDL-C, HDL-C, SBP, diabetes, current smoking, BMI, and eGFR

prevent_data <- prevent_data %>% mutate(
  age_nonhdlc = age55_cen*nonhdlc_cen,
  age_hdlc = age55_cen*hdlc_cen,
  age_sbp110plus = age55_cen*sbp_110plus_cen,
  age_diabetes = age55_cen*diabetes,
  age_currsmoke = age55_cen*currsmoke,
  age_egfrless60 = age55_cen*egfr_less60_cen,
  age_bmi30plus = age55_cen*bmi_30plus_cen
)

# Create the 'female' column based on the 'sex_at_birth' column
prevent_data <- prevent_data %>%
  mutate(female = if_else(sex_at_birth == 'Female', 1, 0))

prevent_data$female <- ifelse(prevent_data$sex_at_birth == 'Female', 1, 0)

head(prevent_data)

# RISK EQUATIONS FROM KHAN ET AL SUPPLEMENT

# ASCVD Risk Calculation
prevent_data <- prevent_data %>% mutate(
  prevent_sum_ascvd = ifelse(female ==1, -3.819975 + 0.719883*age55_cen + 0.1176967*nonhdlc_cen + -0.151185*hdlc_cen + 
                                  -0.0835358*sbp_less110_cen + 0.3592852*sbp_110plus_cen + 
                                  0.8348585*diabetes + 0.4831078*currsmoke + 
                                  0.4864619*egfr_less60_cen + 0.0397779*egfr_60plus_cen + 
                                  0.2265309*htnmed + -0.0592374*statinmed + 
                                  -0.0395762*sbp_110plus_centx + 0.0844423*nonhdlc_centx + 
                                  -0.0567839*age_nonhdlc + 0.0325692*age_hdlc + -0.1035985*age_sbp110plus + -0.2417542*age_diabetes +
                                  -0.0791142*age_currsmoke + -0.1671492*age_egfrless60, # Female Equation
                                -3.500655 + 0.7099847*age55_cen + 0.1658663*nonhdlc_cen + -0.1144285*hdlc_cen + 
                                  -0.2837212*sbp_less110_cen + 0.3239977*sbp_110plus_cen + 
                                  0.7189597*diabetes + 0.3956973*currsmoke + 
                                  0.3690075*egfr_less60_cen + 0.0203619*egfr_60plus_cen + 
                                  0.2036522*htnmed + -0.0865581*statinmed + 
                                 -0.0322916*sbp_110plus_centx + 0.114563*nonhdlc_centx + 
                                  -0.0300005*age_nonhdlc + 0.0232747*age_hdlc + -0.0927024*age_sbp110plus + -0.2018525*age_diabetes +
                                  -0.0970527*age_currsmoke + -0.1217081*age_egfrless60) #Male Equation

# Total CVD Risk Calculation
prevent_data <- prevent_data %>% mutate(
  prevent_sum_totalcvd = ifelse(female ==1, -3.307728 + 0.7939329*age55_cen + 0.0305239*nonhdlc_cen + -0.1606857*hdlc_cen + 
                                  -0.2394003*sbp_less110_cen + 0.3600781*sbp_110plus_cen + 
                                  0.8667604*diabetes + 0.5360739*currsmoke + 
                                  0.6045917*egfr_less60_cen + 0.0433769*egfr_60plus_cen + 
                                  0.3151672*htnmed + -0.1477655*statinmed + 
                                  -0.0663612*sbp_110plus_centx + 0.1197879*nonhdlc_centx + 
                                  -0.0819715*age_nonhdlc + 0.0306769*age_hdlc + -0.0946348*age_sbp110plus + -0.27057*age_diabetes +
                                  -0.078715*age_currsmoke + -0.1637806*age_egfrless60, # Female Equation
                                -3.031168  + 0.7688528*age55_cen + 0.0736174*nonhdlc_cen + -0.0954431*hdlc_cen + 
                                  -0.4347345*sbp_less110_cen + 0.3362658*sbp_110plus_cen + 
                                  0.7692857*diabetes + 0.4386871*currsmoke + 
                                  0.5378979*egfr_less60_cen + 0.0164827*egfr_60plus_cen + 
                                  0.288879*htnmed + -0.1337349*statinmed + 
                                  -0.0475924*sbp_110plus_centx + 0.150273*nonhdlc_centx + 
                                  -0.0517874*age_nonhdlc + 0.0191169*age_hdlc + -0.1049477*age_sbp110plus + -0.2251948*age_diabetes +
                                  -0.0895067*age_currsmoke + -0.1543702*age_egfrless60) #Male Equation

# Coronary Heart Disease Risk Equation
prevent_data <- prevent_data %>% mutate(
  prevent_sum_chd = ifelse(female ==1, -4.608751 + 0.7587146*age55_cen + 0.1810949*nonhdlc_cen + -0.2014507*hdlc_cen + 
                                  -0.0881827*sbp_less110_cen + 0.3547731*sbp_110plus_cen + 
                                  0.9045358*diabetes + 0.5410917*currsmoke + 
                                  0.5198725*egfr_less60_cen + 0.0325935*egfr_60plus_cen + 
                                  0.2010642*htnmed + -0.036195*statinmed + 
                                  -0.0891238*sbp_110plus_centx + 0.0750716*nonhdlc_centx + 
                                  -0.0683256*age_nonhdlc + 0.0484755*age_hdlc + -0.0898086*age_sbp110plus + -0.2569041*age_diabetes +
                                  -0.0786607*age_currsmoke + -0.1597513*age_egfrless60, # Female Equation
                                -4.156753 + 0.7423283*age55_cen + 0.2572109*nonhdlc_cen + -0.1820374*hdlc_cen + 
                                  -0.3174515*sbp_less110_cen + 0.312778*sbp_110plus_cen + 
                                  0.7485249*diabetes + 0.3912047*currsmoke + 
                                  0.376487*egfr_less60_cen + 0.0193687*egfr_60plus_cen + 
                                  0.1588199*htnmed + -0.0494555*statinmed + 
                                 -0.0577851*sbp_110plus_centx + 0.0809765*nonhdlc_centx + 
                                  -0.0517872*age_nonhdlc + 0.0489033*age_hdlc + -0.0850404*age_sbp110plus + -0.2107552*age_diabetes +
                                  -0.1206397*age_currsmoke + -0.07795*age_egfrless60) #Male Equation

# Stroke Risk Calculation
prevent_data <- prevent_data %>% mutate(
  prevent_sum_stroke = ifelse(female ==1, -4.409199 + 0.6907849*age55_cen + 0.0534279*nonhdlc_cen + -0.1055109*hdlc_cen + 
                                  -0.113078*sbp_less110_cen + 0.3665217*sbp_110plus_cen + 
                                  0.8013721*diabetes + 0.4187039*currsmoke + 
                                  0.4539767*egfr_less60_cen + 0.0515087*egfr_60plus_cen + 
                                  0.2494624*htnmed + -0.0798829*statinmed + 
                                  -0.0079039*sbp_110plus_centx + 0.0833101*nonhdlc_centx + 
                                  -0.0409242*age_nonhdlc + 0.016994*age_hdlc + -0.1191213*age_sbp110plus + -0.2480549*age_diabetes +
                                  -0.0998063*age_currsmoke + -0.1759075*age_egfrless60, # Female Equation
                                -4.20881 + 0.722513*age55_cen + 0.0263348*nonhdlc_cen + -0.0248959*hdlc_cen + 
                                  -0.268104*sbp_less110_cen + 0.3474634*sbp_110plus_cen + 
                                  0.684699*diabetes + 0.3874844*currsmoke + 
                                  0.3877827*egfr_less60_cen + 0.0201965*egfr_60plus_cen + 
                                  0.232963*htnmed + -0.1178935*statinmed + 
                                 0.0120926*sbp_110plus_centx + 0.155739*nonhdlc_centx + 
                                  0.0141928*age_nonhdlc + -0.0111745*age_hdlc + -0.1155391*age_sbp110plus + -0.2123743*age_diabetes +
                                  -0.0824133*age_currsmoke + -0.180789*age_egfrless60) #Male Equation

# Heart Failure Risk Calculation
prevent_data <- prevent_data %>% mutate(
  prevent_sum_heartfailure = ifelse(female ==1, -4.310409 + 0.8998235*age55_cen + 
                                  -0.4559771*sbp_less110_cen + 0.3576505*sbp_110plus_cen + 
                                  1.038346*diabetes + 0.583916*currsmoke + 
                               -0.0072294*bmi_less30_cen + 0.2997706*bmi_30plus_cen +
                                  0.7451638*egfr_less60_cen + 0.0557087*egfr_60plus_cen + 
                                  0.3534442*htnmed +  
                                  -0.0981511*sbp_110plus_centx + 
                                   -0.0946663*age_sbp110plus + -0.3581041*age_diabetes +
                                  -0.1159453*age_currsmoke + -0.003878*age_bmi30plus + -0.1759075*age_egfrless60, # Female Equation
                               -3.946391 + 0.8972642*age55_cen + 
                                  -0.6811466*sbp_less110_cen + 0.3634461*sbp_110plus_cen + 
                                  0.923776*diabetes + 0.5023736*currsmoke + 
                               -0.0485841*bmi_less30_cen + 0.3726929*bmi_30plus_cen +
                                  0.6926917*egfr_less60_cen + 0.0251827*egfr_60plus_cen + 
                                  0.2980922*htnmed +  
                                  -0.0497731*sbp_110plus_centx + 
                                   -0.1289201*age_sbp110plus + -0.3040924*age_diabetes +
                                  -0.1401688*age_currsmoke + 0.0068126*age_bmi30plus + -0.1797778*age_egfrless60) #Male Equation

# Calculating PREVENT 10-year Risk Score
prevent_data <- prevent_data %>% mutate(
  prevent_10yrisk_totalcvd = exp(prevent_sum_totalcvd)/(1 +exp(prevent_sum_totalcvd)),
  prevent_10yrisk_ascvd = exp(prevent_sum_ascvd)/(1+exp(prevent_sum_ascvd)),
  prevent_10yrisk_chd = exp(prevent_sum_chd)/(1+exp(prevent_sum_chd)),
  prevent_10yrisk_stroke = exp(prevent_sum_stroke)/(1+exp(prevent_sum_stroke)) ,
  prevent_10yrisk_heartfailure = exp(prevent_sum_heartfailure)/(1+exp(prevent_sum_heartfailure)) 
)
