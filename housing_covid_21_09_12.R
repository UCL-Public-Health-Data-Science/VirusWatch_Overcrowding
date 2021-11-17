#  =============
#  Purpose: This script produces a cohort of Virus Watch participants with linked
#  housing, occupation, antibody and antigen results.
#  
#  Contributors: Rob Aldridge and Max Eyre
#  
#  Related manuscript: This analysis file relates to the first version of the paper
#  published on Wellcome Open. 
#  
#  An earlier version of the paper was published as a pre-print at https://doi.org/10.1101/2021.05.10.21256912
#  =============

#  =============
#  Libraries
#  =============

library(stringi)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(gtsummary)
library(lubridate)
library(lme4)
library(skimr)


#  ==========================================
#  set working directory and read data
#  ==========================================

# Read in source data sets from Virus Watch data pipelines


#  ------------------------------------------
#  Link demographics to household and contact data 
#  ==========================================

# add household and contact data to the cohort demographics file which is at individual level
demographics <- demographics %>%
  left_join(house_charac_hh, by = c("household_id"))  %>%
  left_join(households_with_children, by = c("household_id"))  %>%
  left_join(household_registration, by = c("household_id"))  %>%
  left_join(monthly_soc_contacts, by = c("individual_id")) %>% 
  left_join(occupation, by = c("individual_id")) 

## data checks ## 
# glimpse(demographics)
# uniqueN(demographics$individual_id)
# uniqueN(demographics$household_id)
# table(demographics$UK_SOC_2020_title, useNA = 'always')

#  ------------------------------------------
#  Clean and format variables in baseline demographics 
#  ==========================================


demographics <- demographics %>%
  mutate (hh_combined_total_income_fct = as.factor(hh_combined_total_income),
          hh_combined_total_income_fct = fct_explicit_na (hh_combined_total_income_fct, na_level = "Missing"),
          hh_combined_total_income_fct_qnt = fct_recode(hh_combined_total_income_fct, 
                                  "?0-9,999"          = "1",
                                  "?10,000-24,999"    = "2",
                                  "?25,000-49,999"    = "3",
                                  "?50,000-?74,999"   = "4",
                                  "?75,000-?99,999"   = "5", 
                                  "?100,000+" = "6",
                                  "?100,000+"         = "7",
                                  "?100,000+"         = "8",
                                  "?100,000+"         = "9",
                                  "?100,000+"         = "10",
                                  "Prefer not to say" = "99"),
          hh_combined_total_income_fct_qnt = fct_explicit_na (hh_combined_total_income_fct_qnt, na_level = "Missing"),
          hh_with_child = as.factor(ifelse(is.na(hh_with_child), 0, 1)),
          include = as.integer(1))%>%
  filter(household_region != "Northern Ireland", household_region != "Scotland", (hh_age_on_entry < 100 | is.na(hh_age_on_entry)))


## data and variable checking## 
# table(demographics$no_of_householders, useNA = 'always')
# table(demographics$hh_combined_total_income_fct_qnt, demographics$hh_combined_total_income, useNA = 'always')
# table(demographics$hh_combined_total_income_fct, useNA = 'always')
# glimpse (demographics)


# -----------------------
# create housing dataset
# -----------------------

housing_covid <- demographics %>%
  mutate (num_rooms_minus1 = num_rooms - 1,
          num_rooms_minus1 = ifelse(num_rooms_minus1 <0,0.01,num_rooms_minus1),
          hh_ppr = (no_of_householders/(num_rooms_minus1)),
          hh_overcrowding = as.factor(case_when(
            is.na(hh_ppr) ~ NA_character_,
            hh_ppr < 1 ~ "under occupied",
            hh_ppr == 1 ~ "balanced",
            hh_ppr > 1 ~ "overcrowded")),
            hh_ppr_plus1 = (no_of_householders/(num_rooms)),
            hh_ppr_plus1 = ifelse(num_rooms > 1,hh_ppr_plus1, hh_ppr),
            hh_overcrowding_plus1 = as.factor(case_when(
            is.na(hh_ppr_plus1) ~ NA_character_,
            hh_ppr_plus1 < 1 ~ "under occupied",
            hh_ppr_plus1 == 1 ~ "balanced",
            hh_ppr_plus1 > 1 ~ "overcrowded")),
            hh_overcrowding_binary = as.factor(case_when(
            is.na(hh_ppr) ~ NA_character_,
            hh_ppr < 1 ~ "under occupied",
            hh_ppr >= 1 ~ "overcrowded")),
            num_rooms_fct = num_rooms,
            num_rooms_fct = ifelse(num_rooms_fct > 7, 8, num_rooms_fct),
            num_rooms_fct = as.factor(num_rooms_fct),
            num_rooms_fct = fct_recode(num_rooms_fct, 
                                  "0-2" = "0",
                                  "0-2" = "1",
                                  "0-2" = "2",
                                  "3-5" = "3",
                                  "3-5" = "4",
                                  "3-5" = "5",
                                  "6-7" = "6",
                                  "6-7" = "7",
                                  "8+" = "8"),
            num_rooms_fct = fct_explicit_na (num_rooms_fct, na_level = "Missing"),
            accom_damp_mould = house_mould,
            accom_damp_mould = ifelse(house_damp==1, 1, accom_damp_mould),
            accom_damp_mould = as.factor(accom_damp_mould),
            accom_damp_mould = fct_recode(accom_damp_mould, 
                                       "Yes" = "1",
                                       "No" = "0"),
          accom_damp_mould = fct_explicit_na (accom_damp_mould, na_level = "Missing"),
            accom_type = as.factor(accom_type),
            accom_type = fct_recode(accom_type, 
              "Detached whole house or bungalow" = "1",
              "Semi-detached whole house or bungalow" = "2",
              "Terraced (including end-terrace) wholehouse or bungalow" = "3",
              "Flat, maisonette, or apartment in apurpose-built block" = "4",
              "Flat, maisonette, or apartment in acommercial building" = "5",
              "A caravan or other mobile or temporarystructure" = "6"),
            accom_type = fct_explicit_na (accom_type, na_level = "Missing"),
            accom_contained = as.factor(accom_contained),
            accom_contained = fct_recode(accom_contained, 
                                  "Yes" = "1",
                                  "No" = "0"),
            accom_contained = fct_explicit_na (accom_contained, na_level = "Missing"),
            accom_centralh = as.factor(accom_centralh),
            accom_centralh = fct_recode(accom_centralh, 
                                       "Yes" = "1",
                                       "No" = "0"),
            accom_centralh = fct_explicit_na (accom_centralh, na_level = "Missing"),
            accom_own_rent = as.factor(accom_own_rent),
            accom_own_rent = fct_recode(accom_own_rent, 
                                  "own outright" = "1",
                                  "own with mortgage/finance or loan" = "2",
                                  "part own and part rent (shared ownership)" = "3",
                                  "rent (with or without housing benefit)" = "4",
                                  "live here rent free" = "5",
                                  "other" = "6"),
            accom_own_rent = fct_explicit_na (accom_own_rent, na_level = "Missing")) 


housing_covid$hh_overcrowding <- factor(housing_covid$hh_overcrowding, levels = c("under occupied", "balanced", "overcrowded"))
housing_covid$hh_overcrowding_plus1 <- factor(housing_covid$hh_overcrowding_plus1, levels = c("under occupied", "balanced", "overcrowded"))
housing_covid$hh_overcrowding_binary <- factor(housing_covid$hh_overcrowding_binary, levels = c("under occupied", "overcrowded"))

# check the overcrowding variable
table(housing_covid$monthly3_complete, useNA = 'always')
housing_covid %>%
  select(hh_overcrowding, hh_overcrowding_binary) %>%
  tbl_cross (row =hh_overcrowding, col = hh_overcrowding_binary)



#set variable names
housing_covid <- housing_covid %>%
  set_variable_labels(num_rooms_fct = "Number of rooms",
                      no_of_householders = "Number of householders",
                      hh_overcrowding = "Household overcrowding category",
                      accom_own_rent= "Does the household rent or own this accommodation",
                      accom_type = "Type of accommodation",
                      accom_contained = "Is the accommodation self-contained",
                      accom_centralh = "Does the accommodation have central heating",
                      accom_damp_mould= "Is there damp or mould")

# -----------------------
# clean and link covid PCR antigen results data to housing
# -----------------------


# Read in source datasets from Virus Watch data pipelines


# -----------------------
# clean and link antibody results data to housing
# -----------------------

# Read in source datasets from Virus Watch data pipelines


# -----------------------
# Table 1 -comparison of people in all VW with those who completed the monthly survey
# -----------------------


t1a <- housing_pcr_antibody %>%
  filter(surveydate <=  dmy("01-03-2021")) %>%
  select(age_group, ethnicity, sex, no_of_householders, household_region, major_group_text, hh_overcrowding) %>%
  tbl_summary(percent = "col")  %>%
  add_n () %>%
  bold_labels()


t1b <- housing_pcr_antibody %>%
  filter(monthly3_complete==1, !is.na(hh_overcrowding), !is.na(num_close_contacts_cat))  %>%
  select(age_group, ethnicity, sex, no_of_householders, household_region, major_group_text, hh_overcrowding) %>%
  tbl_summary(percent = "col") %>%
  add_n () %>%
  bold_labels()


tbl_merge (
  tbls = list(t1a, t1b),
  tab_spanner = c("**All Virus Watch participants on 28th Feb 2021**", "**Virus Watch Participants completing monthly survey in Feb 2021**")
)


# -----------------------
# Table 2 - PCR and antibody results
# -----------------------

table(housing_pcr_antibody$covid_pcr, useNA = 'always')

tb2a <- housing_pcr_antibody %>%
  filter(monthly3_complete ==1, !is.na(hh_overcrowding))  %>%
  select(covid_pcr, hh_overcrowding, age_group, sex, ethnicity, hh_combined_total_income_fct_qnt, household_region) %>%
  tbl_summary(by = covid_pcr, percent = "row")  %>%
  add_n () %>%
  bold_labels()


tb2b <- housing_pcr_antibody %>%
  filter(monthly3_complete ==1, !is.na(hh_overcrowding),  thriva_cohort ==1, accessionDate <=  dmy("10-08-2021"))  %>%
  select(covid_ab, hh_overcrowding, age_group, sex, ethnicity, hh_combined_total_income_fct_qnt, household_region) %>%
  tbl_summary(by = covid_ab,  percent = "row")  %>%
  add_n () %>%
  bold_labels()  


tbl_merge (
  tbls = list(tb2a, tb2b),
  tab_spanner = c("**COVID PCR**", "**SARS-CoV2 antibody**")
)


# -----------------------
# Table 3 - antigen results
# -----------------------


housing_pcr_m3 <- housing_pcr_antibody %>%
  filter(monthly3_complete ==1, !is.na(hh_overcrowding), 
          sex != "Intersex", sex != "Prefer not to say", ethnicity != "Prefer not to say", household_region !="")

housing_pcr_m3$age_group <- relevel(housing_pcr_m3$age_group, ref = "65+")
housing_pcr_m3$household_region <- relevel(housing_pcr_m3$household_region, ref = "East of England")
housing_pcr_m3$hh_combined_total_income_fct_qnt <- relevel(housing_pcr_m3$hh_combined_total_income_fct_qnt, ref = "?10,000-24,999")



#univariate
t3a_glmer1 <- glmer(covid_pcr ~ hh_overcrowding + (1|household_id), data = housing_pcr_m3, family = binomial,
                   nAGQ = 8, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000)))

summary(t3a_glmer1)
confint(t3a_glmer1, method = "Wald", level = 0.95)


t3a_glmer2 <- glmer(covid_pcr ~ age_group + (1|household_id), data = housing_pcr_m3, family = binomial,
                    nAGQ = 8, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000)))

summary(t3a_glmer2)
confint(t3a_glmer2, method = "Wald", level = 0.95)


t3a_glmer3 <- glmer(covid_pcr ~ sex + (1|household_id), data = housing_pcr_m3, family = binomial,
                    nAGQ = 8, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000)))

summary(t3a_glmer3)
confint(t3a_glmer3, method = "Wald", level = 0.95)



t3a_glmer4 <- glmer(covid_pcr ~ ethnicity + (1|household_id), data = housing_pcr_m3, family = binomial,
                    nAGQ = 8, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000)))

summary(t3a_glmer4)
confint(t3a_glmer4, method = "Wald", level = 0.95)



t3a_glmer5 <- glmer(covid_pcr ~ hh_combined_total_income_fct_qnt + (1|household_id), data = housing_pcr_m3, family = binomial,
                    nAGQ = 8, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000)))

summary(t3a_glmer5)
confint(t3a_glmer5, method = "Wald", level = 0.95)


t3a_glmer6 <- glmer(covid_pcr ~ household_region + (1|household_id), data = housing_pcr_m3, family = binomial,
                    nAGQ = 8, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000)))

summary(t3a_glmer6)
confint(t3a_glmer6, method = "Wald", level = 0.95)


#multivariate
t3a_glmer <- glmer(covid_pcr ~ hh_overcrowding + age_group + sex + ethnicity + hh_combined_total_income_fct_qnt + household_region + (1|household_id), data = housing_pcr_m3, family = binomial,
                  nAGQ = 8, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000)))

summary(t3a_glmer)
confint(t3a_glmer, method = "Wald", level = 0.95)
performance::icc(t3a_glmer)


#DAG informed model
t3b_cglmer <- glmer(covid_pcr ~ hh_overcrowding + hh_combined_total_income_fct_qnt + household_region + (1|household_id), data = housing_pcr_m3, family = binomial,
        nAGQ = 8, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000))) 

summary(t3b_cglmer)
confint(t3b_cglmer, method = "Wald", level = 0.95)


  
#VanderWeele informed model 2
t3d_vw_glmer <- glmer(covid_pcr ~ hh_overcrowding + age_group + sex + ethnicity + hh_combined_total_income_fct_qnt + as.factor(major_group_text) + (1|household_id), data = housing_pcr_m3, family = binomial,
                  nAGQ = 8, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000))) 

summary(t3d_vw_glmer)
confint(t3d_vw_glmer, method = "Wald", level = 0.95)


# -----------------------
# Table 4 - antibody results
# -----------------------


housing_antibody_m3 <- housing_pcr_antibody %>%
  filter(monthly3_complete ==1, !is.na(hh_overcrowding), thriva_cohort ==1,
         sex != "Intersex", sex != "Prefer not to say", ethnicity != "Prefer not to say", household_region !="")


housing_antibody_m3$age_group <- relevel(housing_antibody_m3$age_group, ref = "65+")
housing_antibody_m3$household_region <- relevel(housing_antibody_m3$household_region, ref = "East of England")
housing_antibody_m3$hh_combined_total_income_fct_qnt <- relevel(housing_antibody_m3$hh_combined_total_income_fct_qnt, ref = "?10,000-24,999")


#univariate
t4_glmer1 <- glmer(covid_ab ~ hh_overcrowding  + (1|household_id), data = housing_antibody_m3, family = binomial,
                  nAGQ = 8, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000)))

summary(t4_glmer1)
confint(t4_glmer1, method = "Wald", level = 0.95)

t4_glmer2 <- glmer(covid_ab ~ age_group + (1|household_id), data = housing_antibody_m3, family = binomial,
                   nAGQ = 8, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000)))

summary(t4_glmer2)
confint(t4_glmer2, method = "Wald", level = 0.95)

t4_glmer3 <- glmer(covid_ab ~ sex + (1|household_id), data = housing_antibody_m3, family = binomial,
                   nAGQ = 8, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000)))

summary(t4_glmer3)
confint(t4_glmer3, method = "Wald", level = 0.95)

t4_glmer4 <- glmer(covid_ab ~ ethnicity + (1|household_id), data = housing_antibody_m3, family = binomial,
                   nAGQ = 8, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000)))

summary(t4_glmer4)
confint(t4_glmer4, method = "Wald", level = 0.95)

t4_glmer5 <- glmer(covid_ab ~ hh_combined_total_income_fct_qnt + (1|household_id), data = housing_antibody_m3, family = binomial,
                   nAGQ = 8, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000)))

summary(t4_glmer5)
confint(t4_glmer5, method = "Wald", level = 0.95)


t4_glmer6 <- glmer(covid_ab ~ household_region + (1|household_id), data = housing_antibody_m3, family = binomial,
                   nAGQ = 8, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000)))

summary(t4_glmer6)
confint(t4_glmer6, method = "Wald", level = 0.95)





#multivariate
t4_glmer <- glmer(covid_ab ~ hh_overcrowding + age_group + sex + ethnicity + hh_combined_total_income_fct_qnt + household_region + (1|household_id), data = housing_antibody_m3, family = binomial,
                  nAGQ = 8, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000)))

summary(t4_glmer)
confint(t4_glmer, method = "Wald", level = 0.95)
performance::icc(t4_glmer)



#DAG informed model

t4b_glmer <- glmer(covid_ab ~ hh_overcrowding + hh_combined_total_income_fct_qnt + household_region + (1|household_id), data = housing_antibody_m3, family = binomial,
                  nAGQ = 8, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000))) 

summary(t4b_glmer)
confint(t4b_glmer, method = "Wald", level = 0.95)


#VanderWeele informed model


t4d_vw_glmer <- glmer(covid_ab ~ hh_overcrowding + age_group + sex + ethnicity + hh_combined_total_income_fct_qnt + as.factor(major_group_text) + (1|household_id), data = housing_antibody_m3, family = binomial,
                      nAGQ = 8, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=100000))) 

summary(t4d_vw_glmer)
confint(t4d_vw_glmer, method = "Wald", level = 0.95)


