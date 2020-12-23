#### Install Packages ####
install.packages("tidyverse")
install.packages("devtools")
devtools::install_github("hodgettsp/cesR")
install.packages("haven")
library("tidyverse")
library("cesR")
library("haven")
library("labelled")
install.packages("lme4")
install.packages("brms")
install.packages("tidybayes")
install.packages("caret")
install.packages("ROCR")
library(lme4)
library(brms)
library(tidybayes)
library(caret)
library(ROCR)
#### Load Survey Data & Variable Selection ####
# Load CES 2019 web survey
get_ces("ces2019_web")
# Convert into factor types
ces2019_web<-to_factor(ces2019_web)
head(ces2019_web)
#Select variables
selected_ces<-ces2019_web %>% select(
  cps19_votechoice, 
  cps19_gender, 
  cps19_age, 
  cps19_province,
  cps19_education)
#### Clean Survey Data ####
# Remove observations with NA value
survey_dataset<-na.omit(selected_ces)
# Adjust education levels
survey_dataset<-survey_dataset %>% 
  mutate(education_level = case_when(
    cps19_education=="Completed secondary/ high school"~
      "Secondary (high) school diploma or equivalency certificate",
    cps19_education=="Some technical, community college, CEGEP, College Classique"~
      "Apprenticeship or trades certificate or diploma",
    cps19_education=="Completed technical, community college, CEGEP, College Classique"~
      "College, CEGEP or other non-university certificate or diploma",
    cps19_education=="Some university"~
      "University certificate or diploma below bachelor level",
    cps19_education=="Bachelor's degree"~
      "University certificate or diploma below bachelor level",
    cps19_education=="Master's degree"~
      "Highest certificate, diploma or degree",
    cps19_education=="Professional degree or doctorate"~
      "Highest certificate, diploma or degree",
    cps19_education=="Don't know/ Prefer not to answer"~
      "Other",
    cps19_education=="Some secondary/ high school"~
      "Other"))
#Adjust gender name
survey_dataset<- survey_dataset %>%
  mutate(gender=case_when(cps19_gender=="A woman" ~ "Female",
                       cps19_gender=="A man" ~ "Male"))
survey_dataset<-survey_dataset %>% filter(gender=="Female" | gender=="Male")
#Age
survey_dataset<-survey_dataset %>% filter(cps19_age>=25 & cps19_age<=64)
survey_dataset<-survey_dataset %>% mutate(Age=case_when(
  cps19_age>=25 & cps19_age<=34 ~ "25 to 34",
  cps19_age>=35 & cps19_age<=44 ~ "35 to 44",
  cps19_age>=45 & cps19_age<=54 ~ "45 to 54",
  cps19_age>=55 & cps19_age<=64 ~ "55 to 64"))
#Whether respondent will vote for Liberal party?
survey_dataset_clean<- survey_dataset %>% mutate(cps19_votechoice
                                          =ifelse(cps19_votechoice=="Liberal Party", 
                                                  "Liberal Party", "Others"))
survey_dataset_clean<- survey_dataset_clean %>% mutate(cps19_votechoice=
                                                         ifelse(cps19_votechoice=="Liberal Party",
                                                                1,0))

#### Load Census Data & Variable Selection ####
# Load Census Data (2016 census highlight education table)
raw_census_data<-read_csv("98-402-X2016010-T1-CANPR-eng.csv")
# Pivot data
edu_levels<-c("Total - Highest certificate, diploma or degree (2016 counts)",
              "No certificate, diploma or degree (2016 counts)",
              "Secondary (high) school diploma or equivalency certificate (2016 counts)",
              "Apprenticeship or trades certificate or diploma (2016 counts)",
              "College, CEGEP or other non-university certificate or diploma (2016 counts)",
              "University certificate or diploma below bachelor level (2016 counts)")
pivot_censuc_data<-raw_census_data %>% 
  select(c("Age","Sex","Geographic name",edu_levels)) %>%
  pivot_longer(cols=educ_cols_count, names_to='education',values_to="total_count")
# Change education levels' names
censuc_dataset<-pivot_censuc_data %>% 
  mutate(education_level=case_when(
    education =="Total - Highest certificate, diploma or degree (2016 counts)"~
      "Highest certificate, diploma or degree",
    education=="No certificate, diploma or degree (2016 counts)"~
      "Other",
    education=="Secondary (high) school diploma or equivalency certificate (2016 counts)"~
      "Secondary (high) school diploma or equivalency certificate",
    education=="Apprenticeship or trades certificate or diploma (2016 counts)"~
      "Apprenticeship or trades certificate or diploma",
    education=="College, CEGEP or other non-university certificate or diploma (2016 counts)"~
      "College, CEGEP or other non-university certificate or diploma",
    education=="University certificate or diploma below bachelor level (2016 counts)"~
      "University certificate or diploma below bachelor level"
    ))
# Remove columns for summary
census_dataset<-censuc_dataset %>% filter(Sex!="Both sexes") %>% 
  filter(Age!="All ages, 15-plus") %>% filter(`Geographic name`!= "Canada") %>% filter(Age!= "25 to 64")
#Rename columns
census_dataset<-rename(censuc_dataset, gender=Sex)
census_dataset<-rename(censuc_dataset, province=`Geographic name`)

#Save Files
write_csv(survey_dataset,"/cloud/project/survey_dataset.csv")
write_csv(census_dataset,"/cloud/project/census_dataset.csv")
write_csv(survey_dataset_clean,"/cloud/project/survey_dataset_clean.csv")

