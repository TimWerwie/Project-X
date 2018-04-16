# Set up ----
rm(list = ls())

set.seed(53211)
options(max.print=1000000)

library(tidyverse)
library(tableone)
library(janitor)
library(gmodels)
library(haven)
library(sjlabelled)
library(sjPlot)
library(survey)

# Creating the Datasets ----
# set working directory
setwd("~/Documents/JHSPH/5. Practicum/0. CCP/Zoe Hendrickson/Audience Segmentation")

FQ.kinshasa <- read_dta("PMA2016_CDR5_Kinshasa_HHQFQ_v1_23Feb2017.dta")
FQ.kongo <- read_dta("PMA2016_CDR5_KongoCentral_HHQFQ_v1_23Feb2017.dta")

#merged.data <- merge(FQ.kinshasa, FQ.kongo, by=c("FQtoday", "province", "FQweight", "FQ_age", "school", "FQmarital_status", "marriage_history", "other_wives", "husband_cohabit_now",
                                                 "birth_events", "sons_daughters", "any_child_death", "death_count", "child_alive", "pregnant", "more_children", "more_children_pregnant",
                                                 "wait_birth", "wait_birth_pregnant", "heard_female_sterilization", "heard_male_sterilization", "heard_implants", "heard_IUD",
                                                 "heard_injectables", "heard_pill", "heard_emergency", "heard_male_condoms", "heard_female_condoms", "heard_diaphragm", "heard_foamjelly", 
                                                 "heard_beads", "heard_LAM", "heard_rhythm", "heard_withdrawal", "heard_other", "fp_ever_used", "current_user", "sterilization_permanent_inform",
                                                 "knows_fp_sdp", "future_user_not_current", "future_user_pregnant", "recent_user", "why_not_using", "why_not_usingnotmarr", "why_not_usingnosex",
                                                 "why_not_usingmeno", "why_not_usingsubfec", "why_not_usingnomens", "why_not_usingbreastfd", "why_not_usinghsbndaway","why_not_usinguptogod", 
                                                 "why_not_usingrespopp", "why_not_usinghusbopp", "why_not_usingotheropp", "why_not_usingrelig", "why_not_usingdkmethod", "why_not_usingdksource", 
                                                 "why_not_usingfearside", "why_not_usinghealth", "why_not_usingaccess", "why_not_usingcost", "why_not_usingprfnotavail",  "why_not_usingnomethod", 
                                                 "why_not_usinginconv",  "why_not_usingbodyproc", "why_not_usingother", "why_not_usingdontknow",  "why_not_usingnoresponse",  "why_not_usingtime",
                                                 "why_not_usingintend",  "why_not_usingmil", "unmet", "unmettot", "visited_by_health_worker", "visited_a_facility", "facility_fp_discussion", 
                                                 "fp_ad_radio", "fp_ad_tv", "fp_ad_magazine", "fp_ad_image",  "fp_ad_where", "fp_ad_where_hc", "fp_ad_where_street", "fp_ad_where_phar",  
                                                 "fp_ad_where_bb", "fp_ad_where_tv", "fp_ad_where_leaf",  "fp_ad_where_vest", "fp_ad_where_other",  "fp_ad_meaning", "fp_ad_mean_family_planning", 
                                                 "fp_ad_mean_birth_spacing", "fp_ad_mean_reproduc_health",  "fp_ad_mean_family", "fp_ad_mean_other",  "fp_ad_fam_image", "fp_ad_fam_meaning", 
                                                 "fp_ad_fam_mean_family_planning", "fp_ad_fam_mean_birth_spacing", "fp_ad_fam_mean_reproduc_health", "fp_ad_fam_mean_family", "fp_ad_fam_mean_other",  
                                                 "last_time_sex_value", "anychildren", "libala", "libala_about", "libala_about_comm", "libala_about_harmony",           
                                                 "libala_about_fp_methods", "libala_about_antenatal", "libala_about_postpartum", "libala_about_amenorrheic", "libala_about_spacing", 
                                                 "libala_about_breast_feeding", "libala_about_other", "libala_fp",  "libala_talk", "libala_whom", "libala_whom_mp", "libala_whom_spouse",             
                                                 "libala_whom_other_relative", "libala_whom_friend", "libala_whom_pharmacy", "libala_whom_outreach_worker", "libala_whom_people_seminar", 
                                                 "libala_whom_other", "elengi", "elengi_about", "elengi_about_comm", "elengi_about_harmony",  "elengi_about_fp_methods",  "elengi_about_antenatal", 
                                                 "elengi_about_postpartum", "elengi_about_amenorrheic", "elengi_about_spacing", "elengi_about_breast_feeding", "elengi_about_other", "partner_conv", 
                                                 "convince_partner",  "site_go",  "get_method",  "use_partner", "use_friends", "use_religion", "begin_confer", "wealthquintile")
                     , all=TRUE)

#FQ.variables <- merged.data %>%
  #filter(FQtoday != "") %>%
  #select("province", "FQweight", "FQ_age", "school", "FQmarital_status", "marriage_history", "other_wives", "husband_cohabit_now",
         "birth_events", "sons_daughters", "any_child_death", "death_count", "child_alive", "pregnant", "more_children", "more_children_pregnant",
         "wait_birth", "wait_birth_pregnant", "heard_female_sterilization", "heard_male_sterilization", "heard_implants", "heard_IUD",
         "heard_injectables", "heard_pill", "heard_emergency", "heard_male_condoms", "heard_female_condoms", "heard_diaphragm", "heard_foamjelly", 
         "heard_beads", "heard_LAM", "heard_rhythm", "heard_withdrawal", "heard_other", "fp_ever_used", "current_user", "sterilization_permanent_inform",
         "knows_fp_sdp", "future_user_not_current", "future_user_pregnant", "recent_user", "why_not_using", "why_not_usingnotmarr", "why_not_usingnosex",
         "why_not_usingmeno", "why_not_usingsubfec", "why_not_usingnomens", "why_not_usingbreastfd", "why_not_usinghsbndaway","why_not_usinguptogod", 
         "why_not_usingrespopp", "why_not_usinghusbopp", "why_not_usingotheropp", "why_not_usingrelig", "why_not_usingdkmethod", "why_not_usingdksource", 
         "why_not_usingfearside", "why_not_usinghealth", "why_not_usingaccess", "why_not_usingcost", "why_not_usingprfnotavail",  "why_not_usingnomethod", 
         "why_not_usinginconv",  "why_not_usingbodyproc", "why_not_usingother", "why_not_usingdontknow",  "why_not_usingnoresponse",  "why_not_usingtime",
         "why_not_usingintend",  "why_not_usingmil", "unmet", "unmettot", "visited_by_health_worker", "visited_a_facility", "facility_fp_discussion", 
         "fp_ad_radio", "fp_ad_tv", "fp_ad_magazine", "fp_ad_image",  "fp_ad_where", "fp_ad_where_hc", "fp_ad_where_street", "fp_ad_where_phar",  
         "fp_ad_where_bb", "fp_ad_where_tv", "fp_ad_where_leaf",  "fp_ad_where_vest", "fp_ad_where_other",  "fp_ad_meaning", "fp_ad_mean_family_planning", 
         "fp_ad_mean_birth_spacing", "fp_ad_mean_reproduc_health",  "fp_ad_mean_family", "fp_ad_mean_other",  "fp_ad_fam_image", "fp_ad_fam_meaning", 
         "fp_ad_fam_mean_family_planning", "fp_ad_fam_mean_birth_spacing", "fp_ad_fam_mean_reproduc_health", "fp_ad_fam_mean_family", "fp_ad_fam_mean_other",  
         "last_time_sex_value", "anychildren", "libala", "libala_about", "libala_about_comm", "libala_about_harmony",           
         "libala_about_fp_methods", "libala_about_antenatal", "libala_about_postpartum", "libala_about_amenorrheic", "libala_about_spacing", 
         "libala_about_breast_feeding", "libala_about_other", "libala_fp",  "libala_talk", "libala_whom", "libala_whom_mp", "libala_whom_spouse",             
         "libala_whom_other_relative", "libala_whom_friend", "libala_whom_pharmacy", "libala_whom_outreach_worker", "libala_whom_people_seminar", 
         "libala_whom_other", "elengi", "elengi_about", "elengi_about_comm", "elengi_about_harmony",  "elengi_about_fp_methods",  "elengi_about_antenatal", 
         "elengi_about_postpartum", "elengi_about_amenorrheic", "elengi_about_spacing", "elengi_about_breast_feeding", "elengi_about_other", "partner_conv", 
         "convince_partner",  "site_go",  "get_method",  "use_partner", "use_friends", "use_religion", "begin_confer", "wealthquintile")

# If Looking only at Kinshasa
# If Looking only at Kinshasa
# If Looking only at Kinshasa
# If Looking only at Kinshasa
# If Looking only at Kinshasa
# If Looking only at Kinshasa
# If Looking only at Kinshasa
# If Looking only at Kinshasa
# If Looking only at Kinshasa

FQ.variables <- FQ.kinshasa %>%
  filter(FQtoday != "") %>%
  select("province", "FQweight", "FQ_age", "school", "FQmarital_status", "marriage_history", "other_wives", "husband_cohabit_now",
         "birth_events", "sons_daughters", "any_child_death", "death_count", "child_alive", "pregnant", "more_children", "more_children_pregnant",
         "wait_birth", "wait_birth_pregnant", "heard_female_sterilization", "heard_male_sterilization", "heard_implants", "heard_IUD",
         "heard_injectables", "heard_pill", "heard_emergency", "heard_male_condoms", "heard_female_condoms", "heard_diaphragm", "heard_foamjelly", 
         "heard_beads", "heard_LAM", "heard_rhythm", "heard_withdrawal", "heard_other", "fp_ever_used", "current_user", "sterilization_permanent_inform",
         "knows_fp_sdp", "future_user_not_current", "future_user_pregnant", "recent_user", "why_not_using", "why_not_usingnotmarr", "why_not_usingnosex",
         "why_not_usingmeno", "why_not_usingsubfec", "why_not_usingnomens", "why_not_usingbreastfd", "why_not_usinghsbndaway","why_not_usinguptogod", 
         "why_not_usingrespopp", "why_not_usinghusbopp", "why_not_usingotheropp", "why_not_usingrelig", "why_not_usingdkmethod", "why_not_usingdksource", 
         "why_not_usingfearside", "why_not_usinghealth", "why_not_usingaccess", "why_not_usingcost", "why_not_usingprfnotavail",  "why_not_usingnomethod", 
         "why_not_usinginconv",  "why_not_usingbodyproc", "why_not_usingother", "why_not_usingdontknow",  "why_not_usingnoresponse",  "why_not_usingtime",
         "why_not_usingintend",  "why_not_usingmil", "unmet", "unmettot", "visited_by_health_worker", "visited_a_facility", "facility_fp_discussion", 
         "fp_ad_radio", "fp_ad_tv", "fp_ad_magazine", "fp_ad_image",  "fp_ad_where", "fp_ad_where_hc", "fp_ad_where_street", "fp_ad_where_phar",  
         "fp_ad_where_bb", "fp_ad_where_tv", "fp_ad_where_leaf",  "fp_ad_where_vest", "fp_ad_where_other",  "fp_ad_meaning", "fp_ad_mean_family_planning", 
         "fp_ad_mean_birth_spacing", "fp_ad_mean_reproduc_health",  "fp_ad_mean_family", "fp_ad_mean_other",  "fp_ad_fam_image", "fp_ad_fam_meaning", 
         "fp_ad_fam_mean_family_planning", "fp_ad_fam_mean_birth_spacing", "fp_ad_fam_mean_reproduc_health", "fp_ad_fam_mean_family", "fp_ad_fam_mean_other",  
         "last_time_sex_value", "anychildren", "libala", "libala_about", "libala_about_comm", "libala_about_harmony",           
         "libala_about_fp_methods", "libala_about_antenatal", "libala_about_postpartum", "libala_about_amenorrheic", "libala_about_spacing", 
         "libala_about_breast_feeding", "libala_about_other", "libala_fp",  "libala_talk", "libala_whom", "libala_whom_mp", "libala_whom_spouse",             
         "libala_whom_other_relative", "libala_whom_friend", "libala_whom_pharmacy", "libala_whom_outreach_worker", "libala_whom_people_seminar", 
         "libala_whom_other", "elengi", "elengi_about", "elengi_about_comm", "elengi_about_harmony",  "elengi_about_fp_methods",  "elengi_about_antenatal", 
         "elengi_about_postpartum", "elengi_about_amenorrheic", "elengi_about_spacing", "elengi_about_breast_feeding", "elengi_about_other", "partner_conv", 
         "convince_partner",  "site_go",  "get_method",  "use_partner", "use_friends", "use_religion", "begin_confer", "wealthquintile")

FQ.short <- FQ.variables %>%
  select("province", "FQweight", "FQ_age", "school", "FQmarital_status", "wealthquintile", "other_wives", "husband_cohabit_now", "any_child_death",
         "more_children", "wait_birth", "fp_ever_used", "current_user", "knows_fp_sdp", "future_user_not_current", "future_user_pregnant", 
         "recent_user", "unmet", "visited_by_health_worker", "visited_a_facility", "facility_fp_discussion", "fp_ad_radio", "fp_ad_tv", 
         "fp_ad_magazine", "fp_ad_image", "anychildren", "libala", "libala_talk", "elengi", "partner_conv", "convince_partner",  "site_go",  
         "get_method",  "use_partner", "use_friends", "use_religion", "begin_confer")

#excluding current users
FQ.short.nonuser <- FQ.short %>%
  filter(current_user != 1)

names(FQ.short)
names(FQ.short.nonuser)

# Making FQ.names with Stata labels ----

# creating a dataset with each categorical variable's value LABELS
# creating a dataset with each categorical variable's value LABELS
# creating a dataset with each categorical variable's value LABELS
# creating a dataset with each categorical variable's value LABELS
# creating a dataset with each categorical variable's value LABELS
# creating a dataset with each categorical variable's value LABELS
# creating a dataset with each categorical variable's value LABELS
# creating a dataset with each categorical variable's value LABELS
# creating a dataset with each categorical variable's value LABELS
# creating a dataset with each categorical variable's value LABELS

# write the function
functionname <- function(x){ #name the function
  a <- attr(x,which="labels",exact=TRUE) #for x, access the the attribute "labels", and match them exactly
  b <- factor(x,levels=a,labels = names(a)) #create a factor x, whose levels are from the data passed through a, and whose labels come from names accessed in a
  return(b) #return b
}

#prepping a new dataset for the factor level labels
FQ.names.temp <- as.data.frame(FQ.short.nonuser) #renaming the FQ.short.nonuser dataset, coercing into a dataframe
FQ.names.temp <- FQ.names.temp[,-1] #taking out province
FQ.names.temp <- FQ.names.temp[, -2] #taking out FQweight
FQ.names.temp <- FQ.names.temp[, -3] #taking out FQage
names(FQ.names.temp)
FQ.names <- matrix(NA, nrow = nrow(FQ.names.temp), ncol = ncol(FQ.names.temp)) #creating a new dataset of the same dimensions as dat, full of NA's

#is there an apply function we could write?
for (i in 1:ncol(FQ.names.temp)){ #for loop
  FQ.names[,i] <- as.character(functionname(FQ.names.temp[,i])) #for every column in newdat, run the function on dat and coerce it all into character strings
}

colnames(FQ.names) <- colnames(FQ.names.temp) #replace the column names of dat as the column names in newdat

FQ.names.df <- as.data.frame(FQ.names) #make it a dataframe
str(FQ.names.df)
summary(FQ.names.df)

# Regrouping and recoding ----

# Regrouping variables
# Regrouping variables
# Regrouping variables
# Regrouping variables
# Regrouping variables
# Regrouping variables
# Regrouping variables
# Regrouping variables
# Regrouping variables
# Regrouping variables

# school
# school
# school
# school
# school
# school
# school
# school
# school
# school
# school
# school

get_labels(FQ.short.nonuser$school)
get_label(FQ.short.nonuser$school)
summary(as.factor(FQ.short.nonuser$school))

#first create list of NAs for that variable, then use which to find locations and change the categorical level to whatever I want
new.school.temp <- rep(NA, length(FQ.short.nonuser$school))
#locate where marital status is coded as 1, recode them to be 1
new.school.temp[which(FQ.short.nonuser$school == 0)] <- 0 # 0. Primary or less
new.school.temp[which(FQ.short.nonuser$school == 1)] <- 0 # 0. Primary or less
new.school.temp[which(FQ.short.nonuser$school == 2)] <- 1 # 1. Secondary
new.school.temp[which(FQ.short.nonuser$school == 3)] <- 2 # 2. Tertiary

new.school.temp <- set_labels(new.school.temp, labels = c("0. Primary or less", "1. Secondary", "2. Tertiary")) #set labels
new.school.temp <- set_label(new.school.temp, label = c("Highest level of school attended")) # variable label

new.school <- functionname(new.school.temp) # create variable of labels for FQ.names.df
get_labels(new.school.temp) # returns NULL, need to create a column name for FQ.names.df?
get_labels(new.school)

# Marital Status
# Marital Status
# Marital Status
# Marital Status
# Marital Status
# Marital Status

str(FQ.short.nonuser$FQmarital_status)
levels(as.factor(FQ.short.nonuser$FQmarital_status))
summary(as.factor(FQ.short.nonuser$FQmarital_status))
get_labels(FQ.short.nonuser$FQmarital_status)
is.ordered(FQ.short.nonuser$FQmarital_status)
summary(FQ.names.df$FQmarital_status)
str(FQ.names.df$FQmarital_status)
levels(FQ.names.df$FQmarital_status)

#first create list of NAs for that variable, then use which to find locations and change the categorical level to whatever I want
new.marital.temp <- rep(NA, length(FQ.short.nonuser$FQmarital_status))
#locate where marital status is coded as 1, recode them to be 1
new.marital.temp[which(FQ.short.nonuser$FQmarital_status == 1)] <- 1
new.marital.temp[which(FQ.short.nonuser$FQmarital_status == 2)] <- 1 # 1. Married
new.marital.temp[which(FQ.short.nonuser$FQmarital_status == 3)] <- 0 # 0. Not Married
new.marital.temp[which(FQ.short.nonuser$FQmarital_status == 4)] <- 0
new.marital.temp[which(FQ.short.nonuser$FQmarital_status == 5)] <- 0

new.marital.temp <- set_labels(new.marital.temp, labels = c("0. No", "1. Yes")) #set labels
str(new.marital.temp)
summary(as.factor(new.marital.temp))
summary(as.factor(FQ.short.nonuser$FQmarital_status))

new.marital.temp <- set_label(new.marital.temp, label = c("Currently Married?")) # variable label
str(new.marital.temp) # this should be added to FQ.short
summary(FQ.names.df$FQmarital_status)
summary(as.factor(new.marital.temp))
get_labels(new.marital.temp) # checking
get_label(new.marital.temp) # returns the correct label

new.marital <- functionname(new.marital.temp) # create variable of labels for FQ.names.df
str(new.marital)
summary(new.marital)
get_label(new.marital) # returns NULL, need to create a column name for FQ.names.df?
get_labels(new.marital)
str(FQ.names.df$FQmarital_status)

# More Children
# More Children
# More Children
# More Children
# More Children
# More Children
# More Children
# More Children
# More Children

str(FQ.short.nonuser$more_children)
levels(as.factor(FQ.short.nonuser$more_children))
summary(as.factor(FQ.short.nonuser$more_children))
summary(FQ.names.df$more_children)
str(FQ.names.df$more_children)
levels(FQ.names.df$more_children)

#first create list of NAs for that variable, then use which to find locations and change the categorical level to whatever I want
new.morechildren.temp <- rep(NA, length(FQ.short.nonuser$more_children))
#locate
new.morechildren.temp[which(FQ.short.nonuser$more_children == 1)] <- 1 # 1. Yes
new.morechildren.temp[which(FQ.short.nonuser$more_children == 2)] <- 0 # 0. No
new.morechildren.temp[which(FQ.short.nonuser$more_children == 3)] <- 0 
new.morechildren.temp[which(FQ.short.nonuser$more_children == -88)] <- 0

new.morechildren.temp <- set_labels(new.morechildren.temp, labels = c("0. No", "1. Yes")) #set labels
str(new.morechildren.temp)
summary(as.factor(new.morechildren.temp))
summary(as.factor(FQ.short.nonuser$more_children))

new.morechildren.temp <- set_label(new.morechildren.temp, label = c("Prefer to have another child?")) # variable label
str(new.morechildren.temp) # this should be added to FQ.short
get_labels(new.morechildren.temp) # checking
get_label(new.morechildren.temp)

new.morechildren <- functionname(new.morechildren.temp) # create variable of labels for FQ.names.df
str(new.morechildren)
summary(new.morechildren)
get_label(new.morechildren)
get_labels(new.morechildren)

# Unmet need
# Unmet need
# Unmet need
# Unmet need
# Unmet need
# Unmet need
# Unmet need
# Unmet need
# Unmet need
# Unmet need

str(FQ.short.nonuser$unmet)
levels(as.factor(FQ.short.nonuser$unmet))
summary(as.factor(FQ.short.nonuser$unmet))
summary(FQ.names.df$unmet)
str(FQ.names.df$unmet)
levels(FQ.names.df$unmet)
get_label(FQ.short$unmet)

#first create list of NAs for that variable, then use which to find locations and change the categorical level to whatever I want
new.unmet.temp <- rep(NA, length(FQ.short.nonuser$unmet))
#locate
new.unmet.temp[which(FQ.short.nonuser$unmet == 1)] <- 1 # 1. Unmet for spacing
new.unmet.temp[which(FQ.short.nonuser$unmet == 2)] <- 1 # 2. Unmet for limiting
new.unmet.temp[which(FQ.short.nonuser$unmet == 3)] <- 0 # 3. Using for spacing
new.unmet.temp[which(FQ.short.nonuser$unmet == 4)] <- 0 # 4. Using for limiting
new.unmet.temp[which(FQ.short.nonuser$unmet == 7)] <- 0 # 5. No unmet need
new.unmet.temp[which(FQ.short.nonuser$unmet == 9)] <- 0 # 9. Infecund
new.unmet.temp[which(FQ.short.nonuser$unmet == -97)] <- 0 # -97. Not sexually active

new.unmet.temp <- set_labels(new.unmet.temp, labels = c("0. No", "1. Yes")) #set labels
str(new.unmet.temp)
summary(as.factor(new.unmet.temp))
summary(as.factor(FQ.short.nonuser$unmet))

new.unmet.temp <- set_label(new.unmet.temp, label = c("Unmet need")) # variable label
str(new.unmet.temp) # this should be added to FQ.short
get_labels(new.unmet.temp) # checking
get_label(new.unmet.temp)

new.unmet <- functionname(new.unmet.temp) # create variable of labels for FQ.names.df
str(new.unmet)
summary(new.unmet)
get_label(new.unmet) # NULL
get_labels(new.unmet)

#Any Children
#Any Children
#Any Children
#Any Children
#Any Children
#Any Children
#Any Children
#Any Children
#Any Children
#Any Children
#Any Children
#Any Children
#Any Children

str(FQ.short.nonuser$anychildren)
levels(as.factor(FQ.short.nonuser$anychildren))
summary(as.factor(FQ.short.nonuser$anychildren))
summary(FQ.names.df$anychildren) # must not have labels from the Stata
str(FQ.names.df$anychildren)
levels(FQ.names.df$anychildren)
get_label(FQ.short$anychildren)

#first create list of NAs for that variable, then use which to find locations and change the categorical level to whatever I want
new.anychildren.temp <- rep(NA, length(FQ.short.nonuser$anychildren))
#locate
new.anychildren.temp[which(FQ.short.nonuser$anychildren == 0)] <- 0 # 0
new.anychildren.temp[which(FQ.short.nonuser$anychildren == 1)] <- 1 # 1
new.anychildren.temp[which(FQ.short.nonuser$anychildren == 2)] <- 2 # 2
new.anychildren.temp[which(FQ.short.nonuser$anychildren == 3)] <- 2 # 3
new.anychildren.temp[which(FQ.short.nonuser$anychildren == 4)] <- 2 # 4
new.anychildren.temp[which(FQ.short.nonuser$anychildren == 5)] <- 2 # 5 

new.anychildren.temp <- set_labels(new.anychildren.temp, labels = c("0. 0", "1. 1", "2. 2+")) #set labels. Stopped at 2+ because too few in 3, 4, 5
str(new.anychildren.temp)
summary(as.factor(new.anychildren.temp))
summary(as.factor(FQ.short.nonuser$anychildren))

new.anychildren.temp <- set_label(new.anychildren.temp, label = c("Number of children under 5 living in the household for which respondent is primary caregiver")) # variable label
str(new.anychildren.temp) # this should be added to FQ.short
get_labels(new.anychildren.temp) # checking
get_label(new.anychildren.temp)

new.anychildren <- functionname(new.anychildren.temp) # create variable of labels for FQ.names.df
str(new.anychildren)
summary(new.anychildren)
get_label(new.anychildren) # NULL
get_labels(new.anychildren)

# Other wives - changing -99 to NA and regrouping -88 to No
# Other wives - changing -99 to NA and regrouping -88 to No
# Other wives - changing -99 to NA and regrouping -88 to No
# Other wives - changing -99 to NA and regrouping -88 to No
# Other wives - changing -99 to NA and regrouping -88 to No
# Other wives - changing -99 to NA and regrouping -88 to No
# Other wives - changing -99 to NA and regrouping -88 to No
# Other wives - changing -99 to NA and regrouping -88 to No

levels(as.factor(FQ.short.nonuser$other_wives))
get_labels(FQ.short.nonuser$other_wives)
summary(as.factor(FQ.short.nonuser$other_wives))
summary(FQ.names.df$other_wives)
get_label(FQ.short.nonuser$other_wives)

#first create list of NAs for that variable, then use which to find locations and change the categorical level to whatever I want
new.otherwives.temp <- rep(NA, length(FQ.short.nonuser$other_wives))
#locate where marital status is coded as 1, recode them to be 1
new.otherwives.temp[which(FQ.short.nonuser$other_wives == 0)] <- 0 # 0. No
new.otherwives.temp[which(FQ.short.nonuser$other_wives == -88)] <- 0 # 0. No
new.otherwives.temp[which(FQ.short.nonuser$other_wives == 1)] <- 1 # 1. Yes

new.otherwives.temp <- set_labels(new.otherwives.temp, labels = c("0. No", "1. Yes")) #set labels
new.otherwives.temp <- set_label(new.otherwives.temp, label = c("Partner has other wives")) # variable label

new.otherwives <- functionname(new.otherwives.temp) # create variable of labels for FQ.names.df
str(new.otherwives)
summary(new.otherwives)

# Changing -88 and -88. -88 to "no"
# Changing -88 and -88. -88 to "no"
# Changing -88 and -88. -88 to "no"
# Changing -88 and -88. -88 to "no"
# Changing -88 and -88. -88 to "no"
# Changing -88 and -88. -88 to "no"
# Changing -88 and -88. -88 to "no"
# Changing -88 and -88. -88 to "no"
# Changing -88 and -88. -88 to "no"
# Changing -88 and -88. -88 to "no"

# only wait_birth, and fp_ad_image have -88
str(FQ.short.nonuser$wait_birth)
levels(FQ.names.df$wait_birth)
summary(as.factor(FQ.short.nonuser$wait_birth))

#first create list of NAs for that variable, then use which to find locations and change the categorical level to whatever I want
new.waitbirth.temp <- rep(NA, length(FQ.short.nonuser$wait_birth))
#locate where marital status is coded as 1, recode them to be 1
new.waitbirth.temp[which(FQ.short.nonuser$wait_birth == 1)] <- 0 # 0. soon
new.waitbirth.temp[which(FQ.short.nonuser$wait_birth == 2)] <- 1 # 1. months
new.waitbirth.temp[which(FQ.short.nonuser$wait_birth == 3)] <- 2 # 2. years
new.waitbirth.temp[which(FQ.short.nonuser$wait_birth == 5)] <- 3 # 3. other (previously 5. other)
new.waitbirth.temp[which(FQ.short.nonuser$wait_birth == -88)] <- 3 # 3. other (previously don't know)
new.waitbirth.temp[which(FQ.short.nonuser$wait_birth == 4)] <- NA # (previously infertile)

new.waitbirth.temp <- set_labels(new.waitbirth.temp, labels = c("0. soon", "1. months", "2. years", "3. other")) #set labels
new.waitbirth.temp <- set_label(new.waitbirth.temp, label = c("How long would you like to wait until next child - not pregnant")) # variable label

new.waitbirth <- functionname(new.waitbirth.temp) # create variable of labels for FQ.names.df

# changing fp_ad_image -88 to no
# changing fp_ad_image -88 to no
# changing fp_ad_image -88 to no
# changing fp_ad_image -88 to no
# changing fp_ad_image -88 to no
# changing fp_ad_image -88 to no
# changing fp_ad_image -88 to no
# changing fp_ad_image -88 to no
# changing fp_ad_image -88 to no
# changing fp_ad_image -88 to no

levels(FQ.names.df$fp_ad_image)
summary(as.factor(FQ.short.nonuser$fp_ad_image))
get_label(FQ.short.nonuser$fp_ad_image)
#first create list of NAs for that variable, then use which to find locations and change the categorical level to whatever I want
new.fp.ad.image.temp <- rep(NA, length(FQ.short.nonuser$fp_ad_image))
#locate where marital status is coded as 1, recode them to be 1
new.fp.ad.image.temp[which(FQ.short.nonuser$fp_ad_image == 0)] <- 0 # 0. no
new.fp.ad.image.temp[which(FQ.short.nonuser$fp_ad_image == -88)] <- 0 # 0. no
new.fp.ad.image.temp[which(FQ.short.nonuser$fp_ad_image == 1)] <- 1 # 1. yes

new.fp.ad.image.temp <- set_labels(new.fp.ad.image.temp, labels = c("0. no", "1. yes")) #set labels
new.fp.ad.image.temp <- set_label(new.fp.ad.image.temp, label = c("Have you seen this image?")) # variable label

new.fp.ad.image <- functionname(new.fp.ad.image.temp) # create variable of labels for FQ.names.df

#adding new variables to datasets
#adding new variables to datasets
#adding new variables to datasets
#adding new variables to datasets
#adding new variables to datasets
#adding new variables to datasets
#adding new variables to datasets
#adding new variables to datasets
#adding new variables to datasets
#adding new variables to datasets

# create new dataframe of new variables
fq.names.df.regroup <- cbind.data.frame(new.marital, new.school, new.otherwives,  new.unmet, new.morechildren, 
                                        new.anychildren, new.waitbirth, new.fp.ad.image)
fq.short.regroup <- cbind.data.frame(new.marital.temp, new.school.temp, new.otherwives.temp, new.unmet.temp, 
                                     new.morechildren.temp, new.anychildren.temp, new.waitbirth.temp, new.fp.ad.image.temp)

#add them to existing dataframes

FQ.names.df.2 <- bind_cols(FQ.names.df, fq.names.df.regroup)

FQ.short.nonuser.2 <- bind_cols(FQ.short.nonuser, fq.short.regroup)

#Dropping columns and creating new dataframe
#Dropping columns and creating new dataframe
#Dropping columns and creating new dataframe
#Dropping columns and creating new dataframe
#Dropping columns and creating new dataframe
#Dropping columns and creating new dataframe
#Dropping columns and creating new dataframe
#Dropping columns and creating new dataframe
#Dropping columns and creating new dataframe
#Dropping columns and creating new dataframe

fq.names.df.drop <- c("FQmarital_status", "fp_ad_image", "unmet", "more_children", "anychildren", "other_wives", "wait_birth", "school", "pregnant")
fq.short.drop <- c("FQmarital_status", "fp_ad_image", "unmet", "more_children", "anychildren", "other_wives", "wait_birth", "school", "pregnant")

FQ.names.df.2 <- FQ.names.df.2 %>% modify_at(fq.names.df.drop, ~NULL)
FQ.short.nonuser.2 <- FQ.short.nonuser.2 %>% modify_at(fq.short.drop, ~NULL)

# Changing 99 and -99. -99 to NA
# Changing 99 and -99. -99 to NA
# Changing 99 and -99. -99 to NA
# Changing 99 and -99. -99 to NA
# Changing 99 and -99. -99 to NA
# Changing 99 and -99. -99 to NA
# Changing 99 and -99. -99 to NA
# Changing 99 and -99. -99 to NA
# Changing 99 and -99. -99 to NA
# Changing 99 and -99. -99 to NA
# Changing 99 and -99. -99 to NA

FQ.short.nonuser.2[FQ.short.nonuser.2 == -99] <- NA
FQ.names.df.2[FQ.names.df.2 == "-99. -99"] <- NA
# writing function to drop the unused level "-99. -99"
replace.with.NA <- function(x)
{
  return(factor(x, exclude= "-99. -99"))
}
# applying function and making it into a data.frame
FQ.names.df.2 <- apply(FQ.names.df.2, 2, replace.with.NA)
FQ.names.df.2 <- as.data.frame(FQ.names.df.2)

# drop the unused levels
factor(FQ.names.df.2$new.waitbirth, exclude= "-88. -88")
factor(FQ.names.df.2$new.waitbirth, exclude= "4. infertile")
factor(FQ.names.df.2$new.waitbirth, exclude= "5. other")
factor(FQ.names.df.2$new.fp.ad.image, exclude= "-88. -88")

#check
summary(FQ.names.df.2$new.waitbirth)
levels(FQ.names.df.2$new.waitbirth)
summary(FQ.names.df.2$new.fp.ad.image)
levels(FQ.names.df.2$new.fp.ad.image)

#check data.frames FQ.short.nonuser.2 and FQ.names.df.2

summary(as.factor(FQ.short.nonuser.2$new.fp.ad.image.temp))
levels(FQ.names.df.2$new.fp.ad.image)
levels(as.factor(FQ.short.nonuser.2$new.fp.ad.image.temp))
summary(FQ.names.df.2$knows_fp_sdp)
levels(FQ.names.df.2$visited_by_health_worker)
levels(FQ.names.df.2$future_user_not_current)
summary(FQ.names.df.2$new.waitbirth)
summary(as.factor(FQ.short.nonuser.2$new.waitbirth.temp))
summary(as.factor(FQ.short.nonuser.2$future_user_not_current))
summary(as.factor(FQ.short.nonuser.2$new.fp.ad.image.temp))

View(FQ.names.df.2); View(FQ.short.nonuser.2)

# adding province, FQweight and Age back to FQ.names.df.2
# adding province, FQweight and Age back to FQ.names.df.2
# adding province, FQweight and Age back to FQ.names.df.2
# adding province, FQweight and Age back to FQ.names.df.2
# adding province, FQweight and Age back to FQ.names.df.2
# adding province, FQweight and Age back to FQ.names.df.2
# adding province, FQweight and Age back to FQ.names.df.2
# adding province, FQweight and Age back to FQ.names.df.2
# adding province, FQweight and Age back to FQ.names.df.2
# adding province, FQweight and Age back to FQ.names.df.2
# adding province, FQweight and Age back to FQ.names.df.2
# adding province, FQweight and Age back to FQ.names.df.2

FQ.names.df.2$Age <- FQ.short.nonuser$FQ_age
FQ.names.df.2$province <- FQ.short.nonuser$province
FQ.names.df.2$FQweight <- FQ.short.nonuser$FQweight
# Table Ones and summaries ----

# Create TableOne stratified by Intention
# Create TableOne stratified by Intention
# Create TableOne stratified by Intention
# Create TableOne stratified by Intention
# Create TableOne stratified by Intention
# Create TableOne stratified by Intention
# Create TableOne stratified by Intention
# Create TableOne stratified by Intention

names(FQ.names.df.2)
vars.2 <- c("new.school", "wealthquintile", "new.marital", "new.otherwives", "husband_cohabit_now", "new.anychildren", 
            "any_child_death", # socio-demo
            "new.morechildren", "new.waitbirth", "fp_ever_used", "new.unmet", # fertility preferences
            "knows_fp_sdp", "visited_by_health_worker", "visited_a_facility", "facility_fp_discussion", # knowledge/service
            "fp_ad_radio", "fp_ad_tv",  "fp_ad_magazine", "new.fp.ad.image", "libala", "libala_talk", "elengi") # ads/comms

factors.2 <- c("new.school", "wealthquintile", "new.marital", "new.otherwives", "husband_cohabit_now", "new.anychildren", 
               "any_child_death", # socio-demo
               "new.morechildren", "new.waitbirth", "fp_ever_used", "new.unmet", # fertility preferences
               "knows_fp_sdp", "visited_by_health_worker", "visited_a_facility", "facility_fp_discussion", # knowledge/service
               "fp_ad_radio", "fp_ad_tv",  "fp_ad_magazine", "new.fp.ad.image", "libala", "libala_talk", "elengi") # ads/comms

FQ.names.table1.2 <- CreateTableOne(vars = vars.2, data = FQ.names.df.2, factorVars = factors.2, strata = "future_user_not_current")
print(FQ.names.table1.2, showAllLevels = TRUE)
summary(FQ.names.table1.2)

#table 1 summary for nonusers, CONTINUOUS 
FQ.short.nonuser.2 %>% 
  select(FQ_age) %>%
  summarize(n = n(), mean = round(mean(FQ_age), 0), sd = round(sd(FQ_age), 2), min = min(FQ_age), 
            med = median(FQ_age), max = max(FQ_age), missing = sum(is.na(FQ_age)),
            perc = n()/(nrow(FQ.short.nonuser.2)))

# group_by intent vs no intent, CONTINUOUS
FQ.short.nonuser.2 %>% 
  select(FQ_age, future_user_not_current) %>%
  group_by(future_user_not_current) %>%
  summarize(n = n(), mean = round(mean(FQ_age), 0), sd = round(sd(FQ_age), 2), min = min(FQ_age), 
            med = median(FQ_age), max = max(FQ_age), missing = sum(is.na(FQ_age)),
            perc = n()/(nrow(FQ.short.nonuser.2)))

dat.table1.cont.2 <- CreateTableOne(vars = c("FQ_age"), data = FQ.short.nonuser.2, strata = "future_user_not_current")
print(dat.table1.cont.2)

# Create TableOne for Visted By Health Worker
# Create TableOne for Visted By Health Worker
# Create TableOne for Visted By Health Worker
# Create TableOne for Visted By Health Worker
# Create TableOne for Visted By Health Worker
# Create TableOne for Visted By Health Worker
# Create TableOne for Visted By Health Worker
# Create TableOne for Visted By Health Worker

vars.CHW.2 <- c("new.school", "new.marital", "wealthquintile", "husband_cohabit_now", "future_user_not_current", "future_user_pregnant", 
          "recent_user", "new.unmet", "visited_a_facility", "facility_fp_discussion", "new.anychildren")
factors.CHW.2 <- c("new.school", "new.marital", "wealthquintile", "husband_cohabit_now", "future_user_not_current", "future_user_pregnant", 
             "recent_user", "new.unmet", "visited_a_facility", "facility_fp_discussion", "new.anychildren")

CHW.visit.2 <- CreateTableOne(vars = vars.CHW.2, data = FQ.names.df.2, factorVars = factors.CHW.2, strata = "visited_by_health_worker")
print(CHW.visit.2, showAllLevels = TRUE)
summary(CHW.visit.2)

# Create TableOne for Visted By Health Worker CONTINUOUS
# Create TableOne for Visted By Health Worker CONTINUOUS
# Create TableOne for Visted By Health Worker CONTINUOUS
# Create TableOne for Visted By Health Worker CONTINUOUS
# Create TableOne for Visted By Health Worker CONTINUOUS
# Create TableOne for Visted By Health Worker CONTINUOUS
# Create TableOne for Visted By Health Worker CONTINUOUS
# Create TableOne for Visted By Health Worker CONTINUOUS
# Create TableOne for Visted By Health Worker CONTINUOUS

CHW.table1.cont.2 <- CreateTableOne(vars = c("FQ_age"), data = FQ.short.nonuser.2, strata = "visited_by_health_worker")
print(CHW.table1.cont.2, quote = TRUE, noSpaces = TRUE, showAllLevels = TRUE)

# Summary Tables
# Summary Tables
# Summary Tables
# Summary Tables
# Summary Tables
# Summary Tables

print(dat.table1.cont.2, quote = TRUE, noSpaces = TRUE, showAllLevels = TRUE) #FQ_age
summary(dat.table1.cont.2) #FQ_age
print(FQ.names.table1.2, quote = TRUE, noSpaces = TRUE, showAllLevels = TRUE) #categorical from FQ.names.df
summary(FQ.names.table1.2) #categorical from FQ.names.df
print(CHW.visit.2, quote = TRUE, noSpaces = TRUE, showAllLevels = TRUE) #categorical from FQ.names.df
summary(CHW.visit.2) #categorical from FQ.names.df
print(CHW.table1.cont.2, quote = TRUE, noSpaces = TRUE, showAllLevels = TRUE) #FQ_age health worker
# Complex survey analysis ----
# Complex survey analysis
# Complex survey analysis
# Complex survey analysis
# Complex survey analysis
# Complex survey analysis
# Complex survey analysis
# Complex survey analysis
# Complex survey analysis
# Complex survey analysis
# Complex survey analysis
# Complex survey analysis
# Complex survey analysis

# create object, no cluster ID ####will not accept strata with missing value
des <- svydesign(ids = ~1, strata = ~province, weights = ~FQweight, data = FQ.names.df.2[is.na(FQ.names.df.2$FQweight) == F,])

svytable(~ future_user_not_current + visited_by_health_worker, design = des)
prop.table(svytable(~ future_user_not_current + visited_by_health_worker, design = des), margin = 2)

# Table 1 for design object
# Table 1 for design object
# Table 1 for design object
# Table 1 for design object
# Table 1 for design object
# Table 1 for design object
# Table 1 for design object
# Table 1 for design object
# Table 1 for design object

options(survey.lonely.psu="adjust") # necessary because of lonely psu errors
tab1 <- svyCreateCatTable(vars = vars.2, strata = c("province", "future_user_not_current"), data = des, 
                  includeNA = FALSE, test = TRUE, testApprox = svyTestChisq, argsApprox = NULL,
                  smd = TRUE)
print(tab1, quote = TRUE, noSpaces= TRUE, showAllLevels = TRUE)
summary(tab1)

# Getting Age
# Getting Age
# Getting Age
# Getting Age
# Getting Age
# Getting Age
# Getting Age
# Getting Age
# Getting Age
# Getting Age

tab2 <- svyCreateTableOne(vars = c("Age"), strata = c("province", "future_user_not_current"), data = des, 
                  includeNA = FALSE, test = TRUE, argsApprox = NULL,
                  smd = TRUE)
print(tab2, quote = TRUE, noSpaces= TRUE, showAllLevels = TRUE)
summary(tab2)
svymean(~Age, des) # just provides a total, not by strata

# chisq
# chisq
# chisq
# chisq
# chisq
# chisq
# chisq
# chisq
# chisq
# chisq
# chisq

#SRS
chisq.test(FQ.names.df.2$future_user_not_current, FQ.names.df.2$visited_by_health_worker, correct = FALSE)

# Complex
svychisq(~future_user_not_current + visited_by_health_worker, des, statistic = "Chisq", na.rm = TRUE)





# Other stuff ----
#need to recode 
#need to recode 
#need to recode 
#need to recode 
#need to recode 
#need to recode 
#need to recode 
#need to recode 
#need to recode 
#need to recode 

"more_children", "more_children_pregnant", "wait_birth", "wait_birth_pregnant","heard_female_sterilization", "heard_male_sterilization", "heard_implants", 
"heard_IUD", "heard_injectables", "heard_pill", "heard_emergency", 
"heard_male_condoms", "heard_female_condoms", "heard_diaphragm", "heard_foamjelly", "heard_beads", "heard_LAM", "heard_rhythm", "heard_withdrawal", 
"heard_other", "why_not_using", "why_not_usingnotmarr", "why_not_usingnosex", "why_not_usingmeno", "why_not_usingsubfec", "why_not_usingnomens", 
"why_not_usingbreastfd", "why_not_usinghsbndaway","why_not_usinguptogod", "why_not_usingrespopp", "why_not_usinghusbopp", "why_not_usingotheropp", 
"why_not_usingrelig", "why_not_usingdkmethod", "why_not_usingdksource", "why_not_usingfearside", "why_not_usinghealth", "why_not_usingaccess", 
"why_not_usingcost", "why_not_usingprfnotavail",  "why_not_usingnomethod", "why_not_usinginconv",  "why_not_usingbodyproc", "why_not_usingother", 
"why_not_usingdontknow",  "why_not_usingnoresponse",  "why_not_usingtime", "why_not_usingintend",  "why_not_usingmil", "fp_ad_radio", "fp_ad_tv", 
"fp_ad_magazine", "fp_ad_image", "fp_ad_where", 
"fp_ad_where_hc", "fp_ad_where_street", "fp_ad_where_phar", "fp_ad_where_bb", "fp_ad_where_tv", "fp_ad_where_leaf",  "fp_ad_where_vest", 
"fp_ad_where_other",  "fp_ad_meaning", "fp_ad_mean_family_planning", "fp_ad_mean_birth_spacing", "fp_ad_mean_reproduc_health",  
"fp_ad_mean_family", "fp_ad_mean_other",  "fp_ad_fam_image", "fp_ad_fam_meaning", "fp_ad_fam_mean_family_planning", "fp_ad_fam_mean_birth_spacing",
"fp_ad_fam_mean_reproduc_health", "fp_ad_fam_mean_family", "fp_ad_fam_mean_other", "last_time_sex_value", "libala_about", "libala_about_comm", 
"libala_about_harmony", "libala_about_fp_methods", "libala_about_antenatal", "libala_about_postpartum", "libala_about_amenorrheic", 
"libala_about_spacing", "libala_about_breast_feeding", "libala_about_other", "libala_fp", "libala_whom", "libala_whom_mp", "libala_whom_spouse", 
"libala_whom_other_relative", "libala_whom_friend", "libala_whom_pharmacy", "libala_whom_outreach_worker", "libala_whom_people_seminar", 
"libala_whom_other", "elengi_about", "elengi_about_comm", "elengi_about_harmony",  "elengi_about_fp_methods",  "elengi_about_antenatal", 
"elengi_about_postpartum", "elengi_about_amenorrheic", "elengi_about_spacing", "elengi_about_breast_feeding", "elengi_about_other", 
"sterilization_permanent_inform"

#summing rows
#summing rows
#summing rows
#summing rows
#summing rows
#summing rows
#summing rows
#summing rows
#summing rows
#summing rows

#summing rows (for fp_ad_where, or heard_about, etc)
fq_row_sum <- apply(FQ.variables, 1, function(x) {sum(x)})

# Creating frequency and prop tables
# Creating frequency and prop tables
# Creating frequency and prop tables
# Creating frequency and prop tables
# Creating frequency and prop tables
# Creating frequency and prop tables
# Creating frequency and prop tables
# Creating frequency and prop tables
# Creating frequency and prop tables

FQ.names.freq <- apply(FQ.names.df.2, 2, function(x) {summary(factor(x))}) #create a new dataset, apply to data columns, the function: summary of rows
FQ.names.prop <- lapply(FQ.names.freq, prop.table) #return a list or proportions from newdat.freq
FQ.names.prop <- lapply(FQ.names.prop, round, 2) #rounding the prop.table to 2 digits
FQ.names.cbind <- Map(cbind, FQ.names.freq, FQ.names.prop) #cbind the frequency table and prop.table into one, for each individual variable
FQ.names.cbind













# Why doesn't this work?
# Why doesn't this work?
# Why doesn't this work?
# Why doesn't this work?
# Why doesn't this work?
# Why doesn't this work?
# Why doesn't this work?
# Why doesn't this work?
# Why doesn't this work?
# Why doesn't this work?

#function to summarize the variables in the data (from R for Publc Health)
summarize.vars<-function(data){
  
  #use dummies package to turn all factors into dummies
  require(dummies, quietly=TRUE)
  dat.d<-dummy.data.frame(data, dummy.class="factor")
  
  #use apply to calculate statistics for each variable
  mat<-t(apply(dat.d, 2, function(x) c(length(x), 
                                       count(x, na.rm=TRUE),
                                       length(x)-length(x[!is.na(x)]))))
  
  #assign column names and rownames to output table
  colnames(mat)<-c("N","Min","Num Missing")
  rownames(mat)<-colnames(dat.d)
  return(mat)
}

summarize.vars(FQ.short.nonuser.2)
