#Working R codes 
#Load all packages
library(pacman)
p_load(lme4, gamm4, mgcv, dplyr, tidyr, purrr, forcats, ggplot2, irr, 
       kableExtra, tidyverse, tableone, gtsummary, Hmisc, haven, survey, jtools, remotes,svrepmisc,car,lmtest)

#import csv data
setwd("replace with the folder wherer the csv data is saved")
rui = read.csv("rui.csv")
names(rui)

#Check pain outcomes at follow-up
table(rui$pain_last_month_y2,useNA = "always")
table(rui$pain_last_month_y3,useNA = "always")

table(rui$painscale_y2,useNA = "always")
table(rui$painscale_y3,useNA = "always")

table(rui$pain_limit_y2,useNA = "always")
table(rui$pain_limit_y3,useNA = "always")

table(rui$pain_region_y2,useNA = "always")
table(rui$pain_region_y3,useNA = "always")

#Create pain presence at year 2 and year 3 ("yes" answer to the question about past month pain)
rui$pain_y2 = NA
rui$pain_y2 [rui$pain_last_month_y2==1]= 1
rui$pain_y2 [rui$pain_last_month_y2==0]= 0
rui$pain_y3 = NA
rui$pain_y3 [rui$pain_last_month_y3==1]= 1
rui$pain_y3 [rui$pain_last_month_y3==0]= 0
table(rui$pain_y2, exclude = NULL)
table(rui$pain_y3, exclude = NULL)

#Create moderate to severe pain for year 2 and year 3 (average pain >=4)
rui$pain_intense_y2 = ifelse(!is.na(rui$painscale_y2) & rui$painscale_y2>3, 1, ifelse(rui$pain_last_month_y2 %in% c("0","1"), 0, NA))
table(rui$pain_intense_y2, exclude = NULL)
prop.table(table(rui$pain_intense_y2))

rui$pain_intense_y3 = ifelse(!is.na(rui$painscale_y3) & rui$painscale_y3>3, 1, ifelse(rui$pain_last_month_y3 %in% c("0","1"), 0, NA))
table(rui$pain_intense_y3, exclude = NULL)
prop.table(table(rui$pain_intense_y3))

table(rui$pain_intense_y2, rui$pain_intense_y3)

#Create multi-region pain for year 2 and year 3 using 2 definitions (>1)
#Multi-region pain defined as 2 or more regions
rui$multi_region_pain_y2 [rui$pain_last_month_y2 == 0] = 0
rui$multi_region_pain_y3 [rui$pain_last_month_y3 == 0] = 0
table(rui$multi_region_pain_y2, exclude = NULL)
table(rui$multi_region_pain_y3, exclude = NULL)
prop.table(table(rui$multi_region_pain_y2))
prop.table(table(rui$multi_region_pain_y3))
table(rui$multi_region_pain_y2, rui$multi_region_pain_y3)


#Distribution of chronotype and weekly sleep duration 
summary(rui$sdweek_y2)
hist(rui$sdweek_y2)
sd(rui$sdweek_y2, na.rm = T)
mean(rui$sdweek_y2, na.rm = T) - 3*sd(rui$sdweek_y2, na.rm = T)
mean(rui$sdweek_y2, na.rm = T) + 3*sd(rui$sdweek_y2, na.rm = T)

summary(rui$chrono_y2)
hist(rui$chrono_y2)
sd(rui$chrono_y2, na.rm = T)
percentiles <- quantile(rui$chrono_y2, c(0, 0.01,0.05,0.25,0.5,0.75,0.95,0.99,1), na.rm = T)
print(percentiles)
mean(rui$chrono_y2, na.rm = T) - 3*sd(rui$chrono_y2, na.rm = T)
mean(rui$chrono_y2, na.rm = T) + 3*sd(rui$chrono_y2, na.rm = T)


#Determine the study sample
#Limit the analytic sample to participants with 
  #1) weekly sleep duration greater than 4 hours and less than 14 hours (between 3 SDs around the mean), and 
  #2) chronotype between 0 and 11 (3 SDs) around the mean, and 
  #3) not reporting pain at baseline


#Total number of participants at year 2
names(rui)
#11220 at year 1 (94.5% of the baseline sample)
rui$year1 = 0
rui$year1 [!is.na(rui$site_id_l_y1)] = 1
table(rui$year1,exclude = NULL)
prop.table(table(rui$year1))
#10973 at year 2 (92.5% of the baseline sample)
rui$year2 = 0
rui$year2 [!is.na(rui$site_id_l_y2)] = 1
table(rui$year2,exclude = NULL)
prop.table(table(rui$year2))
table(rui$year1,rui$year2)
#10336 at year 3 (87.1% of the baseline sample)
rui$year3 = 0
rui$year3 [!is.na(rui$site_id_l_y3)] = 1
table(rui$year3,exclude = NULL)
prop.table(table(rui$year3))
table(rui$year2,rui$year3)


#Construct the study sample: 9304/10973 (84.8%) of the year 2 sample had "legitimate" sleep and chronotype data
table(rui$year2, exclude = NULL)
rui$chrono_include_y2 = ifelse(rui$year2 == 0, NA, 
                               ifelse(rui$year2 == 1 & 
                                        (is.na(rui$sdweek_y2) | is.na(rui$chrono_y2)), 
                                      0,
                                      ifelse(rui$year2 == 1 & 
                                               rui$sdweek_y2 >= 4 & rui$sdweek_y2 < 14 & 
                                               rui$chrono_y2 >= 0 & rui$chrono_y2 < 11, 
                                             1, 0)))
table(rui$chrono_include_y2, exclude = NULL)
prop.table(table(rui$chrono_include_y2))
summary(rui$chrono_y2)
rui$chrono_y2_yes = ifelse(!is.na(rui$chrono_y2), 1,0)
table(rui$chrono_y2_yes, exclude = NULL)
table(rui[rui$chrono_y2_yes==1,]$chrono_include_y2, exclude = NULL)
prop.table(table(rui[rui$chrono_y2_yes==1,]$chrono_include_y2, exclude = NULL))

#year2 pain presence for participants with valid chronotype data
chrono = subset(rui, rui$chrono_include_y2==1)
#of 9304 participants, 24 refused to answer whether they had pain, 4 did not complete the pain questionnaire
#5991 did not had pain, 3285 reported pain in the past month
table(chrono$pain_last_month_y2, exclude = NULL) 

#reformat categorical variables
rui$rel_family_id_y0 = factor(rui$rel_family_id_y0)
rui$race_ethnicity = factor(rui$race_ethnicity)
rui$income_4level = factor(rui$income_4level)
rui$employ = factor(rui$employ)
rui$region = factor(rui$region)

#Create the analytic database - dat1 - including youth without pain at year2 (n = 5991)
dat1 = subset(rui, rui$chrono_include_y2 == 1 & rui$pain_last_month_y2 == 0)

#Summarize descriptive statistics for the entire sample with chronotype data available, accounting for sampling design (N = 6049)
nhc <- svydesign(id=~site_id_l_y2, weights=~acs_raked_propensity_score, nest=TRUE, survey.lonely.psu = "adjust", data=dat1)
#Descriptive statistics with continuous variables
#Age
summary(dat1$age_y_y2)
svymean(~age_y_y2, nhc, deff = TRUE)
svysd(~age_y_y2,design = nhc, na = TRUE)
#Family size
svyquantile(~familysize, design = nhc, na = TRUE, c(.25,.5,.75),ci=TRUE)
#Weekday sleep duration
summary(dat1$sdw_y2)
svymean(~sdw_y2, nhc, deff = TRUE)
svysd(~sdw_y2,design = nhc, na = TRUE)
#Free day sleep duration
summary(dat1$sdf_y2)
svymean(~sdf_y2, nhc, deff = TRUE)
svysd(~sdf_y2,design = nhc, na = TRUE)

#Descriptive statistics for categorical variables
table(dat1$female, exclude = NULL)
prop.table(svytable(~female, design = nhc, exclude = NULL))

table(dat1$gender, exclude = NULL)
prop.table(svytable(~gender, design = nhc, exclude = NULL))

table(dat1$puberty_3cat, exclude = NULL)
prop.table(svytable(~puberty_3cat, design = nhc, exclude = NULL))

table(dat1$race_ethnicity, exclude = NULL)
prop.table(svytable(~race_ethnicity, design = nhc, exclude = NULL))

table(dat1$income_4level, exclude = NULL)
prop.table(svytable(~income_4level, design = nhc, exclude = NULL))

table(dat1$marital, exclude = NULL)
prop.table(svytable(~marital, design = nhc, exclude = NULL))

table(dat1$employ, exclude = NULL)
prop.table(svytable(~employ, design = nhc, exclude = NULL))

table(dat1$region, exclude = NULL)
prop.table(svytable(~region, design = nhc, exclude = NULL))

table(dat1$covid, exclude = NULL)
prop.table(svytable(~covid, design = nhc, exclude = NULL))

table(dat1$depression_y2, exclude = NULL)
prop.table(svytable(~depression_y2, design = nhc, exclude = NULL))

table(dat1$anxiety_y2, exclude = NULL)
prop.table(svytable(~anxiety_y2, design = nhc, exclude = NULL))

table(dat1$insomnia_symptom_y2, exclude = NULL)
prop.table(svytable(~insomnia_symptom_y2, design = nhc, exclude = NULL))

#Chronotype values by demographic and developmental characteristics, accounting for sampling design effects
#Across the sample 
svymean(~chrono_y2, nhc, deff = TRUE)
svysd(~chrono_y2,design = nhc, na = TRUE)
svymean(~chrono_y3, nhc, deff = TRUE, na = TRUE)
svysd(~chrono_y3,design = nhc, na = TRUE)

#By sex
svymean(~chrono_y2,design=subset(nhc,female == 0))
svysd(~chrono_y2,design=subset(nhc,female == 0))
svymean(~chrono_y2,design=subset(nhc,female == 1))
svysd(~chrono_y2,design=subset(nhc,female == 1))
svyttest(chrono_y2~female, nhc)
#By gender
svymean(~chrono_y2,design=subset(nhc,gender == 1))
svysd(~chrono_y2,design=subset(nhc,gender == 1))
svymean(~chrono_y2,design=subset(nhc,gender == 2))
svysd(~chrono_y2,design=subset(nhc,gender == 2))
svymean(~chrono_y2,design=subset(nhc,gender == 3))
svysd(~chrono_y2,design=subset(nhc,gender == 3))
model <- svyglm(chrono_y2 ~ factor(gender), design = nhc)
wald_result <- waldtest(model)
print(wald_result)
#By pubertal stage
svymean(~chrono_y2,design=subset(nhc,puberty_3cat == 1))
svysd(~chrono_y2,design=subset(nhc,puberty_3cat == 1))
svymean(~chrono_y2,design=subset(nhc,puberty_3cat == 2))
svysd(~chrono_y2,design=subset(nhc,puberty_3cat == 2))
svymean(~chrono_y2,design=subset(nhc,puberty_3cat == 3))
svysd(~chrono_y2,design=subset(nhc,puberty_3cat == 3))
model <- svyglm(chrono_y2 ~ factor(puberty_3cat), design = nhc)
wald_result <- waldtest(model)
print(wald_result)
#By race and ethnicity
svymean(~chrono_y2,design=subset(nhc,race_ethnicity == 1))
svysd(~chrono_y2,design=subset(nhc,race_ethnicity == 1))
svymean(~chrono_y2,design=subset(nhc,race_ethnicity == 2))
svysd(~chrono_y2,design=subset(nhc,race_ethnicity == 2))
svymean(~chrono_y2,design=subset(nhc,race_ethnicity == 3))
svysd(~chrono_y2,design=subset(nhc,race_ethnicity == 3))
svymean(~chrono_y2,design=subset(nhc,race_ethnicity == 4))
svysd(~chrono_y2,design=subset(nhc,race_ethnicity == 4))
svymean(~chrono_y2,design=subset(nhc,race_ethnicity == 5))
svysd(~chrono_y2,design=subset(nhc,race_ethnicity == 5))
model <- svyglm(chrono_y2 ~ factor(race_ethnicity), design = nhc)
wald_result <- waldtest(model)
print(wald_result)
#By family income
svymean(~chrono_y2,design=subset(nhc,income_4level == 1))
svysd(~chrono_y2,design=subset(nhc,income_4level == 1))
svymean(~chrono_y2,design=subset(nhc,income_4level == 2))
svysd(~chrono_y2,design=subset(nhc,income_4level == 2))
svymean(~chrono_y2,design=subset(nhc,income_4level == 3))
svysd(~chrono_y2,design=subset(nhc,income_4level == 3))
svymean(~chrono_y2,design=subset(nhc,income_4level == 4))
svysd(~chrono_y2,design=subset(nhc,income_4level == 4))
model <- svyglm(chrono_y2 ~ factor(income_4level), design = nhc)
wald_result <- waldtest(model)
print(wald_result)
#By marital status
svymean(~chrono_y2,design=subset(nhc,marital == 1))
svysd(~chrono_y2,design=subset(nhc,marital == 1))
svymean(~chrono_y2,design=subset(nhc,marital == 0))
svysd(~chrono_y2,design=subset(nhc,marital == 0))
summary(svyglm(chrono_y2 ~ marital, design = nhc))
#By parent employment status
svymean(~chrono_y2,design=subset(nhc,employ == 1))
svysd(~chrono_y2,design=subset(nhc,employ == 1))
svymean(~chrono_y2,design=subset(nhc,employ == 2))
svysd(~chrono_y2,design=subset(nhc,employ == 2))
svymean(~chrono_y2,design=subset(nhc,employ == 3))
svysd(~chrono_y2,design=subset(nhc,employ == 3))
model <- svyglm(chrono_y2 ~ factor(employ), design = nhc)
wald_result <- waldtest(model)
print(wald_result)
#By census region
svymean(~chrono_y2,design=subset(nhc,region == 1))
svysd(~chrono_y2,design=subset(nhc,region == 1))
svymean(~chrono_y2,design=subset(nhc,region == 2))
svysd(~chrono_y2,design=subset(nhc,region == 2))
svymean(~chrono_y2,design=subset(nhc,region == 3))
svysd(~chrono_y2,design=subset(nhc,region == 3))
svymean(~chrono_y2,design=subset(nhc,region == 4))
svysd(~chrono_y2,design=subset(nhc,region == 4))
model <- svyglm(chrono_y2 ~ factor(region), design = nhc)
wald_result <- waldtest(model)
print(wald_result)
#By covid
svymean(~chrono_y2,design=subset(nhc,covid == 0))
svysd(~chrono_y2,design=subset(nhc,covid == 0))
svymean(~chrono_y2,design=subset(nhc,covid == 1))
svysd(~chrono_y2,design=subset(nhc,covid == 1))
svyttest(chrono_y2~covid, nhc)

#Social jetlag by demographic and developmental characteristics
#Across the sample 
svyquantile(~sjl_abs_y2, design = nhc, na = TRUE, c(.25,.5,.75),ci=FALSE)
svyquantile(~sjl_abs_y3, design = nhc, na = TRUE, c(.25,.5,.75),ci=FALSE)
#By sex
svyquantile(~sjl_abs_y2,design=subset(nhc,female == 0), na = TRUE, c(.25,.5,.75),ci=F)
svyquantile(~sjl_abs_y2,design=subset(nhc,female == 1), na = TRUE, c(.25,.5,.75),ci=F)
svyranktest(sjl_abs_y2~female, design = nhc, na = TRUE, test = c("wilcoxon"))
#By gender
svyquantile(~sjl_abs_y2,design=subset(nhc,gender == 1), na = TRUE, c(.25,.5,.75),ci=F)
svyquantile(~sjl_abs_y2,design=subset(nhc,gender == 2), na = TRUE, c(.25,.5,.75),ci=F)
svyquantile(~sjl_abs_y2,design=subset(nhc,gender == 3), na = TRUE, c(.25,.5,.75),ci=F)
svyranktest(sjl_abs_y2~gender, design = nhc, na = TRUE, test=("KruskalWallis"))
#By pubertal stage
svyquantile(~sjl_abs_y2,design=subset(nhc,puberty_3cat == 1), na = TRUE, c(.25,.5,.75),ci=F)
svyquantile(~sjl_abs_y2,design=subset(nhc,puberty_3cat == 2), na = TRUE, c(.25,.5,.75),ci=F)
svyquantile(~sjl_abs_y2,design=subset(nhc,puberty_3cat == 3), na = TRUE, c(.25,.5,.75),ci=F)
svyranktest(sjl_abs_y2~puberty_3cat, design = nhc, na = TRUE, test=("KruskalWallis"))
#By race and ethnicity
svyquantile(~sjl_abs_y2,design=subset(nhc,race_ethnicity == 1), na = TRUE, c(.25,.5,.75),ci=F)
svyquantile(~sjl_abs_y2,design=subset(nhc,race_ethnicity == 2), na = TRUE, c(.25,.5,.75),ci=F)
svyquantile(~sjl_abs_y2,design=subset(nhc,race_ethnicity == 3), na = TRUE, c(.25,.5,.75),ci=F)
svyquantile(~sjl_abs_y2,design=subset(nhc,race_ethnicity == 4), na = TRUE, c(.25,.5,.75),ci=F)
svyquantile(~sjl_abs_y2,design=subset(nhc,race_ethnicity == 5), na = TRUE, c(.25,.5,.75),ci=F)
svyranktest(sjl_abs_y2~race_ethnicity, design = nhc, na = TRUE, test=("KruskalWallis"))
#By family income
svyquantile(~sjl_abs_y2,design=subset(nhc,income_4level == 1), na = TRUE, c(.25,.5,.75),ci=F)
svyquantile(~sjl_abs_y2,design=subset(nhc,income_4level == 2), na = TRUE, c(.25,.5,.75),ci=F)
svyquantile(~sjl_abs_y2,design=subset(nhc,income_4level == 3), na = TRUE, c(.25,.5,.75),ci=F)
svyquantile(~sjl_abs_y2,design=subset(nhc,income_4level == 4), na = TRUE, c(.25,.5,.75),ci=F)
svyranktest(sjl_abs_y2~income_4level, design = nhc, na = TRUE, test=("KruskalWallis"))
#By marital status
svyquantile(~sjl_abs_y2,design=subset(nhc,marital == 1), na = TRUE, c(.25,.5,.75),ci=F)
svyquantile(~sjl_abs_y2,design=subset(nhc,marital == 0), na = TRUE, c(.25,.5,.75),ci=F)
svyranktest(sjl_abs_y2~marital, design = nhc, na = TRUE, test = c("wilcoxon"))
#By parent employment status
svyquantile(~sjl_abs_y2,design=subset(nhc,employ == 1), na = TRUE, c(.25,.5,.75),ci=F)
svyquantile(~sjl_abs_y2,design=subset(nhc,employ == 2), na = TRUE, c(.25,.5,.75),ci=F)
svyquantile(~sjl_abs_y2,design=subset(nhc,employ == 3), na = TRUE, c(.25,.5,.75),ci=F)
svyranktest(sjl_abs_y2~employ, design = nhc, na = TRUE, test=("KruskalWallis"))
#By census region
svyquantile(~sjl_abs_y2,design=subset(nhc,region == 1), na = TRUE, c(.25,.5,.75),ci=F)
svyquantile(~sjl_abs_y2,design=subset(nhc,region == 2), na = TRUE, c(.25,.5,.75),ci=F)
svyquantile(~sjl_abs_y2,design=subset(nhc,region == 3), na = TRUE, c(.25,.5,.75),ci=F)
svyquantile(~sjl_abs_y2,design=subset(nhc,region == 4), na = TRUE, c(.25,.5,.75),ci=F)
svyranktest(sjl_abs_y2~region, design = nhc, na = TRUE, test=("KruskalWallis"))
#By covid
svyquantile(~sjl_abs_y2,design=subset(nhc,covid == 0), na = TRUE, c(.25,.5,.75),ci=F)
svyquantile(~sjl_abs_y2,design=subset(nhc,covid == 1), na = TRUE, c(.25,.5,.75),ci=F)
svyranktest(sjl_abs_y2~covid, design = nhc, na = TRUE, test = c("wilcoxon"))


#Baseline characteristics by pain incidence - Account for sampling design effect
#Overall incidence
prop.table(svytable(~pain_y3, design = nhc))
#By sex
prop.table(svytable(~female+pain_y3, design = nhc), margin = 1)
svychisq(~female + pain_y3, design = nhc)
#By gender
prop.table(svytable(~gender+pain_y3, design = nhc), margin = 1)
svychisq(~gender + pain_y3, design = nhc)
#By pubertal stage
prop.table(svytable(~puberty_3cat+pain_y3, design = nhc), margin = 1)
svychisq(~puberty_3cat + pain_y3, design = nhc)
#By race and ethnicity
prop.table(svytable(~race_ethnicity+pain_y3, design = nhc), margin = 1)
svychisq(~race_ethnicity + pain_y3, design = nhc)
#By family income
prop.table(svytable(~income_4level+pain_y3, design = nhc), margin = 1)
svychisq(~income_4level + pain_y3, design = nhc)
#By marital status
prop.table(svytable(~marital+pain_y3, design = nhc), margin = 1)
svychisq(~marital+pain_y3, design = nhc)
#By employment status
prop.table(svytable(~employ+pain_y3, design = nhc), margin = 1)
svychisq(~employ+pain_y3, design = nhc)
#By Census region
prop.table(svytable(~region+pain_y3, design = nhc), margin = 1)
svychisq(~region+pain_y3, design = nhc)
#By covid
prop.table(svytable(~covid+pain_y3, design = nhc), margin = 1)
svychisq(~covid+pain_y3, design = nhc)


#Baseline characteristics by moderate to severe pain incidence - Account for sampling design effect
#Overall incidence
prop.table(svytable(~pain_intense_y3, design = nhc))
#By sex
prop.table(svytable(~female+pain_intense_y3, design = nhc), margin = 1)
svychisq(~female + pain_intense_y3, design = nhc)
#By gender
prop.table(svytable(~gender+pain_intense_y3, design = nhc), margin = 1)
svychisq(~gender + pain_intense_y3, design = nhc)
#By pubertal stage
prop.table(svytable(~puberty_3cat+pain_intense_y3, design = nhc), margin = 1)
svychisq(~puberty_3cat + pain_intense_y3, design = nhc)
#By race and ethnicity
prop.table(svytable(~race_ethnicity+pain_intense_y3, design = nhc), margin = 1)
svychisq(~race_ethnicity + pain_intense_y3, design = nhc)
#By family income
prop.table(svytable(~income_4level+pain_intense_y3, design = nhc), margin = 1)
svychisq(~income_4level + pain_intense_y3, design = nhc)
#By marital status
prop.table(svytable(~marital+pain_intense_y3, design = nhc), margin = 1)
svychisq(~marital+pain_intense_y3, design = nhc)
#By employment status
prop.table(svytable(~employ+pain_intense_y3, design = nhc), margin = 1)
svychisq(~employ+pain_intense_y3, design = nhc)
#By Census region
prop.table(svytable(~region+pain_intense_y3, design = nhc), margin = 1)
svychisq(~region+pain_intense_y3, design = nhc)
#By covid
prop.table(svytable(~covid+pain_intense_y3, design = nhc), margin = 1)
svychisq(~covid+pain_intense_y3, design = nhc)

#Baseline characteristics by multi-region pain - Account for sampling design effect
#Overall incidence
prop.table(svytable(~multi_region_pain_y3, design = nhc))
#By sex
prop.table(svytable(~female+multi_region_pain_y3, design = nhc), margin = 1)
svychisq(~female + multi_region_pain_y3, design = nhc)
#By gender
prop.table(svytable(~gender+multi_region_pain_y3, design = nhc), margin = 1)
svychisq(~gender + multi_region_pain_y3, design = nhc)
#By pubertal stage
prop.table(svytable(~puberty_3cat+multi_region_pain_y3, design = nhc), margin = 1)
svychisq(~puberty_3cat + multi_region_pain_y3, design = nhc)
#By race and ethnicity
prop.table(svytable(~race_ethnicity+multi_region_pain_y3, design = nhc), margin = 1)
svychisq(~race_ethnicity + multi_region_pain_y3, design = nhc)
#By family income
prop.table(svytable(~income_4level+multi_region_pain_y3, design = nhc), margin = 1)
svychisq(~income_4level + multi_region_pain_y3, design = nhc)
#By marital status
prop.table(svytable(~marital+multi_region_pain_y3, design = nhc), margin = 1)
svychisq(~marital+multi_region_pain_y3, design = nhc)
#By employment status
prop.table(svytable(~employ+multi_region_pain_y3, design = nhc), margin = 1)
svychisq(~employ+multi_region_pain_y3, design = nhc)
#By Census region
prop.table(svytable(~region+multi_region_pain_y3, design = nhc), margin = 1)
svychisq(~region+multi_region_pain_y3, design = nhc)
#By covid
prop.table(svytable(~covid+multi_region_pain_y3, design = nhc), margin = 1)
svychisq(~covid+multi_region_pain_y3, design = nhc)


#Distribution of chronotype at year 2 and year3
summary(dat1$chrono_y2)
quantile(dat1$chrono_y2, probs = c(0, 0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99, 1))
hist(dat1$chrono_y2)

summary(dat1$chrono_y3)
quantile(dat1$chrono_y3, probs = c(0, 0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99, 1),na.rm = TRUE)
hist(dat1$chrono_y3)

#Correspondence between chronotype at year 2 and year 3 - ICC(2,1)
icc_result <- icc(dat1[, c("chrono_y2", "chrono_y3")], model = "twoway", type = "consistency", unit = "single")
print(icc_result)

#Correspondence between social jetlag at year 2 and year 3 - ICC(2,1)
summary(dat1$sjl_abs_y2)
dat1$sjl_abs_y2_gt1 = NA
dat1$sjl_abs_y2_gt1 [dat1$sjl_abs_y2>1]= 1
dat1$sjl_abs_y2_gt1 [dat1$sjl_abs_y2 <=1]= 0
table(dat1$sjl_abs_y2_gt1, exclude = NULL)
prop.table(table(dat1$sjl_abs_y2_gt1, exclude = NULL))

dat1$sjl_abs_y2_log = log(dat1$sjl_abs_y2+0.1)
dat1$sjl_abs_y3_log = log(dat1$sjl_abs_y3+0.1)
icc_result <- icc(dat1[, c("sjl_abs_y2_log", "sjl_abs_y3_log")], model = "twoway", type = "consistency", unit = "single")
print(icc_result)


##################################################################
#Main analyses
#Modeling approach: 3-level random intercept logistic regression model adjusting for age, sex, race and ethnicity, family income, family size, family type, 
#employment, region, pds score, and potential impact of COVID-19 school disclosure 

#1. Chronotype and pain incidence 
#Main model: examine non-linear relationship - almost linear
mod1 = gam(pain_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
             employ + region + pds_mean + covid + sdw_y2 + insomnia_symptom_y2 + s(chrono_y2),
           random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", data = dat1)
summary(mod1) #edf = 1.001
new_data <- expand.grid(
  chrono_y2 = seq(0, 12, length.out = 100),
  age_y_y2 = mean(dat1$age_y_y2, na.rm = TRUE),
  female = 0,
  race_ethnicity = 1,
  income_4level = 3,
  familysize = 4,
  marital = 1,
  employ = 1,
  region = 4,
  covid = 0,
  pds_mean = mean(dat1$pds_mean, na.rm = TRUE),
  sdw_y2 = mean(dat1$sdw_y2, na.rm = TRUE),
  insomnia_symptom_y2 = 0
)
new_data$predicted <- predict(mod1, newdata = new_data, type="response")
knitr::opts_chunk$set(fig.width=12, fig.height=8) 
ggplot(new_data, aes(x = chrono_y2, y = predicted)) + 
  geom_line() + 
  labs(title = "Probability of developing pain at 1-year follow-up",
       x = "Chronotype",
       y = "Predicted probability of pain") +
  scale_x_continuous(breaks = seq(0, 12, 2), limits = c(0, 12)) + 
  scale_y_continuous(breaks = seq(0, 0.30, 0.05), limits = c(0, 0.30)) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14, hjust = 0.5), 
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

mod1 <- gam(pain_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
              employ + region + covid + pds_mean + sdw_y2 + insomnia_symptom_y2 + chrono_y2,
            random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", data = dat1)
summary(mod1)
summary1 = summary(mod1)
coef1 <- summary1$p.coeff
beta = round(coef1, 4)
se = round(summary1$se, 4)
p_value = round(summary1$p.pv, 4)
odds_ratios <- round(exp(coef1),2)
lower_ci <- round(exp(coef1 - 1.96 * summary(mod1)$se),2)
upper_ci <- round(exp(coef1 + 1.96 * summary(mod1)$se),2)
p_table <- data.frame(beta, se, odds_ratios,lower_ci, upper_ci, p_value)
p_table

#Interaction by sex: not sig
library(mgcv)
library(multcomp)
mod1 <- gam(pain_y3 ~ age_y_y2 + race_ethnicity + income_4level + familysize + marital + employ + 
              region + covid + pds_mean + sdw_y2 + insomnia_symptom_y2 + chrono_y2*female,
            random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", data = dat1)
summary(mod1)

# Extract coefficients and variance-covariance matrix
coefs <- coef(mod1)
vcov_matrix <- vcov(mod1)

# Function to calculate ORs and CIs for group-specific effects
get_or_ci_pvalue <- function(female_level) {
  # Adjusting for female = 0 or female = 1
  combined_effect <- coefs["chrono_y2"] + female_level * coefs["chrono_y2:female"]
  combined_var <- vcov_matrix["chrono_y2", "chrono_y2"] + 
    (female_level^2 * vcov_matrix["chrono_y2:female", "chrono_y2:female"]) + 
    (2 * female_level * vcov_matrix["chrono_y2", "chrono_y2:female"])
  
  lower_bound <- combined_effect - 1.96 * sqrt(combined_var)
  upper_bound <- combined_effect + 1.96 * sqrt(combined_var)
  
  or <- round(exp(combined_effect), 2)
  ci <- round(exp(c(lower_bound, upper_bound)), 2)
  
  # Wald test for p-value
  z_value <- combined_effect / sqrt(combined_var)
  p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)
  
  return(list(or=or, ci=ci, p_value=p_value))
}

# Calculate ORs, CIs, and P-values for female = 0 and female = 1
results <- list("female = 0" = get_or_ci_pvalue(0), "female = 1" = get_or_ci_pvalue(1))
results


#Interaction by race and ethnicity (white, black, hispanic): not sig.
mod1 = gam(pain_y3 ~ age_y_y2 + female + income_4level + familysize + marital + employ + 
             region + pds_mean + covid + sdw_y2 + insomnia_symptom_y2 + chrono_y2*race_ethnicity,
           random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", 
           data = dat1[dat1$race_ethnicity %in% 1:3,])
summary(mod1)

#Derive overall interaction effect
mod_reduced = gam(pain_y3 ~ age_y_y2 + female + income_4level + familysize + marital + employ + 
                    region + pds_mean + covid + sdw_y2 + insomnia_symptom_y2 + chrono_y2 + race_ethnicity,
                  random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", 
                  data = dat1[dat1$race_ethnicity %in% 1:3,])

# Compare the models
anova(mod_reduced, mod1, test="Chisq")

#Derive group-specific effects 
coefs <- coef(mod1)
vcov_matrix <- vcov(mod1)

# Function to calculate ORs and CIs for group-specific effects
get_or_ci_pvalue <- function(race_ethnicity_level) {
  base_effect <- coefs["chrono_y2"]
  
  # Update these lines based on the actual names of the coefficients
  interaction_term_name <- paste("chrono_y2:race_ethnicity", race_ethnicity_level, sep="")
  interaction_effect <- ifelse(interaction_term_name %in% names(coefs), coefs[interaction_term_name], 0)
  
  combined_effect <- base_effect + interaction_effect
  
  base_var <- vcov_matrix["chrono_y2", "chrono_y2"]
  interaction_var <- ifelse(interaction_term_name %in% names(coefs), 
                            vcov_matrix[interaction_term_name, interaction_term_name], 
                            0)
  covar <- ifelse(interaction_term_name %in% names(coefs), 
                  vcov_matrix["chrono_y2", interaction_term_name], 
                  0)
  
  combined_var <- base_var + interaction_var + 2 * covar
  
  lower_bound <- combined_effect - 1.96 * sqrt(combined_var)
  upper_bound <- combined_effect + 1.96 * sqrt(combined_var)
  
  or <- round(exp(combined_effect), 2)
  ci <- round(exp(c(lower_bound, upper_bound)), 2)
  
  z_value <- combined_effect / sqrt(combined_var)
  p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)
  
  return(list(or=or, ci=ci, p_value=p_value))
}

# Calculate ORs, CIs, and P-values for each race_ethnicity level
results <- lapply(1:3, get_or_ci_pvalue)
names(results) <- paste("race_ethnicity =", 1:3)
results


#Interaction by pubertal stage: not sig. 
dat1$puberty_3cat = factor(dat1$puberty_3cat)
mod1 = gam(pain_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
             employ + region + covid + sdw_y2 + insomnia_symptom_y2 + chrono_y2*puberty_3cat,
           random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", data = dat1)
summary(mod1)

mod_reduced = gam(pain_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
                    employ + region + covid + sdw_y2 + insomnia_symptom_y2 + chrono_y2 + puberty_3cat,
                  random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", data = dat1)
anova(mod_reduced, mod1, test="Chisq")

#Derive group-specific effects 
coefs <- coef(mod1)
vcov_matrix <- vcov(mod1)

# Function to calculate ORs and CIs for group-specific effects
get_or_ci_pvalue <- function(puberty_3cat_level) {
  base_effect <- coefs["chrono_y2"]
  
  # Update these lines based on the actual names of the coefficients
  interaction_term_name <- paste("chrono_y2:puberty_3cat", puberty_3cat_level, sep="")
  interaction_effect <- ifelse(interaction_term_name %in% names(coefs), coefs[interaction_term_name], 0)
  
  combined_effect <- base_effect + interaction_effect
  
  base_var <- vcov_matrix["chrono_y2", "chrono_y2"]
  interaction_var <- ifelse(interaction_term_name %in% names(coefs), 
                            vcov_matrix[interaction_term_name, interaction_term_name], 
                            0)
  covar <- ifelse(interaction_term_name %in% names(coefs), 
                  vcov_matrix["chrono_y2", interaction_term_name], 
                  0)
  
  combined_var <- base_var + interaction_var + 2 * covar
  
  lower_bound <- combined_effect - 1.96 * sqrt(combined_var)
  upper_bound <- combined_effect + 1.96 * sqrt(combined_var)
  
  or <- round(exp(combined_effect), 2)
  ci <- round(exp(c(lower_bound, upper_bound)), 2)
  
  z_value <- combined_effect / sqrt(combined_var)
  p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)
  
  return(list(or=or, ci=ci, p_value=p_value))
}

# Calculate ORs, CIs, and P-values for each race_ethnicity level
results <- lapply(1:3, get_or_ci_pvalue)
names(results) <- paste("puberty_3cat =", 1:3)
results



#2. Chronotype and moderate to severe pain
#Distribution of pain intensity
hist(dat1$painscale_y3)
summary(dat1$painscale_y3)
sd(dat1$painscale_y3, na.rm=TRUE)

#Main model: examine non-linear relationship - almost linear
mod1 = gam(pain_intense_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
             employ + region + pds_mean + covid + sdw_y2 + insomnia_symptom_y2 + s(chrono_y2),
           random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", data = dat1)
summary(mod1) #edf = 1.036

#Use the continuous chrnotype variable
mod1 <- gam(pain_intense_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + 
              marital + employ + region + covid + pds_mean + sdw_y2 + insomnia_symptom_y2 + chrono_y2,
            random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", data = dat1)
summary(mod1)
summary1 = summary(mod1)
coef1 <- summary1$p.coeff
beta = round(coef1, 4)
se = round(summary1$se, 4)
p_value = round(summary1$p.pv, 4)
odds_ratios <- round(exp(coef1),2)
lower_ci <- round(exp(coef1 - 1.96 * summary(mod1)$se),2)
upper_ci <- round(exp(coef1 + 1.96 * summary(mod1)$se),2)
p_table <- data.frame(beta, se, odds_ratios,lower_ci, upper_ci, p_value)
p_table

#Interaction by sex: not sig
mod1 <- gam(pain_intense_y3 ~ age_y_y2 + race_ethnicity + income_4level + familysize + marital + 
              employ + region + covid + pds_mean + sdw_y2 + insomnia_symptom_y2 + chrono_y2*female,
            random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", data = dat1)
summary(mod1)

coefs <- coef(mod1)
vcov_matrix <- vcov(mod1)

get_or_ci_pvalue <- function(female_level) {
  combined_effect <- coefs["chrono_y2"] + female_level * coefs["chrono_y2:female"]
  combined_var <- vcov_matrix["chrono_y2", "chrono_y2"] + 
    (female_level^2 * vcov_matrix["chrono_y2:female", "chrono_y2:female"]) + 
    (2 * female_level * vcov_matrix["chrono_y2", "chrono_y2:female"])
  
  lower_bound <- combined_effect - 1.96 * sqrt(combined_var)
  upper_bound <- combined_effect + 1.96 * sqrt(combined_var)
  
  or <- round(exp(combined_effect), 2)
  ci <- round(exp(c(lower_bound, upper_bound)), 2)
  
  z_value <- combined_effect / sqrt(combined_var)
  p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)
  
  return(list(or=or, ci=ci, p_value=p_value))
}

list("female = 0" = get_or_ci_pvalue(0), "female = 1" = get_or_ci_pvalue(1))

#Interaction by race and ethnicity (white, black, hispanic): not sig.
mod1 = gam(pain_intense_y3 ~ age_y_y2 + female + income_4level + familysize + marital + employ + 
             region + pds_mean + covid + sdw_y2 + insomnia_symptom_y2 + chrono_y2*race_ethnicity,
           random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", 
           data = dat1[dat1$race_ethnicity %in% 1:3,])
summary(mod1)

mod_reduced = gam(pain_intense_y3 ~ age_y_y2 + female + income_4level + familysize + marital + employ + 
                    region + pds_mean + covid + sdw_y2 + insomnia_symptom_y2 + chrono_y2 + race_ethnicity,
                  random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", 
                  data = dat1[dat1$race_ethnicity %in% 1:3,])

# Compare the models
anova(mod_reduced, mod1, test="Chisq")

coefs <- coef(mod1)
vcov_matrix <- vcov(mod1)

get_or_ci_pvalue <- function(race_ethnicity_level) {
  base_effect <- coefs["chrono_y2"]
  
  interaction_term_name <- paste("chrono_y2:race_ethnicity", race_ethnicity_level, sep="")
  interaction_effect <- ifelse(interaction_term_name %in% names(coefs), coefs[interaction_term_name], 0)
  
  combined_effect <- base_effect + interaction_effect
  
  base_var <- vcov_matrix["chrono_y2", "chrono_y2"]
  interaction_var <- ifelse(interaction_term_name %in% names(coefs), 
                            vcov_matrix[interaction_term_name, interaction_term_name], 
                            0)
  covar <- ifelse(interaction_term_name %in% names(coefs), 
                  vcov_matrix["chrono_y2", interaction_term_name], 
                  0)
  
  combined_var <- base_var + interaction_var + 2 * covar
  
  lower_bound <- combined_effect - 1.96 * sqrt(combined_var)
  upper_bound <- combined_effect + 1.96 * sqrt(combined_var)
  
  or <- round(exp(combined_effect), 2)
  ci <- round(exp(c(lower_bound, upper_bound)), 2)
  
  z_value <- combined_effect / sqrt(combined_var)
  p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)
  
  return(list(or=or, ci=ci, p_value=p_value))
}

results <- lapply(1:3, get_or_ci_pvalue)
names(results) <- paste("race_ethnicity =", 1:3)
results

#Interaction by pubertal stage: not sig. 
mod1 = gam(pain_intense_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
             employ + region + covid + sdw_y2 + insomnia_symptom_y2 + chrono_y2*puberty_3cat,
           random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", data = dat1)
summary(mod1)

mod_reduced = gam(pain_intense_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
                    employ + region + covid + sdw_y2 + insomnia_symptom_y2 + chrono_y2 + puberty_3cat,
                  random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", data = dat1)
anova(mod_reduced, mod1, test="Chisq")

#Derive group-specific effects 
coefs <- coef(mod1)
vcov_matrix <- vcov(mod1)

# Function to calculate ORs and CIs for group-specific effects
get_or_ci_pvalue <- function(puberty_3cat_level) {
  base_effect <- coefs["chrono_y2"]
  
  interaction_term_name <- paste("chrono_y2:puberty_3cat", puberty_3cat_level, sep="")
  interaction_effect <- ifelse(interaction_term_name %in% names(coefs), coefs[interaction_term_name], 0)
  
  combined_effect <- base_effect + interaction_effect
  
  base_var <- vcov_matrix["chrono_y2", "chrono_y2"]
  interaction_var <- ifelse(interaction_term_name %in% names(coefs), 
                            vcov_matrix[interaction_term_name, interaction_term_name], 
                            0)
  covar <- ifelse(interaction_term_name %in% names(coefs), 
                  vcov_matrix["chrono_y2", interaction_term_name], 
                  0)
  
  combined_var <- base_var + interaction_var + 2 * covar
  
  lower_bound <- combined_effect - 1.96 * sqrt(combined_var)
  upper_bound <- combined_effect + 1.96 * sqrt(combined_var)
  
  or <- round(exp(combined_effect), 2)
  ci <- round(exp(c(lower_bound, upper_bound)), 2)
  
  z_value <- combined_effect / sqrt(combined_var)
  p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)
  
  return(list(or=or, ci=ci, p_value=p_value))
}

results <- lapply(1:3, get_or_ci_pvalue)
names(results) <- paste("puberty_3cat =", 1:3)
results



#3. Chronotype and multi-region pain
#Distribution of pain regions
hist(dat1[dat1$pain_last_month_y3==1,]$pain_region_y3)
summary(dat1[dat1$pain_last_month_y3==1,]$pain_region_y3)

#Main model: examine non-linear relationship - almost linear
mod1 = gam(multi_region_pain_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
             employ + region + pds_mean + covid + sdw_y2 + insomnia_symptom_y2 + s(chrono_y2),
           random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", data = dat1)
summary(mod1) #edf = 1.002

#Use the continuous chrnotype variable
mod1 <- gam(multi_region_pain_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
              employ + region + covid + pds_mean + sdw_y2 + insomnia_symptom_y2 + chrono_y2,
            random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", data = dat1)
summary(mod1)
summary1 = summary(mod1)
coef1 <- summary1$p.coeff
beta = round(coef1, 4)
se = round(summary1$se, 4)
p_value = round(summary1$p.pv, 4)
odds_ratios <- round(exp(coef1),2)
lower_ci <- round(exp(coef1 - 1.96 * summary(mod1)$se),2)
upper_ci <- round(exp(coef1 + 1.96 * summary(mod1)$se),2)
p_table <- data.frame(beta, se, odds_ratios,lower_ci, upper_ci, p_value)
p_table

#Interaction by sex: not sig
mod1 <- gam(multi_region_pain_y3 ~ age_y_y2 + race_ethnicity + income_4level + familysize + marital + employ + 
              region + covid + pds_mean + sdw_y2 + insomnia_symptom_y2 + chrono_y2*female,
            random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", data = dat1)
summary(mod1)

coefs <- coef(mod1)
vcov_matrix <- vcov(mod1)

get_or_ci_pvalue <- function(female_level) {
  combined_effect <- coefs["chrono_y2"] + female_level * coefs["chrono_y2:female"]
  combined_var <- vcov_matrix["chrono_y2", "chrono_y2"] + 
    (female_level^2 * vcov_matrix["chrono_y2:female", "chrono_y2:female"]) + 
    (2 * female_level * vcov_matrix["chrono_y2", "chrono_y2:female"])
  
  lower_bound <- combined_effect - 1.96 * sqrt(combined_var)
  upper_bound <- combined_effect + 1.96 * sqrt(combined_var)
  
  or <- round(exp(combined_effect), 2)
  ci <- round(exp(c(lower_bound, upper_bound)), 2)
  
  z_value <- combined_effect / sqrt(combined_var)
  p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)
  
  return(list(or=or, ci=ci, p_value=p_value))
}

list("female = 0" = get_or_ci_pvalue(0), "female = 1" = get_or_ci_pvalue(1))

#Interaction by race and ethnicity (white, black, hispanic): not sig.
mod1 = gam(multi_region_pain_y3 ~ age_y_y2 + female + income_4level + familysize + marital + employ + region + 
             pds_mean + covid + sdw_y2 + insomnia_symptom_y2 + chrono_y2*race_ethnicity,
           random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", 
           data = dat1[dat1$race_ethnicity %in% 1:3,])
summary(mod1)

mod_reduced = gam(multi_region_pain_y3 ~ age_y_y2 + female + income_4level + familysize + marital + employ + 
                    region + pds_mean + covid + sdw_y2 + insomnia_symptom_y2 + chrono_y2 + race_ethnicity,
                  random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", 
                  data = dat1[dat1$race_ethnicity %in% 1:3,])

# Compare the models
anova(mod_reduced, mod1, test="Chisq")

coefs <- coef(mod1)
vcov_matrix <- vcov(mod1)

get_or_ci_pvalue <- function(race_ethnicity_level) {
  base_effect <- coefs["chrono_y2"]
  
  interaction_term_name <- paste("chrono_y2:race_ethnicity", race_ethnicity_level, sep="")
  interaction_effect <- ifelse(interaction_term_name %in% names(coefs), coefs[interaction_term_name], 0)
  
  combined_effect <- base_effect + interaction_effect
  
  base_var <- vcov_matrix["chrono_y2", "chrono_y2"]
  interaction_var <- ifelse(interaction_term_name %in% names(coefs), 
                            vcov_matrix[interaction_term_name, interaction_term_name], 
                            0)
  covar <- ifelse(interaction_term_name %in% names(coefs), 
                  vcov_matrix["chrono_y2", interaction_term_name], 
                  0)
  
  combined_var <- base_var + interaction_var + 2 * covar
  
  lower_bound <- combined_effect - 1.96 * sqrt(combined_var)
  upper_bound <- combined_effect + 1.96 * sqrt(combined_var)
  
  or <- round(exp(combined_effect), 2)
  ci <- round(exp(c(lower_bound, upper_bound)), 2)
  
  z_value <- combined_effect / sqrt(combined_var)
  p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)
  
  return(list(or=or, ci=ci, p_value=p_value))
}

results <- lapply(1:3, get_or_ci_pvalue)
names(results) <- paste("race_ethnicity =", 1:3)
results

#Interaction by pubertal stage: not sig. 
mod1 = gam(multi_region_pain_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
             employ + region + covid + sdw_y2 + insomnia_symptom_y2 + chrono_y2*puberty_3cat,
           random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", data = dat1)
summary(mod1) 

mod_reduced = gam(multi_region_pain_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
                    employ + region + covid + sdw_y2 + insomnia_symptom_y2 + chrono_y2 + puberty_3cat,
                  random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", data = dat1)
anova(mod_reduced, mod1, test="Chisq")

coefs <- coef(mod1)
vcov_matrix <- vcov(mod1)

get_or_ci_pvalue <- function(puberty_3cat_level) {
  base_effect <- coefs["chrono_y2"]
  
  interaction_term_name <- paste("chrono_y2:puberty_3cat", puberty_3cat_level, sep="")
  interaction_effect <- ifelse(interaction_term_name %in% names(coefs), coefs[interaction_term_name], 0)
  
  combined_effect <- base_effect + interaction_effect
  
  base_var <- vcov_matrix["chrono_y2", "chrono_y2"]
  interaction_var <- ifelse(interaction_term_name %in% names(coefs), 
                            vcov_matrix[interaction_term_name, interaction_term_name], 
                            0)
  covar <- ifelse(interaction_term_name %in% names(coefs), 
                  vcov_matrix["chrono_y2", interaction_term_name], 
                  0)
  
  combined_var <- base_var + interaction_var + 2 * covar
  
  lower_bound <- combined_effect - 1.96 * sqrt(combined_var)
  upper_bound <- combined_effect + 1.96 * sqrt(combined_var)
  
  or <- round(exp(combined_effect), 2)
  ci <- round(exp(c(lower_bound, upper_bound)), 2)
  
  z_value <- combined_effect / sqrt(combined_var)
  p_value <- 2 * pnorm(abs(z_value), lower.tail = FALSE)
  
  return(list(or=or, ci=ci, p_value=p_value))
}

results <- lapply(1:3, get_or_ci_pvalue)
names(results) <- paste("puberty_3cat =", 1:3)
results


#4. Interaction by between chronotype and sleep duration and insomnia - moderate to severe pain
#interaction between weekly sleep duration and chronotype - P for interaction = 0.30
dat1$chrono_y2_z <- scale(dat1$chrono_y2)
mod1 <- gam(pain_intense_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
              employ + region + covid + pds_mean + insomnia_symptom_y2 + chrono_y2_z*sdw_y2,
            random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", data = dat1)
summary(mod1)

#interaction between insomnia and chronotype: not sig (P = 0.69)
mod1 = gam(pain_intense_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
             employ + region + covid + pds_mean + sdw_y2 + chrono_y2_z*insomnia_symptom_y2,
           random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", data = dat1)
summary(mod1)


##################################################################
#Sensitivity analyses
#1. Pain intensity and body regions as continuous outcomes

#Create pain intensity assigning 0 to those not having pain
dat1$pain_lvl_y3 = NA
dat1$pain_lvl_y3 [dat1$pain_last_month_y3==0] = 0
indices <- which(dat1$pain_last_month_y3 == 1)
dat1$pain_lvl_y3[indices] <- dat1$painscale_y3[indices]
table(dat1$pain_lvl_y3, exclude = NULL)
table(dat1$painscale_y3, exclude = NULL)
table(dat1$pain_last_month_y3, exclude = NULL)

#Create pain body regions assigning 0 to those not having pain 
dat1$pain_region_n_y3 = NA
dat1$pain_region_n_y3 [dat1$pain_last_month_y3==0] = 0
indices <- which(dat1$pain_last_month_y3 == 1)
dat1$pain_region_n_y3[indices] <- dat1$pain_region_y3[indices]
table(dat1$pain_region_n_y3, exclude = NULL)

#Linear mixed regression models for the entire sample
#Pain intensity
mod1 = gam(pain_lvl_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
             employ + region + pds_mean + covid + sdw_y2 + insomnia_symptom_y2 + chrono_y2,
           random = ~ (1|site_id_l_y0/rel_family_id_y0), data = dat1)
summary(mod1)
summary1 = summary(mod1)
coef1 <- summary1$p.coeff
beta = round(coef1, 4)
se = round(summary1$se, 4)
p_value = round(summary1$p.pv, 4)
lower_ci <- round(coef1 - 1.96 * summary(mod1)$se,2)
upper_ci <- round(coef1 + 1.96 * summary(mod1)$se,2)
p_table <- data.frame(beta, se, lower_ci, upper_ci, p_value)
p_table

mod1 = gam(pain_lvl_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
             employ + region + pds_mean + covid + sdw_y2 + insomnia_symptom_y2 + sjl_abs_y2_log,
           random = ~ (1|site_id_l_y0/rel_family_id_y0), data = dat1)
summary(mod1)
summary1 = summary(mod1)
coef1 <- summary1$p.coeff
beta = round(coef1, 4)
se = round(summary1$se, 4)
p_value = round(summary1$p.pv, 4)
lower_ci <- round(coef1 - 1.96 * summary(mod1)$se,2)
upper_ci <- round(coef1 + 1.96 * summary(mod1)$se,2)
p_table <- data.frame(beta, se, lower_ci, upper_ci, p_value)
p_table


#Pain regions
mod1 = gam(pain_region_n_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
             employ + region + pds_mean + covid + sdw_y2 + insomnia_symptom_y2 + chrono_y2,
           random = ~ (1|site_id_l_y0/rel_family_id_y0), data = dat1)
summary(mod1)
summary1 = summary(mod1)
coef1 <- summary1$p.coeff
beta = round(coef1, 4)
se = round(summary1$se, 4)
p_value = round(summary1$p.pv, 4)
lower_ci <- round(coef1 - 1.96 * summary(mod1)$se,2)
upper_ci <- round(coef1 + 1.96 * summary(mod1)$se,2)
p_table <- data.frame(beta, se, lower_ci, upper_ci, p_value)
p_table

mod1 = gam(pain_region_n_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
             employ + region + pds_mean + covid + sdw_y2 + insomnia_symptom_y2 + sjl_abs_y2_log,
           random = ~ (1|site_id_l_y0/rel_family_id_y0), data = dat1)
summary(mod1)
summary1 = summary(mod1)
coef1 <- summary1$p.coeff
beta = round(coef1, 4)
se = round(summary1$se, 4)
p_value = round(summary1$p.pv, 4)
lower_ci <- round(coef1 - 1.96 * summary(mod1)$se,2)
upper_ci <- round(coef1 + 1.96 * summary(mod1)$se,2)
p_table <- data.frame(beta, se, lower_ci, upper_ci, p_value)
p_table


#Linear mixed regression models for the subset analyses
#Pain intensity
mod1 = gam(pain_lvl_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
             employ + region + pds_mean + covid + sdw_y2 + insomnia_symptom_y2 + chrono_y2,
           random = ~ (1|site_id_l_y0/rel_family_id_y0), data = dat1[dat1$pain_last_month_y3 == 1,])
summary(mod1)
summary1 = summary(mod1)
coef1 <- summary1$p.coeff
beta = round(coef1, 4)
se = round(summary1$se, 4)
p_value = round(summary1$p.pv, 4)
lower_ci <- round(coef1 - 1.96 * summary(mod1)$se,2)
upper_ci <- round(coef1 + 1.96 * summary(mod1)$se,2)
p_table <- data.frame(beta, se, lower_ci, upper_ci, p_value)
p_table

mod1 = gam(pain_lvl_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
             employ + region + pds_mean + covid + sdw_y2 + insomnia_symptom_y2 + sjl_abs_y2_log,
           random = ~ (1|site_id_l_y0/rel_family_id_y0), data = dat1[dat1$pain_last_month_y3 == 1,])
summary(mod1)
summary1 = summary(mod1)
coef1 <- summary1$p.coeff
beta = round(coef1, 4)
se = round(summary1$se, 4)
p_value = round(summary1$p.pv, 4)
lower_ci <- round(coef1 - 1.96 * summary(mod1)$se,2)
upper_ci <- round(coef1 + 1.96 * summary(mod1)$se,2)
p_table <- data.frame(beta, se, lower_ci, upper_ci, p_value)
p_table


#Pain regions
mod1 = gam(pain_region_n_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
             employ + region + pds_mean + covid + sdw_y2 + insomnia_symptom_y2 + chrono_y2,
           random = ~ (1|site_id_l_y0/rel_family_id_y0), data = dat1[dat1$pain_last_month_y3 == 1,])
summary(mod1)
summary1 = summary(mod1)
coef1 <- summary1$p.coeff
beta = round(coef1, 4)
se = round(summary1$se, 4)
p_value = round(summary1$p.pv, 4)
lower_ci <- round(coef1 - 1.96 * summary(mod1)$se,2)
upper_ci <- round(coef1 + 1.96 * summary(mod1)$se,2)
p_table <- data.frame(beta, se, lower_ci, upper_ci, p_value)
p_table

mod1 = gam(pain_region_n_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
             employ + region + pds_mean + covid + sdw_y2 + insomnia_symptom_y2 + sjl_abs_y2_log,
           random = ~ (1|site_id_l_y0/rel_family_id_y0), data = dat1[dat1$pain_last_month_y3 == 1,])
summary(mod1)
summary1 = summary(mod1)
coef1 <- summary1$p.coeff
beta = round(coef1, 4)
se = round(summary1$se, 4)
p_value = round(summary1$p.pv, 4)
lower_ci <- round(coef1 - 1.96 * summary(mod1)$se,2)
upper_ci <- round(coef1 + 1.96 * summary(mod1)$se,2)
p_table <- data.frame(beta, se, lower_ci, upper_ci, p_value)
p_table

#2. In adolescents with pain at baseline, examine chronotyoe and social jetlat in relation to prognosis (dat2)
dat2 = subset(rui, rui$chrono_include_y2 == 1 & rui$pain_last_month_y2 == "1")

#Create pain intensity assigning 0 to those not having pain
dat2$pain_lvl_y2 = dat2$painscale_y2
table(dat2$pain_lvl_y2, exclude = NULL)
dat2$pain_lvl_y3 = NA
dat2$pain_lvl_y3 [dat2$pain_last_month_y3==0] = 0
indices <- which(dat2$pain_last_month_y3 == 1)
dat2$pain_lvl_y3[indices] <- dat2$painscale_y3[indices]
table(dat2$pain_lvl_y3, exclude = NULL)

#Create pain body regions assigning 0 to those not having pain 
dat2$pain_region_n_y2 = dat2$pain_region_y2
table(dat2$pain_region_n_y2, exclude = NULL)
dat2$pain_region_n_y3 = NA
dat2$pain_region_n_y3 [dat2$pain_last_month_y3==0] = 0
indices <- which(dat2$pain_last_month_y3 == 1)
dat2$pain_region_n_y3[indices] <- dat2$pain_region_y3[indices]
table(dat2$pain_region_n_y3, exclude = NULL)

#Linear mixed regression models
#Pain intensity
mod1 = gam(pain_lvl_y3 ~ pain_lvl_y2 + age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
             employ + region + pds_mean + covid + sdw_y2 + insomnia_symptom_y2 + chrono_y2,
           random = ~ (1|site_id_l_y0/rel_family_id_y0), data = dat2)
summary(mod1)
summary1 = summary(mod1)
coef1 <- summary1$p.coeff
beta = round(coef1, 4)
se = round(summary1$se, 4)
p_value = round(summary1$p.pv, 4)
lower_ci <- round(coef1 - 1.96 * summary(mod1)$se,2)
upper_ci <- round(coef1 + 1.96 * summary(mod1)$se,2)
p_table <- data.frame(beta, se, lower_ci, upper_ci, p_value)
p_table

mod1 = gam(pain_lvl_y3 ~ pain_lvl_y2 + age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
             employ + region + pds_mean + covid + sdw_y2 + insomnia_symptom_y2 + sjl_abs_y2_log,
           random = ~ (1|site_id_l_y0/rel_family_id_y0), data = dat2)
summary(mod1)
summary1 = summary(mod1)
coef1 <- summary1$p.coeff
beta = round(coef1, 4)
se = round(summary1$se, 4)
p_value = round(summary1$p.pv, 4)
lower_ci <- round(coef1 - 1.96 * summary(mod1)$se,2)
upper_ci <- round(coef1 + 1.96 * summary(mod1)$se,2)
p_table <- data.frame(beta, se, lower_ci, upper_ci, p_value)
p_table

#Pain regions
mod1 = gam(pain_region_n_y3 ~ pain_region_y2 + age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
             employ + region + pds_mean + covid + sdw_y2 + insomnia_symptom_y2 + chrono_y2,
           random = ~ (1|site_id_l_y0/rel_family_id_y0), data = dat2)
summary(mod1)
summary1 = summary(mod1)
coef1 <- summary1$p.coeff
beta = round(coef1, 4)
se = round(summary1$se, 4)
p_value = round(summary1$p.pv, 4)
lower_ci <- round(coef1 - 1.96 * summary(mod1)$se,2)
upper_ci <- round(coef1 + 1.96 * summary(mod1)$se,2)
p_table <- data.frame(beta, se, lower_ci, upper_ci, p_value)
p_table

mod1 = gam(pain_region_n_y3 ~ pain_region_y2 + age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
             employ + region + pds_mean + covid + sdw_y2 + insomnia_symptom_y2 + sjl_abs_y2_log,
           random = ~ (1|site_id_l_y0/rel_family_id_y0), data = dat2)
summary(mod1)
summary1 = summary(mod1)
coef1 <- summary1$p.coeff
beta = round(coef1, 4)
se = round(summary1$se, 4)
p_value = round(summary1$p.pv, 4)
lower_ci <- round(coef1 - 1.96 * summary(mod1)$se,2)
upper_ci <- round(coef1 + 1.96 * summary(mod1)$se,2)
p_table <- data.frame(beta, se, lower_ci, upper_ci, p_value)
p_table

#3.Missing data analysis within the analytic sample of N = 5,991
#Lost to follow-up: 451 participants were lost to follow-up (7.5%)
table(dat1$year3, exclude = NULL)
prop.table(table(dat1$year3, exclude = NULL))

#Excluded due to missing pain outcome data: 37 did not have pain outcome data at year 3 
#0.6% for the entire sample and 0.7% for those who participated in year 3 follow-up
# of these 37 participants, 22 refused to answer and 15 did not complete the pain questionnaire
table(dat1[dat1$year3==1,]$pain_last_month_y3, exclude = NULL)
prop.table(table(dat1[dat1$year3==1,]$pain_last_month_y3, exclude = NULL))

#Pattern of missingness of all variables
# List of variables used in the model
variables <- c("pain_intense_y3", "pain_limiting_y3", "multi_region_pain_y3", 
               "age_y_y2", "female", "race_ethnicity", "income_4level", "familysize", 
               "marital", "employ", "region", "covid", "pds_mean", 
               "sdw_y2", "insomnia_symptom_y2", "chrono_y2", "sjl_abs_y2_log",
               "site_id_l_y0", "rel_family_id_y0")

# Check for missing values in each variable
missing_info <- function(data, vars) {
  missing_count <- sapply(data[vars], function(x) sum(is.na(x)))
  total_count <- nrow(data)
  missing_percent <- (missing_count / total_count) * 100
  return(data.frame(Variable = vars, MissingCount = missing_count, MissingPercent = missing_percent))
}
missing_values_info <- missing_info(dat1, variables)
print(missing_values_info)

#Percentge of complete cases and any missingness: 4977 had complete data (82.3%)
dat1$complete_case <- apply(dat1[variables], 1, function(x) all(!is.na(x)))
dat1$complete_case <- as.numeric(dat1$complete_case)
table(dat1$complete_case, exclude = NULL)
prop.table(table(dat1$complete_case, exclude = NULL))

#Perform multiple imputation
#Due to computing time, multiple imputation was performed in SAS


#4. Impact of adjusting for depression and anxiety
#Additionally adjusting for Year1 depressive and anxiety symptoms

#Chronotype and pain
mod1 <- gam(pain_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
              employ + region + covid + pds_mean + sdw_y2 + insomnia_symptom_y2 + chrono_y2 +
              depression_y2 + anxiety_y2,
            random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", data = dat1)
summary(mod1)
summary1 = summary(mod1)
coef1 <- summary1$p.coeff
beta = round(coef1, 4)
se = round(summary1$se, 4)
p_value = round(summary1$p.pv, 4)
odds_ratios <- round(exp(coef1),2)
lower_ci <- round(exp(coef1 - 1.96 * summary(mod1)$se),2)
upper_ci <- round(exp(coef1 + 1.96 * summary(mod1)$se),2)
p_table <- data.frame(beta, se, odds_ratios,lower_ci, upper_ci, p_value)
p_table

#Chronotype and moderate to severe pain
mod1 <- gam(pain_intense_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
              employ + region + covid + pds_mean + sdw_y2 + insomnia_symptom_y2 + chrono_y2 +
              depression_y2 + anxiety_y2,
            random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", data = dat1)
summary(mod1)
summary1 = summary(mod1)
coef1 <- summary1$p.coeff
beta = round(coef1, 4)
se = round(summary1$se, 4)
p_value = round(summary1$p.pv, 4)
odds_ratios <- round(exp(coef1),2)
lower_ci <- round(exp(coef1 - 1.96 * summary(mod1)$se),2)
upper_ci <- round(exp(coef1 + 1.96 * summary(mod1)$se),2)
p_table <- data.frame(beta, se, odds_ratios,lower_ci, upper_ci, p_value)
p_table

#Chronotype and multi-region pain
mod1 <- gam(multi_region_pain_y3 ~ age_y_y2 + female + race_ethnicity + income_4level + familysize + marital + 
              employ + region + covid + pds_mean + sdw_y2 + insomnia_symptom_y2 + chrono_y2 +
              depression_y2 + anxiety_y2,
            random = ~ (1|site_id_l_y0/rel_family_id_y0), family = "binomial", data = dat1)
summary(mod1)
summary1 = summary(mod1)
coef1 <- summary1$p.coeff
beta = round(coef1, 4)
se = round(summary1$se, 4)
p_value = round(summary1$p.pv, 4)
odds_ratios <- round(exp(coef1),2)
lower_ci <- round(exp(coef1 - 1.96 * summary(mod1)$se),2)
upper_ci <- round(exp(coef1 + 1.96 * summary(mod1)$se),2)
p_table <- data.frame(beta, se, odds_ratios,lower_ci, upper_ci, p_value)
p_table