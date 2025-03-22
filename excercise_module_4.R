################
#This code is related to the exercises as part of EdX course on Impact Evaluation Methods with applications in low and middle-income countries
#The data set is taken from: Tarozzi, A., Mahajan, A., Blackburn, B., Kopf, D., Krishnan, L., and Yoong, J. (2014). Micro-loans, Insecticide-Treated Bednets, and Malaria: Evidence from a Randomized Controlled Trial in Orissa, India. American Economic Review, 104(7), 1909-41
################


# Install packages
#install.packages("tidyverse")
#install.packages("ggplot2")

library(tidyverse)
library(ggplot2)
library(haven)


# Define the root directory path
current_dir <- getwd()
print(current_dir)

# Data must be downloaded from ...
data_file <- "panel_biomarkers.dta"

# Load data
full_path <- file.path(current_dir, data_file)
panel_biomarkers <- read_dta(full_path, "/data/panel_biomarkers.dta")


#Summary statistics, confidence intervals and t-tests
##Calculate the mean of the variable "survey_m61" and 95% confidence interval. 
##Note: "survey_m61" shows the total number of reported malaria episodes that the individual experienced in the prior 6 months
t.test(panel_biomarkers$survey_m61, conf.level = 0.95)

##Calculate 90% confidence interval
t.test(panel_biomarkers$survey_m61, conf.level = 0.90)

##Calculate 99% confidence interval
t.test(panel_biomarkers$survey_m61, conf.level = 0.99)

##Test the null hypothesis that the population mean of `survey_m61` equals 1 
##against the alternative hypothesis that the population average is not equal to 1
t.test(panel_biomarkers$survey_m61, mu=1)

##Test the null hypothesis that the population mean of `survey_m61` equals `0.095`
##against the alternative hypothesis that the population average is not equal to 0.095
t.test(panel_biomarkers$survey_m61, mu=0.095)

#Comparing means
##Run a t-test to determine whether the average of `netlast_fup` is different
##among the control group and treatment group that was offered a loan
##Note: "netlast_fup" shows people that reported using a net during the follow-up survey

##Create a new variable "treat_loan" where 1 is mf loan group and o is control group
panel_biomarkers_with_treat_loan <- panel_biomarkers %>%
  mutate(treat_loan = factor(case_when(arm_mf == 1 ~ 1, arm_c == 1 ~ 0),
         levels = c(0,1), 
         labels = c("Control", "MF loan")))

##Double-check that "treat_loan has been created
"treat_loan" %in% names(panel_biomarkers_with_treat_loan)

##Run t-test for two samples
t.test(netlast_fup ~ treat_loan, panel_biomarkers_with_treat_loan)

##Run a t-test to determine whether the average of `netlast_fup` is different among the treatment group that was offered a loan 
##and the treatment group that was given a free bed net
panel_biomarkers_with_treat_free <- panel_biomarkers %>%
  mutate(treat_free = factor(case_when(arm_free == 1 ~ 1, arm_mf == 1 ~ 0),
                             levels = c(0,1), 
                             labels = c("MF loan", "Free")))
##Double-check that "treat_free" has been created
"treat_free" %in% names(panel_biomarkers_with_treat_free)

##Run t-test for two samples
t.test(netlast_fup ~ treat_free, panel_biomarkers_with_treat_free)

