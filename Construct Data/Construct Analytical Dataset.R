################################################################################
# AUTHOR:             JACOB JAMESON
# LAST UPDATED:       8/7/2022
# PURPOSE:            CONSTRUCT W1 to W4 Analytical Dataset 20745
################################################################################

# Load packages necessary for preparing final data
library(tidyverse)
library(haven)
library(labelled)
library(scales)

# Create and prepare wave 1 and wave 4 data
data_path <- '~/Desktop/Add Health Substance Use Project'
w1_prep <- paste0(data_path, '/Construct Dataset/Prepare W1.R')
w4_prep <- paste0(data_path, '/Construct Dataset/Prepare W4.R')

source(w1_prep)
source(w4_prep)

# Get SES data
ses_path <- '~/Desktop/Add Health Substance Use Project/Data/Constructed SES Variables'
ses.data <- read_xpt(paste0(ses_path, '/conses4.xpt'))
names(ses.data) <- tolower(names(ses.data))


# Merge wave 1, wave 4, and SES data together
final.df <- merge(wave.1, wave.4, by=c('aid', 'psuscid', 'region'), all=TRUE)
final.df <- merge(final.df, ses.data, by='aid')


# Clear environment
rm(list=setdiff(ls(), 'final.df'))


################################################################################
# CREATE VARIABLES THAT WILL BE USED IN ANALYSIS:
#
#   - school_self: Where does the adolescent GE start relative to school and
#                  what is their trajectory going into adulthood
################################################################################

    
final.df <- final.df %>%
  mutate(above_school_avg = case_when(school_avg_GE >= w1.GE_male ~ 1,
                                      school_avg_GE < w1.GE_male ~ 0,
                                      TRUE ~ NaN),
        delta_w1_w4_GE = w4.GE_male_std - w1.GE_male_std,
         school_self = case_when(above_school_avg == 1 & delta_w1_w4_GE > 0~1,
                                 above_school_avg == 1 & delta_w1_w4_GE < 0~2,
                                 above_school_avg == 0 & delta_w1_w4_GE > 0~3,
                                 above_school_avg == 0 & delta_w1_w4_GE < 0~4),
         self_self = case_when(w1.GE_male_std >= 0 & delta_w1_w4_GE > 0~1,
                               w1.GE_male_std >= 0 & delta_w1_w4_GE < 0~2,
                               w1.GE_male_std < 0 & delta_w1_w4_GE > 0~3,
                               w1.GE_male_std < 0 & delta_w1_w4_GE < 0~4))

final.df$school_self <- factor(final.df$school_self, levels = c(1,2,3,4),
                         labels = c("Above School Avg and Increasing",
                                    "Above School Avg and Decreasing",
                                    "Below School Avg and Increasing", 
                                    "Below School Avg and Decreasing"))

final.df$self_self <- factor(final.df$self_self, levels = c(1,2,3,4),
                               labels = c("Above Adolescent Male Avg and Increasing",
                                          "Above Adolescent Male Avg and Decreasing",
                                          "Below Adolescent Male Avg and Increasing", 
                                          "Below Adolescent Male Avg and Decreasing"))

## Outcomes and controls for substance use
final.df <- final.df %>% 
  mutate(w1.cigarettes = case_when(h1to5 > 0 & h1to5 < 96 ~ 1,
                                   h1to5 == 0 | h1to5 == 97 ~ 0,
                                   TRUE ~ 0),
         w1.marijuana = case_when(h1to30 > 0 & h1to30 < 96 ~ 1,
                                  h1to30 == 0 | h1to30 == 98  ~ 0,
                                  TRUE ~ 0),
         w1.recreational = case_when(h1to34 > 0 &  h1to34 < 96 ~ 1,
                                     h1to38 > 0 & h1to38 < 996 ~ 1,
                                     h1to41 > 0 & h1to41 < 996 ~ 1,
                                     h1to43 == 1 ~ 1,
                                     (h1to34 == 0 & h1to38 == 997 & 
                                       h1to41 == 997 &
                                       (h1to43 == 0 | h1to43 == 7)) ~ 0,
                                     TRUE ~ 0),
         w1.drunk = case_when(h1to18 < 7 & h1to18 >= 1 ~ 1,
                              h1to18 == 7 | h1to18 == 97 ~ 0,
                              TRUE ~ 0),
         w4.recreational = case_when(h4to65a == 1 ~ 1,
                                     h4to65c == 1 ~ 1,
                                     h4to65d == 1 ~ 1,
                                     h4to65e == 1 ~ 1,
                                     h4to66 == 1 ~ 1,
                                     (h4to65a == 0 & h4to65c == 0 & 
                                       h4to65d == 0 & h4to65e == 0 & 
                                       h4to66 == 0) ~ 0,
                                     TRUE ~ 0),
         w4.marijuana = case_when(h4to65b == 1 ~ 1,
                                  h4to65b == 0 ~ 0,
                                  TRUE ~ 0),
         w4.drunk = case_when(h4to38 > 0 & h4to38 < 96 ~ 1,
                              h4to38 == 0 | h4to38 == 97 ~ 0,
                              TRUE ~ 0),
         w4.cigarettes = case_when(h4to3 == 1 ~ 1,
                                   h4to3 == 0 | h4to3 == 7 ~ 0,
                                   TRUE ~ 0),
         w4.prescription = case_when(h4to63 == 1 ~ 1,
                                     h4to63 == 0 ~ 0,
                                     TRUE ~ 0))



vars.keep <- c('w1.cigarettes', 'w1.marijuana', 'w1.recreational', 'w1.drunk', 
               'w4.recreational', 'w4.marijuana', 'w4.drunk', 'w4.cigarettes',
               'w4.prescription', 'aid', 'region', 'psuscid', 'gswgt4_2', 'sschlcde', 
               'w1_male', 'self_self', 'school_self', 'delta_w1_w4_GE', 'w1.GE_male',
               'w4.GE_male', 'school_avg_GE', 'w1.GE_male_std', 'w4.GE_male_std',
               'sespc_al', 'nhood1_d', 'pseudo.gpa', 'race', 'w4_male')


final.df <- final.df[, vars.keep]
final.df <- filter(final.df, is.na(gswgt4_2) == F)

final.df <- final.df %>%
  mutate(in_sample = ifelse(w1_male == w4_male & w1_male == 1, 1, 0),
         in_sample = ifelse(is.na(w1.GE_male) == F & is.na(w4.GE_male) == F, in_sample, 0))

write_csv(final.df, '~/Desktop/Add Health Substance Use Project/Data/Final.csv')
################################################################################