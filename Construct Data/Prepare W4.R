################################################################################
# AUTHOR:             JACOB JAMESON
# LAST UPDATED:       8/7/2022
# PURPOSE:            CONSTRUCT AND PREPARE WAVE 4 DATASET
################################################################################

# Load packages necessary for preparing wave 4
library(tidyverse)
library(haven)
library(scales)

# Data paths
data_path <- '~/Desktop/Add Health Substance Use Project/Data'
inhome_path <-  paste0(data_path, '/Wave IV In Home Interview Data/wave4')
weights_path <-  paste0(data_path, '//Wave IV Grand Sample Weights/weights4')

# Load the wave 4 data and wave 4 weights
allwave.4 <- read_xpt(paste0(inhome_path, '/wave4.xpt'))
homeweights.4 <- read_xpt(paste0(weights_path, '/weights4.xpt'))

# Merge wave 4 data with the weights
wave.4 <- merge(allwave.4, homeweights.4, by='AID')

# Rename variables lowercase
names(wave.4) <- tolower(names(wave.4))

# Remove data no longer using
rm(allwave.4, homeweights.4)

################################################################################
# CREATE VARIABLES THAT WILL BE USED IN ANALYSIS:
#
#   - w4.GE_male: GE score created based on Flemming et al.
#   - w4.GE_male_std: GE score by males standardized
################################################################################


# Variables indicated by Flemming et.all for GE

ge.vars <- c('h4to25', 'h4cj1', 'h4da16', 'h4pe5', 'h4pe9', 'h4pe2', 'h4da6',
             'h4da23', 'h4da8', 'h4pe4', 'h4re10', 'h4da17', 'h4mi1', 'h4da4',
             'h4mh23', 'h4pe6', 'h4mh7', 'h4pe10',  'h4pe35', 'h4da11', 
             'h4pe22', 'h4pe26')


wave.4 <- wave.4 %>%
  mutate(h4to25 = ifelse(h4to25 > 1 | h4to25 < 0, NA, h4to25),
         h4cj1 = ifelse(h4cj1 > 1 | h4cj1 < 0, NA, h4cj1),
         h4da16 = ifelse(h4da16 > 3 | h4da16 < 1, NA, h4da16),
         h4pe5 = ifelse(h4pe5 > 5 | h4pe5 < 1, NA, h4pe5),
         h4pe9 = ifelse(h4pe9 > 5 | h4pe9 < 1, NA, h4pe9),
         h4pe2 = ifelse(h4pe2 > 5 | h4pe2 < 1, NA, h4pe2),
         h4da6 = ifelse(h4da6 > 7 | h4da6 < 0, NA, h4da6),
         h4da23 = ifelse(h4da23 > 105 | h4da23 < 0, NA, h4da23),
         h4da8 = ifelse(h4da8 > 7 | h4da8 < 0, NA, h4da8),
         h4pe4 = ifelse(h4pe4 > 5 | h4pe4 < 1, NA, h4pe4),
         h4re10 = ifelse(h4re10 > 7 | h4re10 < 0, NA, h4re10),
         h4da17 = ifelse(h4da17 > 99 | h4da17 < 0, NA, h4da17),
         h4mi1 = ifelse(h4mi1 > 1 | h4mi1 < 0, NA, h4mi1),
         h4da4 = ifelse(h4da4 > 7 | h4da4 < 0, NA, h4da4),
         h4mh23 = ifelse(h4mh23 > 3 | h4mh23 < 0, NA, h4mh23),
         h4pe6 = ifelse(h4pe6 > 5 | h4pe6 < 1, NA, h4pe6),
         h4mh7 = ifelse(h4mh7 > 6 | h4mh7 < 1, NA, h4mh7),
         h4pe10 = ifelse(h4pe10 > 5 | h4pe10 < 1, NA, h4pe10),
         h4pe35 = ifelse(h4pe35 > 5 | h4pe35 < 1, NA, h4pe35),
         h4da11 = ifelse(h4da11 > 1 | h4da11 < 0, NA, h4da11),
         h4pe22 = ifelse(h4pe22 > 5 | h4pe22 < 1, NA, h4pe22),
         h4pe26 = ifelse(h4pe26 > 5 | h4pe26 < 1, NA, h4pe26))



# Logistic regression to predict male using GE variables 
wave.4$w4_male <- factor(ifelse(wave.4$bio_sex4 == 1, 1, 0))
wave.4$w4_female <- factor(ifelse(wave.4$bio_sex4 == 2, 1, 0))


# NOTE: Fix this code so that all these variables will be run as factor
predict_male <- glm(w4_male~ factor(h4to25) + factor(h4cj1) + factor(h4da16) + 
                      factor(h4pe5) + factor(h4pe9) + factor(h4pe2) + 
                      factor(h4da6) + h4da23 + factor(h4da8) + factor(h4pe4) + 
                      factor(h4re10) + h4da17 + factor(h4mi1) + factor(h4da4) + 
                      factor(h4mh23) + factor(h4pe6) + factor(h4mh7) + 
                      factor(h4pe10) + factor(h4pe35) + factor(h4da11) + 
                      factor(h4pe22) + factor(h4pe26) , data = wave.4, 
                    family = "binomial")


# Check model fit
summary(predict_male)

# Get a prediction score for male. This will be our GE score
wave.4$w4.GE_male <- predict(predict_male, wave.4, type="response")

wave.4 <- wave.4 %>%
  group_by(w4_male) %>%
  mutate(w4.GE_male_std = scale(w4.GE_male))
################################################################################