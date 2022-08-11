################################################################################
# AUTHOR:             JACOB JAMESON
# LAST UPDATED:       8/7/2022
# PURPOSE:            CONSTRUCT NETWORK VARIABLES
################################################################################

# Load packages necessary for preparing final data
library(tidyverse)
library(haven)
library(labelled)
library(scales)
library(rcompanion) 

data_path <- 'W:/Data Update 2.2022/Friendship Files'
inschool_path <- 'W:/Data Upload 7.2021/Core Files - Wave I/'
inschool_path <-  paste0(inschool_path, '/Wave I In-School Questionnaire Data')

#
inschool <- read_xpt(paste0(inschool_path, '/inschool.xpt'))
inschool <- inschool[,c('AID', 'SQID', 'SSCHLCDE')]

inschool <- inschool %>%
  filter(duplicated(SQID) == F) %>%
  mutate(AID = ifelse(AID == '', SQID, AID))

#
friend.df <- read_xpt(paste0(data_path, 
                    '/Wave I In-School Friendship Nominations/sfriend.xpt'))

friend.df <- friend.df %>%
  filter(duplicated(SQID) == F) 
  
#
friend.df <- merge(friend.df, inschool, by='SQID')
friend.df[] <- lapply(friend.df, as.character)

# Rename variables lowercase
names(friend.df) <- tolower(names(friend.df))


# Clear environment
rm(list=setdiff(ls(), 'friend.df'))

################################################################################
# CREATE VARIABLES THAT WILL BE USED IN ANALYSIS:
#
#   - num_friend_noms: Number of friendship nominations
#   - num_bff_noms: Number of BFF friendship nominations
#   - fbff_reciprocity: Female BFF reciprocity
#   - mbff_reciprocity: Male BFF reciprocity
#   - katz_centrality: Katz centrality score 
################################################################################


# Replace friendship nominations that are 77777777, 99999999, 88888888, 99959995
# with missing nomination. EXPLAIN REASONING

friend.vars <- c('mf1aid', 'mf2aid', 'mf3aid', 'mf4aid', 'mf5aid', 'ff1aid',
                 'ff2aid', 'ff3aid', 'ff4aid', 'ff5aid')

id.replace <- c('77777777', '99999999', '88888888', '99959995')


friend.df <- friend.df %>%
  mutate(mf1aid = ifelse(mf1aid %in% id.replace, NA, mf1aid),
         mf2aid = ifelse(mf2aid %in% id.replace, NA, mf2aid),
         mf3aid = ifelse(mf3aid %in% id.replace, NA, mf3aid),
         mf4aid = ifelse(mf4aid %in% id.replace, NA, mf4aid),
         mf5aid = ifelse(mf5aid %in% id.replace, NA, mf5aid),
         ff1aid = ifelse(ff1aid %in% id.replace, NA, ff1aid),
         ff2aid = ifelse(ff2aid %in% id.replace, NA, ff2aid),
         ff3aid = ifelse(ff3aid %in% id.replace, NA, ff3aid),
         ff4aid = ifelse(ff4aid %in% id.replace, NA, ff4aid),
         ff5aid = ifelse(ff5aid %in% id.replace, NA, ff5aid))

################################################################################
# Number of BFF nominations dataframe
bff_noms <- data.frame(list('aid' = c(friend.df$mf1aid, friend.df$ff1aid)))

bff_noms <- bff_noms %>% 
  filter(is.na(aid) == F) %>%
  group_by(aid) %>% summarize(num_bff_noms = n()) 

################################################################################
# Number of friend nominations dataframe
friend_noms <- data.frame(list('aid' = c(friend.df$mf1aid, friend.df$mf2aid,
                                         friend.df$mf3aid, friend.df$mf4aid,
                                         friend.df$mf5aid, friend.df$ff1aid,
                                         friend.df$ff2aid, friend.df$ff3aid,
                                         friend.df$ff4aid, friend.df$ff5aid)))

friend_noms <- friend_noms %>% 
  filter(is.na(aid) == F) %>%
  group_by(aid) %>% summarize(num_friend_noms = n()) 

################################################################################
# Determine best friend reciprocity
male_bff_df <- friend.df[, c('aid', 'mf1aid')] %>% filter(is.na(mf1aid) == F)
female_bff_df <- friend.df[, c('aid', 'ff1aid')] %>% filter(is.na(ff1aid) == F)

bff_df <- merge(male_bff_df, female_bff_df, by='aid', all= TRUE )

mbff_list <- c(bff_df$mf1aid)
mbff_list <- mbff_list[!is.na(mbff_list)]

mbff_df <- bff_df[bff_df$aid %in% mbff_list,] %>%
  rename(mf1aid = aid, mf1aid_mf1aid = mf1aid, mf1aid_ff1aid = ff1aid)

fbff_list <- c(bff_df$ff1aid)
fbff_list <- fbff_list[!is.na(fbff_list)]

fbff_df <- bff_df[bff_df$aid %in% fbff_list,] %>%
  rename(ff1aid = aid, ff1aid_mf1aid = mf1aid, ff1aid_ff1aid = ff1aid)

bff_df <- merge(bff_df, fbff_df, by='ff1aid', all=T)
bff_df <- merge(bff_df, mbff_df, by='mf1aid', all=T)

bff_df <- bff_df %>%
  mutate(mbff_reciprocity = case_when(mf1aid_mf1aid == aid ~ 1,
                                      mf1aid_ff1aid == aid ~ 1,
                                      TRUE ~ 0),
         fbff_reciprocity = case_when(ff1aid_mf1aid == aid ~ 1,
                                      ff1aid_ff1aid == aid ~ 1,
                                      TRUE ~ 0)) %>%
  select(aid, mbff_reciprocity, fbff_reciprocity)


bff_df <- bff_df %>%
  mutate(mbff_reciprocity = 0, fbff_reciprocity = 0)


################################################################################
# Determine Network Centrality



################################################################################
# Combine dataframes together

friend.df <- merge(friend.df, bff_noms, by='aid', all=T)
friend.df <- merge(friend.df, friend_noms, by='aid', all=T)
friend.df <- merge(friend.df, bff_df, by='aid', all=T)


