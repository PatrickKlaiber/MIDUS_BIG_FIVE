###BIG Five Script
###Data used from the MIDUS 2 and MIDUS refresher study

options(scipen = 8)#prefer fixed over scientific notation

#load packages####

library(Hmisc)
library(tidyverse)
library(lmerTest)
library(sjPlot)
library(sjstats)
library(psych)
library(stargazer)
library(apaTables)
library(broom.mixed)
library(reghelper)
library(lm.beta)
library(ggpubr)


#set working directory to file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#load datasets####

#daily diary data (NSDE) refresher
load("data/refresher_NSDE.rda")
NSDE_refresher <- da37083.0001
rm(da37083.0001)

#baseline data MIDUS refresher
load("data/refresher_baseline.rda")
Baseline_refresher <- da36532.0001
rm(da36532.0001) 

#baseline data MIDUS2
load("data/MIDUS2_baseline.rda")
Baseline_midus2 <- M2_P1_Milwaukee_P1_MERGED_N_5555_3_7_12_sav
rm(M2_P1_Milwaukee_P1_MERGED_N_5555_3_7_12_sav)

#daily diary data (NSDE) MIDUS2
load("data/MIDUS2_NSDE.rda")
NSDE_midus2 <- da26841.0001 #rename data set
rm(da26841.0001)


#exclude participant from the baseline questionnaire if they are not part of the NSDE sample

Baseline_refresher <- Baseline_refresher[which(
  Baseline_refresher$MRID %in% NSDE_refresher$MRID),]


Baseline_midus2 <- Baseline_midus2[which(
  Baseline_midus2$M2ID %in% NSDE_midus2$M2ID),]


#Data preparation####

##MIDUS 2

##recode and create sumscore of daily events

NSDE_midus2$B2DF8r <-  recode(as.numeric(NSDE_midus2$B2DF8), `1` = 1L, `2` = 0L) #social
NSDE_midus2$B2DF9r <-  recode(as.numeric(NSDE_midus2$B2DF9), `1` = 1L, `2` = 0L) #work
NSDE_midus2$B2DF10r <- recode(as.numeric(NSDE_midus2$B2DF10), `1` = 1L, `2` = 0L) #home
NSDE_midus2$B2DF11r <- recode(as.numeric(NSDE_midus2$B2DF11), `1` = 1L, `2` = 0L) #to a friend
NSDE_midus2$B2DF12r <- recode(as.numeric(NSDE_midus2$B2DF12), `1` = 1L, `2` = 0L) #other

NSDE_midus2$num_pos_events <-  rowSums(cbind(NSDE_midus2$B2DF8r, 
                                             NSDE_midus2$B2DF9r, 
                                             NSDE_midus2$B2DF10r,
                                             NSDE_midus2$B2DF11r,
                                             NSDE_midus2$B2DF12r), na.rm=TRUE)

NSDE_midus2$pos_social <- NSDE_midus2$B2DF8r




#same for refresher

{
  NSDE_refresher$RA2DF8r <-  recode(as.numeric(NSDE_refresher$RA2DF8), `1` = 1L, `2` = 0L)
  NSDE_refresher$RA2DF9r <-  recode(as.numeric(NSDE_refresher$RA2DF9), `1` = 1L, `2` = 0L)
  NSDE_refresher$RA2DF10r <- recode(as.numeric(NSDE_refresher$RA2DF10), `1` = 1L, `2` = 0L)
  NSDE_refresher$RA2DF11r <- recode(as.numeric(NSDE_refresher$RA2DF11), `1` = 1L, `2` = 0L)
  NSDE_refresher$RA2DF12r <- recode(as.numeric(NSDE_refresher$RA2DF12), `1` = 1L, `2` = 0L)
  
  NSDE_refresher$num_pos_events <-  rowSums(cbind(NSDE_refresher$RA2DF8r, 
                                                  NSDE_refresher$RA2DF9r, 
                                                  NSDE_refresher$RA2DF10r,
                                                  NSDE_refresher$RA2DF11r,
                                                  NSDE_refresher$RA2DF12r), na.rm=F) 
}

describe(NSDE_refresher$num_pos_events)


NSDE_refresher$pos_social <- NSDE_refresher$RA2DF8r

#personmean
NSDE_midus2 <- NSDE_midus2%>%
  group_by(M2ID)%>%
  summarise(npos_pm = mean(num_pos_events))%>%
  merge(NSDE_midus2,.,by ="M2ID")


Baseline_midus2 <- NSDE_midus2%>%
  group_by(M2ID)%>%
  summarise(npos_pm = mean(num_pos_events))%>%
  merge(Baseline_midus2,.,by ="M2ID")

#personmean 

NSDE_refresher <- NSDE_refresher%>%
  group_by(MRID)%>%
  summarise(npos_pm = mean(num_pos_events))%>%
  merge(NSDE_refresher,.,by ="MRID")

Baseline_refresher <- NSDE_refresher%>%
  group_by(MRID)%>%
  summarise(npos_pm = mean(num_pos_events))%>%
  merge(Baseline_refresher,.,by ="MRID")

#personmeancenter number of pos events

NSDE_refresher$npos_pc <- with(NSDE_refresher, num_pos_events - npos_pm)
NSDE_midus2$npos_pc <- with(NSDE_midus2, num_pos_events - npos_pm)

#create personmeans of positive and negative affect

Baseline_midus2 <- NSDE_midus2%>%
  group_by(M2ID)%>%
  summarise(NA_pm = mean(B2DNEGAV, na.rm = T),
            PA_pm = mean(B2DPOSAV, na.rm = T))%>%
  merge(Baseline_midus2,., by = "M2ID")

Baseline_refresher <- NSDE_refresher%>%
  group_by(MRID)%>%
  summarise(NA_pm = mean(RA2DNEGAV, na.rm = T),
            PA_pm = mean(RA2DPOSAV, na.rm = T))%>%
  merge(Baseline_refresher,., by = "MRID")


###recode education

Baseline_refresher$Edu <- Baseline_refresher$RA1PB1 %>%
  as.numeric %>%
  recode_factor(`1` = "up to HS grad",
                `2` = "up to HS grad",
                `3` = "up to HS grad",
                `4` = "up to HS grad",
                `5` = "up to HS grad",
                `6` = "some college",
                `7` = "some college",
                `8` = "some college",
                `9` = "college grad+",
                `10` = "college grad+",
                `11` = "college grad+",
                `12` = "college grad+")


Baseline_midus2$Edu <- Baseline_midus2$B1PB1 %>%
  as.numeric %>%
  recode_factor(`1` = "up to HS grad",
                `2` = "up to HS grad",
                `3` = "up to HS grad",
                `4` = "up to HS grad",
                `5` = "up to HS grad",
                `6` = "some college",
                `7` = "some college",
                `8` = "some college",
                `9` = "college grad+",
                `10` = "college grad+",
                `11` = "college grad+",
                `12` = "college grad+")

#rename variables to combine

Baseline_refresher$race <- Baseline_refresher$RA1PF7A
Baseline_midus2$race <- Baseline_midus2$B1PF7A%>%as.numeric%>%
  recode_factor(`1` = "(1) WHITE",
                `2` = "(2) BLACK AND/OR AFRICAN AMERICAN",
                `3` = "(3) NATIVE AMERICAN OR ALASKA NATIVE ALEUTIAN ISLANDER/ESKIMO",
                `4` = "(4) ASIAN",
                `5` = "(5) NATIVE HAWAIIAN OR PACIFIC ISLANDER",
                `6` = "(6) OTHER (SPECIFY)")




#create variable for time between assessments

Baseline_midus2$year <-  substr(Baseline_midus2$B1PIDATE, 0, 4)%>%as.numeric
Baseline_midus2$month <-  substr(Baseline_midus2$B1PIDATE, 6, 7)%>%as.numeric
Baseline_midus2$baseline_date <- (Baseline_midus2$year *12) + Baseline_midus2$month

Baseline_refresher$year <- Baseline_refresher$RA1PIDATE_YR%>%as.numeric
Baseline_refresher$month <- substr(Baseline_refresher$RA1PIDATE_MO,2,3)%>%as.numeric
Baseline_refresher$baseline_date <- (Baseline_refresher$year *12) + Baseline_refresher$month


NSDE_midus2$date <- NSDE_midus2$B2DIMON + NSDE_midus2$B2DIYEAR*12
NSDE_refresher$date <- (substr(NSDE_refresher$RA2DIMON,2,3)%>%as.numeric) + NSDE_refresher$RA2DIYEAR*12


#rename predictors to combine

Baseline_refresher$NEURO <- Baseline_refresher$RA1SNEURO
Baseline_refresher$EXTRA <- Baseline_refresher$RA1SEXTRA
Baseline_refresher$OPEN <-  Baseline_refresher$RA1SOPEN 
Baseline_refresher$CONS2 <- Baseline_refresher$RA1SCONS2
Baseline_refresher$AGREE <- Baseline_refresher$RA1SAGREE
Baseline_refresher$Age <-   Baseline_refresher$RA1PRAGE
Baseline_refresher$Sex <- Baseline_refresher$RA1PRSEX


Baseline_midus2$NEURO <- Baseline_midus2$B1SNEURO
Baseline_midus2$EXTRA <- Baseline_midus2$B1SEXTRA
Baseline_midus2$OPEN <-  Baseline_midus2$B1SOPEN 
Baseline_midus2$CONS2 <- Baseline_midus2$B1SCONS2
Baseline_midus2$AGREE <- Baseline_midus2$B1SAGREE
Baseline_midus2$Age <-   Baseline_midus2$B1PAGE_M2
Baseline_midus2$Sex <-  Baseline_midus2$B1PRSEX %>%as.numeric%>%
  recode_factor(`1` = "(1) MALE", `2` = "(2) FEMALE")


#combine wide dataset

vars <- c(
  "Edu",
  "Sex",
  "Age",
  "EXTRA",
  "AGREE",
  "CONS2",
  "NEURO",
  "OPEN",
  "race",
  "NA_pm",
  "PA_pm",
  "npos_pm",
  "wave"
  
)

Baseline_midus2$wave <- "midus2"
Baseline_refresher$wave <- "refresher"

midus_comb <- rbind(Baseline_midus2[,vars], Baseline_refresher[,vars])

###center predictors in midus comb

#center predictors for analysis

midus_comb$NEURO_c <- scale(midus_comb$NEURO, scale = F)[,1]
midus_comb$EXTRA_c <- scale(midus_comb$EXTRA, scale = F)[,1]
midus_comb$OPEN_c <-  scale(midus_comb$OPEN , scale = F)[,1]
midus_comb$CONS2_c <- scale(midus_comb$CONS2, scale = F)[,1]
midus_comb$AGREE_c <- scale(midus_comb$AGREE, scale = F)[,1]
midus_comb$Age_c <- scale(midus_comb$Age, scale = F)[,1]



#grandcenter personmean

NSDE_midus2$npos_pm_c <- scale(NSDE_midus2$npos_pm, scale = F)[,1]
Baseline_midus2$npos_pm_c <- scale(Baseline_midus2$npos_pm, scale = F)[,1]


###ad day

NSDE_midus2$day <- NSDE_midus2$B2DDAY%>%as.numeric
NSDE_refresher$day <- NSDE_refresher$RA2DDAY%>%as.numeric



#combine data sets



midus2 <- merge(Baseline_midus2[,c(
  "NEURO",
  "EXTRA",
  "OPEN",
  "CONS2",
  "AGREE",
  "Edu",
  "Sex",
  "Age",
  "baseline_date",
  "wave",
  "M2ID")],
  NSDE_midus2[,c("npos_pm",
                 "npos_pc",
                 "B2DPOSAV",
                 "B2DNEGAV",
                 "date",
                 "M2ID",
                 "num_pos_events",
                 "NA1", 
                 "NA2", 
                 "NA3", 
                 "NA4", 
                 "NA5", 
                 "NA6", 
                 "NA7", 
                 "NA8", 
                 "NA9", 
                 "NA10",
                 "NA11",
                 "NA12",
                 "NA13",
                 "NA14",
                 "PA1", 
                 "PA2", 
                 "PA3", 
                 "PA4", 
                 "PA5", 
                 "PA6", 
                 "PA7", 
                 "PA8", 
                 "PA9", 
                 "PA10",
                 "PA11",
                 "PA12",
                 "PA13",
                 "day")],
  by = "M2ID", all.y = T, all.x = T)



refresher <- merge(Baseline_refresher[,c(
  "NEURO",
  "EXTRA",
  "OPEN",
  "CONS2",
  "AGREE",
  "Edu",
  "Sex",
  "Age",
  "baseline_date",
  "wave",
  "MRID")],
  NSDE_refresher[,c("npos_pm",
                    "npos_pc",
                    "RA2DPOSAV",
                    "RA2DNEGAV",
                    "MRID",
                    "date",
                    "num_pos_events",
                    "NA1", 
                    "NA2", 
                    "NA3", 
                    "NA4", 
                    "NA5", 
                    "NA6", 
                    "NA7", 
                    "NA8", 
                    "NA9", 
                    "NA10",
                    "NA11",
                    "NA12",
                    "NA13",
                    "NA14",
                    "PA1", 
                    "PA2", 
                    "PA3", 
                    "PA4", 
                    "PA5", 
                    "PA6", 
                    "PA7", 
                    "PA8", 
                    "PA9", 
                    "PA10",
                    "PA11",
                    "PA12",
                    "PA13",
                    "day"
  )],
  by = "MRID", all.y = T, all.x = T)

#combine both data sets

midus2$NEGAV <- midus2$B2DNEGAV
midus2$B2DNEGAV <- NULL
midus2$POSAV <- midus2$B2DPOSAV
midus2$B2DPOSAV <- NULL
midus2$ID <- midus2$M2ID
midus2$M2ID <- NULL

refresher$NEGAV <- refresher$RA2DNEGAV
refresher$RA2DNEGAV <- NULL
refresher$POSAV <- refresher$RA2DPOSAV
refresher$RA2DPOSAV <- NULL
refresher$ID <- refresher$MRID
refresher$MRID <- NULL


full <- rbind(midus2,refresher)



##center predictors in the full data set

full$NEURO_c <- scale(full$NEURO, scale = F)[,1]
full$EXTRA_c <- scale(full$EXTRA, scale = F)[,1]
full$OPEN_c <-  scale(full$OPEN , scale = F)[,1]
full$CONS2_c <- scale(full$CONS2, scale = F)[,1]
full$AGREE_c <- scale(full$AGREE, scale = F)[,1]
full$Age_c <- scale(  full$Age, scale = F)[,1]

#grandmeancenter personmean

full$npos_pm_c <- scale(full$npos_pm, scale = F)[,1]

NSDE_refresher$RA2DF8B <- as.numeric(NSDE_refresher$RA2DF8B)
NSDE_refresher$RA2DF9B <- as.numeric(NSDE_refresher$RA2DF9B)
NSDE_refresher$RA2DF10B <- as.numeric(NSDE_refresher$RA2DF10B)
NSDE_refresher$RA2DF11B <- as.numeric(NSDE_refresher$RA2DF11B)
NSDE_refresher$RA2DF12B <- as.numeric(NSDE_refresher$RA2DF12B)



#pleasant

NSDE_refresher$pleasant <- mean_n(NSDE_refresher[,c(
  "RA2DF8B",
  "RA2DF9B",
  "RA2DF10B",
  "RA2DF11B",
  "RA2DF12B")], n = 1)

Baseline_refresher <- NSDE_refresher%>%
  group_by(MRID)%>%
  summarise(pleasant_pm = mean(pleasant, na.rm = T))%>%
  merge(Baseline_refresher,.,by="MRID")

#surprise
NSDE_refresher$RA2DF8WB1 <- as.numeric( NSDE_refresher$RA2DF8WB1 )
NSDE_refresher$RA2DF9WB1 <- as.numeric( NSDE_refresher$RA2DF9WB1 )
NSDE_refresher$RA2DF10WB1 <- as.numeric(NSDE_refresher$RA2DF10WB1)
NSDE_refresher$RA2DF11WB1 <- as.numeric(NSDE_refresher$RA2DF11WB1)
NSDE_refresher$RA2DF12WB1 <- as.numeric(NSDE_refresher$RA2DF12WB1)

NSDE_refresher$surprise <- mean_n(NSDE_refresher[,c(
  "RA2DF8WB1",
  "RA2DF9WB1",
  "RA2DF10WB1",
  "RA2DF11WB1",
  "RA2DF12WB1")], n = 1)

Baseline_refresher <- NSDE_refresher%>%
  group_by(MRID)%>%
  summarise(surprise_pm = mean(surprise, na.rm =T))%>%
  merge(Baseline_refresher,.,by="MRID")

#calm
NSDE_refresher$RA2DF8WB4 <- as.numeric(NSDE_refresher$RA2DF8WB4)
NSDE_refresher$RA2DF9WB4 <- as.numeric(NSDE_refresher$RA2DF9WB4)
NSDE_refresher$RA2DF10WB4 <- as.numeric(NSDE_refresher$RA2DF10WB4)
NSDE_refresher$RA2DF11WB4 <- as.numeric(NSDE_refresher$RA2DF11WB4)
NSDE_refresher$RA2DF12WB4 <- as.numeric(NSDE_refresher$RA2DF12WB4)

NSDE_refresher$calm <- mean_n(NSDE_refresher[,c(
  "RA2DF8WB4",
  "RA2DF9WB4",
  "RA2DF10WB4",
  "RA2DF11WB4",
  "RA2DF12WB4")], n = 1)

Baseline_refresher <- NSDE_refresher%>%
  group_by(MRID)%>%
  summarise(calm_pm = mean(calm, na.rm = T))%>%
  merge(Baseline_refresher,.,by="MRID")

#proud
NSDE_refresher$RA2DF8WB5 <- as.numeric(NSDE_refresher$RA2DF8WB5)
NSDE_refresher$RA2DF9WB5 <- as.numeric(NSDE_refresher$RA2DF9WB5)
NSDE_refresher$RA2DF10WB5 <- as.numeric(NSDE_refresher$RA2DF10WB5)
NSDE_refresher$RA2DF11WB5 <- as.numeric(NSDE_refresher$RA2DF11WB5)
NSDE_refresher$RA2DF12WB5 <- as.numeric(NSDE_refresher$RA2DF12WB5)

NSDE_refresher$proud <- mean_n(NSDE_refresher[,c(
  "RA2DF8WB5",
  "RA2DF9WB5",
  "RA2DF10WB5",
  "RA2DF11WB5",
  "RA2DF12WB5")], n = 1)

Baseline_refresher <- NSDE_refresher%>%
  group_by(MRID)%>%
  summarise(proud_pm = mean(proud, na.rm = T))%>%
  merge(Baseline_refresher,.,by="MRID")

#close

NSDE_refresher$RA2DF8WB8 <-  as.numeric(NSDE_refresher$RA2DF8WB8)
NSDE_refresher$RA2DF9WB8 <-  as.numeric(NSDE_refresher$RA2DF9WB8)
NSDE_refresher$RA2DF10WB8 <- as.numeric(NSDE_refresher$RA2DF10WB8)
NSDE_refresher$RA2DF11WB8 <- as.numeric(NSDE_refresher$RA2DF11WB8)
NSDE_refresher$RA2DF12WB8 <- as.numeric(NSDE_refresher$RA2DF12WB8)

NSDE_refresher$close <- mean_n(NSDE_refresher[,c(
  "RA2DF8WB8",
  "RA2DF9WB8",
  "RA2DF10WB8",
  "RA2DF11WB8",
  "RA2DF12WB8")], n = 1)

Baseline_refresher <- NSDE_refresher%>%
  group_by(MRID)%>%
  summarise(close_pm = mean(close, na.rm =T))%>%
  merge(Baseline_refresher,.,by="MRID")



refresher <- merge(Baseline_refresher[,c("NEURO",
                                         "EXTRA",
                                         "OPEN",
                                         "CONS2",
                                         "AGREE",
                                         "Edu",
                                         "Sex",
                                         "Age",
                                         "wave",
                                         "MRID")],
                   NSDE_refresher[,c("npos_pm",
                                     "npos_pc",
                                     "RA2DPOSAV",
                                     "RA2DNEGAV",
                                     "MRID",
                                     "num_pos_events",
                                     "pleasant",
                                     "calm",
                                     "surprise",
                                     "close",
                                     "proud")],
                   by = "MRID", all.y = T, all.x = T)


#for 3L analysis 
#create 3 level dataset


NSDE_refresher_3L_1 <- data.frame(day = NSDE_refresher$RA2DDAY, MRID =  NSDE_refresher$MRID, pleasant = NSDE_refresher$RA2DF8B ,surprise =NSDE_refresher$RA2DF8WB1 , calm = NSDE_refresher$RA2DF8WB4 , proud = NSDE_refresher$RA2DF8WB5 , close = NSDE_refresher$RA2DF8WB8  )
NSDE_refresher_3L_2 <- data.frame(day = NSDE_refresher$RA2DDAY, MRID =  NSDE_refresher$MRID, pleasant = NSDE_refresher$RA2DF9B ,surprise =NSDE_refresher$RA2DF9WB1 , calm = NSDE_refresher$RA2DF9WB4 , proud = NSDE_refresher$RA2DF9WB5 , close = NSDE_refresher$RA2DF9WB8  )
NSDE_refresher_3L_3 <- data.frame(day = NSDE_refresher$RA2DDAY, MRID =  NSDE_refresher$MRID, pleasant = NSDE_refresher$RA2DF10B,surprise =NSDE_refresher$RA2DF10WB1, calm = NSDE_refresher$RA2DF10WB4, proud = NSDE_refresher$RA2DF10WB5, close = NSDE_refresher$RA2DF10WB8 )
NSDE_refresher_3L_4 <- data.frame(day = NSDE_refresher$RA2DDAY, MRID =  NSDE_refresher$MRID, pleasant = NSDE_refresher$RA2DF11B,surprise =NSDE_refresher$RA2DF11WB1, calm = NSDE_refresher$RA2DF11WB4, proud = NSDE_refresher$RA2DF11WB5, close = NSDE_refresher$RA2DF11WB8 )
NSDE_refresher_3L_5 <- data.frame(day = NSDE_refresher$RA2DDAY, MRID =  NSDE_refresher$MRID, pleasant = NSDE_refresher$RA2DF12B,surprise =NSDE_refresher$RA2DF12WB1, calm = NSDE_refresher$RA2DF12WB4, proud = NSDE_refresher$RA2DF12WB5, close = NSDE_refresher$RA2DF12WB8 )



NSDE_refresher_3L <- rbind(NSDE_refresher_3L_1,
                           NSDE_refresher_3L_2,
                           NSDE_refresher_3L_3,
                           NSDE_refresher_3L_4,
                           NSDE_refresher_3L_5)

refresher_3L <- merge(Baseline_refresher[,c("NEURO",
                                            "EXTRA",
                                            "OPEN",
                                            "CONS2",
                                            "AGREE",
                                            "Edu",
                                            "Sex",
                                            "Age",
                                            "MRID")],
                      NSDE_refresher_3L,
                      by = "MRID", all.y = T, all.x = T)

#personmeancenter subjective emotional experience

NSDE_refresher <- NSDE_refresher%>%
  group_by(MRID)%>%
  summarise(pleasant_pm  = mean(pleasant, na.rm =T),
            calm_pm  = mean(calm, na.rm = T),
            surprise_pm = mean(surprise, na.rm =T),
            close_pm = mean(close, na.rm = T),
            proud_pm = mean(proud, na.rm = T))%>%
  merge(NSDE_refresher,.,by="MRID")

NSDE_refresher$pleasant_pc <- NSDE_refresher$pleasant - NSDE_refresher$pleasant_pm
NSDE_refresher$calm_pc <- NSDE_refresher$calm - NSDE_refresher$calm_pm
NSDE_refresher$surprise_pc <- NSDE_refresher$surprise - NSDE_refresher$surprise_pm
NSDE_refresher$close_pc <- NSDE_refresher$close - NSDE_refresher$close_pm
NSDE_refresher$proud_pc <- NSDE_refresher$proud - NSDE_refresher$proud_pm

#center predictors for analysis

refresher_3L$NEURO_c <- scale(refresher_3L$NEURO, scale = F)[,1]
refresher_3L$EXTRA_c <- scale(refresher_3L$EXTRA, scale = F)[,1]
refresher_3L$OPEN_c <-  scale(refresher_3L$OPEN , scale = F)[,1]
refresher_3L$CONS2_c <- scale(refresher_3L$CONS2, scale = F)[,1]
refresher_3L$AGREE_c <- scale(refresher_3L$AGREE, scale = F)[,1]
refresher_3L$Age_c <-   scale(refresher_3L$Age, scale = F)[,1]

#center in baseline refresher
Baseline_refresher$NEURO_c <- scale(Baseline_refresher$NEURO, scale = F)[,1]
Baseline_refresher$EXTRA_c <- scale(Baseline_refresher$EXTRA, scale = F)[,1]
Baseline_refresher$OPEN_c <-  scale(Baseline_refresher$OPEN , scale = F)[,1]
Baseline_refresher$CONS2_c <- scale(Baseline_refresher$CONS2, scale = F)[,1]
Baseline_refresher$AGREE_c <- scale(Baseline_refresher$AGREE, scale = F)[,1]
Baseline_refresher$Age_c <-   scale(Baseline_refresher$Age, scale = F)[,1]


####Results

#Descriptives####

#Age MIDUS2
describe(Baseline_midus2$Age)

#Age refresher
describe(Baseline_refresher$Age)

#Age combined

describe(full$Age)

#Time between assesments
mean(full$baseline_date - full$date)/12


#gender
table(midus_comb$Sex)/sum(table(midus_comb$Sex))

#ethnicity
table(midus_comb$race)/sum(table(midus_comb$race))

#Education - % of people with some college
as.numeric(table(midus_comb$Edu)/sum(table(midus_comb$Edu)))[2]+
  as.numeric(table(midus_comb$Edu)/sum(table(midus_comb$Edu)))[3]


#ICCC number of pos. events

library(ICC)

ICCest(ID, num_pos_events, full)
ICCest(ID, POSAV, full)
ICCest(ID, NEGAV, full)




#Table 1


apa.cor.table(midus_comb[,c(
  "npos_pm",
  "PA_pm",
  "NA_pm",
  "EXTRA",
  "AGREE",
  "CONS2",
  "NEURO",
  "OPEN")])


#Reliability Analyses####


#Extraversion
#(a, f, k, w, aa)

Baseline_midus2$E1 <- Baseline_midus2$B1SE6A
Baseline_midus2$E2 <- Baseline_midus2$B1SE6F
Baseline_midus2$E3 <- Baseline_midus2$B1SE6K
Baseline_midus2$E4 <- Baseline_midus2$B1SE6W
Baseline_midus2$E5 <- Baseline_midus2$B1SE6AA


#Agreeableness
#b, g, l, r, z

Baseline_midus2$A1 <- Baseline_midus2$B1SE6B
Baseline_midus2$A2 <- Baseline_midus2$B1SE6G
Baseline_midus2$A3 <- Baseline_midus2$B1SE6L
Baseline_midus2$A4 <- Baseline_midus2$B1SE6R
Baseline_midus2$A5 <- Baseline_midus2$B1SE6Z

#Conscientiousness
#d, i, p, x (reverse), ee 

Baseline_midus2$C1 <- Baseline_midus2$B1SE6D
Baseline_midus2$C2 <- Baseline_midus2$B1SE6I
Baseline_midus2$C3 <- Baseline_midus2$B1SE6P
Baseline_midus2$C4 <- Baseline_midus2$B1SE6X #reverse
Baseline_midus2$C5 <- Baseline_midus2$B1SE6EE


#Neuroticism
#c, h, m, s (reverse)

Baseline_midus2$N1 <- Baseline_midus2$B1SE6C
Baseline_midus2$N2 <- Baseline_midus2$B1SE6H
Baseline_midus2$N3 <- Baseline_midus2$B1SE6M #reverse
Baseline_midus2$N4 <- Baseline_midus2$B1SE6S

#Openness
#(n, q, u, v, y, bb, cc)

Baseline_midus2$O1 <- Baseline_midus2$B1SE6N
Baseline_midus2$O2 <- Baseline_midus2$B1SE6Q
Baseline_midus2$O3 <- Baseline_midus2$B1SE6U
Baseline_midus2$O4 <- Baseline_midus2$B1SE6V
Baseline_midus2$O5 <- Baseline_midus2$B1SE6Y
Baseline_midus2$O6 <- Baseline_midus2$B1SE6BB
Baseline_midus2$O7 <- Baseline_midus2$B1SE6CC

##For refresher

fac_num <- function(var){
  library(prettyR)
  lbls <- sort(levels(var))
  lbls <- (sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
  var <- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", var))
  var <- add.value.labels(var, lbls)
  return(var)
}



#Extraversion
#(a, f, k, w, aa)

Baseline_refresher$E1 <- Baseline_refresher$RA1SF6A %>% fac_num()
Baseline_refresher$E2 <- Baseline_refresher$RA1SF6F %>% fac_num()
Baseline_refresher$E3 <- Baseline_refresher$RA1SF6K %>% fac_num()
Baseline_refresher$E4 <- Baseline_refresher$RA1SF6W %>% fac_num()
Baseline_refresher$E5 <- Baseline_refresher$RA1SF6AA %>% fac_num()


#Agreeableness
#b, g, l, r, z

Baseline_refresher$A1 <- Baseline_refresher$RA1SF6B  %>% fac_num()
Baseline_refresher$A2 <- Baseline_refresher$RA1SF6G %>% fac_num()
Baseline_refresher$A3 <- Baseline_refresher$RA1SF6L %>% fac_num()
Baseline_refresher$A4 <- Baseline_refresher$RA1SF6R %>% fac_num()
Baseline_refresher$A5 <- Baseline_refresher$RA1SF6Z %>% fac_num()

#Conscientiousness
#d, i, p, x (reverse), ee 

Baseline_refresher$C1 <- Baseline_refresher$RA1SF6D  %>% fac_num()
Baseline_refresher$C2 <- Baseline_refresher$RA1SF6I  %>% fac_num()
Baseline_refresher$C3 <- Baseline_refresher$RA1SF6P  %>% fac_num()
Baseline_refresher$C4 <- Baseline_refresher$RA1SF6X   %>% fac_num()#reverse
Baseline_refresher$C5 <- Baseline_refresher$RA1SF6EE  %>% fac_num()


#Neuroticism  
#c, h, m, s (reverse)

Baseline_refresher$N1 <- Baseline_refresher$RA1SF6C%>% fac_num()
Baseline_refresher$N2 <- Baseline_refresher$RA1SF6H%>% fac_num()
Baseline_refresher$N3 <- Baseline_refresher$RA1SF6M%>% fac_num()    #reverse
Baseline_refresher$N4 <- Baseline_refresher$RA1SF6S%>% fac_num()

#Openness
#(n, q, u, v, y, bb, cc)

Baseline_refresher$O1 <- Baseline_refresher$RA1SF6N   %>% fac_num()
Baseline_refresher$O2 <- Baseline_refresher$RA1SF6Q %>% fac_num()
Baseline_refresher$O3 <- Baseline_refresher$RA1SF6U %>% fac_num()
Baseline_refresher$O4 <- Baseline_refresher$RA1SF6V %>% fac_num()
Baseline_refresher$O5 <- Baseline_refresher$RA1SF6Y %>% fac_num()
Baseline_refresher$O6 <- Baseline_refresher$RA1SF6BB %>% fac_num()
Baseline_refresher$O7 <- Baseline_refresher$RA1SF6CC %>% fac_num()


#combine both datasets

vars_pers <- c("E1", "E2", "E3", "E4", "E5",
               "A1", "A2", "A3", "A4", "A5",
               "C1", "C2", "C3", "C4", "C5",
               "N1", "N2", "N3", "N4",
               "O1", "O2", "O3", "O4", "O5", "O6", "O7")

pers <- rbind(
  Baseline_midus2[,vars_pers],
  Baseline_refresher[,vars_pers]
)

vars_pers <- NULL

alpha(x = pers[,c("E1", "E2", "E3", "E4", "E5")])[1]
alpha(x = pers[,c("A1", "A2", "A3", "A4", "A5")])[1]
alpha(x = pers[,c("C1", "C2", "C3", "C4", "C5")], keys = c("C4"))[1]
alpha(x = pers[,c("N1", "N2", "N3", "N4")], keys = c("N4"))[1]
alpha(x = pers[,c("O1", "O2", "O3", "O4", "O5", "O6", "O7")])[1]






#Engagement in pos. events####



lmer(num_pos_events ~ 
       EXTRA_c+
       AGREE_c+
       CONS2_c+
       NEURO_c+
       OPEN_c +
       Edu+
       Sex+
       Age_c + (1|ID), data = full) %>%summary

lmer(num_pos_events ~ 
       EXTRA_c+
       AGREE_c+
       CONS2_c+
       NEURO_c+
       OPEN_c +
       Edu+
       Sex+
       Age_c + (1|ID), data = full) %>%confint(oldNames = F)

library(broom.mixed)
m_engage <- lmer(num_pos_events ~ 
       EXTRA_c+
       AGREE_c+
       CONS2_c+
       NEURO_c+
       OPEN_c +
       Edu+
       Sex+
       Age_c + (1|ID), data = full) %>%tidy

#How many more positive events does a person high in E compared to a person low in E report?

#SD of E
sd_E <- sd(midus_comb$EXTRA_c, na.rm = T)

#expected value for -1SD on E

m_engage$estimate[which(m_engage$term == "(Intercept)")] +
  m_engage$estimate[which(m_engage$term == "EXTRA_c")] * -sd_E

#expected value for +1SD on E

m_engage$estimate[which(m_engage$term == "(Intercept)")] +
  m_engage$estimate[which(m_engage$term == "EXTRA_c")] * sd_E

#difference

(m_engage$estimate[which(m_engage$term == "(Intercept)")] +
  m_engage$estimate[which(m_engage$term == "EXTRA_c")] * sd_E) -
 ( m_engage$estimate[which(m_engage$term == "(Intercept)")] +
  m_engage$estimate[which(m_engage$term == "EXTRA_c")] * -sd_E) 

#for a week

((m_engage$estimate[which(m_engage$term == "(Intercept)")] +
    m_engage$estimate[which(m_engage$term == "EXTRA_c")] * sd_E) -
  ( m_engage$estimate[which(m_engage$term == "(Intercept)")] +
      m_engage$estimate[which(m_engage$term == "EXTRA_c")] * -sd_E) )*7

#How many more positive events does a person high in O compared to a person low in O report?

#SD of O
sd_O <- sd(midus_comb$OPEN_c, na.rm = T)

#expected value for -1SD on O

m_engage$estimate[which(m_engage$term == "(Intercept)")] +
  m_engage$estimate[which(m_engage$term == "OPEN_c")] * -sd_O

#expected value for +1SD on O

m_engage$estimate[which(m_engage$term == "(Intercept)")] +
  m_engage$estimate[which(m_engage$term == "OPEN_c")] * sd_O

#difference

(m_engage$estimate[which(m_engage$term == "(Intercept)")] +
    m_engage$estimate[which(m_engage$term == "OPEN_c")] * sd_O) -
  ( m_engage$estimate[which(m_engage$term == "(Intercept)")] +
      m_engage$estimate[which(m_engage$term == "OPEN_c")] * -sd_O) 

#for a week

((m_engage$estimate[which(m_engage$term == "(Intercept)")] +
    m_engage$estimate[which(m_engage$term == "OPEN_c")] * sd_O) -
    ( m_engage$estimate[which(m_engage$term == "(Intercept)")] +
        m_engage$estimate[which(m_engage$term == "OPEN_c")] * -sd_O) )*7


#Subjective emotional experience during pos. events####



#Outcome: pleasant



lmer(pleasant ~  EXTRA_c +   AGREE_c + CONS2_c + NEURO_c + OPEN_c +
       Edu + Sex + Age_c + (1|MRID:day) + (1|MRID), data = refresher_3L)%>% summary()

lmer(pleasant ~  EXTRA_c +   AGREE_c + CONS2_c + NEURO_c + OPEN_c +
       Edu + Sex + Age_c + (1|MRID:day) + (1|MRID), data = refresher_3L)%>% confint(oldNames = F)

#Outcome: calm


lmer(calm ~  EXTRA_c +   AGREE_c + CONS2_c + NEURO_c + OPEN_c +
       Edu + Sex + Age_c + (1|MRID:day) + (1|MRID), data = refresher_3L)%>% summary()

lmer(calm ~  EXTRA_c +   AGREE_c + CONS2_c + NEURO_c + OPEN_c +
       Edu + Sex + Age_c + (1|MRID:day) + (1|MRID), data = refresher_3L)%>% confint(oldNames = F)

#Outcome: surprise


lmer(surprise ~  EXTRA_c +   AGREE_c + CONS2_c + NEURO_c + OPEN_c +
       Edu + Sex + Age_c + (1|MRID:day) + (1|MRID), data = refresher_3L)%>% summary()

lmer(surprise ~  EXTRA_c +   AGREE_c + CONS2_c + NEURO_c + OPEN_c +
       Edu + Sex + Age_c + (1|MRID:day) + (1|MRID), data = refresher_3L)%>%confint()


#Outcome: close to others



lmer(close ~  EXTRA_c +   AGREE_c + CONS2_c + NEURO_c + OPEN_c +
       Edu + Sex + Age_c + (1|MRID:day) + (1|MRID), data = refresher_3L)%>% summary()

lmer(close ~  EXTRA_c +   AGREE_c + CONS2_c + NEURO_c + OPEN_c +
       Edu + Sex + Age_c + (1|MRID:day) + (1|MRID), data = refresher_3L)%>% confint(method = "profile", devtol = 1e8) #profile confidence intervals were not available

#Outcome: proud



lmer(proud ~  EXTRA_c +   AGREE_c + CONS2_c + NEURO_c + OPEN_c +
       Edu + Sex + Age_c + (1|MRID:day) + (1|MRID), data = refresher_3L)%>% summary()

lmer(proud ~  EXTRA_c +   AGREE_c + CONS2_c + NEURO_c + OPEN_c +
       Edu + Sex + Age_c + (1|MRID:day) + (1|MRID), data = refresher_3L)%>% confint(oldNames = F)


#Affective Responsiveness to positive events####

#Outcome: Positive Affect

respo_pa <- lmer(POSAV ~ 
        npos_pc* EXTRA_c  +
       AGREE_c * npos_pc +
       CONS2_c * npos_pc +
       NEURO_c * npos_pc +
       OPEN_c * npos_pc +
       Sex + Age_c + Edu + npos_pm_c+   (1 + npos_pc| ID),
     data = full) 

respo_pa %>% summary()

respo_pa %>% confint(oldNames=F)

library(interactions)

respo_pa %>% sim_slopes(
  pred = "npos_pc",
  modx = "EXTRA_c"
)

respo_pa %>% sim_slopes(
  pred = "npos_pc",
  modx = "NEURO_c"
)


full$POSAV %>% is.na %>% sum

####CODE for plots

#plot Extra


plot_pa_extr <- 
  respo_pa%>%plot_model(type = "eff", terms = c( "npos_pc", "EXTRA_c"),
                                               title ="", 
                                               axis.title = c("# of positive events (person centered)",
                                                              "daily positive affect [0-4]"),
                                               legend.title = "Extraversion", colors =  "bw")+ylim(0,4)+
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), labels = c("-1SD", "mean", "+1SD"))+
  #scale_color_manual(values = c("red", "blue", "green"),)+
  theme(axis.title.x = element_text( size=16),
        axis.title.y = element_text( size=16),
        legend.position = c(0.45, 0.95),
        legend.direction = "horizontal",
        legend.title = element_text( size = 16),
        legend.text = element_text( size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))



#Neuro


plot_pa_neuro <-respo_pa%>%plot_model(type = "eff", terms = c( "npos_pc", "NEURO_c"),
                                                title ="", 
                                                axis.title = c("# of positive events (person centered)",
                                                               "daily positive affect [0-4]"),
                                                legend.title = "Neuroticism", color = "bw"
                      )+ylim(0,4)+
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), labels = c("-1SD", "mean", "+1SD"))+
 # scale_color_manual(values = c("red", "blue", "green"),labels = c("-1SD", "mean", "+1SD"))+
  theme(axis.title.x = element_text( size=16),
        axis.title.y = element_text( size=16),
        legend.position = c(0.45, 0.95),
        legend.direction = "horizontal",
        legend.title = element_text( size = 16),
        legend.text = element_text( size = 16),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))


ggarrange(plot_pa_extr,plot_pa_neuro,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

ggsave(filename = "Figure_3_2.jpg", height = 6, width = 12)

#Outcome: Negative Affect

respo_na <- lmer(NEGAV ~ 
                   EXTRA_c * npos_pc +
                   AGREE_c * npos_pc +
                   CONS2_c * npos_pc +
                   NEURO_c * npos_pc +
                   OPEN_c * npos_pc +
                   Sex + Age_c + Edu + npos_pm_c+  (1 + npos_pc| ID),
                 data = full) 

respo_na %>% summary()

respo_na %>% confint(oldNames = F)
