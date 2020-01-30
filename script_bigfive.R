###BIG Five Script

options(scipen = 8)#prefer fixed over scientific notation

#load packages####

library(Hmisc)
library(tidyverse)
library(lmerTest)
library(sjPlot)
library(sjstats)
library(psych)
library(papaja)
library(stargazer)
library(apaTables)
library(broom.mixed)

#load datasets####

load("data/refresher_NSDE.rda")
NSDE_refresher <- da37083.0001
rm(da37083.0001)

load("data/refresher_baseline.rda")
Baseline_refresher <- da36532.0001
rm(da36532.0001) 

load("data/MIDUS2_baseline.rda")
Baseline_midus2 <- M2_P1_Milwaukee_P1_MERGED_N_5555_3_7_12_sav
rm(M2_P1_Milwaukee_P1_MERGED_N_5555_3_7_12_sav)


load("data/MIDUS2_NSDE.rda")
NSDE_midus2 <- da26841.0001 #rename data set
rm(da26841.0001)


#exclude participant from the baseline questionnaire if they are not part of the NSDE sample

Baseline_refresher <- Baseline_refresher[which(
  Baseline_refresher$MRID %in% NSDE_refresher$MRID),]


Baseline_midus2 <- Baseline_midus2[which(
  Baseline_midus2$M2ID %in% NSDE_midus2$M2ID),]


#Data prep####


##recode and create sumscore of daily events
#the daily pos events variable of the original data set RA2DN_POS
#it has values from 0-6, although it should only have values from 0 t0 5, because it
#is a sum of 5 dichotomous variables

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
  summarise(npos_pm = mean(num_pos_events),
            pos_social_pm = mean(pos_social, na.rm = T))%>%
  merge(NSDE_midus2,.,by ="M2ID")


Baseline_midus2 <- NSDE_midus2%>%
  group_by(M2ID)%>%
  summarise(npos_pm = mean(num_pos_events),
            pos_social_pm = mean(pos_social, na.rm = T))%>%
  merge(Baseline_midus2,.,by ="M2ID")

#personmean 

NSDE_refresher <- NSDE_refresher%>%
  group_by(MRID)%>%
  summarise(npos_pm = mean(num_pos_events),
            pos_social_pm = mean(pos_social, na.rm = T))%>%
  merge(NSDE_refresher,.,by ="MRID")

Baseline_refresher <- NSDE_refresher%>%
  group_by(MRID)%>%
  summarise(npos_pm = mean(num_pos_events),
            pos_social_pm = mean(pos_social, na.rm = T))%>%
  merge(Baseline_refresher,.,by ="MRID")

#personmeancenter numb of pos events

NSDE_refresher$npos_pc <- with(NSDE_refresher, num_pos_events - npos_pm)
NSDE_midus2$npos_pc <- with(NSDE_midus2, num_pos_events - npos_pm)
NSDE_refresher$pos_social_pc <- with(NSDE_refresher, pos_social - pos_social_pm)
NSDE_midus2$pos_social_pc <- with(NSDE_midus2, pos_social - pos_social_pm)

#create personmean of positive and negative affect

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
  "pos_social_pm",
  "npos_pm"
  
)



midus_comb <- rbind(Baseline_midus2[,vars], Baseline_refresher[,vars])

###center predictors in midus comb

#center predictors for analysis

midus_comb$NEURO_c <- scale(midus_comb$NEURO, scale = F)
midus_comb$EXTRA_c <- scale(midus_comb$EXTRA, scale = F)
midus_comb$OPEN_c <-  scale(midus_comb$OPEN , scale = F)
midus_comb$CONS2_c <- scale(midus_comb$CONS2, scale = F)
midus_comb$AGREE_c <- scale(midus_comb$AGREE, scale = F)
midus_comb$Age_c <- scale(midus_comb$Age, scale = F)










#grandcenter personmean

NSDE_midus2$npos_pm_c <- scale(NSDE_midus2$npos_pm, scale = F)
Baseline_midus2$npos_pm_c <- scale(Baseline_midus2$npos_pm, scale = F)
NSDE_midus2$pos_social_pm_c <- scale(NSDE_midus2$pos_social_pm, scale = F)
Baseline_midus2$pos_social_pm_c <- scale(Baseline_midus2$pos_social_pm, scale = F)










#combine variables





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
  "M2ID")],
  NSDE_midus2[,c("npos_pm",
                 "npos_pc",
                 "B2DPOSAV",
                 "B2DNEGAV",
                 "date",
                 "M2ID",
                 "num_pos_events",
                 "pos_social",
                 "pos_social_pm",
                 "pos_social_pc")],
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
  "MRID")],
  NSDE_refresher[,c("npos_pm",
                    "npos_pc",
                    "RA2DPOSAV",
                    "RA2DNEGAV",
                    "MRID",
                    "date",
                    "num_pos_events",
                    "pos_social",
                    "pos_social_pm",
                    "pos_social_pc"
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


#dichotomous predictor

full$npos_dicho <- full$num_pos_events%>%
  recode_factor(`0` = "no",
                `1` = "yes",
                `2` = "yes",
                `3` = "yes",
                `4` = "yes",
                `5` = "yes")


##center predictors in the full data set

full$NEURO_c <- scale(full$NEURO, scale = F)
full$EXTRA_c <- scale(full$EXTRA, scale = F)
full$OPEN_c <-  scale(full$OPEN , scale = F)
full$CONS2_c <- scale(full$CONS2, scale = F)
full$AGREE_c <- scale(full$AGREE, scale = F)
full$Age_c <- scale(  full$Age, scale = F)

#grandmeancenter personmean

full$npos_pm_c <- scale(full$npos_pm, scale = F)
full$pos_social_pm_c <- scale(full$pos_social_pm, scale = F)



#pleasant
NSDE_refresher$RA2DF8B <- as.numeric(NSDE_refresher$RA2DF8B)
NSDE_refresher$RA2DF9B <- as.numeric(NSDE_refresher$RA2DF9B)
NSDE_refresher$RA2DF10B <- as.numeric(NSDE_refresher$RA2DF10B)
NSDE_refresher$RA2DF11B <- as.numeric(NSDE_refresher$RA2DF11B)
NSDE_refresher$RA2DF12B <- as.numeric(NSDE_refresher$RA2DF12B)

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
NSDE_refresher$RA2DF8WB1 <- as.numeric(NSDE_refresher$RA2DF8WB1)
NSDE_refresher$RA2DF9WB1 <- as.numeric(NSDE_refresher$RA2DF9WB1)
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

refresher$NEURO_c <- scale(refresher$NEURO, scale = F)
refresher$EXTRA_c <- scale(refresher$EXTRA, scale = F)
refresher$OPEN_c <-  scale(refresher$OPEN , scale = F)
refresher$CONS2_c <- scale(refresher$CONS2, scale = F)
refresher$AGREE_c <- scale(refresher$AGREE, scale = F)
refresher$Age_c <-   scale(refresher$Age, scale = F)

#center in baseline refresher
Baseline_refresher$NEURO_c <- scale(Baseline_refresher$NEURO, scale = F)
Baseline_refresher$EXTRA_c <- scale(Baseline_refresher$EXTRA, scale = F)
Baseline_refresher$OPEN_c <-  scale(Baseline_refresher$OPEN , scale = F)
Baseline_refresher$CONS2_c <- scale(Baseline_refresher$CONS2, scale = F)
Baseline_refresher$AGREE_c <- scale(Baseline_refresher$AGREE, scale = F)
Baseline_refresher$Age_c <-   scale(Baseline_refresher$Age, scale = F)


#function for outputs

mlm_table <- function(model)
{library(broom.mixed)
  
  estimate_mlm <- function(model, term)
  {library(broom.mixed)
    model_tibble <- tidy(model)
    estimate <- printnum(as.numeric(model_tibble$estimate[which(model_tibble$term == term)]))
    std.error <- printnum(as.numeric(model_tibble$std.error[which(model_tibble$term == term)]))
    return(paste(estimate," (",std.error,")", sep =""))}
  
  t_stats_mlm <- function(model, term)
  {library(broom.mixed)
    model_tibble <- tidy(model)
    
    df <- round(as.numeric(model_tibble$df[which(model_tibble$term == term)],2))
    t <- printnum(as.numeric(model_tibble$statistic[which(model_tibble$term == term)]))
    p <- printp(as.numeric(model_tibble$p.value[which(model_tibble$term == term)]))
    return(ifelse(model_tibble$p.value[which(model_tibble$term == term)] >= 0.001,
                  (paste("$t$(",df,") = ",t,", $p$ = ",p, sep = "")),
                  (paste("$t$(",df,") = ",t,", $p$ ",p, sep = ""))))}
  
  
  df <- data.frame(term = rownames(model@vcov_beta),
                   estimate = estimate_mlm(model, term = rownames(model@vcov_beta)),
                   t_statistic = t_stats_mlm(model, term = rownames(model@vcov_beta)))
  df$t_statistic <- as.character(df$t_statistic)
  df$term <- as.character(df$term)
  #variable_label(df) <- list("term" = NULL,  "estimate" = "$b$ ($SE$)", "t_statistic" = "$t$-statistics")
  
  vars <- as.data.frame(summary(model)$var)
  
  
  df <- rbind(df, data.frame(term = "ICC", estimate = printnum(vars$vcov[1] / sum(vars$vcov), gt1 = FALSE), t_statistic = " "))
  
  variable_label(df$estimate) <- "$b$ ($SE$)"
  variable_label(df$t_statistic) <- "$t$-statistic"
  return(df)
}

simple_slopes_apa <- function(model, terms)
{library(reghelper)
  library(papaja)
  slope_stats <-  simple_slopes(model, levels = terms)
  estimate <- printnum(slope_stats$`Test Estimate`)
  LCI <- printnum(slope_stats$`Test Estimate` - (1.96*slope_stats$`Std. Error`))
  UCI <- printnum(slope_stats$`Test Estimate` + (1.96*slope_stats$`Std. Error`))
  
  return(paste("$simple$ $slope$ = ",estimate," , 95%-$CI$ = [",LCI,"; ",UCI,"]", sep = ""))}

#Descriptives####

#Age MIDUS2
describe(Baseline_midus2$Age)

#Age refresher
describe(Baseline_refresher$Age)


#Time between assesments
mean(full$baseline_date - full$date)/12


#gender
table(midus_comb$Sex)/sum(table(midus_comb$Sex))

#ethnicity
table(midus_comb$race)/sum(table(midus_comb$race))

#Education - % of people with some college
as.numeric(table(midus_comb$Edu)/sum(table(midus_comb$Edu)))[2]+
  as.numeric(table(midus_comb$Edu)/sum(table(midus_comb$Edu)))[3]


#number of pos. events per day

lmer(num_pos_events~1+(1|ID), data = full)

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

#Engagement in pos. events

lm(npos_pm ~ 
     scale(NEURO_c)+
     scale(EXTRA_c)+
     scale(OPEN_c )+
     scale(CONS2_c)+
     scale(AGREE_c)+
     Edu+
     Sex+
     Age_c, data = midus_comb) %>% summary

lmer(num_pos_events ~ 
       scale(NEURO_c)+
       scale(EXTRA_c)+
       scale(OPEN_c )+
       scale(CONS2_c)+
       scale(AGREE_c)+
       Edu+
       Sex+
       Age_c + (1|ID), data = full) %>%summary




