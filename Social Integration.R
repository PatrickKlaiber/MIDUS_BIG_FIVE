


#b2da4am #time spent in PA



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
library(reghelper)
library(lm.beta)
library(ggpubr)


#load datasets####

load("data/MIDUS2_baseline.rda")
Baseline_midus2 <- M2_P1_Milwaukee_P1_MERGED_N_5555_3_7_12_sav
rm(M2_P1_Milwaukee_P1_MERGED_N_5555_3_7_12_sav)


load("data/MIDUS2_NSDE.rda")
NSDE_midus2 <- da26841.0001 #rename data set
rm(da26841.0001)



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





#personmean
NSDE_midus2 <- NSDE_midus2%>%
  group_by(M2ID)%>%
  summarise(npos_pm = mean(num_pos_events)
            )%>%
  merge(NSDE_midus2,.,by ="M2ID")


Baseline_midus2 <- NSDE_midus2%>%
  group_by(M2ID)%>%
  summarise(npos_pm = mean(num_pos_events)
            )%>%
  merge(Baseline_midus2,.,by ="M2ID")
#person-mean center

NSDE_midus2$npos_pc <- with(NSDE_midus2, num_pos_events - npos_pm)

#create personmean of positive and negative affect

Baseline_midus2 <- NSDE_midus2%>%
  group_by(M2ID)%>%
  summarise(NA_pm = mean(B2DNEGAV, na.rm = T),
            PA_pm = mean(B2DPOSAV, na.rm = T))%>%
  merge(Baseline_midus2,., by = "M2ID")

#recode education

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

Baseline_midus2$race <- Baseline_midus2$B1PF7A%>%as.numeric%>%
  recode_factor(`1` = "(1) WHITE",
                `2` = "(2) BLACK AND/OR AFRICAN AMERICAN",
                `3` = "(3) NATIVE AMERICAN OR ALASKA NATIVE ALEUTIAN ISLANDER/ESKIMO",
                `4` = "(4) ASIAN",
                `5` = "(5) NATIVE HAWAIIAN OR PACIFIC ISLANDER",
                `6` = "(6) OTHER (SPECIFY)")


Baseline_midus2$Sex <- Baseline_midus2$B1PRSEX

#create score of physical activity

NSDE_midus2$phys_act <- (NSDE_midus2$B2DA4AH*60) + NSDE_midus2$B2DA4AM

NSDE_midus2 <- NSDE_midus2%>%
  group_by(M2ID)%>%
  summarise(phys_act_pm = mean(phys_act)
  )%>%
  merge(NSDE_midus2,.,by ="M2ID")


Baseline_midus2 <- NSDE_midus2%>%
  group_by(M2ID)%>%
  summarise(phys_act_pm = mean(phys_act)
  )%>%
  merge(Baseline_midus2,.,by ="M2ID")

NSDE_midus2$phys_act_pc <- NSDE_midus2$phys_act - NSDE_midus2$phys_act_pm 

Baseline_midus2$NEURO <- Baseline_midus2$B1SNEURO
Baseline_midus2$EXTRA <- Baseline_midus2$B1SEXTRA
Baseline_midus2$OPEN <-  Baseline_midus2$B1SOPEN 
Baseline_midus2$CONS2 <- Baseline_midus2$B1SCONS2
Baseline_midus2$AGREE <- Baseline_midus2$B1SAGREE
Baseline_midus2$Age <-   Baseline_midus2$B1PAGE_M2
Baseline_midus2$Sex <-  Baseline_midus2$B1PRSEX %>%as.numeric%>%
  recode_factor(`1` = "(1) MALE", `2` = "(2) FEMALE")
Baseline_midus2$AGENCY <- Baseline_midus2$B1SAGENC


NSDE_midus2 <- within(NSDE_midus2, B2DA_STR <- relevel(B2DA_STR, ref = "(2) NO"))

midus2 <- merge(Baseline_midus2[,],
  NSDE_midus2[,],
  by = "M2ID", all.y = T, all.x = T)



midus2_full <- midus2[which(is.na(midus2$npos_pc) == F & is.na(midus2$phys_act_pc)==F ),]

m0 <- lmer(B2DPOSAV ~ 1 + (1 |M2ID), data = midus2_full)
m1 <- lmer(B2DPOSAV ~ npos_pc  + (1|M2ID), data = midus2_full)
m1_r <- lmer(B2DPOSAV ~ npos_pc  + (1+npos_pc|M2ID), data = midus2_full)
m2 <- lmer(B2DPOSAV ~ npos_pc + phys_act_pc  + (1+npos_pc |M2ID), data = midus2_full)
m2_r <- lmer(B2DPOSAV ~ npos_pc + phys_act_pc  + (1+npos_pc +phys_act_pc |M2ID), data = midus2_full)
m3 <- lmer(B2DPOSAV ~ npos_pc * phys_act_pc  + (1+npos_pc |M2ID), data = midus2_full)
m4 <- lmer(B2DPOSAV ~ npos_pc * phys_act_pc  + npos_pc *phys_act_pm.x +(1+npos_pc |M2ID), data = midus2_full)
summary(m4)

anova(m0,m1)
anova(m1,m1_r, refit = F)

anova(m1_r, m2)
anova(m2, m2_r, refit = F)

anova(m2, m3)


#center variables

midus2$socint_c <- scale(midus2$B1SSWBSI, scale = F)[,1]
midus2$Age_c <- scale(midus2$B1PAGE_M2.x, scale = F)[,1]
midus2$NEURO_c <- scale(midus2$NEURO, scale = F)[,1]
midus2$EXTRA_c <- scale(midus2$EXTRA, scale = F)[,1]
midus2$AGREE_c <- scale(midus2$AGREE, scale = F)[,1]
midus2$CONS_c <- scale(midus2$CONS2, scale = F)[,1]
midus2$OPEN_c <- scale(midus2$OPEN, scale = F)[,1]
midus2$PURPOSE_c <- scale(midus2$B1SPWBU2, scale = F)[,1]
midus2$npos_pm_c <- scale(midus2$npos_pm.x, scale = F)[,1]



###on days when people report being more physically active they report less event-related affect

#Social Integration is predicitive of engaging in more pos. events, but less responsive to any given pos. event

(m_engagement <- lmer(num_pos_events ~ socint_c + Edu + Sex +  Age_c +
       NEURO_c + EXTRA_c + AGREE_c + CONS_c+ OPEN_c + PURPOSE_c + B2DA_STR +(1|M2ID), data = midus2))%>% summary

library(sjPlot)

m_engagement%>%tab_model()

(m_respo <- lmer(B2DPOSAV ~ npos_pc * socint_c + NEURO_c + EXTRA_c + AGREE_c + CONS_c+ OPEN_c + 
       PURPOSE_c + Edu + Sex + B2DA_STR + npos_pm_c +(1+npos_pc |M2ID), data = midus2))%>% summary

plot_model(m_respo, type = "pred", title ="", terms = c("npos_pc", "socint_c"), 
                                            axis.title = c("# of positive events (person centered)",
                                                           "daily positive affect [0-4]"),
                                            legend.title = "Social Integration", ci.lvl = NA,
)+ylim(2.5,3.25)+
  scale_color_manual(values = c("red", "blue", "green"),labels = c("-1SD", "mean", "+1SD"))+
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

library(interactions)
sim_slopes(m_respo, pred = "npos_pc", modx = "socint_c")


lmer(B2DNEGAV ~ npos_pc * B1SSWBSI + NEURO + EXTRA + AGREE + CONS2+ OPEN + B1SPWBU2 + Edu + Sex + B2DA_STR +(1+npos_pc + B2DA_STR |M2ID), data = midus2)%>% summary

describe(Baseline_midus2$B1PAGE_M2)

Baseline_midus2$B1PF7A %>% table %>% prop.table()

++-describe(midus2$npos_pm.x)

table(Baseline_midus2$Sex)%>%prop.table()

table(Baseline_midus2$Edu)%>%prop.table

#Age mod
lmer(num_pos_events ~ socint_c*Age_c + Edu + Sex +
                       NEURO_c + EXTRA_c + AGREE_c + CONS_c+ OPEN_c + 
       PURPOSE_c + B2DA_STR +(1|M2ID), data = midus2)%>% summary

lmer(B2DPOSAV ~ npos_pc * socint_c *Age_c + NEURO_c + EXTRA_c + AGREE_c + CONS_c+ OPEN_c + 
                   PURPOSE_c + Edu + Sex + B2DA_STR + npos_pm_c +(1+npos_pc |M2ID), data = midus2)%>% summary
 