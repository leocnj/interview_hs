# hs_analyze.R
# 
# Analyze HS results from both SELT and CAWRS
#
#
#library(plyr)
library(dplyr)
library(irr)
library(magrittr)
library(tidyr)

# make sure to run pers_sc.R to obtain SELT ratings.
# - pers_twornds 2514x9
# - selt.ave other non-personality ratings obtained in the round 1.

# make sure to run bars_sc.R to obtain
# - bars_indv
# - bars_persubj_final

# IRR analysis.

#rBP	rDE	rJS	rMC	rRV
irr_pertype <- pers_all %>%
  group_by(type) %>%
  summarise(icc=icc(cbind(rBP, rDE, rJS, rMC, rRV))$value,
            corr=meancor(cbind(rBP, rDE, rJS, rMC, rRV))$value)

# also including round-1's personality+holistic ratings
irr_pertype2 <- pers_twornds %>%
  group_by(type) %>%
  summarise(icc=icc(cbind(rBP, rDE, rJS, rMC, rRV, R1A, R1B))$value,
            corr=meancor(cbind(rBP, rDE, rJS, rMC, rRV, R1A, R1B))$value)
# BARS coding
# per item
irr_bars1 <- bars_indv %>%
  group_by(gpid) %>%
  summarise(icc=icc(cbind(R1,R2,R3,R4,R5,R6,R7,R8,R9,R10))$value,
            corr=meancor(cbind(R1,R2,R3,R4,R5,R6,R7,R8,R9,R10))$value)

# per question group
irr_bars2 <- bars_indv %>%
  group_by(gp) %>%
  summarise(icc=icc(cbind(R1,R2,R3,R4,R5,R6,R7,R8,R9,R10))$value,
            corr=meancor(cbind(R1,R2,R3,R4,R5,R6,R7,R8,R9,R10))$value)

# create wide table for lm() analysis
# note that each cell contains a score averaged from multiple ratings.

# per subj level
# putting two subj level wide tables together
wide_subj <- merge(pers_wide_subj, bars_persubj_final, by="subj")
fit.subj <- lm(OverallHir~AGREE+CONSC+EMSTB+EXTRAV+OPEN+Holistic, data=wide_subj)

# per item level
# for each question group, use SELT scores to predict CAWRS ratings.
# use selt.ave
wide_item <- merge(selt.ave, bars_indv[,c("videoID", "mean", "gpid", "gp")], by="videoID")

# gp01: Communication:      R^2     0.355 auth
# gp02: Leadership:                 0.566  
# gp03: Persuasion and negotiation: 0.438
# gp04: Teamwork:                   0.433

form <- mean~frd+nvs+awk+conf+eng+exc+clm+auth+nvb+acct+mono+und+prof+soph+coh+hol

fit.gp01<- lm(form, filter(wide_item, gp=="G1"))
fit.gp02<- lm(form, filter(wide_item, gp=="G2"))
fit.gp03<- lm(form, filter(wide_item, gp=="G3"))
fit.gp04<- lm(form, filter(wide_item, gp=="G4"))

# save DFs
save(pers_twornds, df.rnd1.other, bars_indv, bars_persubj, wide_subj, wide_item,
     file="vi2014.RData")

# 2/26/2016
# save for YSY
write.csv(subset(pers_twornds, pers_twornds$type != 'Holistic'),
          file = 'hs_final/interview_2014_pers.csv', row.names = F)                           

write.csv(bars_indv, file = 'hs_final/interview_2014_bars.csv', row.names = F)

