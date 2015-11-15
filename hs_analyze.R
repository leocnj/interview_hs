# hs_analyze.R
# 
# Analyze HS results from both SELT and CAWRS
#
#
#library(plyr)
library(dplyr)
library(irr)
library(magrittr)
# make sure to run pers_sc.R to obtain SELT ratings.

# pers_twornds 2514x9
# IRR analysis.

#rBP	rDE	rJS	rMC	rRV
irr_pertype <- pers_all %>%
  group_by(type) %>%
  summarise(icc=icc(cbind(rBP, rDE, rJS, rMC, rRV))$value,
            corr=meancor(cbind(rBP, rDE, rJS, rMC, rRV))$value)


irr_pertype2 <- pers_twornds %>%
  group_by(type) %>%
  summarise(icc=icc(cbind(rBP, rDE, rJS, rMC, rRV, R1A, R1B))$value,
            corr=meancor(cbind(rBP, rDE, rJS, rMC, rRV, R1A, R1B))$value)

# bars_indv

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

# BARS
# each subj's items' and overall scores
bars_persubj_items <- bars_indv %>%
  group_by(subj) %>%
  summarise(itemsum=sum(mean))

bars_persubj$mean <- rowMeans(bars_persubj[,1:10])

bars_persubj_simp <- bars_persubj %>%
  select(mean, subj, type)

# long to wide
library(tidyr)
tmp <- spread(bars_persubj_simp, type, mean)

bars_persubj_final <- merge(tmp, bars_persubj_items, by="subj")

# PERS
pers_twornds$mean <- rowMeans(pers_twornds[,3:9], na.rm = FALSE)
pers_wide <- spread(pers_twornds[,c("vid", "type","mean")], type, mean)

pers_wide$subj <- substr(pers_wide$vid, 1, 2)
# pers_wide$item <- substr(pers_wide$vid, 12, 13)

pers_wide_subj <- pers_wide %>%
  select(-vid) %>%
  group_by(subj) %>%
  summarise_each(funs(mean(., na.rm = TRUE)))

# putting two wide-subj together
wide_subj <- merge(pers_wide_subj, bars_persubj_final, by="subj")
fit.subj <- lm(OverallHir~AGREE+CONSC+EMSTB+EXTRAV+OPEN+Holistic, data=wide_subj)

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





