# hs_analyze.R
# 
# Analyze HS results from both SELTand CAWRS
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
# per group
irr_bars1 <- bars_indv %>%
  group_by(gpid) %>%
  summarise(icc=icc(cbind(R1,R2,R3,R4,R5,R6,R7,R8,R9,R10))$value,
            corr=meancor(cbind(R1,R2,R3,R4,R5,R6,R7,R8,R9,R10))$value)

irr_bars2 <- bars_indv %>%
  group_by(gp) %>%
  summarise(icc=icc(cbind(R1,R2,R3,R4,R5,R6,R7,R8,R9,R10))$value,
            corr=meancor(cbind(R1,R2,R3,R4,R5,R6,R7,R8,R9,R10))$value)


