# bars_sc.R
#
# analyze BARS scores provided by CAWRS
#
#
# library(xlsx)
library(stringr)
# library(plyr)
library(dplyr)

# convert P(\d+)Q(\d+) id to subj-segment-item style used by SELT
split_PQ <- function(PQ){
  regexp <- "P(\\d+)Q(\\d+)"
  mts <- str_match(PQ, regexp)
  subj <- mts[2]
  item <- mts[3]
  if(str_length(subj) == 1){
    subj <- paste("0", subj, sep="")
  }
  if(str_length(item) == 1){
    item <- paste("0", item, sep="")
  }
  return(paste(subj, "_segment-", item, sep=""))
}

# for persubj ratings, get subj and cat
getsubj <- function(name){
  regexp <- "(\\w+)\\.(\\d+)\\."
  mts <- str_match(name, regexp)
  cat <- mts[2]
  subj <- mts[3]
  if(str_length(subj) == 1){
    subj <- paste("0", subj, sep="")
  }
  return(subj)
}

getcat <- function(name){
  regexp <- "(\\w+)\\.(\\d+)\\."
  mts <- str_match(name, regexp)
  cat <- mts[2]
  subj <- mts[3]
  return(cat)
}


# BARS rating from 10 students; provided by CAWRS
bars <- read.csv("hsraw/bars_rating.csv", header=T, stringsAsFactors=FALSE)
tenraters <- c("R1","R2","R3","R4","R5","R6","R7","R8","R9","R10")

# for each response, 10 raters used BARS to score independently
bars_indv <- data.frame(t(data.matrix(select(bars, P1Q1:P38Q12))))
colnames(bars_indv) <- tenraters

# get videoID to sync with SELT/AD data 
bars_indv$videoID <- unlist(lapply(rownames(bars_indv), FUN=split_PQ))
# get mean score from 10 raters.
bars_indv$mean <- rowMeans(bars_indv[,1:10])
# get median score from 10 raters.
# bars_indv$median <- apply(bars_indv[,1:10], 1, median)

# need group_id 1 to 4 for 12 items.
bars_indv$gpid <- substr(bars_indv$videoID, 12, 13)

bars_indv$gp[bars_indv$gpid=="01"] <- "G1"
bars_indv$gp[bars_indv$gpid=="02"] <- "G1"
bars_indv$gp[bars_indv$gpid=="03"] <- "G1"

bars_indv$gp[bars_indv$gpid=="04"] <- "G2"
bars_indv$gp[bars_indv$gpid=="05"] <- "G2"
bars_indv$gp[bars_indv$gpid=="06"] <- "G2"

bars_indv$gp[bars_indv$gpid=="07"] <- "G3"
bars_indv$gp[bars_indv$gpid=="08"] <- "G3"
bars_indv$gp[bars_indv$gpid=="09"] <- "G3"

bars_indv$gp[bars_indv$gpid=="10"] <- "G4"
bars_indv$gp[bars_indv$gpid=="11"] <- "G4"
bars_indv$gp[bars_indv$gpid=="12"] <- "G4"

bars_indv$subj <- substr(bars_indv$videoID, 1, 2)

# CAWRS's per-subj rating on overall, nvb, and vocal.
bars_persubj <- data.frame(t(data.matrix(select(bars, OverallHir.1.:Vocal.38.))))
colnames(bars_persubj) <- tenraters

bars_persubj$subj <- unlist(lapply(rownames(bars_persubj), FUN=getsubj))
bars_persubj$type <- unlist(lapply(rownames(bars_persubj), FUN=getcat))
