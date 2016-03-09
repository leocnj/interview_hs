# pers_sc.R
#
# - analyze round-2 personality rating scors.
# - also read round-1 rating scores.
# - what to output
#   - personality + holistic, putting 7 cols together
#   - other ratings, only using round-1's
#
library(xlsx)
library(plyr)
library(dplyr)
library(magrittr)
library(tidyr)


file <- "hsraw/interviewRating-round2.xlsx"
pers <-
  read.xlsx(file,
            sheetIndex = 1,
            startRow = 3,
            header = F)  # read first sheet

#rBP	rDE	rJS	rMC	rRV
adRaters <- c("rBP", "rDE", "rJS", "rMC", "rRV")

# five groups corresponding to the BIG-5
# each group contains five rater's judegments
# EXTRAV, AGREE, CONSC, EMSTM, OPEN, HOLISTIC
# full names
# Extraversion, Agreeable, Conscientious, EmoStability, Open, Holistic
ids <- pers[, 1:3]
idCols <- c("subj", "item", "form")
colnames(ids) <- idCols

videoid <- function(x) {
  require(stringr)
  subj.reg <- "\\d+"
  subj.tk <- str_extract(x, subj.reg)
  return(subj.tk)
}

paditem <- function(x) {
  require(stringr)
  padded <- x
  if (str_length(x) == 1) {
    padded <- paste("0", x, sep = "")
  }
  padded
  
}

# create a videoid col
vids <- ids %>%
  mutate(subj.tk = videoid(subj)) %>%
  mutate(item.padded = unlist(lapply(item, FUN = paditem))) %>%
  mutate(vid = paste(subj.tk, "_segment-", item.padded, sep = "")) %>%
  select(vid)

idCols <- "vid"
# need create the unique video-id
extrav <- cbind(vids, pers[, 4:8], "EXTRAV")
colnames(extrav) <- c(idCols, adRaters, "type")

agree <- cbind(vids, pers[, 9:13], "AGREE")
colnames(agree) <- c(idCols, adRaters, "type")

consc <- cbind(vids, pers[, 14:18], "CONSC")
colnames(consc) <- c(idCols, adRaters, "type")

emstm <- cbind(vids, pers[, 19:23], "EMSTB")
colnames(emstm) <- c(idCols, adRaters, "type")

openness <- cbind(vids, pers[, 24:28], "OPEN")
colnames(openness) <- c(idCols, adRaters, "type")

holistic <- cbind(vids, pers[, 29:33], "Holistic")
colnames(holistic) <- c(idCols, adRaters, "type")

pers_all <-
  rbind_list(extrav, emstm, agree, openness, consc, holistic)

# round-1, raters MS & SV
# SELT/AD first impression double-scored dat
file <- "hsraw/humanRatings-interviewTask-ratings.xlsx"
selt <-
  read.xlsx(
    file,
    sheetIndex = 1,
    startRow = 3,
    header = F,
    stringsAsFactors = F
  )  # read first sheet
colnames(selt) <- c(
  "fromID",
  "videoID",
  "FRIENDLY.1",
  "FRIENDLY.2",
  "NERVOUS.1",
  "NERVOUS.2",
  "AWKWARD.1",
  "AWKWARD.2",
  "CONFIDENT.1",
  "CONFIDENT.2",
  "ENGAGED.1",
  "ENGAGED.2",
  "EXCITED.1",
  "EXCITED.2",
  "CALM.1",
  "CALM.2",
  "AUTHENTIC.1",
  "AUTHENTIC.2",
  "NVB.1",
  "NVB.2",
  "ACCENT.1",
  "ACCENT.2",
  "MONO.1",
  "MONO.2",
  "UNDERSD.1",
  "UNDERSD.2",
  "PROF.1",
  "PROF.2",
  "SOPH.1",
  "SOPH.2",
  "COHERENT.1",
  "COHERENT.2",
  "EXTRAV.1",
  "EXTRAV.2",
  "EMSTB.1",
  "EMSTB.2",
  "AGREE.1",
  "AGREE.2",
  "OPEN.1",
  "OPEN.2",
  "CONSC.1",
  "CONSC.2",
  "Holistic.1",
  "Holistic.2"
)

# obtain subj & item by substr() from videoID
selt$subj <- substr(selt$videoID, 1, 2)
selt$item <- substr(selt$videoID, 12, 13)

# move pers + holistic into the pers_all DF.
# - a different way to obtain selt's long table.
onetrait <- function(df, trait.name) {
  trait.df <- df %>%
    select(contains(trait.name))
  trait.df <- cbind(df$videoID, trait.df, trait.name)
  colnames(trait.df) <- c("vid", "R1A", "R1B", "type")
  return(trait.df)
}

df.rnd1.EXTRAV <- onetrait(selt, "EXTRAV")
df.rnd1.EMSTB  <- onetrait(selt, "EMSTB")
df.rnd1.AGREE  <- onetrait(selt, "AGREE")
df.rnd1.OPEN   <- onetrait(selt, "OPEN")
df.rnd1.CONSC  <- onetrait(selt, "CONSC")
df.rnd1.Hol    <- onetrait(selt, "Holistic")

df.rnd1 <- rbind_list(
  df.rnd1.EXTRAV,
  df.rnd1.EMSTB,
  df.rnd1.AGREE,
  df.rnd1.OPEN,
  df.rnd1.CONSC,
  df.rnd1.Hol
)

pers_twornds <- merge(pers_all, df.rnd1, by = c("vid", "type"))
# total 2514 x 9

# 10/27/2015
#
# for each trait, compute average score (removing MAX and MIN)
# format a new DF for each response and its averaged trait scores.
#
# pers_twornds %>%
#  mutate(rAVE = (rBP + rDE + rJS + rMC + rRV + R1A + R1B) / 7.0)

# 11/16/2015
# PERS
pers_twornds$mean <- rowMeans(pers_twornds[,3:9], na.rm = T)

pers_wide <-
  spread(pers_twornds[, c("vid", "type", "mean")], type, mean)

pers_wide$subj <- substr(pers_wide$vid, 1, 2)
# pers_wide$item <- substr(pers_wide$vid, 12, 13)

pers_wide_subj <- pers_wide %>%
  select(-vid) %>%
  group_by(subj) %>%
  summarise_each(funs(mean(., na.rm = TRUE)))

# 11/15/2015
#
# focused on first impression ratings done in the round 1.
selt.ave <- selt %>%
  mutate(frd = (FRIENDLY.1 + FRIENDLY.2) / 2) %>%
  mutate(nvs = (NERVOUS.1 + NERVOUS.2) / 2) %>%
  mutate(awk = (AWKWARD.1 + AWKWARD.2) / 2) %>%
  mutate(conf = (CONFIDENT.1 + CONFIDENT.2) / 2) %>%
  mutate(eng = (ENGAGED.1 + ENGAGED.2) / 2) %>%
  mutate(exc = (EXCITED.1 + EXCITED.2) / 2) %>%
  mutate(clm = (CALM.1 + CALM.2) / 2) %>%
  mutate(auth = (AUTHENTIC.1 + AUTHENTIC.2) / 2) %>%
  mutate(nvb = (NVB.1 + NVB.2) / 2) %>%
  mutate(acct = (ACCENT.1 + ACCENT.2) / 2) %>%
  mutate(mono = (MONO.1 + MONO.2) / 2) %>%
  mutate(und = (UNDERSD.1 + UNDERSD.2) / 2) %>%
  mutate(prof = (PROF.1 + PROF.2) / 2) %>%
  mutate(soph = (SOPH.1 + SOPH.2) / 2) %>%
  mutate(coh = (COHERENT.1 + COHERENT.2) / 2) %>%
  mutate(hol = (Holistic.1 + Holistic.2) / 2) %>%
  select(
    videoID,
    frd,
    nvs,
    awk,
    conf,
    eng,
    exc,
    clm,
    auth,
    nvb,
    acct,
    mono,
    und,
    prof,
    soph,
    coh,
    hol,
    subj,
    item
  )

# need this for computing IRR.
df.rnd1.other <-
  rbind_list(
    onetrait(selt, "FRIENDLY"),
    onetrait(selt, "NERVOUS"),
    onetrait(selt, "AWKWARD"),
    onetrait(selt, "CONFIDENT"),
    onetrait(selt, "ENGAGED"),
    onetrait(selt, "EXCITED"),
    onetrait(selt, "CALM"),
    onetrait(selt, "AUTHENTIC"),
    onetrait(selt, "NVB"),
    onetrait(selt, "ACCENT"),
    onetrait(selt, "MONO"),
    onetrait(selt, "UNDERSD"),
    onetrait(selt, "PROF"),
    onetrait(selt, "SOPH"),
    onetrait(selt, "COHERENT")
  )

# 2/26/2016
# save for YSY
write.csv(subset(pers_twornds, pers_twornds$type != 'Holistic'),
                file = 'hs_final/interview_2014_pers.csv', row.names = F)

# 3/9/2016
# save holistic.
write.csv(
  subset(pers_twornds, pers_twornds$type == 'Holistic'),
  file = 'hs_final/selt_holistic.csv',
  row.names = F
)
