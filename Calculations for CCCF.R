#A novel framework for assessing conservation status and climate vulnerability of coastal species
#Endyke, S.C., Putnam, A., Lockwood, L., Albert, M., & Staudinger, M.D.

#Code Outline:
# - (1) Data input & manipulation
# - (2) Calculating conservation rank
# - (3) Calculating climate vulnerability rank
# - (4) Collating and saving dataframe

################################################################
# (1) Data input and manipulation ----
################################################################
rm(list = ls()) #clear environment

#Install packages and libraries --------------------------------
install.packages("tidyverse"); install.packages("reshape2")
library(tidyverse); library(reshape2)

#Read in data --------------------------------------------------
df <- read.csv("Raw data/Conservation_matrix.csv", na.strings="NA",header=TRUE)
colnames(df) #dataset of available conservation and climate vulnerability information for native species in area of interest

################################################################
# (2) Calculating conservation rank ----
################################################################
#Standardize conservation rankings across sources --------------
df2 <- df %>% mutate(
  ESA = case_when(
    ESA == "T" ~ 4,
    ESA == "E" ~ 5),
  MESA = case_when(
    MESA == "T" ~ 4,
    MESA == "E" ~ 5,
    MESA == "SC" ~ 3),
  IUCN = case_when(
    IUCN == "CR" ~ 5,
    IUCN == "EN" ~ 4,
    IUCN == "VU" ~ 3,
    IUCN == "NT" ~ 2,
    IUCN == "LC" ~ 1),
  RSGCN = case_when(
    RSGCN == "VH" ~ 5,
    RSGCN == "H" ~ 4,
    RSGCN == "M" ~ 3,
    RSGCN == "L" ~ 2),
  G.Rank = case_when(
    G.Rank == "G1" ~ 5,
    G.Rank == "G2" ~ 4,
    G.Rank == "G3" ~ 3,
    G.Rank == "G4" ~ 2,
    G.Rank == "G5" ~ 1),
  S.Rank = case_when(
    S.Rank == "S1" ~ 5,
    S.Rank == "S2" ~ 4,
    S.Rank == "S3" ~ 3,
    S.Rank == "S4" ~ 2,
    S.Rank == "S5" ~ 1),
  USFWS = case_when(
    USFWS == 2 ~ 1,
    USFWS == 3 ~ 1,
    USFWS == 4 ~ 1,
    USFWS == 5 ~ 1,
    USFWS == 6 ~ 1,
    USFWS == 7 ~ 1,
    USFWS == 8 ~ 1,
    USFWS == 9 ~ 1,
    USFWS == 10 ~ 1,
    USFWS == 11 ~ 2,
    USFWS == 12 ~ 2,
    USFWS == 13 ~ 2,
    USFWS == 14 ~ 2,
    USFWS == 15 ~ 2,
    USFWS == 16 ~ 2,
    USFWS == 17 ~ 2,
    USFWS == 18 ~ 2,
    USFWS == 19 ~ 2,
    USFWS == 20 ~ 2,
    USFWS == 21 ~ 3,
    USFWS == 22 ~ 3,
    USFWS == 23 ~ 3,
    USFWS == 24 ~ 3,
    USFWS == 25 ~ 3,
    USFWS == 26 ~ 3,
    USFWS == 27 ~ 3,
    USFWS == 28 ~ 3,
    USFWS == 29 ~ 3,
    USFWS == 30 ~ 3,
    USFWS == 31 ~ 4,
    USFWS == 32 ~ 4,
    USFWS == 33 ~ 4,
    USFWS == 34 ~ 4,
    USFWS == 35 ~ 4,
    USFWS == 36 ~ 4,
    USFWS == 37 ~ 4,
    USFWS == 38 ~ 4,
    USFWS == 39 ~ 4,
    USFWS == 40 ~ 4,
    USFWS == 41 ~ 5,
    USFWS == 42 ~ 5,
    USFWS == 43 ~ 5,
    USFWS == 44 ~ 5,
    USFWS == 45 ~ 5,
    USFWS == 46 ~ 5,
    USFWS == 47 ~ 5,
    USFWS == 48 ~ 5,
    USFWS == 49 ~ 5,
    USFWS == 50 ~ 5))

#Calculate the mean value (excluding NA values) from ESA, state ESA, IUCN, RSGCN, G.Rank, S.Rank, and USFWS scores:
df3 <- df2 %>% rowwise() %>%
  mutate(Conservation.Rank = mean(c_across(6:12), na.rm = TRUE)) %>%
  ungroup()

################################################################
# (4) Calculating climate vulnerability rank ----
################################################################
#Turn values into their rankings
df4 <- df3 %>% mutate(
  "CCVA" = case_when(
    CCVA == "VH" ~ 4,
    CCVA == "H" ~ 3,
    CCVA == "M" ~ 2,
    CCVA == "L" ~ 1),
  "Thresholds" = case_when(
    Thresholds == "Y" ~ 2))

#Create a new column that assigns value to 2 if "CC" exists in threats the string
df4$Threats_CC <- ifelse(grepl("CC", df4$Threats), 2, NA)

#Move Threats_CC column to the 18th column
df4 <- df4[,c(1:17,26,18:25)]

#Calculate the mean value (excluding NA values) from CCVA, thresholds, and threats scores:
df5 <- df4 %>% rowwise() %>%
  mutate(Climate.Rank = mean(c_across(16:18), na.rm = TRUE)) %>%
  ungroup()

################################################################
# (4) Collating and saving dataframe ----
################################################################
write.csv(df5, file="CCCF_data.csv", row.names = F)

