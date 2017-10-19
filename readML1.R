library(rio)
library(plyr)
library(tidyverse)

#fileName <- "Full_Dataset_De-Identified.sav"

fileName <- "CleanedDataset.sav"

df <- import(paste0(getwd(),"/Data/",fileName))

df <- df %>% select(starts_with("anchoring"))


# e.g. "uva" == 31
df.sub <- filter(df, df$sample==31)

((119/200)-(100/200)) / sqrt((119/200) * (1 - (119/200)) / 170)
