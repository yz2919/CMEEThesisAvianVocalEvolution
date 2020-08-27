library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(stringr)
library(sprintf)
avgsong <- read_xlsx("../data/Main_B10K_Song_Average.xlsx",1)
setwd("Birdsong/code")
rawsong <- read_xlsx("../data/Raw_B10K_Song_Traits.xlsx",1) 
class(rawsong)
as.data.frame(rawsong)
ra<-read.csv("../data/Raw_B10K_Song_Traits.csv")
nrate <- ra %>% group_by(HM4.Latin.name) %>% summarise(Mean = mean(Note_Rate,na.rm = TRUE))
onen <- ra %>% group_by(HM4.Latin.name) %>% summarise(Mean = mean(Single.Note.Song.Pace, na.rm = TRUE))
unique(ra$Single.Note.Song.Pace)
unique(onen$Mean)                                                        


nrate <- ra %>% group_by(HM4.Latin.name) %>% 
  summarise(Nrate = mean (Note_Rate),na.rm=T)
unique(ra$Single.Note.Song.Pace)
unique(onen$Mean)



checkpace <- data.frame("single_pace" = ra$Single.Note.Song.Pace, 
                        "note_rate" = ra$Note_Rate)
