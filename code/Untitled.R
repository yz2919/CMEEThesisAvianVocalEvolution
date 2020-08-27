installed.packages("readxl")
library(readxl)
library(stringr)
BirdRaw <- read_excel("Raw_B10K_Song_Traits_new copy.xlsx")
BirdTrait <- read_excel("Main_B10K_Trait_Dataset.xlsx")
length(unique(BirdRaw$HM4.Latin.name))
BirdRaw$HM4.Family
as.da

length(unique(BirdRaw$HM4.Family))
length(unique(BirdTrait$...2))
BirdTrait$...1 <- str_replace(BirdTrait$...1 , "_", " ")
length(which(unique(BirdRaw$HM4.Latin.name) == BirdTrait$...1))
unique(BirdRaw$HM4.Latin.name)
BirdTrait$...1
a <- unique(BirdRaw$HM4.Latin.name)
b <- BirdTrait$...1
b <- b[2:378]
a[!(a %in% b)]
b[!(b %in% a)]
