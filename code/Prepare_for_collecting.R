install.packages(c("readxl","writexl")) 
install.packages("writexl")
library(readxl)
library(writexl)
install.packages("stringr")
library(stringr)
dd <- read_xlsx("Raw_B10K_Song_Traits_new.xlsx",1) 
smp <- read_xlsx("B10K song datasets/B10K song data masterfiles/B10K_SampledSpecies 29 Nov 2017.xlsx")
dd$HM4.Latin.name <- str_replace(dd$HM4.Latin.name, "_", " ")
for (i in 1:nrow(smp)){
  for (j in 1242:nrow(dd)){
    if (smp$`HM4 Latin name`[i]== dd$HM4.Latin.name[j]){
      dd$HM4.Order[j] <- smp$`HM4 Order`[i]
      dd$HM4.Family [j]<- smp$`HM4 Family`[i]
      dd$HM4.Common.name [j]<- smp$`HM4 Common name`[i]
      dd$HM4.Subspecies [j]<- smp$`HM4 Subspecies`[i]
      dd$Site [j] <- smp$Site[i]
      dd$State.Province [j]<- smp$`State/Province`[i]
      dd$County.District [j]<- smp$`County/District`[i]
      dd$Country [j] <- smp$Country[i]
      dd$Captivity.details [j]<- smp$`Captivity details`[i]
    }
  }
}

write_xlsx(dd,"dd1.xlsx")
