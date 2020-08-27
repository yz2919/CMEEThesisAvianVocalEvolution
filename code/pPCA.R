# pPCA code

#clear working environment 
rm(list=ls())

#install all required packages
install.packages("ape",repos="https://cloud.r-project.org",quiet=TRUE)
install.packages("caper",repos="https://cloud.r-project.org",quiet=TRUE)
install.packages("geiger",repos="https://cloud.r-project.org",quiet=TRUE)
install.packages("nlme",repos="https://cloud.r-project.org",quiet=TRUE)
install.packages("phytools",repos="https://cloud.r-project.org",quiet=TRUE)
install.packages("phytools")
install.packages("dplyr")

#load the packages in the environment
require(ape)
require(caper)
library(geiger)
# require(geiger)
require(nlme)
require(phytools)
library(phytools)
require(dplyr)
library(readxl)

#set working directory 
setwd("~/Downloads/Birdsong/code/")

#load vocalisation dataset
diss<-read_xlsx("../data/avgsong.xlsx")
diss<-data.frame(diss)



#load phylogenetic tree 
mytree2<-read.nexus("../data/output.nex")

#create consensus tree
tree.consensus2 = consensus(mytree2, p = 0.5)
tree2 = compute.brlen(tree.consensus2, 1)
tree2 <- mytree2[[1]]


#create a data frame containing all the variables for analysis
diss_complex<-data.frame(diss$HM4.Family,diss$species,diss$No_notes_in_call_bout.song,diss$No_of_Note_Types,diss$Song.Duration,diss$Song.Bandwidth,diss$Note_Peak_Freq_Average,diss$Note_Min_Freq_Average,diss$Note.Max.Freq.Average,diss$Pace1,diss$Note_Rate)

#create a new column removing spaces in species names so they are identical to phylogenetic tree names
# diss_complex_2 = diss_complex
diss_complex$scientific_name <- gsub(" ","_",diss_complex$diss.species)

#log transform and z-standardise all variables used in the pPCA
diss_complex[,3:11] <- lapply(diss_complex[,3:11],as.numeric)
# diss[,10:85] <- lapply(diss[,10:85],as.numeric)

diss.log.complex <- diss_complex %>% mutate_at(vars(diss.No_notes_in_call_bout.song,diss.No_of_Note_Types,diss.Song.Duration,diss.Note.Max.Freq.Average,diss.Note_Min_Freq_Average,diss.Song.Bandwidth,diss.Note_Peak_Freq_Average,diss.Note_Rate),funs(log))
# diss.log.complex$diss.Pace1 <- log(diss$Pace1)
# diss.log.complex$diss.Pace1[is.nan(diss.log.complex$diss.Pace1)]<-NA
diss.scale.log.complex <- diss.log.complex %>% mutate_at(vars(diss.No_notes_in_call_bout.song,diss.No_of_Note_Types,diss.Song.Duration,diss.Note.Max.Freq.Average,diss.Note_Min_Freq_Average,diss.Song.Bandwidth,diss.Note_Peak_Freq_Average,diss.Pace1,diss.Note_Rate),funs(scale))


#match row names from data frame and tree
diss_complex_phy<-diss_complex[match(tree2$tip.label,diss_complex$scientific_name),] 
diss.scale.log.complex <- diss.scale.log.complex[match(tree2$tip.label,diss.scale.log.complex$scientific_name),] 

#combine transformed data frame with tree
diss.combined.data<-comparative.data(data = diss.scale.log.complex,phy=tree2,names.col = "scientific_name",vcv = TRUE, warn.dropped = TRUE)
class(diss.combined.data)
#create a new combined data frame removing variables not needed in this analysis
diss.combined.data2 = diss.combined.data
diss.combined.data2$data$diss.HM4.Latin.name<-NULL
diss.combined.data2$data$diss.IntraIndiv_variation<-NULL
diss.combined.data2$data$diss.IntraSpecific_variation<-NULL

#run pPCA
pPCA.birds<-phyl.pca(tree=diss.combined.data$phy,Y=diss.combined.data$data,method="BM",mode="corr")

#generate pPCA scores data
pPCAScores<-pPCA.birds$S
pPCAScores<-abs(pPCAScores)
write.csv(pPCAScores,"pPCAScores_song_final.csv")

#generate pPCA loadings data
pPCALoadings<-pPCA.birds$L
pPCALoadings
write.csv(pPCALoadings,"pPCALoadings_song_final.csv")

#visualise pPCA ouput in R console
summary(pPCA.birds)

#visualise pPCA in plot
biplot(pPCA.birds)

#create .pdf file to save pPCA plot
pdf(file="biplot_song_final.pdf",width=10,height=10)
biplot(pPCA.birds)
dev.off()
plot(pPCAScores)
