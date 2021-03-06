require(ape)
require(caper)
library(geiger)
require(nlme)
library(phytools)
require(dplyr)
library(readxl)
rm(list=ls())
MyTree<-read.nexus("../data/output.nex") 
tree.consensus2 = consensus(MyTree, p = 0.5)
MyTree.1 = compute.brlen(tree.consensus2, 1)
MyTree.2<-MyTree[[1]]

# First, do a phylogenetic principal component analysis of Song Duration, Note Count and Number of Note Types #get the data ready 
traits<-read_xlsx("../data/avgpassertree.xlsx") 
traits_log<-data.frame(traits$scientific,
                       log(traits$No_of_Note_Types),
                       log(traits$Song.Bandwidth),
                       log(as.numeric(traits$Rate_Pace)),
                       log(traits$Note.Count)
) # log transform and z standardize variables 

#log(as.numeric(traits$Pace1))
# log(traits$Note.Count),
traits_SC<-data.frame(traits$scientific,
                      scale(traits_log$log.traits.No_of_Note_Types.,center=TRUE,scale=TRUE), 
                      scale(traits_log$log.traits.Song.Bandwidth.,center=TRUE,scale=TRUE),
                      scale(traits_log$log.traits.Note.Count.,center=TRUE,scale=TRUE),
                      scale(traits_log$log.as.numeric.traits.Rate_Pace..,center = TRUE, scale = TRUE)
) 
#scale(traits_log$log.as.numeric.traits.Pace1.., center = TRUE,scale = TRUE)
#scale(traits_log$log.traits.Note.Count.,center=TRUE,scale=TRUE), 
write.csv(traits_SC,"../data/traits_SC_passer.csv") # change the names in excel 
# traits3<-read.csv("../data/traits3.csv",header=T) 

traits_SC$traits.scientific <- gsub(" ","_",traits_SC$traits.scientific)
# traits3 <- traits3[match(MyTree.1$tip.label,traits3$traits.scientific),] 

combined.data<-comparative.data(data=traits_SC,phy=MyTree.1,names.col = "traits.scientific",vcv = TRUE,warn.dropped = TRUE)

pPCA.birds_passer<- phyl.pca(tree=combined.data$phy,Y=combined.data$data,method="BM",mode="corr")

pPCAScores_SC_passer<-pPCA.birds_passer$S

write.csv(pPCAScores_SC_passer,"../data/pPCAScores_SC_passer.csv")

pPCALoadings_SC_passer<-pPCA.birds_passer$L

write.csv(pPCALoadings_SC_passer,"../data/pPCALoadings_SC_passer.csv")

summary(pPCA.birds_passer)
# write.csv(sum_8, "../data/pPCAsummary_8.csv")
biplot(pPCA.birds_passer)
pdf(file="../data/biplot_song_SC_passer.pdf",width=10,height=10)
biplot(pPCA.birds_passer)
dev.off()
