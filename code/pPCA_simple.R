require(ape)
require(caper)
library(geiger)
require(nlme)
library(phytools)
require(dplyr)
library(readxl)

MyTree<-read.nexus("../data/output.nex") 
tree.consensus2 = consensus(MyTree, p = 0.5)
MyTree.1 = compute.brlen(tree.consensus2, 1)
MyTree.2<-MyTree[[1]]

# First, do a phylogenetic principal component analysis of Song Duration, Note Count and Number of Note Types #get the data ready 
traits<-read_xlsx("../data/avgsongtree.xlsx") 
traits_log<-data.frame(traits$scientific,
                       log(traits$No_of_Note_Types),
                       log(traits$Song.Duration),
                       log(traits$Song.Bandwidth),
                       log(traits$Note_Peak_Freq_Average),
                       log(traits$Note_Min_Freq_Average),
                       log(traits$Note.Max.Freq.Average),
                       log(as.numeric(traits$Rate_Pace)),
                       log(traits$Note.Count)
                       ) # log transform and z standardize variables 
# log(as.numeric(traits$Note_Rate)),
# log(as.numeric(traits$Pace1)),
# log(traits$Note.Count),
traits3<-data.frame(traits$scientific,
                    scale(traits_log$log.traits.No_of_Note_Types.,center=TRUE,scale=TRUE), 
                    scale(traits_log$log.traits.Song.Duration.,center=TRUE,scale=TRUE),
                    scale(traits_log$log.traits.Song.Bandwidth.,center=TRUE,scale=TRUE),
                    scale(traits_log$log.traits.Note.Count.,center=TRUE,scale=TRUE),
                    scale(traits_log$log.traits.Note_Peak_Freq_Average.,center=TRUE,scale=TRUE),
                    scale(traits_log$log.traits.Note_Min_Freq_Average.,center = TRUE, scale = TRUE),
                    scale(traits_log$log.traits.Note.Max.Freq.Average.,center = TRUE, scale = TRUE),
                    scale(traits_log$log.as.numeric.traits.Rate_Pace..,center = TRUE, scale = TRUE)
                    ) 
# scale(traits_log$log.as.numeric.traits.Note_Rate..,center = TRUE, scale = TRUE),
# scale(traits_log$log.as.numeric.traits.Pace1.., center = TRUE,scale = TRUE)
#scale(traits_log$log.as.numeric.traits.Pace1.., center = TRUE,scale = TRUE)
write.csv(traits3,"../data/traits3.csv") # change the names in excel 
# traits3<-read.csv("../data/traits3.csv",header=T)

traits3$traits.scientific <- gsub(" ","_",traits3$traits.scientific)
# traits3 <- traits3[match(MyTree.1$tip.label,traits3$traits.scientific),] 

combined.data<-comparative.data(data=traits3,phy=MyTree.1,names.col = "traits.scientific",vcv = TRUE,warn.dropped = TRUE)

pPCA.birds<- phyl.pca(tree=combined.data$phy,Y=combined.data$data,method="BM",mode="corr")

# 
# class(pPCA.birds)
# class(MyTree.1)
# 
# rescale(MyTree.1, model = c("BM"))

pPCAScores<-pPCA.birds$S

write.csv(pPCAScores,"../data/pPCAScores_8.csv")

pPCALoadings<-pPCA.birds$L

write.csv(pPCALoadings,"../data/pPCALoadings_8.csv")

summary(pPCA.birds)
# write.csv(sum_8, "../data/pPCAsummary_8.csv")
biplot(pPCA.birds)
pdf(file="../data/biplot_song_final.pdf",width=10,height=10)
biplot(pPCA.birds)
dev.off()

