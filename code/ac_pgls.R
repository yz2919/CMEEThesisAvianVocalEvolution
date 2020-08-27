require(ape)
require(caper)
library(geiger)
require(nlme)
library(phytools)
require(dplyr)
library(readxl)
# b10k<-read_xlsx("../data/b10k.xlsx")
b10k<-read.csv("../data/b10kb.csv")
avgsong <- read_xlsx("../data/avgsongtree.xlsx",1)
pPCAScores_song_2<-read.csv("../data/pPCAScores_8.csv")
pPCAScores_SC<-read.csv("../data/pPCAScores_SC.csv")
# require(ape) require(phytools) require(geiger) require(nlme) require(caper)

###Load tree
MyTree=read.nexus("../data/output.nex") 
tree.consensus = consensus(MyTree, p = 0.5) 
MyTree.1 = compute.brlen(tree.consensus, 1)

#delete species without acoustic data
b10k$scientific <- gsub(" ","_",b10k$scientific)
avgsong$scientific <- gsub(" ","_",avgsong$scientific)
b10k<-b10k[match(avgsong$scientific,b10k$scientific),]

# sum(is.na(b10k$scientific))
# check tip lable match b10k trait
# tree$tip.label[!(tree$tip.label %in% b10k$scientific)]
# select specific columns trait
b10k <- as.data.frame(b10k)
b10k<- subset(b10k, select = -c(b10k.m_mass, b10k.unsexed_mass, b10k.s_bond_stab, b10k.territory) )

# pick b10k trait with score
b10k_AC <- b10k[match(pPCAScores_song_2$X, b10k$scientific),]
b10k_SC <- b10k[match(pPCAScores_SC$X, b10k$scientific),]

select_trait <-data.frame(pPCAScores_song_2,b10k_AC)
# select_trait <- select_trait[match(pPCAScores_song_2$X, b10k_AC$scientific),]
select_trait_SC <-data.frame(pPCAScores_SC,b10k_SC)

#drop na in scientific name
select_trait_AC<-select_trait %>% drop_na("scientific")
select_trait_SC<-select_trait_SC %>% drop_na("scientific")
# select_trait_SC<-na.omit(select_trait_SC)

dataset <- comparative.data(data=select_trait_AC,phy=MyTree.1,names.col = "scientific",vcv = TRUE, warn.dropped = TRUE)
dataset_SC <- comparative.data(data=select_trait_SC,phy=MyTree.1,names.col = "scientific",vcv = TRUE, warn.dropped = TRUE)

# combined.data<-comparative.data(data=traits3,phy=MyTree.1,names.col = "traits.scientific",vcv = TRUE,warn.dropped = TRUE)


##additive models outline
#8 metric
Model1<-pgls(PC1~b10k.mass+binary_migration+binary_s_bond_stab+binary_habitat+binary_unidirectional+binary_territory+bill_PC1,dataset,lambda= "ML")
summary(Model1)
AIC(Model1)
BIC(Model1)
Model15<-pgls(PC1~b10k.mass+binary_migration+binary_s_bond_stab+binary_habitat+binary_unidirectional+binary_territory+bill_PC1,dataset,delta= "ML")
summary(Model15)
AIC(Model15)
Model16<-pgls(PC1~b10k.mass+binary_migration+binary_s_bond_stab+binary_habitat+binary_unidirectional+binary_territory+bill_PC1,dataset,kappa = "ML")
summary(Model16)
AIC(Model16)
# ou.bird<-corMartins(1,phy=MyTree.1)
# # ou.gls<-gls(PC1~b10k.mass,correlation=ou.bird,data=select_trait_AC)
# install.packages("motmot")
# library(motmot)
# b10k_AC$scientific <- gsub(" ","_",b10k_AC$scientific)
# clade <- b10k_AC[match(MyTree.1$tip.label,b10k_AC$scientific_name),] 
# 
# clade<- as.matrix(clade)
# bm.ml <- transformPhylo.ML(phy = MyTree.2, y = clade, model = "bm")


#8 metric
Model2<-pgls(PC2~binary_migration+binary_s_bond_stab+binary_habitat+binary_unidirectional+b10k.mass+binary_territory+bill_PC1,dataset,lambda= "ML")
summary(Model2)
AIC(Model2)
BIC(Model2)

#sc
Model3<-pgls(PC1~binary_migration+binary_s_bond_stab+binary_habitat+binary_unidirectional+b10k.mass+binary_territory+bill_PC1,dataset_SC,lambda= "ML")
summary(Model3)
AIC(Model3)
BIC(Model3)
#sc
Model4<-pgls(PC2~binary_migration+binary_s_bond_stab+binary_habitat+binary_unidirectional+b10k.mass+binary_territory+bill_PC1,dataset_SC,lambda= "ML")
summary(Model4)
AIC(Model4)
BIC(Model4)


bodym<- pgls(PC1~bill_PC1+binary_unidirectional,dataset,lambda= "ML")
summary(bodym)
AIC(bodym)


bodyx<-pgls(PC2~bill_PC1+binary_unidirectional,dataset,lambda = "ML")
summary(bodyx)
AIC(bodyx)



Model5 <- pgls(PC2 ~ b10k.mass, data = dataset_SC, lambda = "ML")
summary(Model5)
AIC(Model5)
Model6 <- pgls(PC1 ~ b10k.mass, data = dataset_SC, lambda = "ML")
summary(Model6)
AIC(Model5)



#k Modelling outline

#load pPCA scores data
pPCAScores_song_2<-read.csv("../data/pPCAScores.csv")

#rename select traits data frame so any changes will not affect other analysis
model.traits.b10k<-select.traits.b10k
model.traits<-select.traits
#create all binary variables necessary: example 
# CW
index3 = model.traits.b10k$b10k.habitat != 3
model.traits.b10k$binary_habitat = model.traits.b10k$b10k.habitat
model.traits.b10k$binary_habitat[index3] = 1
# SR
index3 = model.traits$b10k.b10k.habitat != 3
model.traits$binary_habitat = model.traits$b10k.b10k.habitat
model.traits$binary_habitat[index3] = 1



#log transform and z-standardise necessary variables 
log.model.traits.b10k<-model.traits.b10k %>% mutate_at(vars(b10k.bill_nares,b10k.clutch_size),funs(log))
log.model.traits.b10k[,2:9] <- lapply(log.model.traits.b10k[,2:9],as.numeric)
scale.log.model.b10k<- log.model.traits.b10k %>% mutate_at(vars(b10k.SexualSelectionUnidirectional:b10k.territory),funs(scale))

#create new data frame combining pPCA scores data and transformed trait data frame

scale.log.model.b10k<-scale.log.model.b10k[match(scale.log.model.b10k$b10k.scientific, pPCAScores_song_2$X),]
scale.log.model.b10k<-na.omit(scale.log.model.b10k)
select.traits3<-data.frame(scale.log.model.b10k, pPCAScores_song_2)

b10k <- b10k[match(pPCAScores_song_2$X, b10k$scientific),]
select.trait4 <-data.frame(b10k,pPCAScores_song_2)
select.trait4 <-as.data.frame(select.trait4)
#combine new data frame with tree
dataset <- comparative.data(data=select.trait4,phy=tree2,names.col = "b10k.species",vcv = TRUE, warn.dropped = TRUE)

#run models: outline
Model<-pgls(PC1~binary_sel+binary_habitat+b10k.Bill_Nares+b10k.SpecGenFinalMass+b10k.Latitude_mean+binary_ter,data=dataset,lambda="ML")

#visualise model output
summary(Model)
AIC(Model)
