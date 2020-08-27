require(ape)
require(caper)
library(geiger)
require(nlme)
library(phytools)
require(dplyr)
library(readxl)
# b10k<-read_xlsx("../data/b10k.xlsx")
b10k<-read.csv("../data/b10kb.csv")
avgpasser <- read_xlsx("../data/avgpassertree.xlsx",1)
pPCAScores_passer<-read.csv("../data/pPCAScores_passer.csv")
pPCAScores_SC_passer<-read.csv("../data/pPCAScores_SC_passer.csv")

avgpasser$Family <- str_extract_all(avgpasser$HM4.Family, "^\\w+")
avgpasser$Family <- gsub(' [A-z ]*', '' , avgpasser$Family)
length(unique(avgpasser$Family))
length(unique(avgpasser$HM4.Order))
###Load tree
MyTree=read.nexus("../data/output.nex") 
tree.consensus = consensus(MyTree, p = 0.5) 
MyTree.1 = compute.brlen(tree.consensus, 1)

#delete species without acoustic data
b10k$scientific <- gsub(" ","_",b10k$scientific)
avgpasser$scientific <- gsub(" ","_",avgpasser$scientific)
b10k_pa<-b10k[match(avgpasser$scientific,b10k$scientific),]

# sum(is.na(b10k$scientific))
# check tip lable match b10k trait
# tree$tip.label[!(tree$tip.label %in% b10k$scientific)]
# select specific columns trait
b10k_pa <- as.data.frame(b10k_pa)
b10k_pa<- subset(b10k_pa, select = -c(b10k.m_mass, b10k.unsexed_mass, b10k.s_bond_stab, b10k.territory) )

# pick b10k trait with score
b10k_passer <- b10k_pa[match(pPCAScores_passer$X, b10k_pa$scientific),]
b10k_SC_passer <- b10k_pa[match(pPCAScores_SC_passer$X, b10k_pa$scientific),]
select_trait_passer<-data.frame(pPCAScores_passer,b10k_passer)
select_trait_SC_passer <-data.frame(pPCAScores_SC_passer,b10k_SC_passer)

#drop na in scientific name
select_trait_passer<-select_trait_passer %>% drop_na("scientific")
select_trait_SC_passer<-select_trait_SC_passer %>% drop_na("scientific")
# select_trait_SC<-na.omit(select_trait_SC)

dataset_passer <- comparative.data(data=select_trait_passer,phy=MyTree.1,names.col = "scientific",vcv = TRUE, warn.dropped = TRUE)
dataset_SC_passer <- comparative.data(data=select_trait_SC_passer,phy=MyTree.1,names.col = "scientific",vcv = TRUE, warn.dropped = TRUE)

# combined.data<-comparative.data(data=traits3,phy=MyTree.1,names.col = "traits.scientific",vcv = TRUE,warn.dropped = TRUE)


##additive models outline
#8 metric
passer1<-pgls(PC1~b10k.mass+binary_migration+binary_s_bond_stab+binary_habitat+binary_unidirectional+binary_territory+bill_PC1,dataset_passer,lambda= "ML")
summary(passer1)
AIC(Model1)
BIC(Model1)

#8 metric
passer2<-pgls(PC2~binary_migration+binary_s_bond_stab+binary_habitat+binary_unidirectional+b10k.mass+binary_territory+bill_PC1,dataset_passer,lambda= "ML")
summary(passer2)
AIC(Model2)
BIC(Model2)

#sc
Model3<-pgls(PC1~binary_migration+binary_s_bond_stab+binary_habitat+binary_unidirectional+b10k.mass+binary_territory+bill_PC1,dataset_SC_passer,lambda= "ML")
summary(Model3)
AIC(Model3)
BIC(Model3)
#sc
Model4<-pgls(PC2~binary_migration+binary_s_bond_stab+binary_habitat+binary_unidirectional+b10k.mass+binary_territory+bill_PC1,dataset_SC_passer,lambda= "ML")
summary(Model4)
AIC(Model4)
BIC(Model4)















#k Modelling outline

#load pPCA scores data
pPCAScores_passer<-read.csv("../data/pPCAScores.csv")

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

scale.log.model.b10k<-scale.log.model.b10k[match(scale.log.model.b10k$b10k.scientific, pPCAScores_passer$X),]
scale.log.model.b10k<-na.omit(scale.log.model.b10k)
select.traits3<-data.frame(scale.log.model.b10k, pPCAScores_passer)

b10k <- b10k[match(pPCAScores_passer$X, b10k$scientific),]
select.trait4 <-data.frame(b10k,pPCAScores_passer)
select.trait4 <-as.data.frame(select.trait4)
#combine new data frame with tree
dataset <- comparative.data(data=select.trait4,phy=tree2,names.col = "b10k.species",vcv = TRUE, warn.dropped = TRUE)

#run models: outline
Model<-pgls(PC1~binary_sel+binary_habitat+b10k.Bill_Nares+b10k.SpecGenFinalMass+b10k.Latitude_mean+binary_ter,data=dataset,lambda="ML")

#visualise model output
summary(Model)
AIC(Model)
