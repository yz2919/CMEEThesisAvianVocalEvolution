#Match species for tree download

#clear working environment 
rm(list=ls())

#set working directory 
setwd("~/Downloads/Birdsong/code/")

library(dplyr)
library(tidyr)
library(readxl)


avg_tree <- read_xlsx("../data/avgsong.xlsx",1)
tree <- read.csv("../data/BLIOCPhyloMasterTax.csv")
# tree_match<-tree[match(tree$Scientific,avg_tree$species),]
tree_match<-tree[match(avg_tree$species,tree$Scientific),]
tree_match <- na.omit(tree_match)
avg_tree$species[!(avg_tree$species %in% tree$Scientific)]
write.csv(tree_match, "../data/tree_match.csv")
                                        

species_missed <- read_xlsx("../data/Species.xlsx",1)
species_missed$Avgsong[!(species_missed$Avgsong %in% tree_match$Scientific)]
species_missed$tree[!(species_missed$tree %in% tree_match$Scientific)]
tree_add <- tree[match(species_missed$tree, tree$Scientific),] 
tree_add <- na.omit(tree_add)
tree_match <- rbind(tree_match,tree_add)

write.csv(tree_match, "../data/tree_match.csv")
