# make scientific name attached to average results
avgpasser <- read_xlsx("../data/avgpasser.xlsx",1)
species_missed <- read_xlsx("../data/missed_species.xlsx",1)
avgpasser$scientific <-NA
for (i in 1:nrow(avgpasser)){
  for (j in 1:nrow(species_missed)){
    if (avgpasser$species [i] == species_missed$Avgsong [j]){
      avgpasser$scientific [i] <- species_missed$tree[j]
    }
  }
}
for (i in 1:nrow(avgpasser)) {
  if (is.na (avgpasser$scientific[i]==TRUE)) {
    avgpasser$scientific[i]<-avgpasser$species[i]
  }
}
## make song pace of single note song as note rate
avgpasser$Rate_Pace <- NA
for (i in 1:nrow(avgpasser)){
  if (avgpasser$No_notes_in_call_bout.song [i] ==1 && avgpasser$No_of_Note_Types[i] ==1){
    avgpasser$Rate_Pace[i] <- avgpasser$Single.Note.Song.Pace[i]
  }
  else avgpasser$Rate_Pace[i] <-avgpasser$Note_Rate[i]
}
write_xlsx(avgpasser, "../data/avgpassertree.xlsx")
# attached scientific name to other trait
b10k<-read_xlsx("../data/selected_trait.xlsx",1)
b10k<-data.frame(b10k$species,b10k$unidirectional, b10k$migration, b10k$habitat, b10k$m_mass, b10k$unsexed_mass, b10k$s_bond_stab, b10k$territory,
                 b10k$bill_nares,b10k$bill_total_culmen,b10k$bill_width, b10k$bill_depth)
b10k$scientific <-NA
b10k$b10k.species <-str_replace(b10k$b10k.species, "_", " ")
for (i in 1:nrow(b10k)){
  for (j in 1:nrow(avgpasser)){
    if (b10k$b10k.species [i] == avgpasser$species [j]){
      b10k$scientific [i] <- avgpasser$scientific[j]
    }
  }
}
b10k$b10k.mass <- NA
for (i in 1:nrow(b10k)){
  if (is.na(b10k$b10k.m_mass [i]) == TRUE){
    b10k$b10k.mass [i] <- b10k$b10k.unsexed_mass[i]
  }
  else b10k$b10k.mass [i] <- b10k$b10k.m_mass[i]
}



# make index for trait
b10k$territory <-NA
b10k$territory[which(b10k$b10k.territory== "none")] <-1
b10k$territory[which(b10k$b10k.territory== "weak")] <-2
b10k$territory[which(b10k$b10k.territory== "strong")] <-3

b10k$s_bond_stab <-NA
b10k$s_bond_stab[which(b10k$b10k.s_bond_stab== "none")] <-1
b10k$s_bond_stab[which(b10k$b10k.s_bond_stab== "weak")] <-2
b10k$s_bond_stab[which(b10k$b10k.s_bond_stab== "strong")] <-3


# pca for bill traits

hist(b10k$b10k.bill_nares)
hist(b10k$b10k.bill_depth)
hist(b10k$b10k.bill_total_culmen)
hist(b10k$b10k.bill_width)
bill.log<-data.frame(b10k$scientific,
                     log(b10k$b10k.bill_nares),
                     log(b10k$b10k.bill_depth),
                     log(b10k$b10k.bill_total_culmen),
                     log(b10k$b10k.bill_width))
bill.logscale<-data.frame(bill.log$b10k.scientific,
                          scale(bill.log$log.b10k.b10k.bill_nares.,center=TRUE,scale=TRUE), 
                          scale(bill.log$log.b10k.b10k.bill_depth.,center=TRUE,scale=TRUE),
                          scale(bill.log$log.b10k.b10k.bill_total_culmen.,center=TRUE,scale=TRUE),
                          scale(bill.log$log.b10k.b10k.bill_width.,center=TRUE,scale=TRUE))

MyTree<-read.nexus("../data/output.nex") 
tree.consensus2 = consensus(MyTree, p = 0.5)
MyTree.1 = compute.brlen(tree.consensus2, 1)
# MyTree.2<-MyTree[[1]]
bill.logscale$bill.log.b10k.scientific <-str_replace(bill.logscale$bill.log.b10k.scientific, " ", "_")
bill.logscale <- bill.logscale[match(MyTree.1$tip.label,bill.logscale$bill.log.b10k.scientific),] 
bill.logscale <- na.omit(bill.logscale)
combined.bill<-comparative.data(data=bill.logscale,phy=MyTree.1,names.col = "bill.log.b10k.scientific",vcv = TRUE,warn.dropped = TRUE)

pPCA.bill<- phyl.pca(tree=combined.bill$phy,Y=combined.bill$data,method="BM",mode="corr")
pPCAScores_bill<-pPCA.bill$S
write.csv(pPCAScores_bill,"../data/pPCAScores_bill.csv")
pPCALoadings_bill<-pPCA.bill$L
write.csv(pPCALoadings_bill,"../data/pPCALoadings_bill.csv")
summary(pPCA.bill)

biplot(pPCA.bill)
pdf(file="../data/biplot_bill.pdf",width=10,height=10)
biplot(pPCA.bill)
dev.off()

b10k$bill_PC1 <- NA
b10k$bill_PC2 <- NA
row.names(pPCAScores_bill) <- str_replace(row.names(pPCAScores_bill), "_", " ")
pPCAScores_bill <- as.data.frame(pPCAScores_bill)
for (i in 1:nrow(b10k)){
  for (j in 1:nrow(pPCAScores_bill)){
    if (b10k$b10k.species [i] == row.names(pPCAScores_bill) [j]){
      b10k$bill_PC1 [i] <- pPCAScores_bill$PC1 [j]
      b10k$bill_PC2 [i] <- pPCAScores_bill$PC2 [j]
    }
  }
}

write_xlsx(b10k, "../data/b10k.xlsx")

