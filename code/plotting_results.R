#load data and values from previous script
source("ac_pgls.R")

# Create 2 vectors
a <- select_trait_AC$binary_unidirectional[which(select_trait_AC$binary_unidirectional == 0)]
b <- select_trait_SC$binary_unidirectional[which(select_trait_AC$binary_unidirectional == 3)]

# Make a list of these 2 vectors
C <- list(a,b)

# Change the names of the elements of the list :
names(C) <- c(paste("Category 1\n n=" , length(a) , sep=""), paste("Category 2\n n=" , length(b) , sep=""))

# Change the mgp argument: avoid text overlaps axis
par(mgp=c(3,2,0))

# territory Boxplot
pdf(file="../data/Territoriality.pdf",width=6,height=5)
par(mfrow=c(2,2),mar=c(2, 2, 2, 2),oma = c(2, 0.3, 0.2, 0.3))
par(mai=c(0.5,0.7,0.2,0),bty="l")
boxplot(select_trait_AC$PC1~select_trait_AC$binary_territory, xlab="",ylab="Aves PC1 *", names=c("Weak", "Strong"), col="steelblue4")
par(mai=c(0.5,0.7,0.2,0),bty="l")
boxplot(select_trait_passer$PC1~select_trait_passer$binary_territory, xlab="", ylab="Passerines PC1 *",names=c("Weak", "Strong"), col="tomato4")
par(mai=c(0.7,0.7,0,0),bty="l")
boxplot(select_trait_AC$PC2~select_trait_AC$binary_territory, xlab="", ylab="Aves PC2 *", names=c("Weak", "Strong"),col="steelblue4")
par(mai=c(0.7,0.7,0,0),bty="l")
boxplot(select_trait_passer$PC2~select_trait_passer$binary_territory, xlab="", ylab="Passerines PC2 *", names=c("Weak", "Strong"),col="tomato4")
mtext(text="Territoriality",side=1,line=0,outer=TRUE)
dev.off()


# mating
pdf(file="../data/Mating.pdf",width=6,height=5)
par(mfrow=c(2,2),mar=c(2, 2, 2, 2),oma = c(2, 0.3, 0.2, 0.3))
par(mai=c(0.5,0.7,0.2,0),bty="l")
boxplot(select_trait_AC$PC1~select_trait_AC$binary_unidirectional, xlab="",ylab="Aves PC1", names=c("Monogamous", "Polygamous"), col="steelblue4")
par(mai=c(0.5,0.7,0.2,0),bty="l")
boxplot(select_trait_passer$PC1~select_trait_passer$binary_unidirectional, xlab="", ylab="Passerines PC1",names=c("Monogamous", "Polygamous"), col="tomato4")
par(mai=c(0.7,0.7,0,0),bty="l")
boxplot(select_trait_AC$PC2~select_trait_AC$binary_unidirectional, xlab="", ylab="Aves PC2 *", names=c("Monogamous", "Polygamous"),col="steelblue4")
par(mai=c(0.7,0.7,0,0),bty="l")
boxplot(select_trait_passer$PC2~select_trait_passer$binary_unidirectional, xlab="", ylab="Passerines PC2 *", names=c("Monogamous", "Polygamous"),col="tomato4")
mtext(text="Mating system",side=1,line=0,outer=TRUE)
dev.off()


# habitat
pdf(file="../data/Habitat.pdf",width=6,height=5)
par(mfrow=c(2,2),mar=c(2, 2, 2, 2),oma = c(2, 0.3, 0.2, 0.3))
par(mai=c(0.5,0.7,0.2,0),bty="l")
boxplot(select_trait_AC$PC1~select_trait_AC$binary_habitat, xlab="",ylab="Aves PC1",  col="steelblue4")
par(mai=c(0.5,0.7,0.2,0),bty="l")
boxplot(select_trait_passer$PC1~select_trait_passer$binary_habitat, xlab="", ylab="Passerines PC1",col="tomato4")
par(mai=c(0.7,0.7,0,0),bty="l")
boxplot(select_trait_AC$PC2~select_trait_AC$binary_habitat, xlab="", ylab="Aves PC2 *",col="steelblue4")
par(mai=c(0.7,0.7,0,0),bty="l")
boxplot(select_trait_passer$PC2~select_trait_passer$binary_habitat, xlab="", ylab="Passerines PC2 *", col="tomato4")
mtext(text="Habitat",side=1,line=0,outer=TRUE)
dev.off()

# bill and mass
library(ggplot2)
plot(PC1~b10k.mass, data = select_trait_AC)
abline(pgls(PC1~b10k.mass,dataset,delta = "ML"))
massPC1<- pgls(PC1~b10k.mass,dataset,delta = "ML")
massPC2 <- pgls(PC2~b10k.mass,dataset,delta = "ML")
billPC1<-pgls(PC1~bill_PC1,dataset,delta = "ML")
bilPC2<-pgls(PC2~bill_PC1,dataset,delta = "ML")
pasPC1mass1<- pgls(PC1~b10k.mass,dataset_passer,delta = "ML")
pasPC2mass1<- pgls(PC2~b10k.mass,dataset_passer,delta = "ML")
pasbillPC1 <-pgls(PC1~bill_PC1,dataset_passer,delta = "ML")
pasbillPC2 <-pgls(PC2~bill_PC1,dataset_passer,delta = "ML")

pdf(file="../data/Morphology.pdf",width=7,height=4)
par(mfrow=c(2,4),mar=c(2, 2, 2, 2),oma = c(2, 0, 0.2, 0)) 
par(mai=c(0.5,0.7,0.2,0))
plot(PC1~b10k.mass, data = select_trait_AC, xlab="", ylab = "Aves Vocalisation PC1 *",pch=20, col="steelblue4")
abline(massPC1)
par(mai=c(0.5,0.4,0.2,0.3))
plot(PC1~bill_PC1, data = select_trait_AC, xlab="", ylab = "",pch=20, col="tomato4")
abline(billPC1)
par(mai=c(0.5,0.6,0.2,0.1))
plot(PC2~b10k.mass, data = select_trait_AC, xlab="", ylab = "Aves Vocalisation PC2",pch=20,col="steelblue4")
abline(massPC2)
par(mai=c(0.5,0.3,0.2,0.4))
plot(PC2~bill_PC1, data = select_trait_AC, xlab="", ylab = "",pch=20, col="tomato4")
abline(bilPC2)
par(mai=c(0.7,0.7,0,0))
plot(PC1~b10k.mass, data = select_trait_passer, xlab="log(Body Mass)", ylab = "Passerines Song PC1",pch=20,col="steelblue4")
abline(pasPC1mass1)
par(mai=c(0.7,0.4,0,0.3))
plot(PC1~bill_PC1, data = select_trait_passer, xlab="Bill PC1", ylab = "PC1",pch=16,col="tomato4")
abline(pasbillPC1)
par(mai=c(0.7,0.6,0,0.1))
plot(PC2~b10k.mass, data = select_trait_passer, xlab="log(Body Mass)", ylab = "Passerines Song PC2",pch=20,col="steelblue4")
abline(pasPC2mass1)
par(mai=c(0.7,0.3,0,0.4))
plot(PC2~bill_PC1, data = select_trait_passer, xlab="Bill PC1", ylab = "Passerines Song PC2",pch=16,col="tomato4")
abline(pasbillPC2)
mtext(text="Morphology",side=1,line=0,outer=TRUE)
dev.off()

