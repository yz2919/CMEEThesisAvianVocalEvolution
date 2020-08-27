rm(list=ls())
install.packages(c("readxl","writexl")) 
install.packages("stringr")
install.packages("sprintf")
install.packages("psych",dependencies=TRUE)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(stringr)

setwd("~/Downloads/Birdsong/code/")
rawsong <- read_xlsx("../data/Raw_B10K_Song_Traits.xlsx",1)

rawsong[,26:95] <- lapply(rawsong[,26:95],as.numeric)
rawsong$Single.Note.Song.Pace <- as.numeric(rawsong$Single.Note.Song.Pace)
# rawsong[!apply(rawsong == "", 1, all),]
# summary(rawsong)
rawsong$Family <- str_extract_all(rawsong$HM4.Family, "^\\w+")
avgsong <- read_xlsx("../data/Main_B10K_Song_Average.xlsx",1)
avgsong$species <- str_replace(avgsong$species, "_", " ")


# rawsong <- rawsong %>% nest(-HM4.Latin.name) 
avgsong <- avgsong[(avgsong$species %in% rawsong$HM4.Latin.name),]
rawsong <- rawsong[(rawsong$HM4.Latin.name %in% avgsong$species),]
birdtrait <- read_xlsx("../data/Main_B10K_Trait_datasheet.xlsx",skip = 1)
birdtrait$species <- str_replace(birdtrait$species, "_", " ")

#make note rate for single note song NA
for (i in 1:nrow(rawsong)){
  if (rawsong$No_notes_in_call_bout.song [i] ==1 && rawsong$No_of_Note_Types[i] ==1){
    rawsong$Note_Rate[i] <- NA
  }
}

# mean for single note song pace
single_mean <- rawsong %>% group_by(HM4.Latin.name) %>% summarise(Mean = mean(Single.Note.Song.Pace, na.rm = TRUE))
rawsong <- rawsong %>% nest(-HM4.Latin.name) 



# collecting mean of song traits
for (i in 1:nrow(avgsong)){ 
  for (j in 1:nrow(rawsong)){
    if (avgsong$species [i] == rawsong$HM4.Latin.name [j]){ # match species names
      rawsub <- rawsong$data[[j]] #dubset for the species
      if (length(which(rawsub$No_notes_in_call_bout.song== 1 && rawsub$No_of_Note_Types == 1 )) > 0 ){
        avgsong$`Number of samples`[i] <- nrow(rawsub)
        avgsong$Annotated[i] <- paste("yes")
        avgsong$HM4.Order[i] <- unique(rawsub$HM4.Order)
        avgsong$HM4.Family[i] <- unique(rawsub$HM4.Family)
        avgsong$HM4.Common.name [i] <- unique(rawsub$HM4.Common.name)
        avgsong$HM4.Latin.name [i] <- unique(rawsong$HM4.Latin.name [j])
        # avgsong$HM4.Subspecies [i] <- unique(rawsub$HM4.Subspecies)
        avgsong$No_notes_in_call_bout.song[i]<- mean(rawsub$No_notes_in_call_bout.song,na.rm = TRUE)
        avgsong$No_of_Note_Types[i]<- mean(rawsub$No_of_Note_Types,na.rm = TRUE)
        # avgsong$Single.Note.Song.Pace[i]<- mean(rawsub$Single.Note.Song.Pace,na.rm = TRUE) 
        avgsong[i,15:85] <- rawsub %>%
          select(Note_Min_Freq_Average:Max.Power3) %>%
          colMeans(na.rm = TRUE) %>%
          unname()
        avgsong$Single.Note.Song.Pace[i] <- single_mean$Mean[single_mean$HM4.Latin.name == avgsong$species [i]]
      }
      else {
        avgsong$`Number of samples`[i] <- nrow(rawsub)
        avgsong$Annotated[i] <- paste("yes")
        avgsong$HM4.Order[i] <- unique(rawsub$HM4.Order)
        avgsong$HM4.Family[i] <- unique(rawsub$HM4.Family)
        avgsong$HM4.Common.name [i] <- unique(rawsub$HM4.Common.name)
        avgsong$HM4.Latin.name [i] <- unique(rawsong$HM4.Latin.name [j])
        # avgsong$HM4.Subspecies [i] <- unique(rawsub$HM4.Subspecies)
        avgsong$No_notes_in_call_bout.song[i]<- mean(rawsub$No_notes_in_call_bout.song,na.rm = TRUE)
        avgsong$No_of_Note_Types[i]<- mean(rawsub$No_of_Note_Types,na.rm = TRUE)
        # avgsong$Single.Note.Song.Pace[i]<- mean(rawsub$Single.Note.Song.Pace,na.rm = TRUE) 
        avgsong[i,15:85] <- rawsub %>%
          select(Note_Min_Freq_Average:Max.Power3) %>%
          colMeans(na.rm = TRUE) %>%
          unname()
        avgsong[i,"Single.Note.Song.Pace"] <- NA
      }

    }}
}
write_xlsx(avgsong, "../data/avgsong.xlsx")



write.csv(avgsong, "../data/avgsong.csv")


avgsong_passer <- avgsong[which(avgsong$HM4.Order=='Passeriformes'), ]
write_xlsx(avgsong_passer, "../data/avgpasser.xlsx")




# write_xlsx(avgname, "../data/avgname.xlsx")


mean(avgsong$`Number of samples`)
sd(avgsong$`Number of samples`)

length(which(rawsub$No_notes_in_call_bout.song== 1 && rawsub$No_of_Note_Types == 1 )) >0
# class(single_mean$Mean[single_mean$HM4.Latin.name=="Anthoscopus minutus"])


# paste0(unique(rawsub$Observer), unique(rawsub$`Edited by`), sep = "+")
# class(unique(rawsub$HM4.Subspecies))
# mean_song <- function(rawsong_column,avgsong_column){
#   for (i in 1:nrow(avgsong)){
#     for (j in 1:nrow(rawsong)){
#       if (avgsong$species [i] == rawsong$HM4.Latin.name [j]){
#         rawsub <- rawsong$data[[j]]
#         rawsub <- rawsub %>% as.data.frame()
#         avgsong[,sprintf("%s", avgsong_column)][i]<- mean(rawsub[,sprintf("%s", rawsong_column)],na.rm = TRUE)
#       }}
#   }
# }


avgsong$species[!(avgsong$species %in% birdtrait$species)]
birdtrait$species[!(birdtrait$species %in% avgsong$species)]
length(unique(birdtrait$family))
length(unique(avgsong$species))

length(unique(avgsong$HM4.Family)[!(unique(avgsong$HM4.Family) %in% unique(birdtrait$family))])
length(unique(birdtrait$family)[!(unique(birdtrait$family) %in% unique(avgsong$HM4.Family))])
length(unique(avgsong$HM4.Order))
sort(unique(avgsong$HM4.Order))
sort(unique(avgsong$HM4.Family))

Data.num =
  select(avgsong,
         No_notes_in_call_bout.song,
         No_of_Note_Types,
         Song.Duration,
         Note_Bandwidth_Average,
         Note_Peak_Freq_Average,
         Note.Max.Freq.Average,
         Note_Min_Freq_Average)

library(FSA)

headtail(Data.num)

pairs(data=Data.num,
      ~ No_notes_in_call_bout.song + No_of_Note_Types + Song.Duration + Note_Bandwidth_Average + Note_Peak_Freq_Average + Note.Max.Freq.Average + Note_Min_Freq_Average)
lm


cor(Data.num$No_notes_in_call_bout.song,Data.num$No_of_Note_Types)
cor(Data.num$No_of_Note_Types,Data.num$No_notes_in_call_bout.song)
linearMod1 <- lm(No_notes_in_call_bout.song ~ No_of_Note_Types, data=Data.num)  # build linear regression model on full data
print(linearMod1)
summary(linearMod1)
linearMod2 <- lm(No_of_Note_Types ~ No_notes_in_call_bout.song, data=Data.num)
print(linearMod2)
summary(linearMod2)


general_paired_simpleLM <- function (dat_LHS, dat_RHS) {
  ## matrix and its dimension (n: numbeta.ser of data; p: numbeta.ser of variables)
  dat_LHS <- as.matrix(dat_LHS)
  dat_RHS <- as.matrix(dat_RHS)
  if (nrow(dat_LHS) != nrow(dat_RHS)) stop("'dat_LHS' and 'dat_RHS' don't have same number of rows!")
  n <- nrow(dat_LHS)
  pl <- ncol(dat_LHS)
  pr <- ncol(dat_RHS)
  ## variable summary: mean, (unscaled) covariance and (unscaled) variance
  ml <- colMeans(dat_LHS)
  mr <- colMeans(dat_RHS)
  vl <- colSums(dat_LHS ^ 2) - ml * ml * n
  vr <- colSums(dat_RHS ^ 2) - mr * mr * n
  ##V <- crossprod(dat - rep(m, each = n))  ## cov(u, v) = E[(u - E[u])(v - E[v])]
  V <- crossprod(dat_LHS, dat_RHS) - tcrossprod(ml * sqrt(n), mr * sqrt(n))  ## cov(u, v) = E[uv] - E{u]E[v]
  ## R-squared (explained variance) and its complement
  R2 <- (V ^ 2) * tcrossprod(1 / vl, 1 / vr)
  R2_complement <- 1 - R2
  ## slope and intercept
  beta <- V * rep(1 / vr, each = pl)
  alpha <- ml - beta * rep(mr, each = pl)
  ## residual sum of squares and standard error
  RSS <- R2_complement * vl
  sig <- sqrt(RSS * (1 / (n - 2)))
  ## statistics for slope
  beta.se <- sig * rep(1 / sqrt(vr), each = pl)
  beta.tv <- beta / beta.se
  beta.pv <- 2 * pt(abs(beta.tv), n - 2, lower.tail = FALSE)
  ## F-statistic and p-value
  F.fv <- (n - 2) * R2 / R2_complement
  F.pv <- pf(F.fv, 1, n - 2, lower.tail = FALSE)
  ## export
  data.frame(LHS = rep(colnames(dat_LHS), times = pr),
             RHS = rep(colnames(dat_RHS), each = pl),
             alpha = c(alpha),
             beta = c(beta),
             beta.se = c(beta.se),
             beta.tv = c(beta.tv),
             beta.pv = c(beta.pv),
             sig = c(sig),
             R2 = c(R2),
             F.fv = c(F.fv),
             F.pv = c(F.pv),
             stringsAsFactors = FALSE)
}

pwlm<-general_paired_simpleLM(Data.num, Data.num)


avg_tree <- read_xlsx("../data/avgsong_tree.xlsx",1)
tree <- read.csv("../data/BLIOCPhyloMasterTax.csv")
tree_match<-tree[match(tree$Scientific,avg_tree$species),]      
tree_match <- na.omit(tree_match)
avgsong$species[!(avg_tree$species %in% tree$Scientific)]
write.csv(tree_match, "../data/tree_match.csv")

tree_match<-diss_complex_2[match(tree2$tip.label,diss_complex_2$scientific_name),]                                              
tree2$tip.label

birdtrait_tree <- birdtrait

for (i in 1:nrow(birdtrait_tree)){ 
  for (j in 1:nrow(speciesname)){
    if (birdtrait_tree$species [i] == speciesname$Avgsong [j]){ # match species names
      birdtrait_tree$species[i] <- speciesname$tree[j] #dubset for the species
    }
  }
}
mean_indiperfamily <- avgsong %>% group_by(HM4.Family) %>% summarise(nrow(avgsong), na.rm = TRUE)
length(unique(avgsong$HM4.Family))/length(unique(avgsong$species))


