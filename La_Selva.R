

libraries <- c("vegan", "ggplot2", "dplyr")
lapply(libraries, require, character.only = TRUE)

laselva_full  <- read.csv("data/laSelva.csv")
head(laselva_full)


laselva_sp <- dplyr::select(laselva_full, Baetid:Collemb)
str(laselva_sp)
ncol(laselva_sp)
nrow(laselva_sp)


set.seed(1) # Con este comando, siempre comenzara del mismo lugar.

laselva.mds <- metaMDS(laselva_sp, distance = "bray", k = 2, trymax=100)  #using all the defaults
laselva.mds


plot(laselva.mds, type="t")


#Permanova
adonis(laselva_sp ~ month, data = laselva_full, permutations = 999, method="bray")

library(devtools)
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis") 
library(pairwiseAdonis)

pair.mod<-pairwise.adonis(laselva_sp,factors=laselva_full$month)
pair.mod


#Simper
laselva_simper <- with(laselva_full, simper(laselva_sp, laselva_full$month), permutations = 999)
summary(laselva_simper, ordered = TRUE,
        digits = max(3,getOption("digits") - 3))


# Indicator value

ind_species<-multipatt(laselva_sp, laselva_full$month, max.order=1,
                       func="IndVal",control=how(nperm=999))
summary(ind_species)


