



# ---------------------------------------------
# non Metric Multidimensional Scaling
# 15 Oct 2021
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  

libraries <- c("vegan", "ggplot2", "dplyr")
lapply(libraries, require, character.only = TRUE)


library(devtools)
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis") 
library(pairwiseAdonis)




laselva_full  <- read.csv("data/laSelva.csv")
head(laselva_full)


laselva_sp <- dplyr::select(laselva_full, Baetid:Collemb)
str(laselva_sp)
ncol(laselva_sp)
nrow(laselva_sp)


# nMDS
set.seed(1) # Con este comando, siempre comenzara del mismo lugar.

laselva.mds <- metaMDS(laselva_sp, distance = "bray", k = 2, trymax=100)  #using all the defaults
laselva.mds


plot(laselva.mds, type="t")




#######################################################
# ggplot
######################################################

# data score son los axis de la ordenacion
data.scores <- as.data.frame(scores(laselva.mds)) 


# agregar species 
species.scores <- as.data.frame(scores(laselva.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data


# Agregar el mes a data.scores
month  <- dplyr::select(laselva_full, month)
data.scores$month <- unlist(month)

 
grp.a <- data.scores[data.scores$month == "one", ][chull(data.scores[data.scores$month == 
          "one", c("NMDS1", "NMDS2")]),]  # hull values for grp A
grp.b <- data.scores[data.scores$month == "two", ][chull(data.scores[data.scores$month == 
          "two", c("NMDS1", "NMDS2")]),]  # hull values for grp A  
grp.c <- data.scores[data.scores$month == "three", ][chull(data.scores[data.scores$month == 
          "three", c("NMDS1", "NMDS2")]),]  # hull values for grp A  
grp.d <- data.scores[data.scores$month == "four", ][chull(data.scores[data.scores$month == 
          "four", c("NMDS1", "NMDS2")]),]  # hull values for grp A  

hull.data <- rbind(grp.a, grp.b, grp.c, grp.d)


ggplot() + 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=month,group=month),alpha=0.30) + # add the convex hulls
#  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=month,colour=month),size=4) + # add the point markers
#  scale_colour_manual(values=c("one" = "red", "two" = "blue",
#                                "three"="black", "four"="green")) +
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())


##############################################################################
#Permanova
adonis(laselva_sp ~ month, data = laselva_full, permutations = 999, method="bray")


pair.mod<-pairwise.adonis(laselva_sp,factors=laselva_full$month)
pair.mod


##############################################################################
#Simper
laselva_simper <- with(laselva_full, simper(laselva_sp, laselva_full$month), permutations = 999)
summary(laselva_simper, ordered = TRUE,
        digits = max(3,getOption("digits") - 3))


############################################################################
# Indicator value
ind_species<-multipatt(laselva_sp, laselva_full$month, max.order=3,
                       func="IndVal",control=how(nperm=999))
summary(ind_species)


