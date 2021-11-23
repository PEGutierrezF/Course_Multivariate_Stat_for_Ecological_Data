



library(ggplot2)
library(dplyr)
library(missMDA) # Imputate
library(ggfortify) # autoplot()
library(cluster) #pam
library(factoextra) #get_pca_var()
library(data.table) # data.table()
library(labdsv) #loadings.pca(pca)
library(missForest)

library(devtools)

install_github("vqv/ggbiplot") #ggbiplot
library(ggbiplot)

channel <- read.csv("data/PCA_channel_form.csv", header=TRUE)

channel_1 <- select(channel, -Forma)
summary(channel_1)


# Imputate ----------------------------------------------------------------

df1 <- select(channel_1, Elevacion, Ancho, Velocidad, Rocas, 
              Canto, grava, arena, Limo)

df1_a <- missForest(df1, maxiter = 4, ntree = 100,
                           variablewise = TRUE, decreasing = FALSE, verbose = F, replace = TRUE,
                           classwt = NULL, cutoff = NULL, strata = NULL,
                           sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                           xtrue = NA, parallelize = "no")
class(df1_a$ximp)

df2_a <- select(channel_1, Elevacion, NAtemp, NASatO2)


# New data frame ----------------------------------------------------------


new_channel <- do.call("merge", c(lapply(list(df1_a$ximp, df2_c), data.frame, 
                      row.names=NULL), by = 0, all = TRUE, sort = FALSE))[-1]
#remove 1 elevation
new_channel_1 <- select(new_channel, -Elevacion.y)


channel.pca <- prcomp(new_channel_1, center = TRUE, scale =TRUE)
summary(channel.pca)


layout(matrix(1:2, ncol=2))
screeplot(channel.pca)
screeplot(channel.pca, type="lines")

variance <- (channel.pca$sdev)^2
varPercent <- variance/sum(variance) * 100
barplot(varPercent, xlab='PC', ylab='Percent Variance',
        names.arg=1:length(varPercent), las=1, col='gray') +
  abline(h=1/ncol(new_channel_1)*100, col="red")



PCA<- fviz_pca_biplot(channel.pca, label = "var", habillage=channel$Forma,
                addEllipses=TRUE, ellipse.level=0.95,
                ggtheme = theme_minimal())

PCA
# ggplot  -----------------------------------------------------------------

data <- data.table(PC1=channel.pca$x[,1], PC2=channel.pca$x[,2], Forma= channel[,1])
data <- data[order(channel$Forma),]

ggplot(data, aes(x=PC1,y=PC2)) +
  geom_point(size = 2, aes(color=Forma))
