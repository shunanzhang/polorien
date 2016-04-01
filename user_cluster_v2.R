setwd("~/documents/polorien")
rm(list=ls())
set.seed(1280)

# install.packages("devtools")
#devtools::install_github("kassambara/factoextra")
library("factoextra")
# Helper function : 
# Correlation between variables and principal components
var_cor_func <- function(var.loadings, comp.sdev){
  var.loadings*comp.sdev
}

dat = data.frame(read.csv("data_3.csv"), stringsAsFactors = FALSE)  
colnames(dat) <- c("user", "site", "pv")

pv_per_site = aggregate(pv ~ site, data = dat, FUN = sum, na.rm=TRUE)
pv_per_site = pv_per_site[which(pv_per_site$pv>=10000),]

pv_per_user = aggregate(pv ~ user, data = dat, FUN = sum, na.rm=TRUE)
pv_per_user = pv_per_user[which(pv_per_user$pv>=100),]

Ns = dim(pv_per_site)[1]
Nu = dim(pv_per_user)[1]

o = matrix(NA, Nu, Ns)
for (n in 1:Nu){
  ptm = proc.time()
  for (s in 1:Ns){
    temp = intersect(which(dat$user == pv_per_user$user[n]),
      which(dat$site == pv_per_site$site[s]))
    if (length(temp) != 0){
      o[n,s] =  dat$pv[temp]
    }
  }
  print(paste('finished','user',n))
  print(proc.time() - ptm)
}  
save.image("workspace.RData")

p = data.frame(o/(as.matrix(apply(o,1,sum,na.rm=TRUE)) %*% rep(1,Ns)))
p[is.na(p)] = 0
colnames(p) = pv_per_site$site


pca <- prcomp(p, scale = TRUE)
# Eigenvalues
eig <- (pca$sdev)^2
# Variances in percentage
variance <- eig*100/sum(eig)
# Cumulative variances
cumvar <- cumsum(variance)
eig.p <- data.frame(eig = eig, variance = variance, cumvariance = cumvar)

barplot(eig.p[, 2], names.arg=1:nrow(eig.p), 
        main = "",
        xlab = "Principal Components",
        ylab = "Eigenvalues",
        col ="orange")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.p), 
      eig.p[, 2], 
      type="b", pch=19, col = "black")
var <- get_pca_var(pca)

# Variable correlation/coordinates
loadings <- pca$rotation
sdev <- pca$sdev
var.coord <- var.cor <- t(apply(loadings, 1, var_cor_func, sdev))

# Plot the correlation circle
a <- seq(0, 2*pi, length = 100)
plot( cos(a)*.6, sin(a)*.6, type = 'l', col="gray",
      xlab = "Liberal - Conservative",  ylab = "News - Opinion")
abline(h = 0, v = 0, lty = 2)
# Add active variables
#arrows(0, 0, var.coord[, 2], var.coord[, 1], 
#       length = 0.1, angle = 15, code = 2)
#points(var.coord[,2], var.coord[,1],pch='+')
# Add labels
text(var.coord[,2:1], labels=rownames(var.coord), cex = .6, adj=.5)

ind.coord <- pca$x

plot(ind.coord[5100:5200,1], ind.coord[100:200,2], pch = 19,  
     xlab="PC1",ylab="PC2")
abline(h=0, v=0, lty = 2)
text(ind.coord[,1], ind.coord[,2], labels=rownames(ind.coord),
     cex=0.7, pos = 3)

plot(ind.coord[,1],ind.coord[,2],pch='.', xlab='News-Opinion Orientation', ylab='L-R Orientation')
abline(h=0, v=0, lty = 2, col = 'red')
hist(ind.coord[,2], breaks = 100)
hist(ind.coord[,1], breaks = 100)

# Determine number of clusters
wss <- (nrow(p)-1)*sum(apply(p,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(p,centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
library(mclust)
fit <- Mclust(p)
plot(fit) # plot results 
summary(fit) # display the best model

facebook = c(-0.4802, -0.1605, -0.1731, -0.1565, -0.8883, -0.4446, 0.9136, -0.0585, -0.5242, 
             -0.3117, 0.0503, -0.2705, -0.2565, NA, NA, NA, NA, NA, -0.5225, 0.7754, -0.7749, NA,
             -0.1405, 0.8955, -0.3995, 0.2860,-0.4106, NA, -0.7083, -0.8663, -0.8102, 0.9009, 
             -0.2735, 0.2497, -0.2726, -0.5723, -0.2034, -0.1334, NA, 0.3502, -0.0945, -0.6487, 
             -0.8753, -0.6813, -0.5424, -0.2022, 0.8887, -0.4561, 0.8559, -0.5723, -0.6176, -0.3598,
             -0.7584, 0.8998, NA, 0.6975, 0.1661, -0.8615, -0.3336, -0.8614, -0.0635, NA, -0.6267, -0.4284,
             NA, -0.6591, 0.8124)

df_fs = scale(data.frame(var.coord[,2], facebook))
plot(df_fs, type = 'n', xlab='SmartNews L-R Score', ylab = 'Facebook L-R Score', xlim=c(-3,3), 
     ylim=c(-2,2), panel.first = grid())
text(df_fs, labels=rownames(df_fs), cex = .6, adj=.5)
