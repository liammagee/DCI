#######################################################
#Tutorial in Exploratory Data Analysis (useR!2008, Dormundt August 10th 2008)
#######################################################

###############################
# Correspondence Analysis on perfume
###############################

library(FactoMineR)
perfume = read.table("perfume.txt",header=T,sep="\t",row.names=1)
res.ca = CA(perfume,col.sup=16:39)

plot(res.ca,invisible="row")
plot(res.ca,invisible=c("col","col.sup"))

###############################
# Multiple Correspondence Analysis on tea
###############################

library(FactoMineR)
data(tea)
summary(tea)
par(ask=T)
for (i in 1:6) barplot(table(tea[,i]))

res.mca=MCA(tea,quanti.sup=19,quali.sup=20:36)

plot(res.mca,invisible=c("var","quali.sup","quanti.sup"),cex=0.7)

plot(res.mca,invisible=c("ind","quali.sup","quanti.sup"),cex=0.8)

plot(res.mca,invisible=c("quali.sup","quanti.sup"),cex=0.8)

#########
plot(res.mca$ind$coord[,1:2],col=as.numeric(tea[,17]),pch=20)
legend("topright",legend=levels(tea[,17]),text.col=1:3,col=1:3)
aa=by(res.mca$ind$coord,tea[,17],FUN=mean)[[3]][1:2]
points(aa[1],aa[2],col=3,pch=15,cex=1.5)
x11()
plot(res.mca$ind$coord[,1:2],col=as.numeric(tea[,18]),pch=20)
legend("topright",legend=levels(tea[,18]),text.col=1:6,col=1:6)
bb=by(res.mca$ind$coord,tea[,18],FUN=mean)[[5]][1:2]
points(bb[1],bb[2],col=5,pch=15,cex=1.5)
##########

plot(res.mca,invisible=c("var","ind","quanti.sup"),cex=0.8)

dimdesc(res.mca)

res.mca$eig

#################
aa=as.factor(rep(1:10,each=100))
bb=cbind.data.frame(aa,aa,aa,aa,aa,aa,aa,aa,aa,aa)
colnames(bb)=paste("a",1:10,sep="")
res=MCA(bb,graph=FALSE)
res$eig[1:10,]
#################

###########
burt=t(tab.disjonctif(tea[,1:18]))%*%tab.disjonctif(tea[,1:18])
res.burt=CA(burt)
res.burt$eig[1:10,]
############

res.mca$ind$contrib
res.mca$ind$cos2
res.mca$var$contrib
res.mca$var$cos2


###################
## Clustering
###################


res.mca=MCA(tea,quanti.sup=19,quali.sup=20:36,ncp=20,graph=F)
library(cluster)
classif = agnes(res.mca$ind$coord,method="ward")
plot(classif,main="Dendrogram",ask=F,which.plots=2,labels=FALSE)

clust = cutree(classif,k=3)
tea.comp = cbind.data.frame(tea,res.mca$ind$coord[,1:3],factor(clust))
res.aux=MCA(tea.comp,quanti.sup=c(19,37:39),quali.sup=c(20:36,40),graph=F)
plot(res.aux,invisible=c("quali.sup","var","quanti.sup"),habillage=40)

catdes(tea.comp,ncol(tea.comp))
