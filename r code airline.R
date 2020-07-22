################clustring Airline###########
library(readxl)
eastwest <- read_excel("airine/eastwest.xlsx")
View(eastwest)

eastwest1<-eastwest[,2:11]
View(eastwest1)

#data normalization beacuse all value is one format

normalization<-scale(eastwest1)
View(normalization)

####find out the distance every cluster

#dist<-dist(normalization,method = "euclidean")

##fit herachical model
#fit<-hclust(dist,method = "single")
#str(fit)
#fit
#plot(fit)# check used of every method such as single,average,complet but dendograph is not 
#cleare represation ,hence partition of data
#All data are 3999 entities,hence 1000 each  entities are partition
#fit1<-hclust(dist[1:1000,],method = "complet")


#part 1:1000

new<-normalization
View(new)
# d1<-dist(new,method="euclidean")
 
 #fit1<-hclust(d1,method = "centroid",members = NULL)
# str(fit1)
#plot(fit1)
#plot(fit1,hang = -1)
#fit1$order

#rect.hclust(fit1,k=3,border = "blue")

#group<-cutree(fit1,k=3)
#group

##data is big hence used the non historical data ,used the kmean algorithem
install(plyr)
library("plyr")


?kmeans

fit<-kmeans(new,3)
str(fit)

fit$cluster

final2<-data.frame(eastwest1,fit$cluster)#append 
final2

final3<-final2[,c(ncol(final2),1:(ncol(final2)-1))]


aggregate(eastwest[,2:11],by= list(fit$cluster),FUN=mean)


##selection k for kmean cluster using k selecton

install.packages("kselection")
library(kselection)
install.packages("doParallel")
library(doParallel)
registerDoParallel(cores=3)
k<-kselection(new,parallel=F,k_threshold=0.95,max_centers=14)
k<-selection(new, fun_cluster=status::kmeans,max_centers=3,k_threshold=0.95, progressBar=FALSE, trace=FALSE,parallel=FALSE)
??Kselection
k
twss<-NULL

for(i in  2:3999){
  twss[i]=sum(kmeans(new,centers = i)$tot.withinss)
}
windows()
plot(2:3999,twss,type = "b",xlab = "Number of clusters",ylab = "within groups")
title(sub="k-mean clustering scree-plot")

t<-xy.coords(new)
plot(t)






plot(new,type ="n")

#sir
install.packages("Animation")
library(animation)
km<-kmeans.ani(new,4)
km
str(km)
plot(km)
