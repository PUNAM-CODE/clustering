####################clustring ques 1########
#################crime#####################
library(readr)
crime_data <- read_csv("E:/data science r studio/Assignment code 1/custing/crime_data.csv")

mydata<-crime_data
mydata

###normalize data

normaliza<-scale(mydata[,2:4])

#calculate distance

d<-dist(normaliza,method="euclidean")

#fit herachical distance
fit<-hclust(d,method = "complete")
str(fit)
fit$order
fit$labels
fit$height

#display the dendograph

plot(fit)
plot(fit,hang = -1)
 fit$order

 # represent the size of clustring
 
 rect.hclust(fit,k=4,border = "blue")

 group<-cutree(fit,k=4) 

 group 

 membership<-as.matrix(group)
 View(membership)

 final<-data.frame(mydata,membership) 
final 
View(final)

final123<-final[c(ncol(final),1:(ncol(final)-1))]
View(final123)
write.csv(final123, file="final.csv",row.names = F)
getwd()
aggregate(crime_data[-1],by=list(final123$membership),mean)
View(final123) 

