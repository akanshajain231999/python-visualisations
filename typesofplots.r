rm(list=ls())
setwd("C:/Users/admin/akansha")
getwd()
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')
lapply(x,require,character.only=TRUE)

mtdata=read.csv("marketing_train.csv",header=T)
mtdata$schooling[mtdata$schooling %in% "illiterate"] = "unknown"
mtdata$schooling[mtdata$schooling %in% c("basic.4y","basic.6y","basic.9y","high.school","professional.course")] = "high.school"
mtdata$default[mtdata$default %in% "yes"] = "unknown"
mtdata$default = as.factor(as.character(mtdata$default))
mtdata$marital[mtdata$marital %in% "unknown"] = "married"
mtdata$marital = as.factor(as.character(mtdata$marital))
mtdata$month[mtdata$month %in% c("sep","oct","mar","dec")] = "dec"
mtdata$month[mtdata$month %in% c("aug","jul","jun","may","nov")] = "jun"
mtdata$month = as.factor(as.character(mtdata$month))
mtdata$loan[mtdata$loan %in% "unknown"] = "no"
mtdata$loan = as.factor(as.character(mtdata$loan))
mtdata$schooling = as.factor(as.character(mtdata$schooling))
mtdata$profession[mtdata$profession %in% c("management","unknown","unemployed","admin.")] = "admin."
mtdata$profession[mtdata$profession %in% c("blue-collar","housemaid","services","self-employed","entrepreneur","technician")] = "blue-collar"
mtdata$profession = as.factor(as.character(mtdata$profession))
str(mtdata)
mv=data.frame(apply(mtdata,2,function(x){sum(is.na(x))}))
mv$column=rownames(mv)
rownames(mv)=NULL
names(mv)[1]="missing_percentage"
mv$missing_percentage=(mv$missing_percentage/nrow(mtdata))*100
mv=mv[order(-mv$missing_percentage),]
mv=mv[,c(2,1)]
write.csv(mv,"missing_per.csv",row.names=F)
mtdata=knnImputation(mtdata,k=5)
unique(mtdata$profession)
for(i in 1:ncol(mtdata)){
  if(class(mtdata[,i])=='factor'){
    mtdata[,i]=factor(mtdata[,i],labels=(1:length(levels(factor(mtdata[,i])))))
  }
}
library("ggplot2")
library("scales")
library("psych")
library("gplots")
#bar graph
ggplot(mtdata,aes_string(x=mtdata$profession))+
  geom_bar(stat="count",fill="DarkslateBlue")+
  theme_bw()+xlab("profession")+ylab("count")+
  scale_y_continuous(breaks=pretty_breaks(n=10))+
  ggtitle("Marketing Campaign Analysis")+
  theme(text=element_text(size=15))

# histogram
ggplot(mtdata,aes_string(x=mtdata$custAge))+
  geom_histogram(fill="DarkslateBlue",colour="black")+
  geom_density()+xlab("custage")+ylab("frequency")+
  scale_y_continuous(breaks=pretty_breaks(n=10))+
  theme_bw()+ggtitle("Marketing Campaign Analysis")+
  theme(text=element_text(size=20))
#Boxplot
ggplot(mtdata,aes_string(x=mtdata$responded,y=mtdata$custAge,fill=mtdata$responded))+
  geom_boxplot(outlier.colour="red",outlier.size=3)+
  xlab("responded")+ylab("custAge")+guides(fill=FALSE)
  scale_y_continuous(breaks=pretty_breaks(n=10))+
  theme_bw()+ggtitle("Marketing Campaign Analysis")+
  theme(text=element_text(size=20))
  
#scatter plot
  ggplot(mtdata,aes_string(x=mtdata$campaign,y=mtdata$custAge))+
    geom_point(aes_string(colour=mtdata$responded,shape=mtdata$profession),size=4)+
    xlab("campaign")+ylab("custAge")+guides(fill=FALSE)+
  scale_y_continuous(breaks=pretty_breaks(n=10))+
    scale_x_continuous(breaks=pretty_breaks(n=10))+
    scale_colour_discrete(name="responded")+
    scale_shape_discrete(name="profession")+
    theme_bw()+ggtitle("scatter plot Analysis")+
    theme(text=element_text(size=20))
  
  
  




