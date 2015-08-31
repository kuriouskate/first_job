setwd('C:/Users/kated/Desktop/k9Campaign') #set working directory
options(scipen=999) #turn off scientific notation
dat.all<-read.csv("k9_all_wknd1.csv")
dat.wash<-read.csv("k9_wash_wknd1.csv")
#dat.ret<-read.csv("k9_return.csv")
dat.all<-as.data.frame(dat.all)
dat.wash<-as.data.frame(dat.wash)
#dat.ret<-as.data.frame(dat.ret)
dat.all<-dat.all[which(dat.all$SPEND>0),]
dat.huh<-dat.all[which(dat.all$ITEMS==0),]
dat.all<-dat.all[which(!(dat.all$UUID %in% dat.huh$UUID)),]
#dat.ret<-dat.ret[which(dat.ret$SPEND>0),]
#dat.huh1<-dat.ret[which(dat.ret$ITEMS==0),]
#dat.ret<-dat.ret[which(!(dat.ret$UUID %in% dat.huh1$UUID)),]

dat.wash1<-dat.all[which((dat.all$UUID %in% dat.wash$UUID)),]
dat.wash<-merge(dat.wash,dat.wash1,by=c("UUID", "JRSTORE", "SPEND", "ITEMS"))

dat.wash.problems<-dat.wash[which(dat.wash$diff_items==0 & dat.wash$diff!=0),]
dat.wash<-dat.wash[which(!(dat.wash$UUID %in% dat.wash.problems$UUID)),]

#wash.match<-dat.wash[which(!(dat.wash$UUID %in% dat.wash2$UUID)),]
#mis.match<-dat.wash2[which(dat.wash2$SPEND.x!=dat.wash2$SPEND.y),]

dat.all$FIRST_DT<-as.character(dat.all$FIRST_DT)
dat.all$LAST_DT<-as.character(dat.all$LAST_DT)
dat.wash$FIRST_DT<-as.character(dat.wash$FIRST_DT)
dat.wash$LAST_DT<-as.character(dat.wash$LAST_DT)
#dat.ret$FIRST_DT<-as.character(dat.ret$FIRST_DT)
#dat.ret$LAST_DT<-as.character(dat.ret$LAST_DT)

#create difference variables
dat.wash$diff<-dat.wash$SPEND-dat.wash$W_SPEND
dat.wash$diff_items<-dat.wash$ITEMS-dat.wash$W_ITEMS
dat.all$avg_price<-dat.all$SPEND/dat.all$ITEMS
dat.wash$avg_price<-dat.wash$SPEND/dat.wash$ITEMS
dat.wash$avg_wash_price<-dat.wash$W_SPEND/dat.wash$W_ITEMS
dat.wash$diff_price<-dat.wash$diff/dat.wash$diff_items

#subset not wash vs. wash only vs. wash plus
not.wash<-dat.all[which(!(dat.all$UUID %in% dat.wash$UUID)),]

#weird<-dat.wash[which(dat.wash$W_SPEND>dat.wash$SPEND),]
#non.out<-dat.all[which(dat.all$SPEND<1000),]

wash.plus<-dat.wash[which(!(dat.wash$diff==0)),]
wash.only<-dat.wash[which((dat.wash$diff==0)),]

#subset known, new, and old customers
known.all<-dat.all[which(dat.all$FIRST_DT!=""),]
new.all<-dat.all[which(dat.all$FIRST_DT=="7/4/2015" | dat.all$FIRST_DT=="7/5/2015"),]
old.all<-known.all[which(!(known.all$UUID %in% new.all$UUID)),]

known.wash<-dat.wash[which(dat.wash$FIRST_DT!=""),]
new.wash<-dat.wash[which(dat.wash$FIRST_DT=="7/4/2015" | dat.wash$FIRST_DT=="7/5/2015"),]
old.wash<-known.wash[which(!(known.wash$UUID %in% new.wash$UUID)),]

known.not<-not.wash[which(not.wash$FIRST_DT!=""),]
new.not<-not.wash[which(not.wash$FIRST_DT=="7/4/2015" | not.wash$FIRST_DT=="7/5/2015"),]
old.not<-known.not[which(!(known.not$UUID %in% new.not$UUID)),]

known.wash.only<-wash.only[which(wash.only$FIRST_DT!=""),]
new.wash.only<-wash.only[which(wash.only$FIRST_DT=="7/4/2015" | wash.only$FIRST_DT=="7/5/2015"),]
old.wash.only<-known.wash.only[which(!(known.wash.only$UUID %in% new.wash.only$UUID)),]

known.wash.plus<-wash.plus[which(wash.plus$FIRST_DT!=""),]
new.wash.plus<-wash.plus[which(wash.plus$FIRST_DT=="7/4/2015" | wash.plus$FIRST_DT=="7/5/2015"),]
old.wash.plus<-known.wash.plus[which(!(known.wash.plus$UUID %in% new.wash.plus$UUID)),]

#boxplot(dat.all$SPEND)
results<-data.frame(matrix(NA, nrow=36, ncol=12))
names(results)<-c("N", "Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max", "SD", "SE", "CI_low", "CI_up", "skew")
spend.vars<-list(dat.all$SPEND, known.all$SPEND, new.all$SPEND, old.all$SPEND,
              dat.wash$SPEND, known.wash$SPEND, new.wash$SPEND, old.wash$SPEND,
              not.wash$SPEND, known.not$SPEND, new.not$SPEND, old.not$SPEND,
              dat.wash$W_SPEND, known.wash$W_SPEND, new.wash$W_SPEND, old.wash$W_SPEND,
              dat.wash$diff, known.wash$diff, new.wash$diff, old.wash$diff,
              wash.plus$SPEND, known.wash.plus$SPEND, new.wash.plus$SPEND, old.wash.plus$SPEND,
              wash.only$SPEND, known.wash.only$SPEND, new.wash.only$SPEND, old.wash.only$SPEND,
              wash.plus$diff, known.wash.plus$diff, new.wash.plus$diff, old.wash.plus$diff,
              wash.plus$W_SPEND, known.wash.plus$W_SPEND, new.wash.plus$W_SPEND, old.wash.plus$W_SPEND)

library(e1071)
for (i in 1:length(spend.vars)) {
  k<-spend.vars[[i]]
  results[i,1]<-length(k)
  m<-summary(k)
  results[i,2:7]<-c(m[[1]], m[[2]],m[[3]], m[[4]], m[[5]], m[[6]])
  results[i,8]<-sd(k)
  sd<-sd(k)
  n<-length(k)
  mean<-mean(k)
  me <- qnorm(.975)*(sd/sqrt(n)) #58.48553
  results[i,9]<-me
  results[i,10]<-mean-me
  results[i,11]<-mean+me
  #skew<-(sum((i-mean)^3))/((n-1)*(sd^3))
  results[i,12]<-skewness(k)
}

library(xlsx)
write.xlsx(results, "C:/Users/kated/Desktop/wknd1_avg_spend.xlsx")


#barplots
tab1<-as.data.frame(prop.table(table(dat.all$ITEMS)))
tab1<-tab1[which(tab1$Freq>0.01),]
tab2<-as.data.frame(prop.table(table(known.all$ITEMS)))
tab2<-tab2[which(tab2$Freq>0.01),]
tab3<-as.data.frame(prop.table(table(new.all$ITEMS)))
tab3<-tab3[which(tab3$Freq>0.01),]
tab4<-as.data.frame(prop.table(table(old.all$ITEMS)))
tab4<-tab4[which(tab4$Freq>0.01),]

try<-merge(tab1,tab2, by=c("Var1"), all=T)
try1<-merge(tab3,tab4, by=c("Var1"), all=T)
try2<-merge(try,try1, by=c("Var1"), all=T)
order(try2)

barplot(t(try[,2:3]), names.arg=try$Var1, col=c("darkblue", "red"),beside=T)

barplot(try, xlab="# of items bought", ylab="Frequency", axes=T, border=c("darkblue", "red"), beside=T)
#title(main="Frequency of Items Bought")
barplot(prop.table(table(dat.all$ITEMS)), las=2, cex.names=0.75, xlab="# of items bought", ylab="Frequency", axes=T, border="red", add=T)


#histograms

#returning customers
ret.new.all<-new.all[which(new.all$LAST_DT!='7/18/2015' & new.all$LAST_DT!='7/19/2015'),]
ret.new.wash<-new.wash[which(new.wash$LAST_DT!='7/18/2015' & new.wash$LAST_DT!='7/19/2015'),]
ret.new.not<-new.not[which(new.not$LAST_DT!='7/18/2015' & new.not$LAST_DT!='7/19/2015'),]


ret.tab<-prop.table(table(ret.new.wash$LAST_DT))
barplot(ret.tab, las=2, col="seagreen2", main="Frequency of New Pet Wash Customer Returns across Following Days")
barplot(ret.all.tab, las=2, col="orange1", main="Frequency of Customer Returns across Following Days")
ret.all.tab<-prop.table(table(dat.ret$LAST_DT))

ret.all<-known.all[which(known.all$LAST_DT!='7/18/2015' & known.all$LAST_DT!='7/19/2015'),]

ret.old.all<-dat.ret[which(dat.ret$FIRST_DT!='7/18/2015' & dat.ret$FIRST_DT!='7/18/2015')]
