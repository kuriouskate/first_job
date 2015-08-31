#read data into R
setwd('C:/Users/kated/Documents/Data Analysis Work')
dat<-read.csv("BLA_Cust_Details.csv")
names(dat)<-c("Card_ID", "name", "store", "date", "reg_no", "trans_no","type",
              "line_no", "sku_no", "UPC", "receipt_desc", "weighed", "extprice",
              "chgprice", "net_price","qty","tax","COGS", "orig_unit_P",
              "item_perc_D", "promo_disc", "promo_qty", "promo_ext",
              "promo_ad_disc", "promo_ad", "custprice_disc", "dept_no", "class_no",
              "line_no2","UUID","first_vis", "last_vis", "first_st", "last_st",
              "primary_st", "first_UUID", "last_UUID", "acct_type", "cc_type",
              "loyalty", "dog", "cat")

#format date column
library(chron)
dat$date<-as.character(dat$date) #format date column as a character string
dtparts = t(as.data.frame(strsplit(dat$date,' '))) #split the date from the time
row.names(dtparts) = NULL
thetimes = chron(dates=dtparts[,1], format=c('m/d/y')) #format date column as date
dat$date<-thetimes #replace date column in dataframe with newly formatted

uni.name<-as.character(unique(dat$name)) #create vector of customers


#creates matrix of customers, number of visits, avg time between visits
avg.time<-data.frame(matrix(nrow=length(uni.name),ncol=3))
names(avg.time)<-c("name", "number_of_visits", "avg time")
for (i in 1:length(uni.name)) {
  avg.time[i,1]<-uni.name[i]
  sub<-dat[which(dat$name==uni.name[i]),]
  sub<-aggregate(net_price~date,sub,sum)
  avg.time[i,2]<-nrow(sub)
  diff<-c()
  for (n in 1:(nrow(sub)-1)) {
    diff[n]<-sub$date[n+1]-sub$date[n]
  }
  avg.time[i,3]<-mean(diff)
}
