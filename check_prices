#read data into R
setwd('C:/Users/kated/Documents') #set working directory
options(scipen=999) #turn off scientific notation
dat_dmart<-read.csv("PRICES_DATAMART.csv") #import CSV file
dat_rover<-read.csv("THIRD_ROVER_PRICES.csv")
names(dat_dmart)<-c("store", "LIN","datamart_p") #name columns
names(dat_rover)<-c("store", "LIN","rover_p", "TOMAX_p") #name columns

#Rob's file is too big for dbeaver so I am segmenting it into three sections
#stores between 1000-3000
first_dmart<-dat_dmart[which(dat_dmart$store>=3030),] #subset first stores
first_dmart<-first_dmart[order(first_dmart$store),] #order data by store
first_rover<-dat_rover[which(dat_rover$LIN %in% first_dmart$LIN),]
#subset items in ROVER df that are active and at subsetted store
 
first_rover$rover_p<-as.numeric(levels(first_rover$rover_p) [first_rover$rover_p]) 
#change data type from factor to numeric for rover prices
first_rover$TOMAX_p<-as.numeric(levels(first_rover$TOMAX_p) [first_rover$TOMAX_p])

for (i in 1:nrow(first_dmart)) {
  if (is.null(first_rover[which(first_rover$LIN==first_dmart$LIN[i] & first_rover$store==first_dmart$store[i]),"rover_p"])) {
    first_dmart$rover_p[i]<-"NULL"
  } else {
    first_dmart$rover_p[i]<-first_rover[which(first_rover$LIN==first_dmart$LIN[i] & first_rover$store==first_dmart$store[i]),"rover_p"] #rover price
  }
  if (is.null(first_dmart$TOMAX_p[i]<-first_rover[which(first_rover$LIN==first_dmart$LIN[i] & first_rover$store==first_dmart$store[i]),"TOMAX_p"])) {
    first_dmart$rover_p[i]<-"NULL"
  } else {
    first_dmart$TOMAX_p[i]<-first_rover[which(first_rover$LIN==first_dmart$LIN[i] & first_rover$store==first_dmart$store[i]),"TOMAX_p"]
  }
  if (first_dmart$datamart_p[i]==first_dmart$rover_p[i] & first_dmart$datamart_p[i]==first_dmart$TOMAX_p[i]) { #if the price matches
    first_dmart$match[i]<-T #say TRUE
  } else { #if they don't match
    first_dmart$match[i]<-F #say FALSE
  }
}

dat<-rbind(dmart1,dmart2,dmart3)

library(xlsx)
write.xlsx(dat, "C:/Users/kated/Desktop/DataMart_Price_Check.xlsx")


#EXTRA BITS
new_dat<-data.frame(matrix(NA,5570,5)) #create new empty df
names(new_dat)<-c("store", "LIN","dmart","rover","match") #name columns in new_dat
first_rover<-first_rover[which(duplicated(first_rover$LIN)==F),] #elimate duplicates
uni_store<-unique(dat_dmart$store) #create vector of unique store numbers
