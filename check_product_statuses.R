#CHECK PRODUCT STATUS

setwd('C:/Users/kated/Desktop') #set working directory
options(scipen=999) #turn off scientific notation
d.dat<-read.csv("dmart_product_status.csv") #import CSV file
r.spec<-read.csv("rover_special.csv")

d.active<-d.dat[which(d.dat$PRODUCT_STATUS==""),]
d.dis<-d.dat[which(d.dat$PRODUCT_STATUS=="Discontinued"),]
d.spec<-d.dat[which(d.dat$PRODUCT_STATUS=="Special Order"),]

st_check<-data.frame(matrix(NA, nrow(d.spec),4))

for (i in 1:nrow(d.spec)) {
  if (is.null(r.spec[which(r.spec$LIN==d.spec$PRODUCT_CODE[i]),"LIN"])) {
    d.spec$exists[i]<-"NULL"
  } else {
    r.spec$[i]<-first_rover[which(first_rover$LIN==first_dmart$LIN[i] & first_rover$store==first_dmart$store[i]),"rover_p"] #rover price
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
