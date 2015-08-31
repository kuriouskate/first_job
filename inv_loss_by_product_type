#read data into R
setwd('C:/Users/kated/Documents/Data Analysis Work')
options(scipen=999)
inv_adj<-read.csv("invadjust1.csv")
names(inv_adj)<-c("date", "Store", "Product", "Type_Code", "Reason_Code",
                  "Act_Quant", "Adj_Quant", "Act_VaP", "Adj_VaP", "Act_VaC",
                  "Adj_VaC", "Valid_Record", "Process_Date")
df<-as.data.frame(inv_adj)
df<-inv_adj[which(df$Adj_Quant<0),]

#format date column
library(chron)
df$date<-as.character(df$date) #format date column as a character string
dtparts = t(as.data.frame(strsplit(df$date,' '))) #split the date from the time
row.names(dtparts) = NULL
thetimes = chron(dates=dtparts[,1], format=c('m/d/y')) #format date column as date
df$date<-thetimes #replace date column in dataframe with newly formatted

for (i in 1:nrow(df)) {
  if (df$Product[i]<1000000000000000) {
    df$Product[i]<-paste(0,df$Product[i])}
  else {df$Product[i]<-as.character(df$Product[i])}
}


df[grep("^0100", df$Product), "class"]<-"Premium Food"
df[grep("^0110", df$Product), "class"]<-"Premium Dog Bags"
df[grep("^0120", df$Product), "class"]<-"Premium Dog Cans"
df[grep("^0160", df$Product), "class"]<-"Premium Cat Bags"
df[grep("^0170", df$Product), "class"]<-"Premium Cat Cans"
df[grep("^0190", df$Product), "class"]<-"Raw Diets"
df[grep("^0200", df$Product), "class"]<-"Supermarket Food"
df[grep("^0210", df$Product), "class"]<-"Supermarket Dog Bags"
df[grep("^0220", df$Product), "class"]<-"Supermarket Dog Cans"
df[grep("^0260", df$Product), "class"]<-"Supermarket Cat Bags"
df[grep("^0270", df$Product), "class"]<-"Supermarket Cat Cans"
df[grep("^0300", df$Product), "class"]<-"Super Premium"
df[grep("^0310", df$Product), "class"]<-"Super Premium Dog Bags"
df[grep("^0320", df$Product), "class"]<-"Super Premium Dog Cans"
df[grep("^0360", df$Product), "class"]<-"Super Premium Cat Bags"
df[grep("^0370", df$Product), "class"]<-"Super Premium Cat Cans"
df[grep("^0400", df$Product), "class"]<-"Super Premium Plus"
df[grep("^0410", df$Product), "class"]<-"Super Premium Plus Dog Bags"
df[grep("^0420", df$Product), "class"]<-"Super Premium Plus Dog Cans"
df[grep("^0430", df$Product), "class"]<-"Super Premium Plus Cat Bags"
df[grep("^0440", df$Product), "class"]<-"Super Premium Plus Cat Cans"
df[grep("^0500", df$Product), "class"]<-"Litters"
df[grep("^0600", df$Product), "class"]<-"Books"
df[grep("^0700", df$Product), "class"]<-"Rawhide"
df[grep("^0800", df$Product), "class"]<-"Treats"
df[grep("^0860", df$Product), "class"]<-"Cat Treats"
df[grep("^0900", df$Product), "class"]<-"Body Parts"
df[grep("^1000", df$Product), "class"]<-"Bird Food"
df[grep("^1100", df$Product), "class"]<-"Bird Supplies"
df[grep("^1200", df$Product), "class"]<-"Fish Supplies"
df[grep("^1300", df$Product), "class"]<-"Reptiles"
df[grep("^1400", df$Product), "class"]<-"Fish and Reptile Food"
df[grep("^1450", df$Product), "class"]<-"Live Feed"
df[grep("^1500", df$Product), "class"]<-"Tanks"
df[grep("^1600", df$Product), "class"]<-"Small Animal"
df[grep("^1700", df$Product), "class"]<-"Small Animal Food"
df[grep("^1800", df$Product), "class"]<-"Dog Toys/Signs"
df[grep("^1900", df$Product), "class"]<-"Cat Toys"
df[grep("^2000", df$Product), "class"]<-"Flea Products"
df[grep("^2100", df$Product), "class"]<-"Vet Flea Products"
df[grep("^2200", df$Product), "class"]<-"Carriers"
df[grep("^2300", df$Product), "class"]<-"Dog Houses"
df[grep("^2400", df$Product), "class"]<-"Cat Furniture"
df[grep("^2500", df$Product), "class"]<-"Pet Doors"
df[grep("^2600", df$Product), "class"]<-"Beds"
df[grep("^2700", df$Product), "class"]<-"Grooming"
df[grep("^2800", df$Product), "class"]<-"Sanitation"
df[grep("^2900", df$Product), "class"]<-"Bowls/Feeders/Storage"
df[grep("^3000", df$Product), "class"]<-"Shampoo/Condition/Spritz"
df[grep("^3100", df$Product), "class"]<-"Wash"
df[grep("^3200", df$Product), "class"]<-"Novelties"
df[grep("^3210", df$Product), "class"]<-"Cards"
df[grep("^3220", df$Product), "class"]<-"Memorials"
df[grep("^3230", df$Product), "class"]<-"Shopping Bags"
df[grep("^3300", df$Product), "class"]<-"Magazines"
df[grep("^3400", df$Product), "class"]<-"Ferret Supplies"
df[grep("^3500", df$Product), "class"]<-"Ferret Food"
df[grep("^3600", df$Product), "class"]<-"ID Tags"
df[grep("^3700", df$Product), "class"]<-"Dental"
df[grep("^3800", df$Product), "class"]<-"Livestock"
df[grep("^3850", df$Product), "class"]<-"Apparel"
df[grep("^3860", df$Product), "class"]<-"Boots"
df[grep("^3890", df$Product), "class"]<-"Hanukkah"
df[grep("^3900", df$Product), "class"]<-"Calendars"
df[grep("^3910", df$Product), "class"]<-"Spring"
df[grep("^3920", df$Product), "class"]<-"Valentines"
df[grep("^3930", df$Product), "class"]<-"Summer"
df[grep("^3940", df$Product), "class"]<-"Halloween"
df[grep("^3950", df$Product), "class"]<-"Fall/Thanksgiving"
df[grep("^3960", df$Product), "class"]<-"Raincoats"
df[grep("^3970", df$Product), "class"]<-"Sweaters"
df[grep("^3980", df$Product), "class"]<-"Boots"
df[grep("^3990", df$Product), "class"]<-"Christmas"
df[grep("^3991", df$Product), "class"]<-"Hanukkah"
df[grep("^4000", df$Product), "class"]<-"Bulk"
df[grep("^4100", df$Product), "class"]<-"Vitamins/Supplements/Meds"
df[grep("^4200", df$Product), "class"]<-"Collars and Leashes"
df[grep("^4300", df$Product), "class"]<-"Training Aids"
df[grep("^4400", df$Product), "class"]<-"Dog Licenses"
df[grep("^4500", df$Product), "class"]<-"Nontaxable Feed"
df[grep("^4700", df$Product), "class"]<-"Donated Product"
df[grep("^9000", df$Product), "class"]<-"Freight"
df[grep("^9990", df$Product), "class"]<-"Other"
df[grep("^9999", df$Product), "class"]<-"Other"

df$class<-as.factor(df$class)
tab<-table(df$class)
sort<-sort(tab,decreasing=T)
barplot(sort, cex.names=0.75, ylab="No. of Inventory Adj Requests",
        axes=F, col="green", las=2)
title(main="Classes vs. Instances of Product Loss")
text(locator(1),"using negative adjustment quantity data" )
ax<-c(0,50,100,150,200,250,300,350,400,450,500)
axis(2, at=ax, las=2)
abline(h = 35.64, col = "black"))

table<-aggregate(Adj_Quant~class,df,sum)
table$Adj_Quant<-abs(table$Adj_Quant)
table<-table[with(table, order(-Adj_Quant)), ]
barplot(table$Adj_Quant, names.arg=table$class, las=2, col="red",
        ylab="No. of Lost Items", axes=F)
title(main="Classes vs. Number of Lost Items")
text(locator(1),"using negative adjustment quantity data" )
ax<-c(0,100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100)
axis(2, at=ax, las=2)
