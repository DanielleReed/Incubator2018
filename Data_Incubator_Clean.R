#Rising stars and falling fortunes in the biomedical research firmament 
#Danielle R. Reed, Ph.D.
#Jan 28th 2018
#download from NIH Exporter https://exporter.nih.gov/
#home
setwd("C:/Users/Reed/Dropbox/Dani life/2018 Data Incubator")
#work
#setwd("~/Documents/My Dropbox/Dani life/2018 Data Incubator")
library(readr)
#
#I can automate the download step later
#
FY2017 <- read_csv("./FY2017.csv")
FY2007 <- read_csv("./FY2007.csv")
FY2012 <- read_csv("./FY2012.csv")
#rbind these three files after standardizing the variables
v1 <-  c("TOTAL_COST", "ORG_CITY", "ORG_STATE", "ORG_ZIPCODE", "ORG_COUNTRY", "ACTIVITY", "ORG_NAME")
FY2017_v1 <- FY2017[, v1]
FY2017_v1$YEAR <- rep(2017,nrow(FY2017_v1))
FY2007_v1 <- FY2007[, v1]
FY2007_v1$YEAR <- rep(2007,nrow(FY2007_v1))
FY2012_v1 <- FY2012[, v1]
FY2012_v1$YEAR <- rep(2012,nrow(FY2012_v1))
All <- rbind(FY2012_v1, FY2007_v1, FY2017_v1)
#make sure all text is upper case
All <- as.data.frame(sapply(All, toupper))
#make total cost numberic and not a factor
All$TOTAL_COST <- as.numeric(as.character(All$TOTAL_COST))
#make city unique by adding state
All$CITYSTATE <- paste(All$ORG_CITY, All$ORG_STATE, sep='_')
#some data reshape and summarizing
library(dplyr)
library(reshape2)
#aggregate dollars by year for citystate
#limit by US
All1 <- All[All$ORG_COUNTRY %in% c("UNITED STATES"),]
All2 <-aggregate(All1$TOTAL_COST, by=list(All1$CITYSTATE, All1$YEAR),  FUN=sum, na.rm=TRUE)
#cast by year to make graphing easier
All_wide <- dcast(All2, Group.1 ~ Group.2, value.var = "x", na.rm = TRUE)
#How to define growth - loss -- stable
#first calculate percentages from 2007 to 2012 to 2017
#I think I have to omit cities that have missing data
All_wide1 <- na.omit(All_wide)
#I think we further need to restrict the analysis to Cities that had more than 1M in any year
All_wide2 <- subset(All_wide1, All_wide1$`2007` >= 10000000 | All_wide1$`2012` >= 10000000 | All_wide1$`2017` >= 10000000)
#Zero values are troubling - eliminate them but revisit later
All_wide3 <- subset(All_wide2, All_wide2$`2007` >= 1 &  All_wide2$`2012` >= 1 & All_wide2$`2017` >= 1)
#compute percent change from the two date periods
All_wide4 <- mutate(All_wide3, PerIncrease1 = ((All_wide3$'2012'/All_wide3$'2007')*100)-100)
All_wide5 <- mutate(All_wide4, PerIncrease2 = ((All_wide4$'2017'/All_wide4$'2012')*100)-100)
#volitility
All_wide6 <- mutate(All_wide5, Volitility = abs(PerIncrease1 - PerIncrease2))
#make categories
All_wide7 <- mutate(All_wide6, Category = ifelse(PerIncrease1 > 0 & PerIncrease2 > 0 & Volitility < 100, "Growing",
ifelse(PerIncrease1 < 0 & PerIncrease2 < 0 & Volitility < 100, "Not_Growing", 
ifelse(PerIncrease1 > -15 & PerIncrease1 < 15 & PerIncrease2 > -15 & PerIncrease2 < 15, "Stagnant", "Volitile"))))

All_wide7 <- mutate(All_wide6, Category = ifelse(PerIncrease1 > 0 & PerIncrease2 > 0 & Volitility < 100, "A. Stable growth",
                                                 ifelse(PerIncrease1 < 0 & PerIncrease2 < 0 & Volitility < 100, "B. Declining", 
                                                        ifelse(PerIncrease1 > -15 & PerIncrease1 < 15 & PerIncrease2 > -15 & PerIncrease2 < 15, "C. Stagnant", 
                                                               ifelse(Volitility >= 100 & Volitility <= 1000, "E. Violent see-saw", 
                                                                      ifelse(PerIncrease1 > -100 & PerIncrease1 < 100 & PerIncrease2 > -100 & PerIncrease2 < 100, "D. See-saw", "F. Up then Crazy"))))))
#Make a totals column
All_wide7$Total <- All_wide7$`2007` + All_wide7$`2012` + All_wide7$`2017`

library(ggplot2)
#need to melt data
All_long<- melt(All_wide7)
clean <- c("2007", "2012", "2017")
All_long1 <- All_long[All_long$variable %in% clean,]
names(All_long1)[3]<-"Year"
names(All_long1)[4]<- "NIH_Award_Dollars"
#Violin
plot1 <- ggplot(All_long1, aes(x=Year, y=NIH_Award_Dollars)) + theme_bw() +  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + scale_y_log10()  
#Line panel by categories  
plot2<- ggplot(data=All_long1, aes(x=Year, y=NIH_Award_Dollars, group=Group.1)) + geom_line() +
  geom_point() + facet_wrap(~Category, scale = "free") + scale_y_log10() + theme_minimal()

