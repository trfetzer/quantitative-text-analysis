setwd("~/Dropbox/Teaching/Data Science Warwick/Course Material/Lecture 2")
HOUSES<-data.table(read.dta(file="R/houseprices.dta"))
HOUSES$sqft<-as.numeric(HOUSES$sqft)
HOUSES$builtyryr<-as.numeric(gsub("Built in ([0-9]{4})","\\1", HOUSES$builtyr))
HOUSES$zestimateclean<-as.numeric(gsub("\\$([0-9]+)K","\\1", HOUSES$zestimate))
HOUSES$baths<-as.numeric(HOUSES$baths)
HOUSES$bedrooms<-as.numeric(HOUSES$bedrooms)
HOUSES$soldprice<-as.numeric(HOUSES$soldprice)

HOUSES<-HOUSES[!is.na(builtyr) & !is.na(bedrooms) & !is.na(soldprice) &!is.na(zestimateclean) & !is.na(sqft) & !is.na(baths)]

HOUSES<-HOUSES[builtyr>1000]
HOUSES<-HOUSES[soldprice>1000]
HOUSES<-HOUSES[bedrooms>0]
HOUSES<-HOUSES[zestimateclean>0]

HOUSES<-HOUSES[sample(20000)]

HOUSES<-HOUSES[log(pricepersqft)<10]
HOUSES[, pricepersqft := soldprice/sqft]
save(HOUSES, file="HOUSES.rdata")