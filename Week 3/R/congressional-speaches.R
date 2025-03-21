
library(RJSONIO)
library(data.table)


##LOADING CONGRESSIONAL SPEECHES BY IMPORANT ACTORS IN 2016 ELECTION

TEMP<-readLines(con="Data/capitol-words-py2.json")

SPEACHES<-lapply(TEMP, function(x) data.frame(fromJSON(x)))
SPEACHES<-rbindlist(SPEACHES)
##CREATING A DOCUMENT IDENTIFIER
SPEACHES[, docid := 1:nrow(SPEACHES)]



