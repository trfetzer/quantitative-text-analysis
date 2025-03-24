
is_installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])

load_or_install<-function(package_names)
{

  for(package_name in package_names)
  {
    if(!is_installed(package_name))
    {
       install.packages(package_name,repos="http://lib.stat.cmu.edu/R/CRAN")
    }
    library(package_name,character.only=TRUE,quietly=TRUE,verbose=FALSE)
  }
}

wordfreq <-
function(txt, EF=1, stopws=stopwords,stem=FALSE) {
  txt <- unlist(strsplit(txt, " ", fixed = TRUE))
  
  if(stem==TRUE) {
  txt <- wordStem(txt)
  
  }
  #if (!is.null(stopwords)) 
  #       txt = txt[!txt %in% stopwords]
  tab <- sort(table(txt), decreasing = TRUE)
  return(data.frame(docs=EF, terms = names(tab), Freq = tab, row.names = NULL))
}



library(RecordLinkage)

ClosestMatch2 = function(string, stringVector){

  distance = levenshteinSim(string, stringVector);
  
  data.table(stringVector[distance == max(distance)],max(distance))	  
}

library(RecordLinkage)

ClosestMatch2 = function(string, stringVector){

  distance = levenshteinSim(string, stringVector);
  
  data.table(stringVector[distance == max(distance)],max(distance))	  
}


bottom <- function(frame, rows=5) {

frame[(nrow(frame)-rows:nrow(frame)),]

}

DTUniqueBy <- function(data, varvec) {
  data <- as.data.table(data)
  data[!duplicated(data.frame(data[, varvec, with=F]))]
}
 
 
preProcess <- function(content, keepnum=FALSE, keepperiod=FALSE,tolower=TRUE,removesingleletter=TRUE) {
if(keepperiod==TRUE) {
content <- gsub("[^[:alnum:].,&]", " ", content )
} 
else {
content <- gsub("[^[:alnum:]]", " ", content )
}
if(removesingleletter==TRUE) {
content <- gsub("\\b([A-Za-z]{1})\\b", "", content)
}
#content <- gsub("[[:space:]]+", " ", content )
if(tolower==TRUE) {
content <- tolower(content)
}

#content  <- content [!grepl("^(| |\\.)*$", content )]

content  <- gsub("^ *", "  ", content )
if(keepnum==FALSE) {
content  <- gsub("([0-9])*", "", content )
}
content  <- gsub("^-*", "", content )
content  <- gsub(" +", " ", content )
content  <- gsub(" *$", "", content )
#content  <- content[!grepl("^(| |\\.\\,)*$",content )]
content  <- gsub(" *$", "", content )
content  <- gsub("^ *", "", content )

content
}


setwd("/Users/thiemo/Dropbox/Research/Sam Jon Thiemo/")

options(stringsAsFactors=FALSE)
load_or_install(c("doMC","countrycode","lubridate","date","igraph","kriging","geosphere","zoo","rgeos","fields","raster","rootSolve","R.oo","stringr","classInt","rgdal", "maptools","XML","plyr","RMySQL","data.table","depmixS4"))
con <- dbConnect(MySQL(), username="root", password="", dbname="resourcebooms", unix.socket="/tmp/mysql.sock")

create_matrix<-function (textColumns, language = "english", minDocFreq = 1, 
    maxDocFreq = Inf, minWordLength = 3, maxWordLength = Inf, 
    ngramLength = 1, originalMatrix = NULL, removeNumbers = FALSE, 
    removePunctuation = TRUE, removeSparseTerms = 0, removeStopwords = TRUE, 
    stemWords = FALSE, stripWhitespace = TRUE, toLower = TRUE, 
    weighting = weightTf) 
{
    stem_words <- function(x) {
        split <- strsplit(x, " ")
        return(wordStem(unlist(split), language = language))
    }
    tokenize_ngrams <- function(x, n = ngramLength) return(rownames(as.data.frame(unclass(textcnt(x, 
        method = "string", n = n)))))
    control <- list(bounds = list(local = c(minDocFreq, maxDocFreq)), 
        language = language, tolower = toLower, removeNumbers = removeNumbers, 
        removePunctuation = removePunctuation, stopwords = removeStopwords, 
        stripWhitespace = stripWhitespace, wordLengths = c(minWordLength, 
            maxWordLength), weighting = weighting)
    if (ngramLength > 1) {
        control <- append(control, list(tokenize = tokenize_ngrams), 
            after = 7)
    }
    else {
        control <- append(control, list(tokenize = scan_tokenizer), 
            after = 4)
    }
    if (stemWords == TRUE && ngramLength == 1) 
        control <- append(control, list(stemming = stem_words), 
            after = 7)
    trainingColumn <- apply(as.matrix(textColumns), 1, paste, 
        collapse = " ")
    trainingColumn <- sapply(as.vector(trainingColumn, mode = "character"), 
        iconv, to = "UTF8", sub = "byte")
    corpus <- Corpus(VectorSource(trainingColumn), readerControl = list(language = language))
    matrix <- DocumentTermMatrix(corpus, control = control)
    if (removeSparseTerms > 0) 
        matrix <- removeSparseTerms(matrix, removeSparseTerms)
    if (!is.null(originalMatrix)) {
        terms <- colnames(originalMatrix[, which(!colnames(originalMatrix) %in% 
            colnames(matrix))])
        weight <- 0
        if (attr(weighting, "acronym") == "tf-idf") 
            weight <- 1e-09
        amat <- matrix(weight, nrow = nrow(matrix), ncol = length(terms))
        colnames(amat) <- terms
        rownames(amat) <- rownames(matrix)
        fixed <- as.DocumentTermMatrix(cbind(matrix[, which(colnames(matrix) %in% 
            colnames(originalMatrix))], amat), weighting = weighting)
        matrix <- fixed
    }
    matrix <- matrix[, sort(colnames(matrix))]
    gc()
    return(matrix)
}


write.csv(BEM[, .N, by= DS_TIPO_BEM_CANDIDATO], file="BEM.TYPES.ALT.csv")

load("Elections/bem_candidado.rdata")

BEM <-BEM[sample(1:nrow(BEM), 10000)]
BEM$DS_TIPO_BEM_CANDIDATO<-preProcess(BEM$DS_TIPO_BEM_CANDIDATO)


BEM.TYPES<-data.table(read.csv(file="~/Dropbox/Research/Sam Jon Thiemo/BEM.TYPES.ALT.csv"))
BEM.TYPES[, X.1 := NULL]

BEM.TYPES<-BEM.TYPES[TYPE!=""]
BEM.TYPES<-BEM.TYPES[TYPE!="NOTHING"]

BEM<-join(BEM, BEM.TYPES)

BEM$TYPENUM<-as.numeric(as.factor(BEM$TYPE))
BEM$BEMDETAIL<-preProcess(BEM$DETALHE_BEM)

set.seed(2020)

BEM.TRAIN<-BEM[!is.na(TYPE)]
BEM.TRAIN<-BEM.TRAIN[sample(1:nrow(BEM.TRAIN), 20000)]


testsize<-2000

valid<-sample(1:nrow(BEM.TRAIN), testsize)
BEM.TRAIN$validation<- 0
BEM.TRAIN[valid]$validation<-1

BEM.TRAIN<-BEM.TRAIN[order(validation)]

OUT<-NULL
k = 1
for(i in c(0.999,0.9995,0.9999,.99995,0.99999,0.999999)) {
cat(i, " ")
DOC<-create_matrix(c(BEM.TRAIN[,paste(BEMDETAIL,sep=" ")]),removeStopwords=FALSE,
                   removeNumbers=TRUE,stemWords=FALSE,removePunctuation=TRUE,removeSparseTerms=i)
                             
DOCCONT<-create_container(DOC,BEM.TRAIN$TYPENUM, trainSize=1:(nrow(BEM.TRAIN)-testsize),testSize=(nrow(BEM.TRAIN)-testsize+1):nrow(BEM.TRAIN), virgin=TRUE)
MOD <- train_models(DOCCONT, algorithms=c("SVM","MAXENT"))
RES <- classify_models(DOCCONT, MOD)
analytics <- create_analytics(DOCCONT, RES)
res<-data.table(analytics@document_summary)

VALID<-cbind(BEM.TRAIN[validation==1],res)

OUT[[k]] <- sum(diag(3) * table(VALID$CONSENSUS_CODE,VALID$TYPE))/nrow(VALID)
k = k+1
}

BEM.MISSING<-BEM[is.na(TYPE)]

BEM.MISSING$SVM_LABEL<-0
BEM.MISSING$SVM_PROB<-0

BEM.MISSING$MAXENTROPY_LABEL<-0
BEM.MISSING$MAXENTROPY_PROB<-0


lower<-seq(1,nrow(BEM.MISSING),100000)
upper<-c(seq(1,nrow(BEM.MISSING),100000)[2:length(lower)],nrow(BEM.MISSING))
for(i in 1:length(lower)) {
cat(i , " ")
PREDICT<-create_matrix(c(BEM.MISSING[lower[i]:upper[i]][,paste(BEMDETAIL,sep=" ")]),originalMatrix=DOC)
predSize = nrow(PREDICT)
PREDICT.CONT <- create_container(PREDICT, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)
      
RES2 <- classify_models(PREDICT.CONT, MOD)

BEM.MISSING[lower[i]:upper[i]]$SVM_LABEL <-RES2$SVM_LABEL
BEM.MISSING[lower[i]:upper[i]]$SVM_PROB <-RES2$SVM_PROB

BEM.MISSING[lower[i]:upper[i]]$MAXENTROPY_LABEL <-RES2$MAXENTROPY_LABEL
BEM.MISSING[lower[i]:upper[i]]$MAXENTROPY_PROB <-RES2$MAXENTROPY_PROB


}


BEM.MATCH<-rbind(BEM[!is.na(TYPE)], BEM.MISSING, fill=TRUE)



#NOW I NEED TO MERGE IN THE CANDIDATE PROFILE INFORMATION
§


BRA<-readOGR(dsn="/Users/thiemo/Dropbox/Research/Sam Jon Thiemo/Shapefiles/2013 Municipio",layer="BRA-MUNIC2013")
 braextent<-extent(BRA)
 BRA.data<-data.table(BRA@data)
BRA.data$codigo2013<-BRA.data$CD_GEOCODM
BRA.data$munic<-tolower(BRA.data$NM_MUNICIP)
BRA.data$munic<-preProcess(BRA.data$NM_MUNICIP)



###TERRA
BEM$DESCPROCESS<-preProcess(BEM$DETALHE_BEM)

FREQ<-data.table(wordfreq(BEM$DESCPROCESS))
BEM<-BEM[grep("lot|terr|rura|situ|faze", DESCPROCESS, ignore.case=TRUE)]

TEMP<-levenshteinSim(BEM[1]$DESCPROCESS,BRA.data$munic)





CONCAND<-CONCAND[DESC_SIT_TOT_TURNO %in% c("ELEITO POR QP","ELEITO POR QUOCIENTE PARTIDÁRIO","ELEITO","TURNO")]

BIRTHMUNIC<-CONCAND[, .N, by=c("SIGLA_UF_NASCIMENTO","NOME_MUNICIPIO_NASCIMENTO")]
BIRTHMUNIC$NOMECLEAN<-preProcess(BIRTHMUNIC$NOME_MUNICIPIO_NASCIMENTO)


BIRTHMUNIC$matchingmunic<-""

BIRTHMUNIC$codigo2013<-""
for(i in 1:nrow(BIRTHMUNIC)) {
cat(i, " ")
if(BIRTHMUNIC[i]$NOMECLEAN !="") {
 if(nrow(BRA.data[statecode==BIRTHMUNIC[i]$SIGLA_UF_NASCIMENTO])>0) {
  mmatch<- ClosestMatch2(BIRTHMUNIC[i]$NOMECLEAN, BRA.data[statecode==BIRTHMUNIC[i]$SIGLA_UF_NASCIMENTO]$munic)[1]
   if(length(mmatch)>0) {
	if(mmatch$V2>.35) {
  	BIRTHMUNIC[i]$codigo2013 <- BRA.data[statecode==BIRTHMUNIC[i]$SIGLA_UF_NASCIMENTO & munic==mmatch$V1][1]$codigo2013
  	BIRTHMUNIC[i]$matchingmunic <- mmatch$V1

	}
	
	}
}
}
}


CONCAD<-join(CONCAND,BIRTHMUNIC)

REPRESENT<-CONCAD[DESCRICAO_CARGO %in% c("GOVERNADOR","SENADOR","DEPUTADO FEDERAL"), list(representatives=.N), by=c("ANO_ELEICAO","NOMECLEAN","codigo2013")]


MAP<-data.table(read.csv(file="R scripts/mapping_2013to2000census.csv"))

REPRESENT<-join(REPRESENT, MAP[, list(codigo2013=CD_GEOCODM,GEOCODIGO=GEOCODIGO2000Census)])

REPRESENT<-REPRESENT[, list(representatives=sum(representatives)), by=c("ANO_ELEICAO","GEOCODIGO")]

setnames(REPRESENT, "ANO_ELEICAO", "year")
REPRESENT[, year:=as.numeric(as.character(year))]
REPRESENT[, GEOCODIGO:=as.numeric(as.character(GEOCODIGO))]
write.dta(REPRESENT, file="REPRESENTATIVES.dta")
