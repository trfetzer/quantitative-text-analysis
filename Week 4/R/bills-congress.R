


TEXT <- textfile(list.files(path = "/Users/thiemo/test", pattern = "*.txt",  full.names = TRUE, recursive = TRUE), cache = FALSE)
CORP <- corpus(TEXT, docnames=list.files(path = "/Users/thiemo/test/", pattern = "*.txt",  full.names = FALSE, recursive = TRUE))


CORP.PARA<-changeunits(CORP, to="paragraphs")
SUM<-summary(CORP.PARA,n=length(texts(CORP.PARA)), verbose=FALSE)

docvars(CORP.PARA)[["Sentences"]] <- SUM$Sentences


docvars(CORP)[["id"]] <- docnames(CORP)
docvars(CORP)[["bill"]] <- gsub("([a-z]+)/([a-z]+[0-9]+)/text-versions/([a-z]+)/document.txt$","\\2",docnames(CORP))
docvars(CORP)[["version"]] <- gsub("([a-z]+)/([a-z]+[0-9]+)/text-versions/([a-z]+)/document.txt$","\\3",docnames(CORP))
docvars(CORP)[["doctype"]] <- gsub("([a-z]+)/([a-z]+[0-9]+)/text-versions/([a-z]+)/document.txt$","\\1",docnames(CORP))
docvars(CORP)[["congress"]] <- 106

CORP<-subset(CORP, version %in% c("ih","enr"))
CORP.dfm<-dfm(CORP, ignoredFeatures = c("will", stopwords("english")), stem = TRUE)


CORP.PARA.dfm<-dfm(CORP.PARA, ignoredFeatures = c("will", stopwords("english")), stem = TRUE)






load("/Users/thiemo/Dropbox/Research/sunlight/D.rdata")
load("/Users/thiemo/Dropbox/Research/sunlight/P.rdata")
P[, passed:="passed"]
D[, passed := "not passed"]

PD<-rbind(P[,intersect(names(P),names(D)),with=F],D[,intersect(names(P),names(D)),with=F])


PD$rid<-PD[, 1:nrow(PD)]


PD<-PD[congress==105]

SECTIONS<-corpus(PD$segment,docnames= PD$rid,docvars=PD[, setdiff(names(D), c("segment","segnostop","segstemmed")), with=F])

SEC.dfm<-dfm(SECTIONS, ignoredFeatures = c("will", stopwords("english")), stem = TRUE)
SEC.dfm <-trim(SEC.dfm, minCount=5, minDoc = 2)




D$rid<-D[, 1:nrow(D)]

SECTIONS<-corpus(D$segment,docnames= D$rid,docvars=D[, setdiff(names(D), c("segment","segnostop","segstemmed")), with=F])
docvars(SECTIONS) <- D[, setdiff(names(D), c("segment","segnostop","segstemmed")), with=F]


SECTIONS<-rbind(D[, names(P),with=F],P)

SECTIONS$rid<-SECTIONS[, 1:nrow(SECTIONS)]
SEC<-corpus(SECTIONS$segstemmed,docnames= SECTIONS$sid,docvars=SECTIONS[, setdiff(names(SECTIONS), c("segment","segnostop","segstemmed")), with=F])

SEC.dfm<-dfm(SEC, ignoredFeatures = c("will", stopwords("english")), stem = TRUE)


COMBINED <-trim(COMBINED, minCount=5, minDoc = 2)

trim(myDfm, minCount = 10, minDoc = 2) 


SECTIONS.dfm<-dfm(SECTIONS, ignoredFeatures = c("will", stopwords("english")), stem = TRUE)

##PASSED SECTIONS

PASSED<-corpus(P$segment)
docvars(PASSED) <- P[, setdiff(names(P), c("segment","segnostop","segstemmed")), with=F]
docvars(PASSED)[["passed"]]<-1

PASSED.dfm<-dfm(PASSED, ignoredFeatures = c("will", stopwords("english")), stem = TRUE)


