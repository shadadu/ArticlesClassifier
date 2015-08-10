textclass <-function(){
  require(tm)
  require(class)
  
  wb <- read.table("wb.txt", skip = 0, header=TRUE,  sep="\t", stringsAsFactors=FALSE, quote="")
  View(wb)
  csvf <- read.table("train.csv", skip = 0, sep="\t", header=TRUE, stringsAsFactors=FALSE, quote="")
  View(csvf)
    
  csvfFeatures <-csvf$title
  targetData <- csvf$act_code
  rawDocsId <- wb$id
  
  csvfCorpus <- Corpus(VectorSource(csvfFeatures))
  csvfWords <- tm_map(csvfCorpus, removeWords, stopwords("english"))
  csvfWords <- tm_map(csvfWords, stripWhitespace)
  csvfWords <- tm_map(csvfWords, content_transformer(tolower))
  csvfWords <- tm_map(csvfWords, stemDocument, language = "english")
  dtmCsvf<- DocumentTermMatrix(csvfWords)
  inspect(dtmCsvf)
  
  dtmCsvf <-removeSparseTerms(dtmCsvf, 0.9)
  inspect(dtmCsvf)
  mCsvf <- as.data.frame(dtmCsvf)
  trainIndx <- as.integer(0.8*nrow(mCsvf))                   # use 80% of data for training  
  trainData <- mCsvf[1:trainIndx,]
  testData <- mCsvf[(trainIndx+1):nrow(mCsvf),]              # and remaining 20% as for testing
  trainTarget <-targetData[1:trainIndx]
  rm(targetData)
  num <-as.integer(sqrt((nrow(mCsvf))))
  
  if(num %% 2 == 0)                              # use odd number of nearest neighbors
    num <- num +1
  
  #ml <- knn(train = trainData, test = testData, cl = trainTarget, k = num)
  #table(targetdata, ml)
  
  
  #wbCorpus <- Corpus(VectorSource(wb$title)) 
  #wbWords <- tm_map(wbCorpus, removeWords, stopwords("english"))
  #wbWords <- tm_map(wbWords, stripWhitespace)
  #wbWords <- tm_map(wbWords, content_transformer(tolower))
  #dtmWb <- DocumentTermMatrix(wbWords)
  
  
  
  
  
  
  
  
}