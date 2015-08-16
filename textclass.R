textclass <-function(){
  require(tm)
  require(class)
  
  set.seed(8290)
   
  wb <- read.table("wb.txt", skip = 0, header=TRUE,  sep="\t", stringsAsFactors=FALSE, quote="")
  
  csvf <- read.table("train.csv", skip = 0, sep="\t", header=TRUE, stringsAsFactors=FALSE, quote="")
  
    
  csvfFeatures <-csvf$title
  targetData <- csvf$act_code
  rawDocsId <- wb$id
  
  csvfCorpus <- Corpus(VectorSource(csvfFeatures))
  csvfWords <- tm_map(csvfCorpus, removeWords, stopwords("english"))
  csvfWords <- tm_map(csvfWords, stripWhitespace)
  csvfWords <- tm_map(csvfWords, content_transformer(tolower))
  csvfWords <- tm_map(csvfWords, stemDocument, language = "english")
  dtmCsvf<- DocumentTermMatrix(csvfWords)
   
  dtmCsvf <-removeSparseTerms(dtmCsvf, 0.99)
  inspect(dtmCsvf)
  mCsvf <- as.matrix(dtmCsvf)  
  Terms <- colnames(mCsvf)
  
  # Normalize term frequencies
  normalizer<-function(x){
    if(max(x)==0){
          return(1)
    }else{return(max(x) - min(x))}
  }
  
   
  
  dCsvf <- mCsvf
  for(i in seq_along(Terms)){   
    dCsvf[,i] <-  mCsvf[,i]/ ( normalizer(mCsvf[,i]) )   
  }
 
   
  # To remove ties when knn decides which cluster a point belongs to,
  # add ~small random (nonbiasing) numbers to the term frequencies
  perturb <- function(x){
   if(x==0){
     x <- 0.0001*runif(1,-1:1) 
   }else{
     x <- x*(1 + 0.0001*runif(1,-1:1)) 
   }
   x
 }
   
  
  for(i in seq_along(colnames(dCsvf))){
    dCsvf[,i] <- sapply(dCsvf[,i], perturb )
    
  } 
 
  
  trainIndx <- as.integer(0.8*nrow(dCsvf))                   
  trainData <- dCsvf[(1:trainIndx),]
  testData <- dCsvf[(trainIndx+1):nrow(dCsvf),]              
  trainTarget <-targetData[1:trainIndx]
  testTarget <- targetData[(trainIndx+1):nrow(dCsvf)]
  
  # use odd number of nearest neighbors (k)
  num <- seq(1,151,10)
  performance <- c(rep(0,length(num)))
                                           
  for(i in seq_along(num)){
  ml <- knn(train = trainData, test = testData, cl = trainTarget, k = num[i])
  tbl <- table(testTarget, ml)
  performance[i] <- 100*sum(diag(tbl))/nrow(testData)
  }
  
  plot(num, performance, type ="l", xlab = "k", ylab ="performance (%)")
  
  
  
  
  
  
  
  
}