---
title: "Bisnis kaos "
author: "Farras W '152' & Yaumal Y '156' "
date: "1/19/201"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(tm)                                   # Text Mining (Hapus Spasi, Hapus Tanda Petik dll)
library(wordcloud2)                           # Worldcloud
library(twitteR)                              # Twitter
library(rtweet)                               # Twitter API
library(shiny)                                # package shiny
library(syuzhet)                              # package analisis sentimen
library(wordcloud)                            # package wordcloud
library(vroom)
library(here)
library(dplyr)                                # Manipulasi data
library(ggplot2)                              # Plotting
library(RColorBrewer)                         # Handle warna
library(RTextTools)                           # Utils Tambahan
```

```{r} 
api_key<- "zbwirBAshh2lMwDF2m5N9BOhI"
api_secret<- "2TbvtmVXbcszP1X6zHjfLqWy72dndxk1XGOWv0LsfRyAqGQpSg"
access_token<- "397904324-4Oz0EcoNsXEmmNjXdqqWn5a0Cuk4XwSJ9fjQk7fp"
access_token_secret<- "3eXAWIAqvho5l8FlbdePN5m30i17zrZxzQqQIu7ur7opO"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
```

```{r}
# Ganti
tw = searchTwitter('Bisnis kaos', 
                   n = 50,
                   retryOnRateLimit = 10e5, # limt sejita
                   lang = "id")
saveRDS(tw,file = 'jual_kaos.rds')  #sama seperti csv tapi bedanya ini membaca dengan bahasa R
```

```{r}
  tw <- readRDS('jual_kaos.rds') #ngebaca data R
  d = twListToDF(tw) # covert data frame
  komen <- d$text
  komenc <- Corpus(VectorSource(komen)) #mecah kata jadi sebuah vektor yg hasilnya 

  ##hapus URL
  removeURL <- function(x) {
    gsub("http[^[:space:]]*", "", x)
  }
  
  twitclean <- tm_map(komenc, removeURL)
  
  ##hapus New Line
  removeNL <- function(y) {
    gsub("\n", "", y)
  } 
  twitclean <- tm_map(twitclean, removeNL)
  
  ##hapus koma
  replacecomma <- function(y){
    gsub(",", "", y)
  } 
  twitclean <- tm_map(twitclean, replacecomma)
  
  ##hapus retweet
  removeRT <- function(y) {
    gsub("RT ", "", y)
  }
  twitclean <- tm_map(twitclean, removeRT)
  
  ##hapus titik
  removetitik2 <- function(y){ 
    gsub(":", "", y)
    }
  twitclean <- tm_map(twitclean, removetitik2)
  
  ##hapus titik koma
  removetitikkoma <- function(y) {
    gsub(";", " ", y)
  }
  twitclean <- tm_map(twitclean, removetitikkoma)
  
  #hapus titik3
  removetitik3 <- function(y) {
    gsub("p.", "", y)
  }
  twitclean <- tm_map(twitclean, removetitik3)
  
  #hapus &amp
  removeamp <- function(y) {
    gsub("&amp;", "", y)
  }
  twitclean <- tm_map(twitclean, removeamp)
  
  #hapus Mention
  removeUN <- function(z) {
    gsub("@\\w+", "", z)
  }
  twitclean <- tm_map(twitclean, removeUN)
  
  #hapus space dll
  remove.all <- function(xy) {
    gsub("[^[:alpha:][:space:]]*", "", xy)
  }
  twitclean <-tm_map(twitclean,stripWhitespace)
  
  inspect(twitclean[1:10]) # convert ke numeric 
  
  twitclean <- tm_map(twitclean,remove.all)
  twitclean <- tm_map(twitclean, removePunctuation)   #tanda baca
  twitclean <- tm_map(twitclean, tolower)             #mengubah huruf kecil
  
  myStopwords <- readLines("stop.txt", warn = FALSE)
  twitclean <- tm_map(twitclean,removeWords,myStopwords)
  twitclean <- tm_map(twitclean , removeWords, 
                      c('kalo','gak','org',''))
  
  #HAPUS DATA KOSONG
  try.error = function(x)
  {
    # create missing value
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, "error"))
      y = tolower(x)
    # result
    return(y)
  }
  # lower case using try.error with sapply 
  twitclean = sapply(twitclean, try.error)
  # remove NAs in some_txt
  twitclean = twitclean[!is.na(twitclean)]
  names(twitclean) = NULL
```

```{r}
# dataframe data yg sudah bersih
dataframe<-data.frame(text=unlist(sapply(twitclean, `[`)), stringsAsFactors=F)
write.csv(dataframe,'tweet_Bisnis_kaos.csv') # di save
```

```{r}
  ## Lib Naive Bayes
    library(e1071)
    library(caret)
    library(syuzhet)

#digunakan untuk membaca file csv yang sudah di cleaning data 
Bisnis_kaos_dataset <-read.csv("tweet_Bisnis_kaos.csv",stringsAsFactors = FALSE) #GANTI

#digunakan untuk mengeset variabel cloumn text menjadi char
review <- as.character(Bisnis_kaos_dataset$text)
```

```{r}
## Wordcloud
  library(tm) #library untuk penggunaan corpus dalam cleaning data
  library(RTextTools) #library untuk penggunaan corpus dalam cleaning data
  library(e1071) #library yang terdapat sebuah algoritma naivebayes
  library(dplyr) #library yang terdapat sebuah algoritma naivebayes
  library(caret) #library yang terdapat sebuah algoritma naivebayes
  library(shiny)
  library(vroom)
  library(here)
  library(ggplot2)
  library(plotly)
  library(syuzhet)
  library(DT)
  library(wordcloud)

  df<-read.csv("tweet_Bisnis_kaos.csv",stringsAsFactors = FALSE)
  glimpse(df)

  #Set the seed of R's random number generator, which is useful for creating simulations or random objects that can be reproduced.
  set.seed(20) # membuat 20 data random
  df<-df[sample(nrow(df)),]
  df<-df[sample(nrow(df)),]
  glimpse(df)
  df$X=as.factor(df$X)
  corpus<-Corpus(VectorSource(df$text))
  corpus
  inspect(corpus[1:10])

  #fungsinya untuk membersihkan data data yang tidak dibutuhkan 
  corpus.clean<-corpus%>%
    tm_map(content_transformer(tolower))%>%
    tm_map(removePunctuation)%>%
    tm_map(removeNumbers)%>%
    tm_map(removeWords,stopwords(kind="en"))%>%
    tm_map(stripWhitespace) # spasi
  
  dtm<-DocumentTermMatrix(corpus.clean)
  inspect(dtm[1:10,1:20])
  df.train<-df[1:589,]
  df.test<-df[590:1177,]
  dtm.train<-dtm[1:589,]
  dtm.test<-dtm[590:1000,]
  corpus.clean.train<-corpus.clean[1:589]
  corpus.clean.test<-corpus.clean[590:1000]
  dim(dtm.train)
  fivefreq<-findFreqTerms(dtm.train,5)
  length(fivefreq)
  
  dtm.train.nb<- DocumentTermMatrix(corpus.clean.train,control = list(dictionary=fivefreq))

  #dim(dtm.train.nb)
  dtm.test.nb <- DocumentTermMatrix(corpus.clean.test,control = list(dictionary=fivefreq))
  dim(dtm.test.nb)

  convert_count <- function(x){
    y<-ifelse(x>0,1,0)
    y<-factor(y,levels=c(0,1),labels=c("no","yes"))
    y
  }
  trainNB<-apply(dtm.train.nb,2,convert_count)
  testNB<-apply(dtm.test.nb,1,convert_count)
  classifier<-naiveBayes(trainNB,df.train$X,laplace = 1)
```


```{r}
## Shiny

twitter <- read.csv(file="tweet_Bisnis_kaos.csv",header=TRUE)
tweet <- twitter$text

ui <- fluidPage(
  titlePanel("Penggunaan Kata Bisnis kaos Pada Twitter"), #ganti
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Scatterplot", plotOutput("scatterplot")),
                tabPanel("Data Twitter", DT::dataTableOutput('tbl')),
                tabPanel("Wordcloud", plotOutput("Wordcloud"))
    )
  )
)


# SERVER
server <- function(input, output) {
  
  # Tabel
  output$tbl = DT::renderDataTable({
    DT::datatable(twitter, options = list(lengthChange = FALSE))
  })
  
  # Scatter Plot
  output$scatterplot <- renderPlot({
    Bisnis_kaos_dataset<-read.csv("tweet_Bisnis_kaos.csv",
                                  stringsAsFactors = FALSE)
    
    review <-as.character(Bisnis_kaos_dataset$text)
    
    get_nrc_sentiment('happy')
    get_nrc_sentiment('excitement')
    s<-get_nrc_sentiment(review)
    review_combine<-cbind(Bisnis_kaos_dataset$text,s)
    par(mar=rep(3,4))
    barplot(colSums(s),
            col=rainbow(10),
            ylab='count',
            main='sentiment analisis')
  }, height=400)
  
  # Wordcloud
  output$Wordcloud <- renderPlot({
    wordcloud(corpus.clean,
              min.freq = 4,
              max.words=100,
              random.order=F,
              colors=brewer.pal(8,"Dark2"))
  })

}

shinyApp(ui = ui, server = server)
```

