install.packages("NLP")
install.packages("tm")
library(NLP)#Required package for package tm 
library(tm)#Package for function acq
library(SnowballC)
library(RColorBrewer)#Required package for package wordcloud 
library(wordcloud)
library(textreuse)

#1.1 inspect()
data("acq")
for (i in 1:50)
  print (acq[[i]])

meta(acq)
## data frame with 0 columns and 50 rows
doc1<- acq[[1]]
doc1

doc2<- acq[[2]]
doc2

meta(doc1)
meta(doc2)

docTerm <- DocumentTermMatrix(acq)
docTerm


docTermMatrix <- as.matrix(docTerm)

inspect(docTerm[1:15,1:15])

Docfreq <- termFreq(acq[[1]])
Docfreq

testacq2=acq[[2]]
testacq2[1]


#clean the  data
# pre-processiong 1: to lower case
acq_low <- tm_map(acq,content_transformer(tolower))
acq_low[[1]]$content

# pre-procession 2: Remove anything other than English letters or spaces
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
acq_lower_segement <- tm_map(acq_low, content_transformer(removeNumPunct))
acq_lower_segement[[2]]$content

# pre-procession 3: Remove stopwords from the corpus
myStopwords <- c("and", "for", "in", "is", "it", "not", "the", "to", "are", "but", "also", "of")
acq_lower_segement_destop <- tm_map(acq_lower_segement, removeWords, myStopwords)

# pre-procession 4: Remove "\n"
remove_N <- function(x) gsub("(\n)", "", x)
acq_lower_segement_destop_n <- tm_map(acq_lower_segement_destop, content_transformer(remove_N))
acq_lower_segement_destop_n[[1]]$content

#compare 
tdm <- TermDocumentMatrix(acq_lower_segement_destop_n,control=list(wordLengths=c(1,Inf)))
tdm

freq <- findFreqTerms(tdm,lowfreq = 15)
freq

findAssocs(tdm,"states",0.25) 

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq,term.freq>=15)
term.freq

termdf <- data.frame(term=names(term.freq),freq=term.freq)
termdf


##plot
install.packages("ggplot2")
library(ggplot2)
ggplot(termdf, aes(x = term, y = freq))+ geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()

df20 = termdf[1:20,]
ggplot(df20, aes(x = term, y = freq))+ geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()


install.packages("textreuse")
library(textreuse)
number= NULL
words= NULL
for(i in 1:50)
{
  number[i]=i
  doc2 <- acq[[i]]  
  words[i] <- wordcount(doc2)
}
termdf <- data.frame(number,words)
names(termdf) <- c("Name", "Words")
termdf

##top 15
library(plyr)
top <- arrange(termdf,desc(Words))
dfTopfif <- top[1:15,]
dfTopfif


## print the wordcloud for the document

tdmWC <- as.matrix(tdm)
tdm

s<- sort(rowSums(tdmWC), decreasing = T)
wC.termdf <- data.frame(word = names(s),freq=s)
set.seed(1234)
library("wordcloud")
## Loading required package: RColorBrewer
wordcloud(words = wC.termdf$word, freq = wC.df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

##print2
printcloud = function(acq, number = 1, freq = 1) {
  word_fre = termFreq(acq[[number]])
  pal = brewer.pal(9, "BuGn")
  pal = pal[-(1:4)]
  wordcloud(words = names(word_fre), freq = word_fre, min.freq = freq, random.order = F, colors = pal)
}
printcloud(acq_lower_segement_destop_n)


## print the dendropgram for the document
##

printdendrogram = function(acq, number = 1, freq = 1) {
  test0 = inspect(acq[number])
  test2 = TermDocumentMatrix(test0)
  tt21 = rowSums(as.matrix(test2))
  tt2 = subset(tt21, tt21 >= freq)
  distmatrix2 = dist(scale(tt2))
  fit2 = hclust(distmatrix2, method = "ward.D2")
  plot(fit2, xlab = "Document")
}


##longest word in 15 doc

Num=NULL
Len=NULL
for(i in 1:15)
{
  doc=acq[[dfTopfif$Name[i]]]
  s<-as.String(doc)
  tw<-tokenize_words(s, lowercase = TRUE)
  Num[i]=dfTopfif$Name[i]
  Len[i] = tw[which.max(nchar(tokenize_words(s, lowercase = TRUE)))]
}
dframe <- data.frame(Num,Len)
names(dframe) <- c("Document_No","Longest word")
print(dframe)


##longest sentence
Num=NULL
Len=NULL
for(i in 1:15)
{
  doc=acq[[dfTopfif$Name[i]]]
  s<-as.String(doc)
  tw<-tokenize_sentences(s, lowercase = TRUE)
  Num[i]=dfTopfif$Name[i]
  # print(nchar(tokenize_sentences(s, lowercase = TRUE)))
  Len[i] = tw[which.max(nchar(tokenize_sentences(s, lowercase = TRUE)))]
}
dframe <- data.frame(Num,Len)
names(dframe) <- c("Document_No","Longest word")
print(dframe)


##Print a table of the length of each sentence in each of the 15 documents

doc=acq[[dfTopfif$Name[1]]]
s<-as.String(doc)
frame<-as.data.frame(nchar(tokenize_sentences(s, lowercase = TRUE)))
frame$docnum<-dfTopfif$Name[1]

for(i in 2:15)
{
  doc=acq[[dfTopfif$Name[i]]]
  s<-as.String(doc)
  frame1<-as.data.frame(nchar(tokenize_sentences(s, lowercase = TRUE)))
  frame1$docnum<-dfTopfif$Name[i]
  frame<-rbind(frame,frame1)
}
names(frame) <- c( "Length of sentence","Document No")
frame

testacq2=acq[[2]]
testacq2[1]

##For each word print its part of speech using the Wordnet package.
install.packages("wordnet")
install.packages("openNLP")
library(wordnet)
library(openNLP) 

##Analyze word frequency using functions from package zipfR
install.packages("zipfR")
install.packages("languageR")
library(zipfR)
library(languageR)
library(tokenizers)

for(i in 2:10)
{
  doc1=acq[[dfTopfif$Name[9]]]
  s<-as.String(doc1)
  tw<-tokenize_words(s, lowercase = TRUE)
  doc.spc = text2spc.fnc(tw)
  plot(doc.spc)
  doc.spc
  plot(doc.spc,log="x")
  N(doc.spc)
  V(doc.spc)
  Vm(doc.spc,3)
  with(doc.spc, plot(m, Vm, main="Frequency Spectrum"))
}

N(doc.spc)

V(doc.spc)

Vm(doc.spc,1)



##search

word1 <- "computer"
docNum = NULL
lineNum = NULL
indexInSen = NULL

position <- matrix(rep(0,3),ncol=3)

for (i in 50) {
  
  doc <- acq[[i]]
  tdoc<-as.String(doc1)
  
  if(grepl(word1, tdoc))
  {
    docNum <- i
    t2s<-tokenize_sentences(tdoc, lowercase = TRUE)
    for (j in t2s) {
      sdoc <- as.String(t2s[j])
      if(grepl(word1,sdoc))
      {
        lineNum <- j
        s2w <- tokenize_words(sdoc, lowercase = TRUE)
        for (k in s2w) {
          wdoc <- as.String(s2w[j])
          if(grepl(word1,wdoc))
          {
            indexInSen <- k
            position <- rbind(position, c(i,j,k))
          }
        }
      }
    }
  }
}



