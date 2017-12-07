library(showtext)
library(ggplot2)
library(tm)
library(jiebaRD)
library(jiebaR)
#Sys.setenv(JAVA_HOME="C:/Program Files/Java/dk-9.0.1")
#library(rJava)
library(SnowballC)
library(slam)
library(Matrix)
library(magrittr)
library(dplyr)
data.path<-"E:/R/R_text_mining/01lake/"
setwd(data.path)
#Sys.setlocale(category = "LC_ALL", locale = "UTF-8")

#read files from txt 
filenames <- list.files(path=paste0(data.path,"source"),all.files = T, pattern="*.txt",full.names = TRUE)
files <- lapply(filenames, readLines)

docs <- Corpus(VectorSource(files))
#clear data
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "你")
docs <- tm_map(docs, toSpace, "◆")
docs <- tm_map(docs, toSpace, "‧")
docs <- tm_map(docs, toSpace, "的")
docs <- tm_map(docs, toSpace, "我")
docs <- tm_map(docs, toSpace, "是")
docs <- tm_map(docs, toSpace, "[a-zA-Z]")
docs <- tm_map(docs, toSpace, "　")
docs <- tm_map(docs, toSpace, "水滸傳")
docs <- tm_map(docs, toSpace, "施耐庵輯")
docs <- tm_map(docs, toSpace, "上一回 水滸傳目錄 下一回")
docs <- tm_map(docs, toSpace, "\"x\"")
docs <- tm_map(docs, toSpace, "\"1\" \"")
docs <- tm_map(docs, toSpace, "\n")
#move (punctuation)
#move (white space)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, toSpace, "元明")
#View(inspect(docs[1:5]))
View(inspect(docs[120]))

#??jiebaR??
#mixseg = worker(bylines = TRUE, stop_word = "stop.txt")
mixseg = worker()
roles = readLines(paste0(data.path,"role/主角名單含綽號.txt"), encoding="UTF-8")
roles<-subset(roles,nchar(roles)>1)
new_user_word(mixseg, unlist(roles), rep("n", length(unlist(roles))))
#cutter = worker(bylines = TRUE, stop_word = "stop.txt")
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))
freqFrame = freqFrame[order(freqFrame$Freq,decreasing=TRUE), ]
seg = lapply(docs, jieba_tokenizer)



rolesi = readLines(paste0(data.path,"role/主角名單1.txt"), encoding="UTF-8")
rolesi<-subset(rolesi,nchar(rolesi)>1)
namec<-""
for( i in 1:length(seg)){
  segun<-unlist(seg[[i]])
  segun<-gsub("\\b(魯提轄|魯達|花和尚)\\b", "魯智深", segun)
  segun<-gsub("\\b(宋公明|呼保義|及時雨|宋押司)\\b", "宋江",segun)
  segun<-gsub("\\b(黑旋風|鐵牛)\\b", "李逵", segun)
  v <- as.vector(segun %in% rolesi)
  segun<-segun[v]
  seg [[i]]<-segun
vn<-as.data.frame(segun)
# vn<-unique(as.character(segun))
 vx<-vn%>%
     group_by(name=vn$segun)%>%
     summarise(number=n())%>%
      arrange(desc(number))
  vxt<-as.character(vx$name)

 namec<-c(namec,vx$name)
 namec<-unique(namec)
  
}


d.corpus <- Corpus(VectorSource(seg))

inspect(d.corpus[4])
#d.corpus <- tm_map(d.corpus , toSpace, "\n")

tdm <- TermDocumentMatrix(d.corpus, 
                          control = list(wordLengths = c(2, Inf)))

tdm$dimnames$Terms
tdm$dimnames$Terms[9]<-"李四"
tdm$dimnames$Terms[13]<-"魯智深"
tdm$dimnames$Terms[46]<-"晁蓋"
tdm$dimnames$Terms[48]<-"公孫勝"
tdm$dimnames$Terms[81]<-"王英"
tdm$dimnames$Terms[126]<-"祝朝奉"
tdm$dimnames$Terms[44]<-"石寶"
tdm$dimnames$Terms[138]<-"孫立"
tdm$dimnames$Terms[139]<-"孫新"
tdm$dimnames$Terms[141]<-"解寶"
tdm$dimnames$Terms[148]<-"白秀英"
tdm$dimnames$Terms[154]<-"呼延灼"
tdm$dimnames$Terms[152]<-"孫安"
tdm$dimnames$Terms[159]<-"徐寧"
tdm$dimnames$Terms[188]<-"單廷珪"
tdm$dimnames$Terms[193]<-"郁保四"
tdm$dimnames$Terms[197]<-"丁得孫"
tdm$dimnames$Terms[204]<-"張叔夜"
tdm$dimnames$Terms[222]<-"瓊英"
tdm$dimnames$Terms[53]<-"白勝"
tdm$dimnames$Terms[184]<-"孫五"
tdm$dimnames$Terms[213]<-"山士奇" 





#v <- as.vector(tdm1$dimnames$Terms %in% rolesi)

#tdm<- tdm1[v, ]
View(inspect(tdm[1:20,1:20 ]))


# tf-idf computation
N = tdm$ncol
tf <- apply(tdm, 2, sum)
idfCal <- function(word_doc)
{ 
  log2( N / nnzero(word_doc) ) 
}
idf <- apply(tdm, 1, idfCal)


doc.tfidf <- as.matrix(tdm)
for(x in 1:nrow(tdm))
{
  for(y in 1:ncol(tdm))
  {
    doc.tfidf[x,y] <- (doc.tfidf[x,y] / tf[y]) * idf[x]
  }
}




# 可以用 findFreqTerms 
# 看看在所有文件裡出現 200 次以上的關鍵字有哪些。
result = findFreqTerms(tdm,200)
result
ass = findAssocs(tdm, "李逵", 0.1)
ass
ass = findAssocs(tdm, "宋江", 0.1)
ass
library(plotly)
topID = lapply(rownames(as.data.frame(ass)), function(x) 
  which(rownames(tdm) == x))
topID = unlist(topID)
plot_ly(data = as.data.frame(doc.tfidf),
        x = as.numeric(colnames(doc.tfidf)),
        y = doc.tfidf[topID[1],], 
        name = rownames(doc.tfidf)[topID[1]],
        type = "scatter", mode= "box") %>%
  add_trace(y = doc.tfidf[topID[2],],
            name = rownames(doc.tfidf)[topID[2]])

# get short doc matrix
nonzero = (doc.tfidf != rep(0,10))
nonzeroid = which(row_sums(nonzero) != 0)
q <- rownames(doc.tfidf[nonzeroid,])
all.term <- rownames(doc.tfidf)
loc <- which(all.term %in% q)
s.tdm <- doc.tfidf[loc,]
View(s.tdm)



# result : cos similarity ranking
cos.sim <- function(x, y)
{ 
  (as.vector(x) %*% as.vector(y)) / (norm(as.matrix(x)) * norm(y)) 
}
doc.cos <- cos.sim(x=as.matrix(s.tdm[,1]), 
                   y=as.matrix(s.tdm[,2]))
doc.cos <- apply(s.tdm[,2:10], 2, cos.sim,
                 y=as.matrix(s.tdm[,2]))
orderDoc <- doc.cos[order(doc.cos, decreasing = TRUE)]
plot_ly(data = as.data.frame(orderDoc),
        x = rownames(as.data.frame(orderDoc)),
        y = orderDoc, 
        name = rownames(doc.tfidf)[topID[10]],
        type = "bar", mode= "box")

# Kmeans
set.seed(7)
km1 = kmeans(doc.tfidf, 10, nstart=100)

# Plot results
plot(dat, col =(km1$cluster +1) , main="K-Means result with 2 clusters", pch=20, cex=2)


mydata <- doc.tfidf
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:100) wss[i] <- sum(kmeans(mydata,
                                      centers=i)$withinss)
plot(1:100, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

library(stats)
kmeansOut <- kmeans(doc.tfidf, 15, nstart = 40)

library(devtools)
library(ggbiplot)


testTfidf = doc.tfidf
tfidf.pca <- prcomp(testTfidf)
tfidf.kmeans <- as.factor(kmeansOut$cluster)

g <- ggbiplot(tfidf.pca, obs.scale = 1, var.scale = 1, 
              groups = tfidf.kmeans, ellipse = TRUE, 
              circle = TRUE, labels = rownames(testTfidf))
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
#g<-g+ xlim(10, 0.05) + ylim(-0.05, 0)
print(g)
g<-g+ xlim(-0.5, 0.5) + ylim(-0.5, 0.5)
print(g)
g<-g+ xlim(-0.05, 0.05) + ylim(-0.05, 0.05)
print(g)
g<-g+ xlim(0.020, 0.030) + ylim(0.020, 0.04)
print(g)
# tf-idf computation #try use dataframe
#tdm<-tdm$dimnames %in% roles
#class(tdm)

#extract_f<-tdm%>%
#  as.matrix()%>%
#  as.data.frame()
#extract_f$names<-rownames(extract_f)

#extract_t<-filter(extract_f,names  %in% roles)
#extract_t$names<-NULL

#extract_t
