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
data.path<-"E:/R/R_text_mining/01lake/"
setwd(data.path)

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
docs <- tm_map(docs, toSpace, "水滸傳")
docs <- tm_map(docs, toSpace, "施耐庵輯")
docs <- tm_map(docs, toSpace, "上一回 水滸傳目錄 下一回")
docs <- tm_map(docs, toSpace, "\"x\"")
docs <- tm_map(docs, toSpace, "\"1\" \"")

#move (punctuation)
#move (white space)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, toSpace, "元明")

View(inspect(docs[1:5]))
View(inspect(docs[120]))


WaterMargin1 <- data.frame(text=sapply(docs, identity), 
                      stringsAsFactors=F)
WaterMargin2 <- WaterMargin1[1]

WaterMargin<- VectorSource(files)

View(WaterMargin)
roles = readLines(paste0(data.path,"role/統計主角名單.txt"), encoding="UTF-8")
roles
roles<-subset(roles,nchar(roles)>1)
roles
roles1 = paste0("(", gsub(" ", ")|(", roles), ")")
roles1

main_roles = c("宋江","盧俊義","吳用","公孫勝", "關勝")

role_para = sapply(roles1[1:5], grepl, WaterMargin)
colnames(role_para)<-main_roles

role_count = data.frame(role = factor(colnames(role_para), levels = c("宋江","盧俊義","吳用","公孫勝", "關勝")), count = colSums(role_para))

showtext.auto(enable = TRUE)
X11()
ggplot(role_count, aes(x = role, y = count, fill = role)) + 
  geom_bar(stat = "identity", width = 0.75) + 
  xlab("Role") +
  ylab("Count") +
  theme(axis.text=element_text(size=17, family = "wqy-microhei"),
        axis.title=element_text(size=20,face="bold"),
        axis.title.x = element_text(vjust=-2),
        legend.position="none")





#??jiebaR??
#mixseg = worker(bylines = TRUE, stop_word = "stop.txt")
mixseg = worker()
roles = readLines(paste0(data.path,"role/主角名單1.txt"), encoding="UTF-8")
roles<-subset(roles,nchar(roles)>1)

new_user_word(mixseg, unlist(roles), rep("n", length(unlist(roles))))
#cutter = worker(bylines = TRUE, stop_word = "stop.txt")

jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}

seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))
freqFrame = freqFrame[order(freqFrame$Freq,decreasing=TRUE), ]


d.corpus <- Corpus(VectorSource(seg))
tdm1 <- TermDocumentMatrix(d.corpus, 
                          control = list(wordLengths = c(2, Inf)))
View(inspect(tdm1[1:28,100:120 ]))

v <- as.vector(tdm1$dimnames$Terms %in% roles)
tdm<- tdm1[v, ]
View(inspect(tdm[, ]))


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

# ?隞亦 findFreqTerms 
# ????????辣鋆∪? 200 甈∩誑銝??摮?鈭?
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

# Kmeans ??黎
library(stats)
kmeansOut <- kmeans(doc.tfidf, 5, nstart = 40)

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
g<-g+ xlim(0.020, 0.05) + ylim(-0.05, 0)
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
