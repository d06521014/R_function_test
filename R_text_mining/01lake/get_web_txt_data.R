library(rvest)
library(magrittr)
data.path<-"E:/R/R_text_mining/01lake/"
setwd(data.path)
chapter.number=c(1:120)
for (i in 1:length(chapter.number)){#length(chapter.number)
  if(i<10){
    s=paste0("00",as.character(i))
  }
  if(i>=10 && i<100){
    s=paste0("0",as.character(i))
  }
  if(i>=100){
    s=as.character(i)
  }
  url <- paste0("http://www.xn--5rtnx620bw5s.tw/e/e2/",s,".htm")
  raw_html <- read_html(url)
  board_css <- ".main"
  
chapter.content <- raw_html %>%
  html_nodes(css = board_css ) %>%
  html_text()%>%
  gsub(pattern ="Menu" , replacement ="")%>%
 # gsub(pattern ="ä¸Šä?€???" , replacement ="")%>%
 # gsub(pattern ="æ°´æ»¸?‚³?›®???" , replacement ="")%>%
 # gsub(pattern ="ä¸‹ä?€???" , replacement ="")%>%
  gsub(pattern ="/ ", replacement ="")%>%
 # gsub(pattern =" æ°´æ»¸?‚³?€€?€€???(???) ?€? ?–½?€åºµè¼? ", replacement ="")%>%
  gsub(pattern ="\" ", replacement ="")

 write.table(chapter.content,paste0("source/",s,".txt"))
}
