setwd('D:/Workspace/R-Project')
library(rvest)
library(stringr)
library(dplyr)
library(abind)

trim <- function(x) gsub("^\\s+|\\s+$", "", x)

base_url <- "https://movie.naver.com"
url_sub <- '/movie/bi/mi/point.nhn?code=167651'
url <- paste0(base_url,url_sub)

html <- read_html(url)

url2 <- html %>% html_node('iframe.ifr') %>% html_attr('src')
url_add <- "&page=" 
url_ifr <- paste0(base_url,url2,url_add)
pages <- 1:4652

naver_movie <- data.frame(score=c(),review=c(),writer=c(),time=c())

for (n in 1:length(pages)) {
  
  url_pages <- paste0(url_ifr,pages[n])
  html2 <- read_html(url_pages)  
  
  lis <- html2 %>% html_node('div.score_result') %>% html_nodes('li') 
  
  score <- c()
  review <- c()
  writer <- c()
  time <- c()
  
  for (li in lis) {
    # score
    score <- c(score,html_node(li,'.star_score') %>% html_text('em') %>% trim()) 
    # review
    tmp <- li %>% html_node('.score_reple') %>% html_text('p') %>% trim()
    idx <- str_locate(tmp,'\r')
    review <- c(review,str_sub(tmp,1,idx[1]-1))
    # writer
    tmp <- trim(str_sub(tmp,idx[1],-1))
    idx <- str_locate(tmp,'\r')
    writer <- c(writer,str_sub(tmp,1,idx[1]-1))
    # time
    tmp <- trim(str_sub(tmp,idx[1],-1))
    idx <- str_locate(tmp,'\r')
    time <- c(time,str_sub(tmp,1,idx[1]-1))
  }
  
  movie <- data.frame(score=score,review=review,writer=writer,time=time)
  naver_movie <- rbind.data.frame(naver_movie,movie)  
  
}

View(naver_movie)
naver_movie
library(xlsx)
write.xlsx(naver_movie,"naver_movie_극한직업.xlsx")

library(rJava)
library(KoNLP)
library(wordcloud)
library(RColorBrewer)
library(xlsx)
useSejongDic()

data1 <- readLines("naver_movie_극한직업.txt")
data1    # 파일에서 읽은 Raw Data(한글 문장)
extractNoun('진짜 개재밌음 꼭 보삼 류승룡씨는 오랜만에 흥행성공 하시겠네')
data2 <- sapply(data1, USE.NAMES=F)
data2    # 명사들만 있는 데이터(list 형태)
head(unlist(data2), 30) 
data3 <- unlist(data2)
data3    # 리스트 형태가 아닌 명사 데이터

# 원하지 않는 내용 걸러 내기
data3 <- gsub("\\d+", "", data3) 
data3 <- gsub("", "", data3) 
data3 <- gsub("", "", data3)  
data3 <- gsub("", "", data3)  
data3 <- gsub("", "", data3) 
data3 <- gsub(" ", "", data3)
data3 <- gsub("-", "", data3)
data3

write(unlist(data3), "응답소_2015_01.txt") 
data4 <- read.table("응답소_2015_01.txt")
head(data4)
nrow(data4)
wordcount <- table(data4) 
wordcount
head(sort(wordcount, decreasing=T), 20)

data3 <- gsub("OO","",data3)
data3 <- gsub("것","", data3)
data3 <- gsub("수","", data3)
data3 <- gsub("들","", data3)
data3 <- gsub("님","", data3)
data3 <- gsub("년","", data3)
data3 <- gsub("일","", data3)
data3 <- gsub("제목","", data3)
data3 <- gsub("시설","", data3)
data3 <- gsub("Q","", data3)
data3 <- gsub("시장","", data3)
data3 <- gsub("저","", data3)

write(unlist(data3), "응답소_2015_01.txt") 
data4 <- read.table("응답소_2015_01.txt")
data4     # 불필요한 단어가 제거된 명사 데이터
nrow(data4)
wordcount <- table(data4) 
wordcount
head(sort(wordcount, decreasing=T), 20)

# Word Cloud 형태 그래픽으로 출력
palete <- brewer.pal(9, "Set3")
wordcloud(names(wordcount), freq=wordcount, scale=c(5,1),
          rot.per=0.25, min.freq=1,
          random.order=F, random.color=T, colors=palete)
legend(0.3, 1, "서울시 응답소 요청사항 분석", cex=0.8,
       fill=NA, border=NA, bg="white",
       text.col="red", text.font=2, box.col="red")

set.seed(1234)
palete <- brewer.pal(8, "Dark2")
wordcloud(names(wordcount), freq=wordcount, scale=c(5,0.3),
          rot.per=0.1, min.freq=1,
          random.order=F, random.color=T, colors=palete)
legend(0.1, 1, "서울시 응답소 요청사항 분석", cex=0.8,
       fill=NA, border=NA, bg="white",
       text.col="red", text.font=2, box.col="red")