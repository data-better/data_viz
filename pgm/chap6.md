---
title: "6장"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(message = FALSE)
```

#### 예 6-1

```{r}
# install.packages('tm')
# install.packages('SnowballC')
# install.packages('wordcloud')
library(tm)
library(SnowballC)
library(wordcloud)
```

```{r}
# 코퍼스 생성
mytext <- Corpus(DirSource("./data/Text/"))
inspect(mytext)
```

```{r}
# 공백제거 
mytext <- tm_map(mytext, stripWhitespace)

# 소문자화, 대문자를 구별할 필요가 있을 때는 생략
mytext <- tm_map(mytext, tolower)
inspect(mytext)
```

```{r}
# 불용어(stop words) 제거
mytext <- tm_map(mytext, removeWords, stopwords("english"))  

# 단어의 줄기 추출
mytext <- tm_map(mytext, stemDocument)
# R버전 3.1.1 이후에서는 ‘inherits(doc, "TextDocument")는 TRUE가 아닙니다’ 라는
# 에러가 나타날 수 있다. 이때는 다음 명령어를 이용하여야 한다.
# mytext <- tm_map(mytext, PlainTextDocument)
```


```{r}
# 워드 클라우드 생성
wordcloud(mytext, scale=c(5,0.5), max.words=100, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8,"Dark2"))
```


```{r}
# 사용자가 불용어 제거
mytext <- tm_map(mytext, removeWords, "can")

# 여러 개의 불용어를 제거할 때는 다음과 같은명령어를 사용한다.
# mytext <- tm_map(mytext, removeWords, c(“can”, “create”))
wordcloud(mytext, scale=c(5,0.5), max.words=100, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8,"Dark2"))

```

#### 예 6-2

```{r}
# KoNLP 설치
# 출처 : https://www.facebook.com/notes/r-korea-krugkorean-r-user-group/konlp-%EC%84%A4%EC%B9%98-%EC%9D%B4%EC%8A%88-%EA%B3%B5%EC%9C%A0/1847510068715020/

install.packages("multilinguer")
# 이때 mac 사용자는 데스크탑 비밀번호를 물어봅니다. 입력해줘야 설치가 진행됩니다.
library(multilinguer)
install_jdk()
# 의존성 패키지 설치
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")

# github 버전 설치
install.packages("remotes")
# 64bit 에서만 동작합니다.
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
```
```{r}
library(tm)
library(wordcloud)
library(KoNLP)
library(RColorBrewer)


# 파일을 한글 형식으로 불러옴
ktext <- Corpus(DirSource("./data/KText/", encoding="UTF-8", recursive=TRUE))

# 단어 추출
words <- unlist(sapply(ktext[[1]]$content, extractNoun, USE.NAMES=FALSE))

# 단어 빈도수 계산
wordfreq <- table(words)
# 빈도수의 내림차순으로 정렬
sort(wordfreq, decreasing=T)[1:12]

# 워드 클라우드 작성
wordcloud(names(wordfreq), freq=wordfreq, max.words=50)
```

```{r}
# 불용어를 제거하고 워드 클라우드 작성
wordfreq <- wordfreq[!(names(wordfreq) %in% c("등","한","이","개","들이","적","1","것"))]
wordcloud(names(wordfreq), freq=wordfreq, max.words=50)
```

#### 예 6-3

```{r}
require(KoNLP)
require(tm)
require(wordcloud)

# 4 파일에 대한 코퍼스 작성
speech <- Corpus(DirSource("./data/president_speech/", encoding="UTF-8", recursive=TRUE))

# 2행 2열의 그래프 화면 구성
par(mfrow=c(2,2))

# 각 그래프의 타이틀 지정 변수 설정
name <- c("14대 김영삼 대통령","15대 김대중 대통령","16대 노무현 대통령","17대 이명박 대통령")

# 반복문 for를 이용하여 4개의 워드 클라우드를 만듬
for(i in 1:4) {
    words <- unlist(sapply(speech[[i]]$content, extractNoun, USE.NAMES=FALSE))
    wordfreq <- table(words)
    # 불용어 제거
    wordfreq <- wordfreq[!(names(wordfreq) %in% c("우리", "것", "여러분", "한", "적", "수", "년", "전", "해"))]
    wordcloud(names(wordfreq), freq=wordfreq, max.words=50)
    title(name[i])
}

```

#### 예 6-4

```{r}
# install.packages(c("tm", "wordcloud", "twitteR", "KoNLP"))
require(tm)
require(wordcloud)
require(KoNLP)
require(twitteR)

# 트위터를 실시간으로 검색하여 “빅데이터”라는 단어가 포함된 트윗 1000개를 받음
# keyword <- "빅데이터"
# tweets <- searchTwitter(keyword, lang='ko', n=1000, cainfo=cainfo)

# 저장된 데이터를 불러 옴
load("./data/twitter_wordcloud.rdata")

# 처음 세 세 데이터를 봄
tweets[1:3]
tweets[[5]]$getText()
```

```{r}
# 명사 추출
extractNoun(tweets[[5]]$getText())

# 다운로드 받은 1000개 트윗에서 텍스트만 뽑아 문자열의 벡터
tweet_texts <- sapply(tweets, function(tweet) tweet$getText())

# extractNoun() 이용해서 각 트윗 텍스트에서 명사를 추출하여 벡터로 만들자. 
tweet_nouns <- unlist(sapply(tweet_texts, extractNoun, USE.NAMES=FALSE))   

# 단어의 빈도수 조사
wordfreq <- table(tweet_nouns)    

# 워드 클라우드 작성
wordcloud(names(wordfreq), freq=wordfreq)
```

```{r}
# 빈도가 5이상인 워드 클라우드 그림
wordcloud(names(wordfreq), freq=wordfreq, min.freq=5)
```

