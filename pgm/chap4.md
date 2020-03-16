---
title: "4장"
output: html_document
---

#### RMarkdown Option

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(message = FALSE)
```

#### 4.1 시계열의 시각화

```{r}
library(zoo)
econ1 = read.csv("./data/ch4/gdp.csv",header=TRUE)
연도  = seq(as.Date("1970-01-01"), as.Date("2013-12-01"), "quarter")
econ  = zoo(econ1, 연도)
par(new="NanumGothic")
plot(econ/1000, ylab="GDP(조원)", xlab="", col=1:2, screens=1)
```

```{r}
library(ggplot2)
# 데이터 불러오기
gdp_1    <- read.csv("./data/ch4/gdp.csv",header=TRUE)
연도     <- seq(as.Date("1970-01-01"), as.Date("2013-12-01"), "quarter")
gdp_kr   <- cbind(gdp_1, 연도)
# 그래프 그리기
ggplot(data=gdp_kr, aes(x=연도)) + geom_line(aes(y = gdp/1000, colour="원계열")) + 
  geom_line(aes(y = gdpsa/1000, colour = "계절조정계열")) + 
  ylab("GDP (조원)")+scale_color_hue("GDP")+theme(legend.position="bottom") +
  theme_bw(base_family="NanumGothic")
```

#### 4.2 선그래프의 작성

```{r}
library(ggplot2)
library(scales)
# 데이터 불러오기
gdp_1    <- read.csv("./data/ch4/gdp.csv",header=TRUE)
연도     <- seq(as.Date("1970-01-01"), as.Date("2013-12-01"), "quarter")
gdp_kr   <- cbind(gdp_1, 연도)

# 그래프 그리기
ggplot(gdp_kr, aes(x=연도)) + 
  geom_line(aes(y = gdp/1000, colour="원계열")) +
  geom_line(aes(y = gdpsa/1000, colour = "계절조정계열")) +
  scale_x_date(date_breaks="5 years", labels=date_format("%Y")) + 
  labs(x="", y="GDP (조원)") + scale_colour_hue("GDP") +
  theme_bw(base_family = "NanumGothic")
```

```{r}
library(ggplot2)
library(quantmod)
# Yahoo! Finance로터 종합주가지수 종가 데이터 가져오기 
kospi <- na.omit(getSymbols("^KS11", auto.assign = FALSE)[, 4])  
er <- na.omit(getSymbols("KRW=X", auto.assign = FALSE)[,4])  

# 200일 후방 이동평균선을 작성
kospi$ma <- runMean(kospi, n = 200)
er$ma <- runMean(er, n = 200)
colnames(kospi) <- c("종합주가지수", "200일 이평선")
colnames(er) <- c("원/달러 환율", "200일 이평선")
# 그래프 그리기 
autoplot(kospi, facets = NULL) + 
  xlab("연도") + ylab("종합주가지수") +
  theme(panel.background = element_rect(fill = "white", colour = "gray"),  
        legend.position = "none") +
  theme_bw(base_family="NanumGothic")
```

```{r}
library(ggplot2)
library(scales)
library(xts)
# 경기종합지수 데이터 불러와서 xts 객체로 만듬
cycle1  <- read.csv("./data/ch4/cycle.csv",header=TRUE)
연도    <- seq(as.Date("1970-01-01"), as.Date("2014-06-01"), "month")
cycle   <- xts(cycle1[,2],연도)
# 경기 기준순환일 데이터를 가져와서 정리
refdate <- read.csv("./data/ch4/refdate1.csv",header=TRUE)
yrng <- range(cycle)
datebreaks <- seq(as.Date("1970-01-01"), as.Date("2014-01-01"), "2 year")
# 그래프 그리기
p <- autoplot(cycle, facets = NULL) + 
      theme(panel.background = element_rect(fill = "white", colour = "gray"), legend.position = "bottom") + 
      geom_rect(aes(NULL, NULL, xmin = as.Date(start), xmax = as.Date(end), fill = 경기순환), ymin = yrng[1], ymax = yrng[2], data = refdate) + 
      scale_fill_manual(values = alpha(c("yellow", "darkblue"),0.1)) + ylab("") + xlab("") +
             geom_hline(yintercept=100, colour="gray") +
      geom_text(aes(x = as.Date(start), y = yrng[2], label = name1),data = refdate, size = 4, hjust = 0.5, vjust = -0.5) + 
      geom_text(aes(x = as.Date(end), y = yrng[2], label = name2), data = refdate, size = 4, hjust = 0.5, vjust = -0.5)
p + scale_x_date(breaks=datebreaks, labels=date_format("%Y"), expand=c(0.01,0.01))
```

```{r}
library(ggplot2)
# 데이터 불러오기
climate_kr1  <- read.csv("./data/ch4/climate.csv", header=TRUE)
연도         <- seq(as.Date("2008-01-01"), as.Date("2013-12-01"), "month")
climate_kr   <- cbind(연도, climate_kr1)
# 그래프 그리기
ggplot(climate_kr, aes(x=연도, y=j강수량)) + 
  geom_area(colour="black", fill="blue", alpha=.2)+ ylab("강수량 (mm)") +
  theme_gray(base_family = "NanumGothic")

```

```{r}
library(ggplot2)
# 데이터 불러오기
climate_kr1  <- read.csv("./data/ch4/climate1.csv", header=TRUE)
연도         <- rep(seq(as.Date("2008-01-01"), as.Date("2013-12-01"), "month"),3)
climate_kr   <- cbind(연도, climate_kr1)
# 그래프 그리기
h <- ggplot(climate_kr, aes(x=연도))+facet_grid(지역 ~.) +
  geom_ribbon(aes(ymin=최저, ymax=최고), fill="pink", alpha=.7) + geom_line(aes(y=평균), colour="red") + 
  theme_bw(base_family = "NanumGothic")
h + ylab("") + xlab("") + ylim(-12,33)
```

#### 4.3 막대그래프의 작성

```{r}
library(ggplot2)
# 데이터 불러오기
cb <- read.csv("./data/ch4/cb.csv", header=TRUE)
cb$경상수지 <- cb$경상수지/100
cb$pos <- cb$경상수지 >=0
# 그래프 그리기
ggplot(cb, aes(x=연도, y=경상수지, fill=pos)) + 
  geom_bar(stat="identity", position="identity", colour="black", size=0.25) +
  scale_fill_manual(values=c("red", "black"), guide=FALSE) + 
  ylab("경상수지 (억달러)") + theme_bw(base_family = "NanumGothic")

```

#### 4.4 누적그래프의 작성

```{r}
library(ggplot2)
library(reshape2)
library(plyr)
# 데이터 불러온 후 연령대별로 재정렬
pop_kr1  <- read.csv("./data/ch4/krpop.csv", header=TRUE)
pop_kr  <- melt(pop_kr1, id="연령대")
pop_kr$연도 <-as.numeric(substr(pop_kr$variable,2,5))
# 연령대별 인구그래프 그리기
ggplot(pop_kr, aes(x=연도, y=value/10000, fill=연령대)) +
  geom_area(colour="black", size=.1, alpha=.4) +
  scale_fill_brewer(palette="Reds")+ylab("인구 (만명)") +
  scale_x_continuous(breaks=seq(1960, 2060, 5),expand=c(0,0)) + 
  theme_bw(base_family = "NanumGothic")
# 비중을 구함
pop_kr_p <- ddply(pop_kr, "연도", transform, 비중=value/sum(value)*100)
# 연령대별 인구 비중 그래프를 그리기
ggplot(pop_kr_p, aes(x=연도, y=비중, fill=연령대)) +
  geom_area(colour="black", size=.1, alpha=.4) +
  scale_fill_brewer(palette="Reds")+ylab("인구비중 (%)") +
  scale_x_continuous(breaks=seq(1960, 2060, 10), expand=c(0,0))+ 
  theme_bw(base_family = "NanumGothic")
```

```{r}
library(ggplot2)
library(reshape2)
# 데이터 불러옴
gdp_s1 <- read.csv("./data/ch4/gdp_sh.csv", header=TRUE)
# 데이터 불러온 후 재정렬
gdp_s  <- melt(gdp_s1, id="연도")
names(gdp_s) <- c("연도", "산업", "비중")
# 그래프 그리기
ggplot(gdp_s, aes(x=연도, y=비중, fill=산업))+
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=seq(1970, 2010, 5))+ 
  theme(panel.background = element_rect(fill = "white", colour = "gray"),  legend.position="bottom", 
        text = element_text(family="NanumGothic") ) + ylab("비중 (%)") + xlab("")
```

#### 4.5 경로그래프의 작성

```{r}
library(ggplot2)
library(scales)
library(xts)
# 데이터 불러오기
inven1  <- read.csv("./data/ch4/inven_cy.csv",header=TRUE)
연도    <- seq(as.Date("1980-01-01"), as.Date("2014-06-01"), "month")
inven   <- xts(inven1[,2:3],연도)
inven$출하지수증감률 <- (inven$출하지수 - lag(inven$출하지수,12))/lag(inven$출하지수,12) *100
inven$재고지수증감률 <- (inven$재고지수 - lag(inven$재고지수,12))/lag(inven$재고지수,12) *100
# 일정기간 데이터 찾기
inven_1  <-inven[337:366]
# 그래프 그리기
ggplot(inven_1, aes(x=출하지수증감률, y=재고지수증감률)) + theme_bw() + geom_path() + 
  geom_point() + ylim(-21,28)+xlim(-21,28) +
  geom_text(aes(label=substr(index(inven_1),3,7)), size=3, hjust=-0.2, vjust=-0.3, colour="blue") +
  geom_line(aes(x=inven_1$출하지수증감률,y=inven_1$출하지수증감률, colour="red")) + 
  geom_hline(yintercept=0, colour="gray")+ geom_vline(xintercept=0, colour="gray") +
  theme(text=element_text(family="NanumGothic"))
```

#### 4.6 채색달력그래프의 작성

```{r}

library(quantmod) 
library(ggplot2)
library(reshape2)
library(plyr)
library(scales)
# 야후 파이낸스 데이터베이스에서 가져오기
getSymbols("^KS11",src="yahoo")
KS11$주가변동 <- abs((KS11$KS11.Close - lag(KS11$KS11.Close,1))/lag(KS11$KS11.Close,1)) *100
dat<-data.frame(date=index(KS11),KS11)
dat$year<-as.numeric(as.POSIXlt(dat$date)$year+1900)
dat$month<-as.numeric(as.POSIXlt(dat$date)$mon+1)
dat$monthf<-factor(dat$month,levels=as.character(1:12),labels=c("1월","2월","3월","4월","5월","6월","7월","8월","9월","10월","11월","12월"),ordered=TRUE)
dat$weekday = as.POSIXlt(dat$date)$wday
dat$weekdayf<-factor(dat$weekday,levels=rev(1:7),labels=rev(c("월","화","수","목","금","토","일")),ordered=TRUE)
dat$yearmonth<-as.yearmon(dat$date)
dat$yearmonthf<-factor(dat$yearmonth)
dat$week <- as.numeric(format(dat$date,"%W"))
dat<-ddply(dat,.(yearmonthf),transform,monthweek=1+week-min(week))
# 그래프 그리기
ggplot(dat, aes(monthweek, weekdayf, fill = 주가변동)) + 
  geom_tile(colour = "white") + facet_grid(year~monthf) + 
  scale_fill_gradient(limits=c(0, 12), low="lightgray", high="darkred") +  xlab("") + ylab("") + 
  theme(panel.background = element_rect(fill = "white", colour = "gray"), text=element_text(family="NanumGothic"))
```







