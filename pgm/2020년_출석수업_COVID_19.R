# https://cran.r-project.org/web/packages/COVID19/index.html
# Install COVID19
# install.packages("COVID19")
# 국가코드는 아래 사이트에서 찾음(국제표준)
#  https://github.com/covid19datahub/COVID19/blob/master/inst/extdata/db/ISO.csv

# Load COVID19
library(COVID19)
library(ggplot2)
library(dplyr)

# 10개국 선택 : 한국, 이태리, 일본, 미국, 프랑스, 러시아, 독일, 베트남, 
# 브라질, 독일, 스페인
# 중국은 없어서 제외

COVID = covid19(c("KOR", "ITA", "JPN", "USA","FRA", "RUS",
                  "VNM", "BRA", "DEU", "ESP"), level=1)

# 데이터의 선그래프 : 로그변환 : 일부국가가 지나치게 커서 로그변환 
ggplot(COVID, aes(date,log(confirmed), colour=id)) +
  geom_line()

# 데이터의 선그래프 : 로그변환 : 일부국가가 지나치게 커서 로그변환 
ggplot(COVID, aes(date,log(confirmed), colour=id)) +
    geom_line() +
    facet_wrap(~ id, ncol = 2)

ggplot(COVID, aes(date,log(confirmed), fill=id)) +
  geom_bar(stat="identity") +
  facet_wrap(~ id, ncol = 2)

# 한국 데이터 추출
COVID_KR = COVID %>% filter(id == "KOR") %>% select(c(id, date, confirmed)) 
 nn = COVID_KR  %>% summarise(n = n())
 nn1 = nn$n
# 신규 확진자 계산
COVID_KR$New_confirmed = c(0,COVID_KR$confirmed[2:nn1]-COVID_KR$confirmed[1:(nn1-1)])
ggplot(COVID_KR, aes(date,New_confirmed)) + geom_line()

# 미국 데이터 추출과신규 확진자 계산 
COVID_USA = COVID %>% filter(id == "USA") %>% select(c(id, date, confirmed)) 
COVID_USA$New_confirmed = c(0,COVID_USA$confirmed[2:nn1]-COVID_USA$confirmed[1:(nn1-1)])

# 일본 데이터 추출과신규 확진자 계산 
COVID_JPN = COVID %>% filter(id == "JPN") %>% select(c(id, date, confirmed)) 
COVID_JPN$New_confirmed = c(0,COVID_JPN$confirmed[2:nn1]-COVID_JPN$confirmed[1:(nn1-1)])

# 프랑스 데이터 추출과신규 확진자 계산 
COVID_FRA = COVID %>% filter(id == "FRA") %>% select(c(id, date, confirmed)) 
COVID_FRA$New_confirmed = c(0,COVID_FRA$confirmed[2:nn1]-COVID_FRA$confirmed[1:(nn1-1)])

# 전체 데이터 결합과 5월 데이터를 불안정하여 삭제
COVID_C = rbind(COVID_KR, COVID_USA, COVID_JPN, COVID_FRA) %>% filter(months.Date(date)<5)

# 국가별 비교할 수 있는 드래프 작성 log변환, +1은 0인 경우를 대비해서 추가  
ggplot(COVID_C, aes(date,log(New_confirmed+1), colour=id)) +
  geom_line() + geom_smooth(span=0.5)+
  facet_wrap(~ id, ncol = 2)
