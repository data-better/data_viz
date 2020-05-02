#these libraries need to be loaded
library(utils)
library(dplyr)
library(ggplot2)

#read the Dataset sheet into “R”. The dataset will be called "data".
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")

COVID_KR = data %>% filter(geoId =="KR")
COVID_JP = data %>% filter(geoId =="JP")
COVID_CN = data %>% filter(geoId =="CN")
COVID_US = data %>% filter(geoId =="US")
COVID_FR = data %>% filter(geoId =="FR")
COVID_BR = data %>% filter(geoId =="BR")
COVID_DE = data %>% filter(geoId =="DE")
COVID_ES = data %>% filter(geoId =="ES")

COVID_T  = rbind(COVID_KR, COVID_JP, COVID_CN, COVID_US, COVID_BR,
                 COVID_FR, COVID_DE, COVID_ES)

COVID_T1 = COVID_T %>% mutate(lcases = ifelse(cases == 0, 0, log(abs(cases))))
  
# 국가별 비교할 수 있는 드래프 작성 log변환, +1은 0인 경우를 대비해서 추가  
ggplot(COVID_T1, aes(as.Date(dateRep, "%d/%m/%Y"),lcases, colour=geoId)) +
  geom_line() + 
  geom_smooth(span=0.3)+
  xlab("date") + ylab("로그 신규 확진자 수") +
  facet_wrap(~ geoId, ncol = 2)
