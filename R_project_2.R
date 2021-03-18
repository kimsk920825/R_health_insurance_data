## 진료내역 분석

## 데이터 출처 : "국민건강보험공단_진료내역정보" -> 부분 선택 및 변환
##               공공데이터포털(https://www.data.go.kr/data/15007115/fileData.do)

## 데이터 설명 : "국민건강보험 가입자 중 요양기관(병/의원 등)으로부터의 진료이력이 있는 각 연도별 수진자 100만 명에 대한 기본정보(성, 연령대, 시도코드 등)와 진료내역(진료과목코드, 주상병코드, 요양일수, 총처방일수 등)으로 구성된 개방데이터"
##               서울지역 가입자/의과외래/상위 20개 진료과목 진료내역만 선택

## 분석 목표   : 진료내역을 활용한 분석

# 0. 준비------------------------------------------------------------------------------
##패키지 불러오기
library(dplyr)
library(ggplot2)
##데이터 불러오기
T20 = read.csv('data/T20_SAMPLE.csv', fileEncoding='UTF-8')
T20 %>% head()
library(extrafont)
font_import()
theme_set(theme_grey(base_family='NanumGothic'))
#1. 전처리 ------------------------------------------------------------------------
## `연령대` 변수 생성 
T20 = T20 %>% 
  mutate(연령대 = case_when(
    연령대코드==18 ~'85UP',
    연령대코드==1 ~'0004',
    연령대코드==2 ~'0509',
    연령대코드 %in% 3:17 ~paste0((연령대코드-1)*5, 연령대코드*5 -1)
  ))
T20 %>% as_tibble()
## `성별` 변수 생성 
##@ `성별코드`==1 -> '남', '성별코드'==2 -> '여'
T20 = T20 %>% 
  mutate(성별 = ifelse(성별코드==1, '남','여'))
T20 %>% as_tibble()

##`공단부담금` 변수 생성 
##@`요양급여비용총액`-`본인부담금`
T20 = T20 %>% 
  mutate(공단부담금 = 요양급여비용총액 - 본인부담금)
T20 %>% as_tibble()
#2. 데이터 요약과 시각화------------------------------------------------------
## 기본 요약 지표의 계산 
  ### 전체 진료 건수 
  ### 진료이력이 있는 가입자수
  ### 가입자 1인당 진료 건수
  ### 진료 당 요양급여비용총액의 평균
  ### 가입자 1인당 비용 총액 
T20 %>% 
  summarise(진료건수 = n(),
            진료가입자수 = n_distinct(가입자번호),
            평균진료건수 = 진료건수 / 진료가입자수,
            진료비용총액_진료평균 = mean(요양급여비용총액),
            진료비용총액_가입자평균 = sum(요양급여비용총액)/진료가입자수)
## 연령대별 요약지표의 계산 
T20 %>% 
  group_by(연령대) %>% 
  summarise(진료건수 = n(),
            진료가입자수 = n_distinct(가입자번호),
            평균진료건수 = 진료건수/진료가입자수, 
            진료비용총액_진료평균 = mean(요양급여비용총액),
            진료비용총액_가입자평균=sum(요양급여비용총액)/진료가입자수)
## 진료과목코드별 요약지표의 계산 후 적당한 이름으로 저장해보자
agg1 = T20 %>% 
  group_by(진료과목코드) %>% 
  summarise(진료건수 = n(),
            진료가입자수 = n_distinct(가입자번호),
            평균진료건수 = 진료건수/진료가입자수,
            진료비용총액_진료평균 = mean(요양급여비용총액),
            진료비용총액_가입자평균 = sum(요양급여비용총액)/진료가입자수) 
agg1
## merge()를 활용한 데이터 결합 
DSBJT_CD = read.csv('data/DSBJT_CD.csv', fileEncoding='UTF-8')
DSBJT_CD

merge(DSBJT_CD, agg1, by='진료과목코드')
agg1_most_treatment = merge(DSBJT_CD, agg1, by='진료과목코드') %>% 
  arrange(desc(진료건수))
agg1_most_paid = merge(DSBJT_CD, agg1, by='진료과목코드') %>% 
  arrange(desc(진료비용총액_진료평균))

## 연령대/진료과목코드별 진료건수와 요양급여비용총액의 합계 계산
## DSJBT_CD 데이터와 결합
## 연령대, 진료과목별 진료건수와 요양급여비용총액 각각의 열지도 그리기

agg2 = T20 %>% 
  group_by(연령대, 진료과목코드) %>% 
  summarise(진료건수 = n(),
            비용총액합계 = sum(요양급여비용총액))
agg2
#해석: 어느 연령대에 어느 진료를 가장 많이 보고 지출하는가? 

agg2_merged = merge(DSBJT_CD, agg2, by='진료과목코드')

agg2_merged %>% 
  ggplot(aes(연령대, 진료과목, fill=진료건수)) +
  geom_tile()+
  scale_fill_distiller(palette='YlGnBu', direction=1) +theme(axis.text.x = element_text(angle=90))

agg2_merged %>% 
  ggplot(aes(연령대, 진료과목, fill=비용총액합계))+
  geom_tile()+
  scale_fill_distiller(palette = 'YlGnBu',direction=1)+theme(axis.text.x = element_text(angle=90))

agg2_merged %>% 
  mutate(진료건수_천건 = round(진료건수/1000)) %>% 
  ggplot(aes(연령대, 진료과목, fill=비용총액합계, label=진료건수_천건))+
  geom_tile() + 
  scale_fill_distiller(palette = 'YlGnBu', direction=1)+theme(axis.text.x = element_text(angle=90))+
  geom_text()

#3. 데이터 집계를 활용한 부분 관측치 선택 -----------------------------------------------------------------------------
##"내과" 진료만 선택 
T20 %>% 
  filter(진료과목코드 ==1) %>% 
  summarise(진료건수 = n(),
            진료가입자수 = n_distinct(가입자번호),
            평균진료건수 = 진료건수/진료가입자수,
            진료비용총액_진료평균=mean(요양급여비용총액),
            진료비용총액_가입자평균=sum(요양급여비용총액)/진료가입자수)
## 진료 건수가 100건 이상인 가입자 확인 
agg3 = T20 %>% 
  group_by(가입자번호) %>% 
  summarise(N=n()) %>% 
  filter(N>=100)
agg3
##진료 건수가 100건 이상인 가입자의 진료내역 확인 
agg3$가입자번호

T20 %>% 
  filter(가입자번호 %in% agg3$가입자번호) %>% 
  as_tibble()

## 진료 건수가 100건 이상인 가입자와 나머지 가입자의 비교
T20 %>% 
  mutate(GRP = ifelse(가입자번호 %in% agg3$가입자번호,'Heavy','Normal')) %>% 
  group_by(GRP) %>% 
  summarise(진료가입자수 = n_distinct(가입자번호),
            진료비용총액_가입자평균=sum(요양급여비용총액)/진료가입자수)
    