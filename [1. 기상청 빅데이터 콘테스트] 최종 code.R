# 데이터 불러오기
fire_firefighting = read.csv("c:/project/firefighting.csv")
fire_weather = read.csv("c:/project/weather.csv")
fire_weather_special = read.csv("c:/project/weather_special.csv")

# 데이터프레임 변환
df1 = data.frame(fire_firefighting[-c(1,2)]) # 처음 열인 num 제거
df2 = data.frame(fire_weather[-1])
df3 = data.frame(fire_weather_special[-1])

# 데이터 확인
## 반복되는 접두사 제거 "gsub" 함수 사용
colnames(df1) <- gsub("^fire_firefighting\\.", "", colnames(df1))
colnames(df2) <- gsub("^fire_weather\\.", "", colnames(df2))
colnames(df3) <- gsub("^fire_weather_special\\.", "", colnames(df3))

# 설치가 안 되어있는 경우 실행
install.packages('lubridate')
install.packages('dplyr')
install.packages('zoo')
install.packages('car')
install.packages('ranger')
install.packages('corrplot')
install.packages('ggplot2')

#필요한 라이브러리 불러오기
library(lubridate)
library(dplyr)
library(zoo)
library(car)
library(ranger)
library(corrplot)
library(ggplot2)
############################df1####################################
# df1

## 데이터 탐색
head(df1)
View(df1)
summary(df1)
dim(df1)  # 행 236337, 열 15
colnames(df1)
str(df1)

## 불필요 변수 제거
colSums(is.na(df1)) # 모두 0 : NA가 아니라 빈 문자열임 
colSums(df1=="") # fire_type_2 컬럼에만 "" 존재해 # 총 53718개 데이터 
#-> 기타 + 임야 -> 해당 열 제거. fire_type_1 컬럼만 이용
df1 = subset(df1, select=-fire_type_2)

## date type 변경
str(df1)

# 문자형 벡터를 POSIXct 형식으로 변환
df1$tm = as.POSIXct(df1$tm, format = "%Y-%m-%d %H:%M:%S")
# 범주형 변수를 factor type으로 지정
df1$year = as.factor(df1$year); levels(df1$year)
df1$district_1 = as.factor(df1$district_1) ; levels(df1$district_1)
df1$district_2 = as.factor(df1$district_2)
df1$fire_type_1 = as.factor(df1$fire_type_1) ; levels(df1$fire_type_1)
df1$ignition_factor_category_1 = as.factor(df1$ignition_factor_category_1) ; levels(df1$ignition_factor_category_1)
df1$ignition_factor_category_2 = as.factor(df1$ignition_factor_category_2)
df1$location_category_1 = as.factor(df1$location_category_1) ; levels(df1$location_category_1)
df1$location_category_2 = as.factor(df1$location_category_2) ; levels(df1$location_category_2)
df1$location_category_3 = as.factor(df1$location_category_3)

str(df1)

###########################df2#########################
# df2

## 데이터 탐색
head(df2)
View(df2)
summary(df2)
dim(df2)    # 행 552579, 열 15
colnames(df2)
str(df2)

## 불필요 변수 제거
# stn 변수 제거 (stn = aws 지점 코드 -> 지역과 같은 의미)
df2 = subset(df2, select=-stn)

## date type 변경
str(df2)

# 날짜형 변수를 POSIXct 형식으로 변환
df2$tm = as.POSIXct(df2$tm, format = "%Y-%m-%d")

# 범주형 변수를 factor type으로 지정
df2$district_1 = as.factor(df2$district_1) ; levels(df2$district_1)
df2$district_2 = as.factor(df2$district_2) ; levels(df2$district_2)

str(df2)

## 중복 데이터 처리 (날짜, 지역)
# 함수 정의
process_values <- function(x) {
  if (all(x == -99)) {
    return(-99)
  } else if (any(x == -99)) {
    return(mean(x[x != -99]))
  } else {
    return(mean(x))
  }
}

# 날짜와 지역으로 그룹화하여 처리
df2 <- df2 %>%
  group_by(tm, district_1, district_2) %>%
  summarise(across(c(ta_max, ta_min, ta_max_min, rn_day, ws_max, ws_ins_max, ws_mean, ws_min, hm_max, hm_mean, hm_min), process_values), .groups = 'drop')

dim(df2)  # 데이터 552579개 -> 541305개


## 파생 변수 생성
# 월 month
df2$month = month(df2$tm)
df2$month = as.factor(df2$month) ; levels(df2$month)

## 결측치 처리
# 결측치 확인
colSums(is.na(df2))
summary(df2)  # -99라는 결측치가 모든 수치형 데이터에 존재

boxplot(df2$ta_max) 
boxplot(df2$ta_min)
boxplot(df2$ta_max_min) 

# -99를 NaN으로 변환
df2[df2==-99] = NaN
colSums(is.na(df2)) # -99가 NA로 변환됨 확인

# 1) 전체 지역 결측치 처리 시도
# 데이터프레임을 district_1로 그룹화하고, 수치형 변수에 대해 선형 보간을 적용
# na.locf() 함수: NA를 선형 보간하기 전에 가장 가까운 비 NA 값으로 대체
df2_cleaned <- df2 %>%
  group_by(district_1) %>%
  mutate(across(where(is.numeric), ~ na.locf(na.approx(.x, na.rm = FALSE), na.rm = FALSE)))
summary(df2_cleaned) # 여전히 NA 남아있음

# NA 포함한 district_1 찾기
table(df2_cleaned[rowSums(is.na(df2_cleaned)) > 0, "district_1"])
# 전북, 세종, 부산 NA 남아있음

# 2) 전북, 세종, 부산 제외한 지역 선형 보간
df2_other <- df2 %>%
  filter(district_1 != "전북특별자치도" & district_1 != "세종특별자치시" & district_1 != "부산광역시") %>%
  group_by(district_1) %>%
  mutate(across(where(is.numeric), ~ na.locf(na.approx(.x, na.rm = FALSE), na.rm = FALSE)))
summary(df2_other) # 세 지역 제외, 결측치 처리 완료

# NA 포함한 district_1 찾기
table(df2_other[rowSums(is.na(df2_other)) > 0, "district_1"]) # 전부 0


# 3) 전북, 세종, 부산의 기상조건이 유사한 지역 찾기
# 유클리드 거리 계산 함수
euclidean_distance <- function(a, b) {
  sqrt(sum((a - b)^2))
}
# 맨하탄 거리 계산 함수
manhattan_distance <- function(a, b) {
  sum(abs(a - b))
}

# 전라북도
# 전라북도와 다른 지역의 기상 데이터 추출
jeonbuk_weather <- df2 %>% filter(district_1 == "전북특별자치도")
other_weather <- df2 %>% filter(district_1 != "전북특별자치도")

# 전라북도의 기상 조건 평균 계산
jeonbuk_avg <- jeonbuk_weather %>%
  summarise(across(starts_with("ta") | starts_with("rn") | starts_with("ws"), mean, na.rm = TRUE)) # ws_mean, ws_min, hm 관련 변수는 모두 결측치라 제외

# 다른 지역의 기상 조건 평균 계산
other_avg <- other_weather %>%
  group_by(district_1) %>%
  summarise(across(starts_with("ta") | starts_with("rn") | starts_with("ws"), mean, na.rm = TRUE)) %>%
  ungroup()

# 전라북도와 다른 지역 간 유클리드 거리 계산
euclidean_distances <- other_avg %>%
  rowwise() %>%
  mutate(euclidean_distance = euclidean_distance(
    c(ta_max, ta_min, ta_max_min, rn_day, ws_max, ws_ins_max), 
    c(jeonbuk_avg$ta_max, jeonbuk_avg$ta_min, jeonbuk_avg$ta_max_min, jeonbuk_avg$rn_day, jeonbuk_avg$ws_max, jeonbuk_avg$ws_ins_max)
  )) %>%
  ungroup() %>%
  arrange(euclidean_distance)

# 전라북도와 다른 지역 간 맨하탄 거리 계산
manhattan_distances <- other_avg %>%
  rowwise() %>%
  mutate(manhattan_distance = manhattan_distance(
    c(ta_max, ta_min, ta_max_min, rn_day, ws_max, ws_ins_max), 
    c(jeonbuk_avg$ta_max, jeonbuk_avg$ta_min, jeonbuk_avg$ta_max_min, jeonbuk_avg$rn_day, jeonbuk_avg$ws_max,jeonbuk_avg$ws_ins_max)
  )) %>%
  ungroup() %>%
  arrange(manhattan_distance)

# 출력
print(euclidean_distances)
print(manhattan_distances)

# 경상북도, 충청남도, 대전광역시와 기상 유사

# 세종
# 세종특별자치시와 다른 지역의 기상 데이터 추출
sejong_weather <- df2 %>% filter(district_1 == "세종특별자치시")
other_weather <- df2 %>% filter(district_1 != "세종특별자치시")

# 세종특별자치시의 기상 조건 평균 계산
sejong_avg <- sejong_weather %>%
  summarise(across(starts_with("ta") | starts_with("rn") | starts_with("ws"), mean, na.rm = TRUE)) # ws_mean, ws_min, hm 관련 변수는 모두 결측치라 제외

# 다른 지역의 기상 조건 평균 계산
other_avg <- other_weather %>%
  group_by(district_1) %>%
  summarise(across(starts_with("ta") | starts_with("rn") | starts_with("ws"), mean, na.rm = TRUE)) %>%
  ungroup()

# 세종특별자치시와 다른 지역 간 유클리드 거리 계산
euclidean_distances <- other_avg %>%
  rowwise() %>%
  mutate(euclidean_distance = euclidean_distance(
    c(ta_max, ta_min, ta_max_min, rn_day, ws_max, ws_ins_max), 
    c(sejong_avg$ta_max, sejong_avg$ta_min, sejong_avg$ta_max_min, sejong_avg$rn_day, sejong_avg$ws_max, sejong_avg$ws_ins_max)
  )) %>%
  ungroup() %>%
  arrange(euclidean_distance)

# 세종특별자치시와 다른 지역 간 맨하탄 거리 계산
manhattan_distances <- other_avg %>%
  rowwise() %>%
  mutate(manhattan_distance = manhattan_distance(
    c(ta_max, ta_min, ta_max_min, rn_day, ws_max, ws_ins_max), 
    c(sejong_avg$ta_max, sejong_avg$ta_min, sejong_avg$ta_max_min, sejong_avg$rn_day, sejong_avg$ws_max,sejong_avg$ws_ins_max)
  )) %>%
  ungroup() %>%
  arrange(manhattan_distance)

# 출력
print(euclidean_distances)
print(manhattan_distances)

# 충청북도, 충청남도와 기상 유사


# 부산
# 부산광역시와 다른 지역의 기상 데이터 추출
busan_weather <- df2 %>% filter(district_1 == "부산광역시")
other_weather <- df2 %>% filter(district_1 != "부산광역시")

# 부산광역시의 기상 조건 평균 계산
busan_avg <- busan_weather %>%
  summarise(across(starts_with("ta") | starts_with("rn") | starts_with("ws") | starts_with("hm"), mean, na.rm = TRUE)) # 일부 데이터에만 NaN 포함 -> 모든 기상요소 이용 가능

# 다른 지역의 기상 조건 평균 계산
other_avg <- other_weather %>%
  group_by(district_1) %>%
  summarise(across(starts_with("ta") | starts_with("rn") | starts_with("ws") | starts_with("hm"), mean, na.rm = TRUE)) %>%
  ungroup()

# 부산광역시와 다른 지역 간 유클리드 거리 계산
euclidean_distances <- other_avg %>%
  rowwise() %>%
  mutate(euclidean_distance = euclidean_distance(
    c(ta_max, ta_min, ta_max_min, rn_day, ws_max, ws_ins_max, ws_mean, ws_min, hm_max, hm_mean, hm_min), 
    c(busan_avg$ta_max, busan_avg$ta_min, busan_avg$ta_max_min, busan_avg$rn_day, busan_avg$ws_max, busan_avg$ws_ins_max,
      busan_avg$ws_mean, busan_avg$ws_min, busan_avg$hm_max, busan_avg$hm_mean, busan_avg$hm_min)
  )) %>%
  ungroup() %>%
  arrange(euclidean_distance)

# 부산광역시와 다른 지역 간 맨하탄 거리 계산
manhattan_distances <- other_avg %>%
  rowwise() %>%
  mutate(manhattan_distance = manhattan_distance(
    c(ta_max, ta_min, ta_max_min, rn_day, ws_max, ws_ins_max, ws_mean, ws_min, hm_max, hm_mean, hm_min), 
    c(busan_avg$ta_max, busan_avg$ta_min, busan_avg$ta_max_min, busan_avg$rn_day, busan_avg$ws_max, busan_avg$ws_ins_max,
      busan_avg$ws_mean, busan_avg$ws_min, busan_avg$hm_max, busan_avg$hm_mean, busan_avg$hm_min)
  )) %>%
  ungroup() %>%
  arrange(manhattan_distance)

# 출력
print(euclidean_distances)
print(manhattan_distances)

# 울산광역시와 기상 유사


# 4) 선형보간

# 전북
# 전북특별자치도 데이터 저장
df2_jb <- df2 %>%
  filter(district_1 == "전북특별자치도")

# 전북특별자치도 데이터를 대체할 지역의 데이터프레임 생성
df2_jbo <- df2_other %>%
  filter(district_1 %in% c("경상북도", "충청남도", "대전광역시"))

# 수치형 변수의 평균값 계산
df2_jb2 <- df2_jbo %>%
  group_by(tm) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# 전북특별자치도 데이터의 NA 값 보간
df2_jb_cleaned <- df2_jb %>%
  left_join(df2_jb2, by = "tm", suffix = c("", ".mean")) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.x), na.spline(get(paste0(cur_column(), ".mean"))), .x))) %>%
  select(-ends_with(".mean"))

# 결측치 보간된 전북 데이터 확인
summary(df2_jb_cleaned)

# 세종
# 세종특별자치시 데이터 저장
df2_sj <- df2 %>%
  filter(district_1 == "세종특별자치시")

# 세종특별자치도 데이터를 대체할 지역의 데이터프레임 생성
df2_sjo <- df2_other %>%
  filter(district_1 %in% c("충청북도", "충청남도"))

# 수치형 변수의 평균값 계산
df2_sj2 <- df2_sjo %>%
  group_by(tm) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# 세종특별자치도 데이터의 NA 값 보간
df2_sj_cleaned <- df2_sj %>%
  left_join(df2_sj2, by = "tm", suffix = c("", ".mean")) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.x), na.spline(get(paste0(cur_column(), ".mean"))), .x))) %>%
  select(-ends_with(".mean"))

# 결측치 보간된 세종 데이터 확인
summary(df2_sj_cleaned)


# 부산
# 부산광역시 데이터 저장
df2_bs <- df2 %>%
  filter(district_1 == "부산광역시")

# 부산광역시 데이터를 대체할 지역의 데이터프레임 생성
df2_bso <- df2_other %>%
  filter(district_1 %in% c("울산광역시"))

# 수치형 변수의 평균값 계산
df2_bs2 <- df2_bso %>%
  group_by(tm) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# 부산광역시 데이터의 NA 값 보간
df2_bs_cleaned <- df2_bs %>%
  left_join(df2_bs2, by = "tm", suffix = c("", ".mean")) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.x), na.spline(get(paste0(cur_column(), ".mean"))), .x))) %>%
  select(-ends_with(".mean"))

# 결측치 보간된 부산 데이터 확인
summary(df2_bs_cleaned)

# 5) 모든 지역의 df 합치기
df2 <- bind_rows(df2_other, df2_jb_cleaned, df2_sj_cleaned, df2_bs_cleaned)
summary(df2) # 모든 결측치 처리 완료
View(df2)

##################################df3########################################
# df3

## 데이터 탐색
head(df3)
View(df3)
summary(df3)
dim(df3)   # 행 235491, 열 8
colnames(df3)
str(df3)

## 결측치 처리
colSums(is.na(df3))  # NA 없다

## date type 변경
str(df3)

# 날짜형 변수를 POSIXct 형식으로 변환
df3$tm_fc = as.POSIXct(df3$tm_fc, format = "%Y-%m-%d %H:%M:%S")
df3$tm_ef = as.POSIXct(df3$tm_ef, format = "%Y-%m-%d %H:%M:%S")

# 범주형 변수를 factor type으로 지정
df3$district_1 = as.factor(df3$district_1) ; levels(df3$district_1)
df3$district_2 = as.factor(df3$district_2) ; levels(df3$district_2)
df3$stn = as.factor(df3$stn) ; levels(df3$stn)
df3$reg_id = as.factor(df3$reg_id) ; levels(df3$reg_id)
df3$wrn = as.factor(df3$wrn) ; levels(df3$wrn) ; table(df3$wrn)
df3$lvl = as.factor(df3$lvl) ; levels(df3$lvl)
df3$cmd = as.factor(df3$cmd) ; levels(df3$cmd)

str(df3)

## 중복 데이터 제거
# 특보가 발효되기 전에 해제가 발표된 경우 제거 (cmd=3일 때 tm_fc가, cmd=1일 때 tm_fc와 tm_ef 사이에 있는 데이터 제거)
# 'cmd' 값이 1인 행을 'df3_1'에 저장
df3_1 <- df3 %>%
  filter(cmd == 1)

# 'cmd' 값이 3인 행을 'df3_3'에 저장
df3_3 <- df3 %>%
  filter(cmd == 3)

# cmd=3일 때 tm_fc가, cmd=1일 때 tm_fc와 tm_ef 사이에 있는지 확인
joined_data <- df3_1 %>%
  inner_join(df3_3, by = c("reg_id", "wrn"), suffix = c("_1", "_3")) %>%
  filter(tm_fc_3 >= tm_fc_1 & tm_fc_3 <= tm_ef_1)

# 제거할 행의 ID 식별
rows_to_remove <- joined_data %>%
  select(reg_id, wrn, tm_fc_1, tm_fc_3)

# 원본 df3_1에서 필터링된 행 제외
df3_1_filtered <- df3_1 %>%
  anti_join(rows_to_remove, by = c("reg_id", "wrn", "tm_fc" = "tm_fc_1"))

# 특보가 발효되기 전에 해제가 발표된 경우 제거 완료
View(df3_1_filtered)

# district_1 범위에서 1개의 특보가 중복으로 count되는 경우 제거
# tm_fc, tm_ef, district_1, wrn 변수만 남김
df3 <- df3_1_filtered[,-c(4, 5, 6, 8, 9)]

# 중복 데이터 제거해 df3가 district_1 범위에서 특보별 count 의미
df3 <- df3 %>%
  distinct()


#########################df1 + df2 상관분석#################################
# df1 + df2
## 날짜와 지역 기준으로 병합하기
df1 <- df1 %>%
  mutate(date = as.Date(substr(df1$tm,1,10)))
df12 <- df1 %>%
  inner_join(df2, by = c("date" = "tm", "district_1", "district_2"))

View(df12)

## 1. 화재 빈도수
# df2에서 월별, 지역별로 기상 데이터 평균 계산
df2_summ <- df2 %>%
  group_by(month, district_1) %>%
  summarise(across(c(ta_max, ta_min, ta_max_min, rn_day, ws_max, ws_ins_max, ws_mean, ws_min, hm_max, hm_mean, hm_min), mean, na.rm = TRUE), .groups = 'drop')

# 월별, 지역별 화재 발생 빈도수 계산
df12_summ <- df12 %>%
  group_by(month, district_1) %>%
  summarise(fire_count = n(), .groups = 'drop')

# df12_summ와 df2_summ 병합
df_merged <- df12_summ %>%
  left_join(df2_summ, by = c("month", "district_1"))

## 1) 상관계수 계산 및 시각화
# 기상 데이터 간의 상관계수 계산
cor_matrix <- df_merged %>%
  select(ta_max, ta_min, ta_max_min, rn_day, ws_max, ws_ins_max, ws_mean, ws_min, hm_max, hm_mean, hm_min) %>%
  cor(use = "complete.obs")
# 상관계수 행렬 출력
print(cor_matrix)

# 상관계수 행렬 시각화
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7, number.cex = 0.7)

# 변수들 간의 상관관계가 높아 다중공선성 문제 발생

## 2) 다중공선성 해결
# 회귀 모델 적합
model <- lm(fire_count ~  ta_max + ta_min + ta_max_min + rn_day + ws_max + ws_ins_max + ws_mean + ws_min + hm_max + hm_mean + hm_min, data = df_merged)

# VIF 계산
vif_values <- vif(model)
print(vif_values)

# VIF 값이 높은 변수(ta 변수)를 제거하여 다중공선성 해결
df_merged = subset(df_merged, select = -c(ta_max, ta_min, ta_max_min))

# 변수들 간의 상관계수가 높은 변수들 제거
df_merged = subset(df_merged, select = -c(ws_max, ws_ins_max, ws_min, hm_max, hm_min))


## 3) 월, 지역별로 화재발생 빈도수 변수 중요도
# 타겟 변수와 설명 변수들 선택
target_variable <- "fire_count"
predictor_variables <- c("rn_day", "ws_mean", "hm_mean")

# 랜덤 포레스트 모델에 맞게 데이터 준비
df_rf <- df_merged %>%
  select(all_of(c(target_variable, predictor_variables))) %>%
  na.omit()

# 랜덤 포레스트 모델 학습 (ranger 사용)
set.seed(42)
rf_model <- ranger(as.formula(paste(target_variable, "~ .")), data = df_rf, importance = 'impurity')

# 변수 중요도 출력 및 시각화
importance <- rf_model$variable.importance
print(importance)

# 변수 중요도 시각화
barplot(importance, main = "Variable Importance", horiz = TRUE, las = 1)


## 4) 기상 데이터와 화재 빈도수 간의 회귀 분석
# 회귀 분석 모델 생성
lm_model <- lm(fire_count ~ rn_day + ws_mean + hm_mean, data = df_merged)

# 회귀 분석 결과 요약
summary(lm_model)  # 변수들 상관성 유의함


## 5) 지역별 화재빈도수와 기상 변수 간의 상관관계
# 분석할 지역 목록
regions <- c("강원특별자치도", "경기도", "경상남도", "경상북도", "광주광역시", "대구광역시",
             "대전광역시", "부산광역시", "서울특별시", "세종특별자치시", "울산광역시", "인천광역시",
             "전라남도", "전북특별자치도", "제주특별자치도", "충청남도", "충청북도")

# 빈 데이터 프레임 생성
results <- data.frame()

# 반복문을 통해 각 지역별로 상관계수 계산
for (region in regions) {
  # 해당 지역의 데이터 필터링
  df_region <- df_merged %>% filter(district_1 == region)
  
  # 상관계수 행렬 계산
  cor_matrix <- df_region %>%
    select(fire_count, rn_day,ws_mean,hm_mean) %>%
    cor(use = "complete.obs")
  
  # fire_count 행만 추출
  fire_count_cor <- cor_matrix["fire_count", , drop = FALSE]
  
  # 데이터 프레임으로 변환하여 지역명을 추가
  fire_count_cor_df <- as.data.frame(fire_count_cor)
  fire_count_cor_df$region <- region
  
  # 결과 저장
  results <- rbind(results, fire_count_cor_df)
}

# 결과 출력
print(results)


## 2. 재산피해

# df2에서 월별, 지역별로 기상 데이터 평균 계산
df2_summ <- df2 %>%
  group_by(month, district_1) %>%
  summarise(across(c(ta_max, ta_min, ta_max_min, rn_day, ws_max, ws_ins_max, ws_mean, ws_min, hm_max, hm_mean, hm_min), mean, na.rm = TRUE), .groups = 'drop')

# 월별, 지역별 재산 피해 중앙값 계산
df12_summ2 <- df12 %>%
  group_by(month, district_1) %>%
  summarise(median_property_damage = median(property_damage, na.rm = TRUE), .groups = "drop")

# df12_summ2와 df2_summ 병합
df_merged_damage <- df12_summ2 %>%
  left_join(df2_summ, by = c("month", "district_1"))

## 1) 상관계수 계산 및 시각화
# 기상 데이터 간의 상관계수 계산
cor_matrix <- df_merged_damage %>%
  select(ta_max, ta_min, ta_max_min, rn_day, ws_max, ws_ins_max, ws_mean, ws_min, hm_max, hm_mean, hm_min) %>%
  cor(use = "complete.obs")
# 상관계수 행렬 출력
print(cor_matrix)

# 상관계수 행렬 시각화
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7, number.cex = 0.7)

# 변수들 간의 상관관계가 높아 다중공선성 문제 발생

## 2) 다중공선성 해결
# 회귀 모델 적합
model <- lm(median_property_damage ~  ta_max + ta_min + ta_max_min + rn_day + ws_max + ws_ins_max + ws_mean + ws_min + hm_max + hm_mean + hm_min, data = df_merged_damage)

# VIF 계산
vif_values <- vif(model)
print(vif_values)

# VIF 값이 높은 변수(ta 변수)를 제거하여 다중공선성 해결
df_merged_damage = subset(df_merged_damage, select = -c(ta_max, ta_min, ta_max_min))

# 변수들 간의 상관계수가 높은 변수들 제거
df_merged_damage = subset(df_merged_damage, select = -c(ws_max, ws_ins_max, ws_min, hm_max, hm_min))


## 3) 재산피해 변수 중요도
# 타겟 변수와 설명 변수들 선택
target_variable <- "median_property_damage"
predictor_variables <- c("rn_day", "ws_mean", "hm_mean")

df_rf2 <- df_merged_damage %>%
  select(all_of(c(target_variable, predictor_variables))) %>%
  na.omit()

# 랜덤 포레스트 모델 학습 (ranger 사용)
set.seed(42)
rf_model2 <- ranger(as.formula(paste(target_variable, "~ .")), data = df_rf2, importance = 'impurity')

# 변수 중요도 출력 및 시각화
importance <- rf_model2$variable.importance
print(importance)

# 변수 중요도 시각화
barplot(importance, main = "Variable Importance", horiz = TRUE, las = 1)


## 4) 기상 데이터와 재산피해 간의 회귀 분석
# 회귀 분석 모델 생성
lm_model2 <- lm(median_property_damage ~ rn_day+ ws_mean + hm_mean, data = df_merged_damage)

# 회귀 분석 결과 요약
summary(lm_model2) # 변수들 상관성 유의함


## 5) 지역별 피해규모와 기상 변수 간의 상관관계
# 빈 데이터 프레임 생성
results2 <- data.frame()

# 반복문을 통해 각 지역별로 상관계수 계산
for (region in regions) {
  # 해당 지역의 데이터 필터링
  df_region <- df_merged_damage %>% filter(district_1 == region)
  
  # 상관계수 행렬 계산
  cor_matrix <- df_region %>%
    select(median_property_damage, rn_day,ws_mean,hm_mean) %>%
    cor(use = "complete.obs")
  
  # median_property_damage 행만 추출
  property_damage_cor <- cor_matrix["median_property_damage", , drop = FALSE]
  
  # 데이터 프레임으로 변환하여 지역명을 추가
  property_damage_cor_df <- as.data.frame(property_damage_cor)
  property_damage_cor_df$region <- region
  
  # 결과 저장
  results2 <- rbind(results2, property_damage_cor_df)
}

# 결과 출력
print(results2)

###################################시각화##########################################
## 월별, 지역별 화재 빈도수 시각화

# df1에서 month열 생성
df1$month = month(df1$date)
df1$month = as.factor(df1$month) ; levels(df1$month)

# df1에서 월과 district_1별 빈도수 계산
df1_freq <- df1 %>%
  mutate(month = month(date)) %>%
  group_by(month, district_1) %>%
  summarize(count = n(), .groups = 'drop')

# 빈도수를 표준화
df1_freq <- df1_freq %>%
  group_by(district_1) %>%
  mutate(freq_standardized = (count - mean(count)) / sd(count)) %>%
  ungroup()

# 표준화 함수 정의
standardize <- function(x) {
  return((x - mean(x)) / sd(x))
}

#필요한 열만 남기기
# df2에서 필요한 열만 선택하여 df2_removed 변수에 저장
df2_removed <- df2 %>% 
  select(tm, district_1, district_2, rn_day, ws_mean, hm_mean)

# 결과 확인
View(df2_removed)

# month열 추가
df2_removed$month = month(df2_removed$tm)
df2_removed$month = as.factor(df2_removed$month) ; levels(df2_removed$month)

# df2_removed의 hm_mean과 ws_mean 표준화
df2_removed_standardized <- df2_removed %>%
  group_by(district_1, month) %>%
  summarize(hm_mean = median(hm_mean), ws_mean = median(ws_mean), rn_day = median(rn_day)) %>%
  mutate(hm_mean_standardized = standardize(hm_mean),
         ws_mean_standardized = standardize(ws_mean),
         rn_day_standardized = standardize(rn_day)) %>%
  ungroup()

# 그래프 그리기
ggplot() +
  # 막대 그래프 (df1_freq 데이터 사용)
  geom_bar(data = df1_freq, aes(x = factor(month), y = freq_standardized, fill = district_1),
           stat = "identity", position = "dodge") +
  # 꺾은선 그래프 (df2_removed_standardized 데이터 사용)
  geom_point(data = df2_removed_standardized, aes(x = factor(month), y = hm_mean_standardized, color = "hm_mean (평균 상대습도)"), size = 1) +
  geom_line(data = df2_removed_standardized, aes(x = factor(month), y = hm_mean_standardized, group = district_1, color = "hm_mean (평균 상대습도)"), size = 1, linetype = "dashed") +
  geom_point(data = df2_removed_standardized, aes(x = factor(month), y = ws_mean_standardized, color = "ws_mean (평균풍속)"), size = 1) +
  geom_line(data = df2_removed_standardized, aes(x = factor(month), y = ws_mean_standardized, group = district_1, color = "ws_mean (평균풍속)"), size = 1, linetype = "dashed") +
  geom_point(data = df2_removed_standardized, aes(x = factor(month), y = rn_day_standardized, color = "rn_day (강수량)"), size = 1) +
  geom_line(data = df2_removed_standardized, aes(x = factor(month), y = rn_day_standardized, group = district_1, color = "rn_day (강수량)"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("hm_mean (평균 상대습도)" = "blue", "ws_mean (평균풍속)" = "red", "rn_day (강수량)" = "green")) +
  scale_y_continuous(name = "Frequency Standardized (df1)", sec.axis = sec_axis(~., name = "Standardized (df2_removed)")) +
  labs(x = "Month", y = "Frequency Standardized (df1)", fill = "District", color = "Legend") +
  theme_minimal() +
  facet_wrap(~district_1, scales = "free_y", ncol = 4)  # facet_wrap을 사용하여 17개의 그래프를 한 번에 그리기

## 월별, 지역별 property_damage의 median 시각화
# district_1 별로 데이터 프레임 나누기
district_list <- split(df12, df12$district_1)

# 그래프 리스트 생성
plot_list <- list()

# 각 district_1 별로 그래프 생성
for (district in names(district_list)) {
  data <- district_list[[district]]
  
  # month별 property_damage의 중앙값 계산
  median_data <- data %>%
    group_by(month) %>%
    summarize(median_damage = median(property_damage, na.rm = TRUE))
  
  # 그래프 생성
  p <- ggplot(median_data, aes(x = month, y = median_damage)) +
    geom_bar(stat = "identity") +
    geom_point(color = "red") +
    geom_line(aes(group = 1), color = "red") +
    ggtitle(paste("District:", district)) +
    xlab("Month") +
    ylab("Median Property Damage") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 그래프를 리스트에 추가
  plot_list[[district]] <- p
}

# 한 화면에 17개 그래프 배치
do.call(grid.arrange, c(plot_list, ncol = 4))

####################### 유효하게 볼 세 개의 함수 ######################
# df3에서 month열 생성
df3$month = month(df3$tm_ef)
df3$month = as.factor(df3$month) ; levels(df3$month)

# df3의 '전라북도'를 '전북특별자치도'로 변경
df3 <- df3 %>%
  mutate(district_1 = ifelse(district_1 == "전라북도", "전북특별자치도", district_1))

# df3에서 월과 district_1별 빈도수 계산
df3_freq <- df3 %>%
  group_by(month, district_1, wrn) %>%
  summarize(count = n())

# 1. W, ws_mean, 빈도
df3_freq_w <- df3_freq %>%
  filter(wrn == "W")

## 정규화
# 빈도수를 정규화
df3_freq_w <- df3_freq_w %>%
  group_by(district_1) %>%
  mutate(freq_standardized = (count - mean(count)) / sd(count)) %>%
  ungroup()

# 그래프 그리기
ggplot() +
  # 막대 그래프 (df1_freq 데이터 사용)
  geom_bar(data = df1_freq, aes(x = factor(month), y = freq_standardized, fill = district_1),
           stat = "identity", position = "dodge") +
  # 꺾은선 그래프 (df2_removed_standardized 데이터 사용)
  geom_point(data = df3_freq_w, aes(x = factor(month), y = freq_standardized, color = "W (강풍)"), size = 1) +
  geom_line(data = df3_freq_w, aes(x = factor(month), y = freq_standardized, group = district_1, color = "W (강풍)"), size = 1, linetype = "dashed") +
  geom_point(data = df2_removed_standardized, aes(x = factor(month), y = ws_mean_standardized, color = "ws_mean (평균풍속)"), size = 1) +
  geom_line(data = df2_removed_standardized, aes(x = factor(month), y = ws_mean_standardized, group = district_1, color = "ws_mean (평균풍속)"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("W (강풍)" = "dimgrey", "ws_mean (평균풍속)" = "red")) +
  scale_y_continuous(name = "Frequency Standardized (df1)", sec.axis = sec_axis(~., name = "Standardized (df3_freq_w)")) +
  labs(x = "Month", y = "Frequency Standardized (df1)", fill = "District", color = "Legend") +
  theme_minimal() +
  facet_wrap(~district_1, scales = "free_y", ncol = 4)  # facet_wrap을 사용하여 17개의 그래프를 한 번에 그리기

# 2. D, hm_mean, 빈도
df3_freq_d <- df3_freq %>%
  filter(wrn == "D")

## 정규화
# 빈도수를 정규화
df3_freq_d <- df3_freq_d %>%
  group_by(district_1) %>%
  mutate(freq_standardized = (count - mean(count)) / sd(count)) %>%
  ungroup()

# 그래프 그리기
ggplot() +
  # 막대 그래프 (df1_freq 데이터 사용)
  geom_bar(data = df1_freq, aes(x = factor(month), y = freq_standardized, fill = district_1),
           stat = "identity", position = "dodge") +
  # 꺾은선 그래프 (df2_removed_standardized 데이터 사용)
  geom_point(data = df3_freq_d, aes(x = factor(month), y = freq_standardized, color = "D (건조)"), size = 1) +
  geom_line(data = df3_freq_d, aes(x = factor(month), y = freq_standardized, group = district_1, color = "D (건조)"), size = 1, linetype = "dashed") +
  geom_point(data = df2_removed_standardized, aes(x = factor(month), y = hm_mean_standardized, color = "hm_mean (평균 상대습도)"), size = 1) +
  geom_line(data = df2_removed_standardized, aes(x = factor(month), y = hm_mean_standardized, group = district_1, color = "hm_mean (평균 상대습도)"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("D (건조)" = "dimgrey", "hm_mean (평균 상대습도)" = "red")) +
  scale_y_continuous(name = "Frequency Standardized (df1)", sec.axis = sec_axis(~., name = "Standardized (df3_freq_w)")) +
  labs(x = "Month", y = "Frequency Standardized (df1)", fill = "District", color = "Legend") +
  theme_minimal() +
  facet_wrap(~district_1, scales = "free_y", ncol = 4)  # facet_wrap을 사용하여 17개의 그래프를 한 번에 그리기


# 3. H, C, 재산 피해
library(tidyr)
# 재산 피해를 정규화
# df1에서 property_damage를 지역별로 나누고 월별 평균을 계산
df1_property <- df1 %>%
  group_by(district_1, month) %>%
  summarise(avg_damage = mean(property_damage, na.rm = TRUE)) %>%
  ungroup()

df1_property <- df1_property %>%
  group_by(district_1) %>%
  mutate(property_standardized = (avg_damage - mean(avg_damage)) / sd(avg_damage)) %>%
  ungroup()

# 모든 조합의 district_1과 month 값을 포함하는 데이터 프레임 생성
district_month <- expand.grid(
  district_1 = unique(df3$district_1),
  month = unique(df3$month)
)

df1_property <- district_month %>%
  left_join(df1_property, by = c("district_1", "month")) %>%
  replace_na(list(count = 0))

# df3_freq_h
df3_freq_h <- df3_freq %>%
  filter(wrn == "H")

# df3_freq_c
df3_freq_c <- df3_freq %>%
  filter(wrn == "C")


# 원본 데이터프레임과 병합하여 누락된 조합을 0으로 채움
df3_freq_h <- district_month %>%
  left_join(df3_freq_h, by = c("district_1", "month")) %>%
  replace_na(list(count = 0))

df3_freq_c <- district_month %>%
  left_join(df3_freq_c, by = c("district_1", "month")) %>%
  replace_na(list(count = 0))

# 정규화 대신 최댓값으로 나누기
df3_freq_h <- df3_freq_h %>%
  group_by(district_1) %>%
  mutate(freq_standardized = count / max(count)) %>%
  ungroup()

df3_freq_c <- df3_freq_c %>%
  group_by(district_1) %>%
  mutate(freq_standardized = count / max(count)) %>%
  ungroup()

# 그래프 그리기
ggplot() +
  # 막대 그래프 (df1_property 데이터 사용)
  geom_bar(data = df1_property, aes(x = factor(month), y = property_standardized, fill = district_1),
           stat = "identity", position = "dodge") +
  # 꺾은선 그래프 (df2_removed_standardized 데이터 사용)
  geom_point(data = df3_freq_h, aes(x = factor(month), y = freq_standardized, color = "H (폭염)"), size = 1) +
  geom_line(data = df3_freq_h, aes(x = factor(month), y = freq_standardized, group = district_1, color = "H (폭염)"), size = 1, linetype = "dashed") +
  geom_point(data = df3_freq_c, aes(x = factor(month), y = freq_standardized, color = "C (한파)"), size = 1) +
  geom_line(data = df3_freq_c, aes(x = factor(month), y = freq_standardized, group = district_1, color = "C (한파)"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("H (폭염)"="red", "C (한파)"="blue")) +
  scale_y_continuous(name = "Property Standardized (df1)", sec.axis = sec_axis(~., name = "Standardized (df3_freq_w)")) +
  labs(x = "Month", y = "Property Standardized (df1)", fill = "District", color = "Legend") +
  theme_minimal() +
  facet_wrap(~district_1, scales = "free_y", ncol = 4)  # facet_wrap을 사용하여 17개의 그래프를 한 번에 그리기
