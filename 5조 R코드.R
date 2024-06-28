fire_firefighting = read.csv("c:/project/firefighting.csv")
fire_weather = read.csv("c:/project/weather.csv")
fire_weather_special = read.csv("c:/project/weather_special.csv")

# 데이터프레임 변환
df1 = data.frame(fire_firefighting[-c(1,2)]) # 처음 열인 num 제거
df2 = data.frame(fire_weather[-1])
df3 = data.frame(fire_weather_special[-1])

# 데이터 확인
### 반복되는 접두사 제거 "gsub" 함수 사용
colnames(df1) <- gsub("^fire_firefighting\\.", "", colnames(df1))
colnames(df2) <- gsub("^fire_weather\\.", "", colnames(df2))
colnames(df3) <- gsub("^fire_weather_special\\.", "", colnames(df3))


# df1
## 1. 데이터 탐색(조회)
head(df1)
View(df1)
summary(df1)
dim(df1)  # 행 236337, 열 15
colnames(df1)
str(df1)

## 2. 결측치(NA) 처리
### 결측치 확인
colSums(is.na(df1)) # 모두 0 : NA가 아니라 빈 문자열임 
colSums(df1=="") # fire_type_2 컬럼에만 "" 존재해 # 총 53718개 데이터 -> 기타 + 임야 -> 해당 열 제거. fire_type_1 컬럼만 이용

df1 = subset(df1, select=-fire_type_2)

# 3. 범주형 변수 변경
str(df1)

# 문자형 벡터를 POSIXlt 형식으로 변환
df1$tm = as.POSIXlt(df1$tm, format = "%Y-%m-%d %H:%M:%S")

## factor 지정
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

# 4. 수치형 데이터 시각화 및 이상치 처리
# property_damage
boxplot(df1$property_damage) # outlier 많이 발견됨
library(ggplot2)
ggplot(df1, aes(year,property_damage)) + 
  geom_violin()+geom_boxplot(width=0.5) + 
  geom_jitter(alpha=0.5,color="grey50",width=0.1)  # 2019, 2021, 2022, 2023년 최대치 제거

sort(df1$property_damage, decreasing=TRUE)[1:4] # 9자리인 최대 4개 데이터 확인
summary(df1[df1$property_damage < 100000000, "property_damage"]) 

df1 = df1[df1$property_damage < 100000000,]
ggplot(df1, aes(year,property_damage)) +
  geom_violin()+geom_boxplot(width=0.5)+
  geom_jitter(alpha=0.5,color="grey50",width=0.1)


# df2
## 1. 데이터 탐색(조회)
head(df2)
View(df2)
summary(df2)
dim(df2)    # 행 552579, 열 15
colnames(df2)
str(df2)

## 2. 데이터 전처리
## stn 변수 제거 (stn = aws 지점 코드 -> 지역과 같은 의미)
df2 = subset(df2, select=-stn)

##범주형 변수 처리
df2$district_1 = as.factor(df2$district_1) ; levels(df2$district_1)
df2$district_2 = as.factor(df2$district_2) ; levels(df2$district_2)

##날짜형 변수 처리
df2$tm = as.POSIXlt(df2$tm, format = "%Y-%m-%d")

## 3. 이상치 처리
colSums(is.na(df2))
summary(df2)  # -99라는 이상치가 모든 수치형 데이터에 존재

boxplot(df2$ta_max) 
boxplot(df2$ta_min)
boxplot(df2$ta_max_min) 
#시각화를 통해 결측치가 -99로 포함되어 있음을 알 수 있음

# -99를 각 district_1별, month별 중앙값으로 대체
#install.packages("lubridate")
library(lubridate)
df2$month = month(df2$tm)
df2$month = as.factor(df2$month) ; levels(df2$month)

# 비대칭 분포이며 이상치가 많아 중앙값으로 대체함
# 처리할 열 목록
columns_to_process <- c("ta_max", "ta_min", "ta_max_min", "rn_day", "ws_max", 
                        "ws_ins_max", "ws_mean", "ws_min", "hm_max", "hm_mean", "hm_min")

install.packages("dplyr")
library(dplyr)
df2 <- df2 %>%
  group_by(district_1, month) %>%
  mutate(across(all_of(columns_to_process), ~ ifelse(. == -99, median(.[. != -99], na.rm = TRUE), .)))

# 결과 확인
summary(df2)

# district_1와 month로 그룹화해 중앙값으로 대체했지만 전부 -99이라 여전히 -99로 남아있는 것 존재
# -99를 대체할 값
df2[df2 == -99] <- NA  # 먼저 -99를 NA로 변환

# 그래도 NA가 있는 행 제거 (대체할 수 없는 경우)
df2 <- na.omit(df2)

# 결과 출력
summary(df2)
dim(df2) # 행 514555 열 15 (이전 행 552579)


###################################################################################################

# 시각화

# 1. 지역별 화재빈도 
# df1에 month 변수 추가 
df1$month = month(df1$tm)
df1$month = as.factor(df1$month) ; levels(df1$month) 

# 지역별 화재빈도
df1_by_district_1 <- df1 %>%
  group_by(district_1) %>%
  summarise(Fire_Count = n())

# 파스텔톤 색상 팔레트 설정
install.packages("RColorBrewer")
library(scales)
pastel_colors <- hue_pal()(17)

# 지역별 화재 발생 빈도 시각화
ggplot(df1_by_district_1, aes(x = district_1, y = Fire_Count, fill = district_1)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_fill_manual(values = pastel_colors) +
  labs(title = "지역별 화재 발생 빈도",
       x = "지역",
       y = "화재 발생 빈도",
       fill = "지역") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"  # 범례 제거 (색상만 있으므로 불필요)
  )


# 2. 월별 화재빈도
# 월별 화재 발생 빈도 계산
df1_by_month <- df1 %>%
  group_by(month) %>%
  summarise(Fire_Count = n())

# 파스텔톤 색상 팔레트 설정
pastel_colors <- hue_pal()(12)

# 월별 화재 발생 빈도 시각화
ggplot(df1_by_month, aes(x = month, y = Fire_Count, fill = month)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_fill_manual(values = pastel_colors) +
  labs(title = "월별 화재 발생 빈도",
       x = "월",
       y = "화재 발생 빈도",
       fill = "월") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"  # 범례 제거 (색상만 있으므로 불필요)
  )


# 3. 유형별 화재빈도
# 유형별 화재 발생 빈도 계산
df1_by_fire_type_1 <- df1 %>%
  group_by(fire_type_1) %>%
  summarise(Fire_Count = n())

# 파스텔톤 색상 팔레트 설정
pastel_colors <- hue_pal()(6)

# 유형별 화재 발생 빈도 시각화
ggplot(df1_by_fire_type_1, aes(x = fire_type_1, y = Fire_Count, fill = fire_type_1)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_fill_manual(values = pastel_colors) +
  labs(title = "유형별 화재 발생 빈도",
       x = "유형",
       y = "화재 발생 빈도",
       fill = "유형") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 15),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"  # 범례 제거 (색상만 있으므로 불필요)
  )


# 4. 시간대별 화재 발생 빈도
df1_by_time <- df1 %>%
  mutate(Date = as.Date(tm),
         Hour = as.numeric(format(tm, "%H")),  # 시간대에서 시간만 추출
         Minute = as.numeric(format(tm, "%M"))) %>%
  group_by(Hour) %>%
  summarise(Fire_Count = n())
max_fire_hour <- df1_by_time[which.max(df1_by_time$Fire_Count), 'Hour']

# 시계열 그래프 시각화
ggplot(df1_by_time, aes(x = Hour, y = Fire_Count)) +
  geom_smooth(method = "loess", se = FALSE)+
  labs(title = "시간대별 화재 발생 빈도",
       x = "시간",
       y = "평균 화재 발생 빈도") +
  scale_x_continuous(breaks = seq(0, 23, by = 1)) +  # 1시간 간격으로 눈금 설정
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


# 5. 지역별 피해규모
# 지역별 총 피해 규모 계산
damage_by_district_1 <- df1 %>%
  group_by(district_1) %>%
  summarise(total_damage = sum(property_damage, na.rm = TRUE))

# 히스토그램 그리기
ggplot(damage_by_district_1, aes(x = district_1, y = total_damage)) +
  geom_col(fill = "darkgreen") +
  labs(title = "지역별 피해 규모",
       x = "지역",
       y = "총 피해 규모") +
  theme_minimal()


# 6. 유형별 피해규모
# 유형별 총 피해 규모 계산
damage_by_fire_type_1 <- df1 %>%
  group_by(fire_type_1) %>%
  summarise(total_damage = sum(property_damage, na.rm = TRUE))

# 히스토그램 그리기
ggplot(damage_by_fire_type_1, aes(x = fire_type_1, y = total_damage)) +
  geom_col(fill = "darkgreen") +
  labs(title = "유형별 피해 규모",
       x = "유형",
       y = "총 피해 규모") +
  theme_minimal()



# 7. 세부요인별 피해규모
# 세부요인인별 총 피해 규모 계산
damage_by_ignition_factor_category_1 <- df1 %>%
  group_by(ignition_factor_category_1) %>%
  summarise(total_damage = sum(property_damage, na.rm = TRUE))

# 히스토그램 그리기
ggplot(damage_by_ignition_factor_category_1, aes(x = ignition_factor_category_1, y = total_damage)) +
  geom_col(fill = "darkgreen") +
  labs(title = "세부요인별 피해 규모",
       x = "세부요인",
       y = "총 피해 규모") +
  theme_minimal()


# 8. 지역별 인명피해
# 지역별 총 인명피해 계산
casualties_by_district_1 <- df1 %>%
  group_by(district_1) %>%
  summarise(total_casualties = sum(casualties, na.rm = TRUE))

# 히스토그램 그리기
ggplot(casualties_by_district_1, aes(x = district_1, y = total_casualties)) +
  geom_col(fill = "darkgreen") +
  labs(title = "지역별 인명피해",
       x = "지역",
       y = "총 인명 피해") +
  theme_minimal()



# 9. 유형별 인명 피해
# 유형별 총 인명피해 계산
casualties_by_fire_type_1 <- df1 %>%
  group_by(fire_type_1) %>%
  summarise(total_casualties = sum(casualties, na.rm = TRUE))

# 히스토그램 그리기
ggplot(casualties_by_fire_type_1, aes(x = fire_type_1, y = total_casualties)) +
  geom_col(fill = "darkgreen") +
  labs(title = "유형별 피해 규모",
       x = "유형형",
       y = "총 피해 규모") +
  theme_minimal()


# 10. 세부요인별 인명피해
# 세부요인별 총 인명 피해 계산
casualties_by_ignition_factor_category_1 <- df1 %>%
  group_by(ignition_factor_category_1) %>%
  summarise(total_casualties = sum(casualties, na.rm = TRUE))

# 히스토그램 그리기
ggplot(casualties_by_ignition_factor_category_1, aes(x = ignition_factor_category_1, y = total_casualties)) +
  geom_col(fill = "darkgreen") +
  labs(title = "세부요인별 인명피해",
       x = "세부요인인",
       y = "총 인명피해") +
  theme_minimal()

##############################################################################################33
# df2 기상데이터만 이용
#기후 패턴 분석
library(tidyr)

# KMeans 클러스터링 적용
set.seed(42)
kmeans_result <- kmeans(df2[-c(1,2,3,15)], centers = 4)
df2$Cluster <- as.factor(kmeans_result$cluster)

# 클러스터별 기후 패턴 분석
cluster_summary <- df2[-c(1,2,3,15)] %>%
  group_by(Cluster) %>%
  summarise_all(mean)
print(cluster_summary)

# 결과 시각화
cluster_summary_long <- cluster_summary %>%
  pivot_longer(cols = -Cluster, names_to = "Variable", values_to = "Value")

ggplot(cluster_summary_long, aes(x = Variable, y = Value, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Cluster-wise Climate Patterns", x = "Climate Variables", y = "Mean Values")




#df2 clustering
# spring clustering
df2_sp <- df2 %>%
  filter(month %in% c(3,4,5))

df2_sp = df2_sp[-c(1,15)] # 날짜와 month 변수 지우기

##district_1별 중앙값
median_sp <- df2_sp[-2] %>%
  group_by(district_1) %>%
  summarise(across(everything(), median, na.rm = TRUE))

kmeans_result = kmeans(median_sp[-1], centers = 4)
clus = data.frame(median_sp, kmeans = kmeans_result$cluster)

ggplot(clus, aes(ta_max, ta_min)) + 
  geom_text(aes(label=district_1)) + 
  geom_point(aes(color=factor(kmeans)), size = 9, alpha = 0.3) +
  ggtitle("K-means clustering")
ggplot(clus, aes(ta_max, hm_max)) +
  geom_text(aes(label=district_1)) +
  geom_point(aes(color=factor(kmeans)), size = 7, alpha = 0.3) +
  ggtitle("K-means clustering")


# summer clustering
df2_su <- df2 %>%
  filter(month %in% c(6,7,8))

df2_su = df2_su[-c(1,15)] # 날짜와 month 변수 지우기

##district_1별 중앙값
median_su <- df2_su[-2] %>%
  group_by(district_1) %>%
  summarise(across(everything(), median, na.rm = TRUE))

kmeans_result = kmeans(median_su[-1], centers = 4)
clus = data.frame(median_su, kmeans = kmeans_result$cluster)

ggplot(clus, aes(ta_max, ta_min)) + 
  geom_text(aes(label=district_1)) + 
  geom_point(aes(color=factor(kmeans)), size = 9, alpha = 0.3) +
  ggtitle("K-means clustering")
ggplot(clus, aes(ta_max, hm_max)) + 
  geom_text(aes(label=district_1)) + 
  geom_point(aes(color=factor(kmeans)), size = 7, alpha = 0.3) +
  ggtitle("K-means clustering")


# fall clustering
df2_fa <- df2 %>%
  filter(month %in% c(9,10,11))

df2_fa = df2_fa[-c(1,15)] # 날짜와 month 변수 지우기

##district_1별 중앙값
median_fa <- df2_fa[-2] %>%
  group_by(district_1) %>%
  summarise(across(everything(), median, na.rm = TRUE))

kmeans_result = kmeans(median_fa[-1], centers = 4)
clus = data.frame(median_fa, kmeans = kmeans_result$cluster)

ggplot(clus, aes(ta_max, ta_min)) +
  geom_text(aes(label=district_1)) +
  geom_point(aes(color=factor(kmeans)), size = 9, alpha = 0.3) +
  ggtitle("K-means clustering")
ggplot(clus, aes(ta_max, hm_max)) +
  geom_text(aes(label=district_1)) +
  geom_point(aes(color=factor(kmeans)), size = 7, alpha = 0.3) +
  ggtitle("K-means clustering")


# winter clustering
df2_wi <- df2 %>%
  filter(month %in% c(12,1,2))

df2_wi = df2_wi[-c(1,15)] # 날짜와 month 변수 지우기

##district_1별 중앙값
median_wi <- df2_wi[-2] %>%
  group_by(district_1) %>%
  summarise(across(everything(), median, na.rm = TRUE))

kmeans_result = kmeans(median_wi[-1], centers = 4)
clus = data.frame(median_wi, kmeans = kmeans_result$cluster)

ggplot(clus, aes(ta_max, ta_min)) +
  geom_text(aes(label=district_1)) +
  geom_point(aes(color=factor(kmeans)), size = 9, alpha = 0.3) +
  ggtitle("K-means clustering")
ggplot(clus, aes(ta_max, hm_max)) +
  geom_text(aes(label=district_1)) +
  geom_point(aes(color=factor(kmeans)), size = 7, alpha = 0.3) +
  ggtitle("K-means clustering")

##########################################################################
# df1, df2 화재발생, 기상 데이터 모두 이용
# df1과 df2를 날짜와 지역 기준으로 병합하기
df1 <- df1 %>%
  mutate(date = as.Date(tm))

df12 <- df1 %>%
  inner_join(df2, by = c("date" = "tm", "district_1", "district_2"))

head(df12)


# 봄 spring
df12_sp <- df12 %>%
  filter(month %in% c(3,4,5))
head(df12_sp)

# 클러스터링을 위한 데이터 준비
# 필요한 변수 선택
clustering_data2 <- df12_sp %>%
  group_by(district_1) %>%
  summarise(fire_count = n(),
            across(where(is.numeric), median, na.rm = TRUE))

# K-평균 클러스터링 수행
set.seed(123)  # 재현성을 위해 시드 설정
kmeans_result2 <- kmeans(clustering_data2[-c(1:6)], centers = 3)

# 클러스터 결과를 데이터프레임에 추가
clustering_data2$cluster <- kmeans_result2$cluster

cor(clustering_data2[-c(1,3:5,10)])

# 시각화
ggplot(clustering_data2, aes(x = hm_mean, y = property_damage, color = factor(cluster))) +
  geom_point(size = 5, alpha = 0.7) +
  labs(title = "Clustering of Fire Incidents Based on Weather Conditions",
       x = "Average Humidity",
       y = "Property Damage",
       color = "Cluster") +
  geom_text(aes(label=district_1)) +
  theme_minimal()

# 여름 summer
df12_su <- df12 %>%
  filter(month %in% c(6,7,8))
head(df12_su)

# 클러스터링을 위한 데이터 준비
# 필요한 변수 선택
clustering_data3 <- df12_su %>%
  group_by(district_1) %>%
  summarise(fire_count = n(),
            across(where(is.numeric), median, na.rm = TRUE))

# K-평균 클러스터링 수행
set.seed(123)  # 재현성을 위해 시드 설정
kmeans_result3 <- kmeans(clustering_data3[-c(1:6)], centers = 3) 

# 클러스터 결과를 데이터프레임에 추가
clustering_data3$cluster <- kmeans_result3$cluster

cor(clustering_data3[-c(1,3:5,10)])

# 시각화
ggplot(clustering_data3, aes(x = ta_max_min, y = property_damage, color = factor(cluster))) +
  geom_point(size = 5, alpha = 0.7) +
  labs(title = "Clustering of Fire Incidents Based on Weather Conditions",
       x = "Temperature Difference",
       y = "Property Damage",
       color = "Cluster") +
  geom_text(aes(label=district_1)) +
  theme_minimal()

# 가을 fall
df12_fa <- df12 %>%
  filter(month %in% c(9,10,11))
head(df12_fa)

# 클러스터링을 위한 데이터 준비
# 필요한 변수 선택
clustering_data4 <- df12_fa %>%
  group_by(district_1) %>%
  summarise(fire_count = n(),
            across(where(is.numeric), median, na.rm = TRUE))

# K-평균 클러스터링 수행
set.seed(123)  # 재현성을 위해 시드 설정
kmeans_result4 <- kmeans(clustering_data4[-c(1:6)], centers = 3)

# 클러스터 결과를 데이터프레임에 추가
clustering_data4$cluster <- kmeans_result4$cluster

cor(clustering_data4[-c(1,3:5,10)])

# 시각화
ggplot(clustering_data4, aes(x = hm_max, y = property_damage, color = factor(cluster))) +
  geom_point(size = 5, alpha = 0.7) +
  labs(title = "Clustering of Fire Incidents Based on Weather Conditions",
       x = "Maximum Humidity",
       y = "Property Damage",
       color = "Cluster") +
  geom_text(aes(label=district_1)) +
  theme_minimal()


# 겨울 winter
df12_wi <- df12 %>%
  filter(month %in% c(12, 1, 2))
head(df12_wi)

# 클러스터링을 위한 데이터 준비
# 필요한 변수 선택
clustering_data1 <- df12_wi %>%
  group_by(district_1) %>%
  summarise(fire_count = n(),
            across(where(is.numeric), median, na.rm = TRUE))

# K-평균 클러스터링 수행
set.seed(123)  # 재현성을 위해 시드 설정
kmeans_result1 <- kmeans(clustering_data1[-c(1:6)], centers = 3)

# 클러스터 결과를 데이터프레임에 추가
clustering_data1$cluster <- kmeans_result1$cluster

cor(clustering_data1[-c(1,3:5,10)])

# 시각화
ggplot(clustering_data1, aes(x = hm_max, y = property_damage, color = factor(cluster))) +
  geom_point(size = 5, alpha = 0.7) +
  labs(title = "Clustering of Fire Incidents Based on Weather Conditions",
       x = "Maximum Humidity",
       y = "Property Damage",
       color = "Cluster") +
  geom_text(aes(label=district_1)) +
  theme_minimal()