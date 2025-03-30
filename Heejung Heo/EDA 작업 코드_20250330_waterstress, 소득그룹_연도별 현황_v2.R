#1. water_stress 테이블에 IncomeGroup 값을 결합

# water_stress 테이블과 info 테이블을 Country.Code를 기준으로 결합
merged_data <- merge(water_stress, info, by = "Country.Code", all.x = TRUE)

#2.  IncomeGroup별 연도별 비율의 기하 평균 계산

# 필요한 라이브러리 로드
library(dplyr)

# IncomeGroup별 연도별 비율의 기하 평균 계산
geometric_means <- merged_data %>%
  group_by(IncomeGroup) %>%
  summarise(across(starts_with("X"), ~exp(mean(log(.), na.rm = TRUE)), .names = "geom_mean_{.col}"))


#3. IncomeGroup별 평균값을 연도별로 그래프 표시

# 필요한 라이브러리 로드
library(ggplot2)

# 데이터 변환 (Long format으로 변경)
geom_means_long <- geometric_means %>%
  pivot_longer(cols = starts_with("geom_mean"), 
               names_to = "Year", 
               values_to = "Geometric_Mean") %>%
  mutate(Year = gsub("geom_mean_X", "", Year))

# IncomeGroup별 연도별 비율 그래프
ggplot(geom_means_long, aes(x = Year, y = Geometric_Mean, color = IncomeGroup, group = IncomeGroup)) +
  geom_line() +
  geom_point() +
  labs(title = "Geometric Mean of Water Stress by Income Group (1994-2021)",
       x = "Year", y = "Geometric Mean of Water Stress") +
  theme_minimal() +  # ✅ 여기에서 theme()을 적용
  theme(
    legend.position = "bottom", 
    legend.justification = "center",
    plot.title = element_text(hjust = 0.5)  # ✅ 제목 가운데 정렬
  ) +
  guides(color = guide_legend(nrow = 1)) 

