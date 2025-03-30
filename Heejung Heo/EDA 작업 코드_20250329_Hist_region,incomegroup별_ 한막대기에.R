rm(list = ls())

agriculture <- readRDS("agriculture.rds")
domestic <- readRDS("domestic.rds")
gdppc <- readRDS("gdppc.rds")
gdppc_from1993 <- readRDS("gdppc_from1993.rds")
industry <- readRDS("industry.rds")
info <- readRDS("info.rds")
natural_disasters <- readRDS("natural_disasters.rds")
population <- readRDS("population.rds")
private_investment <- readRDS("private_investment.rds")
water_productivity <- readRDS("water_productivity.rds")
water_stress <- readRDS("water_stress.rds")
withdrawals <- readRDS("withdrawals.rds")
available_resources <- readRDS("available_resources.rds")
gdp_ppp <- readRDS("gdp_ppp.rds")
gdp_usd <- readRDS("gdp_usd.rds")





# 필요한 패키지 불러오기
library(dplyr)
library(tidyr)
library(ggplot2)

# ---- 1. 데이터 전처리 ----
# IncomeGroup 정보가 포함된 info 데이터를 병합하고, wide → long 변환
reshape_and_merge <- function(data, category_name) {
  data_long <- data %>%
    pivot_longer(cols = starts_with("X"), 
                 names_to = "Year", 
                 values_to = "Value") %>%
    mutate(Year = as.numeric(gsub("X", "", Year)),
           Category = category_name) %>%  # 데이터 출처를 위한 카테고리 열 추가
    left_join(info, by = "Country.Code") # info 데이터 병합
  
  return(data_long)
}

# 데이터 변환 및 병합
domestic_long <- reshape_and_merge(domestic, "Domestic")
agriculture_long <- reshape_and_merge(agriculture, "Agriculture")
industry_long <- reshape_and_merge(industry, "Industry")

# 모든 데이터 합치기
combined_data <- bind_rows(domestic_long, agriculture_long, industry_long)

# IncomeGroup 결측치 처리
combined_data <- combined_data %>%
  mutate(IncomeGroup = ifelse(is.na(IncomeGroup), "Unknown", IncomeGroup))

# ---- 2. IncomeGroup별 연도별 총 평균 사용량 계산 ----
average_usage_income_group <- combined_data %>%
  group_by(IncomeGroup, Year, Category) %>%
  summarise(average_usage = mean(Value, na.rm = TRUE), .groups = "drop")

# ---- 3. 히스토그램 그리기 함수 ----
plot_income_group_trend <- function(data, income_group_name) {
  income_data <- filter(data, IncomeGroup == income_group_name)
  
  if (nrow(income_data) == 0) {
    message(paste("No data found for IncomeGroup:", income_group_name))
    return(NULL)
  }
  
  ggplot(income_data, aes(x = Year, y = average_usage, fill = Category)) +
    geom_bar(stat = "identity", position = "stack", size = 0.5, width = 0.5) +  
    scale_fill_manual(values = c(
      "Domestic" = "#A6CEE3", 
      "Agriculture" = "#1F77B4", 
      "Industry" = "#004B8C"
    )) + 
    scale_x_continuous(breaks = seq(1994, 2021, by = 1)) +  
    theme_minimal() +
    labs(title = paste(income_group_name, "- 연도별 Domestic, Agriculture, Industry 평균 사용량"),
         x = "Year", y = "Average Usage", fill = "Category") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  
      plot.title = element_text(hjust = 0.5),  
      legend.position = "bottom" 
    )
}


# ---- 4. 예제 실행 ----
plot_income_group_trend(average_usage_income_group, "Upper middle income") 
