rm(list = ls())
library(tidyverse)
library(fixest)
library(pandoc)
library(webshot2)
library(modelsummary)
library(flextable)
library(tinytable)
library(kableExtra)

#データの読み込み
meal_lag_data <- read.csv("Meal_panel_lag_251202.csv", header = TRUE)
cgpi_data <- read.csv("CGPI_panel_260129.csv", header = TRUE)
fiscal_data <- read.csv("Fiscal_panel_260106.csv", header = TRUE)
manage_data <- read.csv("Management_panel_260112.csv", header = TRUE)
student_data <- read.csv("Student_panel_260130.csv", header = TRUE)

#データの整理
student_lag_df <- student_data %>%
  filter(年度 != "R6") %>%
  mutate(平均児童数 = 総児童数/学校数) %>%
  select(年度, 自治体名, 平均児童数)

cgpi_lag_df <- cgpi_data %>%
  rename(年度 = 年) %>%
  filter(年度 != "R6",
         月 == 5) %>%
  select(-月)

fiscal_lag_df <- fiscal_data %>%
  filter(年度 != "R6") %>%
  select("年度", "自治体名", "財政力指数", "経常収支比率") 

manage_lag_df <- manage_data %>%
  filter(年度 != "R6") %>%
  mutate(単独校親子_割合 = (単独校方式 + 親子方式)/完全給食提供校数,
         調理業務委託_割合 = 調理業務委託校数/完全給食提供校数) %>%
  select("年度", "自治体名", "単独校親子_割合", "調理業務委託_割合") 

meal_lag_df <- meal_lag_data %>%
  mutate(across(where(is.character), ~str_replace_all(., " ", ""))) %>%
  mutate(across(3:8, as.numeric)) %>%
  mutate(指導実施校割合 = 指導実施校数/完全給食実施校数,
         都内産食品使用割合 = 東京都内産/完全給食実施校数,
         有機農産物使用割合 = 有機農産物 / 完全給食実施校数,
         特別栽培農産物使用割合 = 特別栽培農産物 / 完全給食実施校数,
         特別農産物 = ifelse(有機農産物 + 特別栽培農産物 > 完全給食実施校数, 
                        完全給食実施校数, 有機農産物 + 特別栽培農産物),
         特別農産物使用校割合 = 特別農産物 / 完全給食実施校数) %>%
  select(-c(完全給食実施校数, 東京都内産, 指導実施校数, 
            有機農産物, 特別栽培農産物, 特別農産物))

CGPI_R2 <- cgpi_lag_df %>%
  filter(年度 == "R2") %>%
  pull(CGPI)

#データの結合
model_lag_df <- meal_lag_df %>%
  left_join(fiscal_lag_df, by = c("年度", "自治体名")) %>%
  left_join(manage_lag_df, by = c("年度", "自治体名")) %>%
  left_join(cgpi_lag_df, by = "年度") %>%
  left_join(student_lag_df, by = c("年度", "自治体名")) %>%
  rename(単独校方式割合 = 単独校親子_割合,
         調理業務委託校割合 = 調理業務委託_割合) %>%
  mutate(CGPI = CGPI/CGPI_R2,
         実質給食費 = 一食あたり単価 / CGPI)

#基本統計量の計算
model_lag_df %>%
  select(指導実施校割合, 特別農産物使用校割合, CGPI, 財政力指数, 一食あたり単価, 
         実質給食費, 単独校方式割合, 調理業務委託校割合, 平均児童数) %>%
  rename(名目給食費 = 一食あたり単価,
         企業物価指数 = CGPI) %>%
  datasummary(All(.) ~ N + Mean + SD + Min + Median + Max + Histogram, 
              data = .,
              fmt = 2,
              output = "table1_lag_260129.png")

tab_stat_lag <- model_lag_df %>%
  select(指導実施校割合, 特別農産物使用校割合, CGPI, 財政力指数, 一食あたり単価, 
         実質給食費, 単独校方式割合, 調理業務委託校割合, 平均児童数) %>%
  rename("名目給食費（円）" = 一食あたり単価,
         "実質給食費（円）" = 実質給食費,
         "企業物価指数" = CGPI) %>%
  datasummary(All(.) ~ N + Mean + SD + Min + Median + Max, # Word用はHistogramなし推奨
              data = ., fmt = 2, output = "flextable")

tab_stat_lag <- tab_stat_lag %>%
  font(fontname = "MS Mincho", part = "all") %>% 
  fontsize(size = 12, part = "all") %>%  
  width(j = 1, width = 2.0) %>% 
  width(j = 2:7, width = 0.7) %>% 
  align(j = 2:7, align = "center", part = "all")

save_as_docx(tab_stat_lag, path = "table_stat_lag_260129.docx")

#相関係数行列
model_lag_df %>%
  select(指導実施校割合, 特別農産物使用校割合, CGPI, 財政力指数, 一食あたり単価, 
         実質給食費, 単独校方式割合, 調理業務委託校割合, 平均児童数) %>%
  rename(名目給食費 = 一食あたり単価,
         企業物価指数 = CGPI) %>%
  datasummary_correlation(
    method = "pearson",
    fmt = 2,
    extra_args = list(lower_triangular = TRUE),
    output = "table2_lag_260129.png"
  )

#モデル1 ---------------------------------
#クロスセクションデータとして単回帰分析
reg1_lag <- lm(formula = 指導実施校割合 ~ 実質給食費 + 単独校方式割合 + 調理業務委託校割合 + 平均児童数,
               data = model_lag_df)
summary(reg1_lag)

reg5_lag <- lm(formula = 特別農産物使用校割合 ~ 実質給食費 + 単独校方式割合 + 調理業務委託校割合 + 平均児童数, 
               data = model_lag_df)
summary(reg5_lag)


#パネルデータで二方向固定効果分析
reg1_lag_two <- feols(fml = 指導実施校割合 ~ 実質給食費 + 単独校方式割合 + 調理業務委託校割合 + 平均児童数
                      | 自治体名 + 年度, 
                      data = model_lag_df)
summary(reg1_lag_two)

reg5_lag_two <- feols(fml = 特別農産物使用校割合 ~ 実質給食費 + 単独校方式割合 + 調理業務委託校割合 + 平均児童数
                      | 自治体名 + 年度, 
                      data = model_lag_df)
summary(reg5_lag_two)

#結果全体の表を作成
model1_lag_two <- list("エネルギー" = reg1_fe_two,
                       "タンパク質" = reg2_fe_two,
                       "使用食品数" = reg3_fe_two,
                       "指導実施校割合" = reg1_lag_two,
                       "特別農産物使用校割合" = reg5_lag_two)

# 固定効果の有無を判定
fe_muni_vec <- sapply(model1_lag_two, 
                      function(x) 
                        if("自治体名" %in% try(x$fixef_vars, silent=TRUE)) "Yes" 
                      else "No")

fe_year_vec <- sapply(model1_lag_two, 
                      function(x) 
                        if("年度" %in% try(x$fixef_vars, silent=TRUE)) "Yes" 
                      else "No")

rows <- data.frame(
  term = c("FE: 自治体名", "FE: 年度"),
  rbind(fe_muni_vec, fe_year_vec),
  row.names = NULL,
  stringsAsFactors = FALSE
)
colnames(rows) <- c("term", names(model1_lag_two))

msummary(model1_lag_two,
         output = "table.results1_fe_260130.png",
         add_rows = rows,
         stars = TRUE,
         gof_omit = "AIC|BIC|Log.Lik|Within.R2|FE")

#クロスセクションデータの結果まとめ(モデル1)
model1_cross <- list("エネルギー" = reg1,
                     "タンパク質" = reg2,
                     "使用食品数" = reg3,
                     "指導実施校割合" = reg1_lag,
                     "特別農産物使用校割合" = reg5_lag)

msummary(model1_cross,
         output = "table.results1_cross_260130.png",
         stars = TRUE,
         gof_omit = "AIC|BIC|Log.Lik|Within.R2") 


#モデル2 --------------------------
#クロスセクションデータとして単回帰分析
reg6_lag <- lm(formula = 指導実施校割合 ~ 財政力指数*CGPI + 単独校方式割合 + 調理業務委託校割合 + 平均児童数,
               data = model_lag_df)
summary(reg6_lag)

reg8_lag <- lm(formula = 特別農産物使用校割合 ~ 財政力指数*CGPI + 単独校方式割合 + 調理業務委託校割合 + 平均児童数,
               data = model_lag_df)
summary(reg8_lag)

#パネルデータで二方向固定効果分析
#財政化指数
reg6_lag_two <- feols(fml = 指導実施校割合 ~ 財政力指数*CGPI + 単独校方式割合 + 調理業務委託校割合 + 平均児童数
                      | 自治体名 + 年度, 
                      data = model_lag_df)
summary(reg6_lag_two)

reg8_lag_two <- feols(fml = 特別農産物使用校割合  ~ 財政力指数*CGPI + 単独校方式割合 + 調理業務委託校割合 + 平均児童数
                      | 自治体名 + 年度, 
                      data = model_lag_df)
summary(reg8_lag_two)


#結果の表を作成
#固定効果のみの表を作成
model2_lag_two <- list("エネルギー" = reg5_fe_two,
                       "タンパク質" = reg6_fe_two,
                       "使用食品数" = reg7_fe_two,
                       "指導実施校割合" = reg6_lag_two,
                       "特別農産物使用校割合" = reg8_lag_two)

# 固定効果の有無を判定
fe_muni_vec <- sapply(model2_lag_two, 
                      function(x) 
                        if("自治体名" %in% try(x$fixef_vars, silent=TRUE)) "Yes" 
                      else "No")

fe_year_vec <- sapply(model2_lag_two, 
                      function(x) 
                        if("年度" %in% try(x$fixef_vars, silent=TRUE)) "Yes" 
                      else "No")

rows <- data.frame(
  term = c("FE: 自治体名", "FE: 年度"),
  rbind(fe_muni_vec, fe_year_vec),
  row.names = NULL,
  stringsAsFactors = FALSE
)
colnames(rows) <- c("term", names(model2_lag_two))

cm <- c(
  "財政力指数" = "財政力指数",
  "財政力指数 × 企業価格指数" = "財政力指数*CGPI",  
  "単独校方式割合" = "単独校方式割合",
  "調理業務委託校割合" = "調理業務委託校割合",
  "平均児童数" = "平均児童数"
)

msummary(model2_lag_two,
         add_rows = rows,
         stars = TRUE,
         gof_omit = "AIC|BIC|Log.Lik|Within.R2|FE") 

msummary(model2_lag_two,
         output = "table.results2_fe_260130.png",
         add_rows = rows,
         stars = TRUE,
         gof_omit = "AIC|BIC|Log.Lik|Within.R2|FE")

#クロスセクションデータの結果まとめ(モデル2)
model2_cross <- list("エネルギー" = reg5,
                     "タンパク質" = reg6,
                     "使用食品数" = reg7,
                     "指導実施校割合" = reg6_lag,
                     "特別農産物使用校割合" = reg8_lag)

msummary(model2_cross,
         output = "table.results2_cross_260130.png",
         stars = TRUE,
         gof_omit = "AIC|BIC|Log.Lik|Within.R2") 
