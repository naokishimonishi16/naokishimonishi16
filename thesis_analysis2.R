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
meal_data <- read.csv("Meal_panel_251215.csv", header = TRUE)
cgpi_data <- read.csv("CGPI_panel_260129.csv", header = TRUE)
fiscal_data <- read.csv("Fiscal_panel_260106.csv", header = TRUE)
manage_data <- read.csv("Management_panel_260112.csv", header = TRUE)
student_data <- read.csv("Student_panel_260130.csv", header = TRUE)

#データの整理
student_df <- student_data %>%
  filter(年度 != "R2") %>%
  mutate(平均児童数 = 総児童数/学校数) %>%
  select(年度, 自治体名, 平均児童数)

cgpi_df <- cgpi_data %>%
  rename(年度 = 年) %>%
  filter(年度 != "R2",
         月 == 5) %>%
  select(-月)

fiscal_df <- fiscal_data %>%
  filter(年度 != "R2") %>%
  select("年度", "自治体名", "財政力指数", "経常収支比率") 

manage_df <- manage_data %>%
  filter(年度 != "R2") %>%
  mutate(単独校親子_割合 = (単独校方式 + 親子方式)/完全給食提供校数,
         調理業務委託_割合 = 調理業務委託校数/完全給食提供校数) %>%
  select("年度", "自治体名", "単独校親子_割合", "調理業務委託_割合") 

meal_df <- meal_data %>%
  mutate(across(where(is.character), ~str_replace_all(., " ", ""))) %>%
  mutate(across(3:32, as.numeric)) %>%
  mutate(パン = パン小麦 + パンイースト + パン食塩 + パンショートニング + パン砂糖類 + パン脱脂粉乳) %>%
  select(-c(パン小麦, パンイースト, パン食塩, パンショートニング, パン砂糖類, パン脱脂粉乳)) %>%
  relocate(パン, .after = 米) 

CGPI_R3 <- cgpi_df %>%
  filter(年度 == "R3") %>%
  pull(CGPI)

#データの結合
model_df <- meal_df %>%
  left_join(fiscal_df, by = c("年度", "自治体名")) %>%
  left_join(manage_df, by = c("年度", "自治体名")) %>%
  left_join(cgpi_df, by = "年度") %>%
  left_join(student_df, by = c("年度", "自治体名")) %>%
  rename(単独校方式割合 = 単独校親子_割合,
         調理業務委託校割合 = 調理業務委託_割合) %>%
  mutate(CGPI = CGPI/CGPI_R3,
         実質給食費 = 一食あたり単価 / CGPI)


#基本統計量の計算
model_df %>%
  select(エネルギー, タンパク質, 使用食品数, CGPI, 財政力指数, 一食あたり単価, 
         実質給食費, 単独校方式割合, 調理業務委託校割合, 平均児童数) %>%
  rename("エネルギー（kcal）" = エネルギー,
         "タンパク質（g）" = タンパク質,
         "名目給食費（円）" = 一食あたり単価,
         "実質給食費（円）" = 実質給食費,
         企業物価指数 = CGPI) %>%
  datasummary(All(.) ~ N + Mean + SD + Min + Median + Max + Histogram, 
              data = .,
              fmt = 2,
              output = "table1_260129.png")

tab_stat <- model_df %>%
  select(エネルギー, タンパク質, 使用食品数, CGPI, 財政力指数, 
         一食あたり単価, 実質給食費, 単独校方式割合, 調理業務委託校割合, 平均児童数) %>%
  rename("エネルギー（kcal）" = エネルギー,
         "タンパク質（g）" = タンパク質,
         "名目給食費（円）" = 一食あたり単価,
         "実質給食費（円）" = 実質給食費,
         "企業物価指数" = CGPI) %>%
  datasummary(All(.) ~ N + Mean + SD + Min + Median + Max, 
              data = ., fmt = 2, output = "flextable")

tab_stat <- tab_stat %>%
  font(fontname = "MS Mincho", part = "all") %>% 
  fontsize(size = 12, part = "all") %>%  
  width(j = 1, width = 2.0) %>% 
  width(j = 2:7, width = 0.7) %>% 
  align(j = 2:7, align = "center", part = "all")

save_as_docx(tab_stat, path = "table_stat_260129.docx")


#相関係数の計算
model_df %>%
  select(エネルギー, タンパク質, 使用食品数, CGPI, 財政力指数, 
         一食あたり単価, 実質給食費, 単独校方式割合, 調理業務委託校割合, 平均児童数) %>%
  rename(名目給食費 = 一食あたり単価,
         企業物価指数 = CGPI) %>%
  datasummary_correlation(
    method = "pearson",
    fmt = 2,
    extra_args = list(lower_triangular = TRUE),
    output = "table2_260129.png"
  )

#モデル1 ---------------------------------
#クロスセクションデータとして単回帰分析
reg1 <- lm(formula = エネルギー ~ 実質給食費 + 単独校方式割合 + 調理業務委託校割合 + 平均児童数, 
           data = model_df)
summary(reg1)

reg2 <- lm(formula = タンパク質 ~ 実質給食費 + 単独校方式割合 + 調理業務委託校割合 + 平均児童数, 
           data = model_df)
summary(reg2)

reg3 <- lm(formula = 使用食品数 ~ 実質給食費 + 単独校方式割合 + 調理業務委託校割合 + 平均児童数, 
           data = model_df)
summary(reg3)

reg4 <- lm(formula = 米飯回数 ~ 実質給食費 + 単独校方式割合 + 調理業務委託校割合 + 平均児童数, 
           data = model_df)
summary(reg4)

#パネルデータで二方向固定効果分析
reg1_fe_two <- feols(fml = エネルギー ~ 実質給食費 + 単独校方式割合 + 調理業務委託校割合 + 平均児童数 
                     | 自治体名 + 年度, 
                     data = model_df)
summary(reg1_fe_two)

reg2_fe_two <- feols(fml = タンパク質 ~ 実質給食費 + 単独校方式割合 + 調理業務委託校割合 + 平均児童数 
                     | 自治体名 + 年度, 
                     data = model_df)
summary(reg2_fe_two)

reg3_fe_two <- feols(fml = 使用食品数 ~ 実質給食費 + 単独校方式割合 + 調理業務委託校割合 + 平均児童数 
                     | 自治体名 + 年度, 
                     data = model_df)
summary(reg3_fe_two)

reg4_fe_two <- feols(fml = 米飯回数 ~ 実質給食費 + 単独校方式割合 + 調理業務委託校割合 + 平均児童数
                     | 自治体名 + 年度, 
                     data = model_df)
summary(reg4_fe_two)

#固定効果のみ
model1_fe_two <- list("エネルギー" = reg1_fe_two,
                      "タンパク質" = reg2_fe_two,
                      "使用食品数" = reg3_fe_two)

msummary(model1_fe_two,
         output = "table.results_fe.png",
         stars = TRUE,      # 有意確率の星をつける
         gof_omit = "AIC|BIC|Log.Lik|Within.R2") # 不要な統計量を削る

#モデル1すべて
model1 <- list("エネルギー" = reg1,
               "エネルギー(FE)" = reg1_fe_two,
               "タンパク質" = reg2,
               "タンパク質(FE)" = reg2_fe_two,
               "使用食品数" = reg3,
               "使用食品数(FE)" = reg3_fe_two)

msummary(model1,
         output = "table.results_260130.png",
         stars = TRUE,      # 有意確率の星をつける
         gof_omit = "AIC|BIC|Log.Lik|Within.R2") # 不要な統計量を削る



#モデル2 --------------------------
#クロスセクションデータで単回帰分析
reg5 <- lm(formula = エネルギー ~ 財政力指数*CGPI + 単独校方式割合 + 調理業務委託校割合 + 平均児童数,
           data = model_df)
summary(reg5)

reg6 <- lm(formula = タンパク質 ~ 財政力指数*CGPI + 単独校方式割合 + 調理業務委託校割合 + 平均児童数, 
           data = model_df)
summary(reg6)

reg7 <- lm(formula = 使用食品数 ~ 財政力指数*CGPI + 単独校方式割合 + 調理業務委託校割合 + 平均児童数, 
           data = model_df)
summary(reg7)

reg8 <- lm(formula = 米飯回数 ~ 財政力指数*CGPI + 単独校方式割合 + 調理業務委託校割合 + 平均児童数,
           data = model_df)
summary(reg8)

#パネルデータで二方向固定効果分析
#財政化指数
reg5_fe_two <- feols(fml = エネルギー ~ 財政力指数*CGPI + 単独校方式割合 + 調理業務委託校割合 + 平均児童数 
                     | 自治体名 + 年度, 
                     data = model_df)
summary(reg5_fe_two)

reg6_fe_two <- feols(fml = タンパク質 ~ 財政力指数*CGPI + 単独校方式割合 + 調理業務委託校割合 + 平均児童数
                     | 自治体名 + 年度, 
                     data = model_df)
summary(reg6_fe_two)

reg7_fe_two <- feols(fml = 使用食品数 ~ 財政力指数*CGPI + 単独校方式割合 + 調理業務委託校割合 + 平均児童数
                     | 自治体名 + 年度, 
                     data = model_df)
summary(reg7_fe_two)

reg8_fe_two <- feols(fml = 米飯回数 ~ 財政力指数*CGPI + 単独校方式割合 + 調理業務委託校割合 + 平均児童数 
                     | 自治体名 + 年度, 
                     data = model_df)
summary(reg8_fe_two)

#経常収支比率
reg9_fe_two <- feols(fml = エネルギー ~ 経常収支比率*CGPI + 単独校方式割合 + 調理業務委託校割合 + 平均児童数
                     | 自治体名 + 年度, 
                     data = model_df)
summary(reg9_fe_two)

reg10_fe_two <- feols(fml = タンパク質 ~ 経常収支比率*CGPI + 単独校方式割合 + 調理業務委託校割合 + 平均児童数
                      | 自治体名 + 年度, 
                      data = model_df)
summary(reg10_fe_two)

reg11_fe_two <- feols(fml = 使用食品数 ~ 経常収支比率*CGPI + 単独校方式割合 + 調理業務委託校割合 + 平均児童数
                      | 自治体名 + 年度, 
                      data = model_df)
summary(reg11_fe_two)

reg12_fe_two <- feols(fml = 米飯回数 ~ 経常収支比率*CGPI + 単独校方式割合 + 調理業務委託校割合 + 平均児童数
                      | 自治体名 + 年度, 
                      data = model_df)
summary(reg12_fe_two)
