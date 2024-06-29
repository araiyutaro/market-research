install.packages(c("cluster", "cluster.datasets", "factoextra","ggrepel", "useful"))
library(cluster)
library(factoextra)
library(ggrepel)
library(useful)
library(tidyverse)
data <- readxl::read_xlsx("data/回答データ【消費者調査2024年度上期調査】.xlsx", sheet = "回答データ【共通調査2024年度上期】",na = " ")

# ryosuke1 
# Q36 自己肯定、幸福度。5とてもそう思う〜1まったくそう思わない
# Q37 他国についての興味
# Q38 価値観
# Q75 どのような働き方がしたいか


# ryosuke2
# Q62 あなたと周りの方の人間関係の充実度
# Q63 
# Q76 


# motoki
# Q7 個人年収 世帯年収
# Q20 広告についての考え方

motoki <- data %>% 
  select(q7_1, q20_1,q20_2,q20_3,q20_4,q20_5) %>% 
  filter(
    q7_1 != 10,
    q7_1 != 11,
    q7_1 != 999,
    q20_1 != 999,
    q20_2 != 999,
    q20_3 != 999,
    q20_4 != 999,
    q20_5 != 999,
  )
motoki$q20_1 <-  6 - motoki$q20_1
motoki$q20_2 <-  6 - motoki$q20_2
motoki$q20_4 <-  6 - motoki$q20_4
motoki$q20_5 <-  6 - motoki$q20_5

motoki <- motoki %>% 
  mutate(
    q20 = (q20_1 + q20_2 + q20_3 + q20_4 + q20_5) / 5
  ) %>% 
  mutate(
    AnualIncome = case_when(
      q7_1 == 1 ~ "120万円未満",
      q7_1 == 2 ~ "120〜240万円未満",
      q7_1 == 3 ~ "240〜360万円未満",
      q7_1 == 4 ~ "360〜480万円未満",
      q7_1 == 5 ~ "480〜600万円未満",
      q7_1 == 6 ~ "600〜720万円未満",
      q7_1 == 7 ~ "720〜840万円未満",
      q7_1 == 8 ~ "840〜1200万円未満",
      q7_1 == 9 ~ "1200万円以上",
      TRUE ~ "OTHERS"
    ),
    AnualIncome2 = case_when(
      q7_1 == 1 ~ "1.-120",
      q7_1 == 2 ~ "2.120-240",
      q7_1 == 3 ~ "3.240-360",
      q7_1 == 4 ~ "4.360-480",
      q7_1 == 5 ~ "5.480-600",
      q7_1 == 6 ~ "6.600-720",
      q7_1 == 7 ~ "7.720-840",
      q7_1 == 8 ~ "8.840-1200",
      q7_1 == 9 ~ "9.1200-",
      TRUE ~ "OTHERS"
    )
  )

p <- ggplot(motoki, mapping = aes(x = AnualIncome2, y = q20))  

p + geom_boxplot() 

motoki <- motoki %>% 
  mutate (AnualIncome3 = case_when(
    q7_1 == 1 ~ 60,
    q7_1 == 2 ~ 180,
    q7_1 == 3 ~ 300,
    q7_1 == 4 ~ 420,
    q7_1 == 5 ~ 540,
    q7_1 == 6 ~ 660,
    q7_1 == 7 ~ 780,
    q7_1 == 8 ~ 1020,
    q7_1 == 9 ~ 0,
    TRUE ~ 0
    )
  ) %>% 
  filter(
    AnualIncome3 != 0
  )

p3 <- ggplot(motoki, mapping = aes(x = AnualIncome3, y = q20))
p3 + geom_point()
p3

reg <- lm(q7_1 ~ q20, data = motoki)
coef(reg)
reg
summary(reg)

# シェアバイクの相関
bike_raw_data <- readr::read_csv("data/day.csv")
bike_data <- bike_raw_data %>% 
  mutate(isRaining = case_when(
    weathersit == 0 ~ 0,
    weathersit == 1 ~ 0,
    weathersit == 2 ~ 1,
    weathersit == 3 ~ 1
  ))

bike_result <- lm(cnt ~ temp + atemp + hum + windspeed + isRaining, data = bike_data)
summary(bike_result)

# ryousuke3
# 消費者のイノベーター度合い(Q16_1〜Q16_5)との関係について
# 説明変数:居住地、年収、職業、ChatGPTを使ったことがあるか、Tiktokを使ったことがあるか

# 人口データ 
# 総務省の令和2年国税調査より取得（https://www.e-stat.go.jp/stat-search/files?page=1&layout=datalist&toukei=00200521&tstat=000001136464&cycle=0&tclass1=000001136466&stat_infid=000032142402&result_page=1&tclass2val=0）
population_data <- readxl::read_xlsx("data/b01_01.xlsx", sheet = "b01_01",na = " ")

# 人口データから余計なデータをフィルタリング、都道府県コードの列を追加
population_data <- population_data %>% 
  select(code, region, poplation) %>% 
  filter(code == "a") %>% 
  mutate(q3 = case_when(
    grepl("00", region) ~ 0,
    grepl("01", region) ~ 1,
    grepl("02", region) ~ 2,
    grepl("03", region) ~ 3,
    grepl("04", region) ~ 4,
    grepl("05", region) ~ 5,
    grepl("06", region) ~ 6,
    grepl("07", region) ~ 7,
    grepl("08", region) ~ 8,
    grepl("09", region) ~ 9,
    grepl("10", region) ~ 10,
    grepl("11", region) ~ 11,
    grepl("12", region) ~ 12,
    grepl("13", region) ~ 13,
    grepl("14", region) ~ 14,
    grepl("15", region) ~ 15,
    grepl("16", region) ~ 16,
    grepl("17", region) ~ 17,
    grepl("18", region) ~ 18,
    grepl("19", region) ~ 19,
    grepl("20", region) ~ 20,
    grepl("21", region) ~ 21,
    grepl("22", region) ~ 22,
    grepl("23", region) ~ 23,
    grepl("24", region) ~ 24,
    grepl("25", region) ~ 25,
    grepl("26", region) ~ 26,
    grepl("27", region) ~ 27,
    grepl("28", region) ~ 28,
    grepl("29", region) ~ 29,
    grepl("30", region) ~ 30,
    grepl("31", region) ~ 31,
    grepl("32", region) ~ 32,
    grepl("33", region) ~ 33,
    grepl("34", region) ~ 34,
    grepl("35", region) ~ 35,
    grepl("36", region) ~ 36,
    grepl("37", region) ~ 37,
    grepl("38", region) ~ 38,
    grepl("39", region) ~ 39,
    grepl("40", region) ~ 40,
    grepl("41", region) ~ 41,
    grepl("42", region) ~ 42,
    grepl("43", region) ~ 43,
    grepl("44", region) ~ 44,
    grepl("45", region) ~ 45,
    grepl("46", region) ~ 46,
    grepl("47", region) ~ 47,
  ))

# ryosukeに調査に使うデータを整理して代入
ryosuke <- data %>% 
  # 吉田データから必要なものだけを抽出
  select(q2t,q3,q4,q5,q7_1,q7_2,
         q16_1,q16_2,q16_3,q16_4,q16_5
         ,q24_1.14,q24_2.14,q24_3.14,q24_4.14,q24_5.14,q24_6.14,q24_7.14
         ,q24_1.24,q24_2.24,q24_3.24,q24_4.24,q24_5.24,q24_6.24,q24_7.24
         ,q26_1.01,q26_2.01,q26_3.01,q26_4.01
         ,q26_1.03,q26_2.03,q26_3.03,q26_4.03
         ,q26_1.12,q26_2.12,q26_3.12,q26_4.12
         ,q26_1.14,q26_2.14,q26_3.14,q26_4.14
         ,q26_1.22,q26_2.22,q26_3.22,q26_4.22
  ) %>% 
  # 未回答や使わないデータを排除
  filter(
    q3 != 99,
    q3 != 999,
    q4 != 999,
    q7_1 != 10,
    q7_1 != 11,
    q7_1 != 999,
    q16_1 != 999,
    q16_2 != 999,
    q16_3 != 999,
    q16_4 != 999,
    q16_5 != 999,
  ) %>% 
  # 人口データと合体
  left_join(population_data, by = "q3") %>% 
  # データを再構築
  reframe(
    # イノベーター度合い（q16_4以外は逆転項目なので6から引いてから使用、5項目の平均値を採用）
    inovator = ((6-q16_1) +(6-q16_2) +(6-q16_3) +q16_4 +(6-q16_5)) / 5,
    age = q2t,
    married = case_when(
      q4 == 1 ~ 1,
      TRUE ~ 0
    ),
    
    # 会社員かどうか、Q5の回答が1,2,3,4なら1,それ以外なら0とする
    work = case_when(
      q5 == 1 ~ 1,
      q5 == 2 ~ 1,
      q5 == 3 ~ 1,
      q5 == 4 ~ 1,
      TRUE ~ 0
    ),
    # 人口
    population = poplation,
    tokyo = case_when(
      q3 == 13 ~ 1,
      TRUE ~ 0
    ),
    
    tv = case_when(
      (q26_1.01 == 1 | q26_2.01 == 1 | q26_3.01 == 1| q26_4.01 == 1) ~ 1,
      TRUE ~ 0
    ),
    newspaper = case_when(
      (q26_1.03 == 1 | q26_2.03 == 1 | q26_3.03 == 1| q26_4.03 == 1) ~ 1,
      TRUE ~ 0
    ),
    kuchikomi = case_when(
      (q26_1.12 == 1 | q26_2.12 == 1 | q26_3.12 == 1| q26_4.12 == 1) ~ 1,
      TRUE ~ 0
    ),
    community_site = case_when(
      (q26_1.14 == 1 | q26_2.14 == 1 | q26_3.14 == 1| q26_4.14 == 1) ~ 1,
      TRUE ~ 0
    ),
    twitter = case_when(
      (q26_1.22 == 1 | q26_2.22 == 1 | q26_3.22 == 1| q26_4.22 == 1) ~ 1,
      TRUE ~ 0
    ),
    
    # 個人年収
    personal_income = q7_1,
    # 世帯年収(回答が少なかったので今回は未使用)
    household_income = q7_2,
    # ChatGPTを使ったことがあるかどうか（Q24_5.14 or Q24_6.14 or Q24_7.14が1なら1,それ以外は0）
    use_gpt = case_when(
      (q24_5.14 == 1 | q24_6.14 == 1 | q24_7.14 == 1) ~ 1,
      TRUE ~ 0
    ),
    # Tiktokを使ったことがあるかどうか（Q24_5.24 or Q24_6.24 or Q24_7.24が1なら1,それ以外は0）
    use_tiktok = case_when(
      (q24_5.24 == 1 | q24_6.24 == 1 | q24_7.24 == 1) ~ 1,
      TRUE ~ 0
    )
  )

# 重回帰分析実行
ryosuke_result <- lm(inovator ~ personal_income + married + tokyo + work + use_gpt + use_tiktok + tv + newspaper + kuchikomi + community_site + twitter + age, data = ryosuke)
# 結果表示
summary(ryosuke_result)
