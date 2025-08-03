# 必要なライブラリを読み込む
library(tidyverse)
library(here)

# 保存したRDSファイルを読み込む
df_students <- readRDS(here("data", "original", "students_renamed.rds"))
list_absence <- readRDS(here("data", "original", "absence_list_renamed.rds"))
# list_absence の各データフレームをクリーニングする
list_absence_cleaned <- map(list_absence, function(df) {
  df |>
    # 必要な列だけを選択（ "blank" 列を削除）
    select(prefecture, n_truants) |>
    # n_truants列を数値型に変換する
    mutate(n_truants = as.numeric(n_truants))
})
#年度がわかるように名前づけ
names(list_absence_cleaned) <- 2013:2022
#名前を列名にして結合
df_absence <- bind_rows(list_absence_cleaned, .id = "year")
# df_absenceのyear列を数値型に
df_absence <- df_absence |>
  mutate(year = as.numeric(year))
# 生徒数を左基準にして結合
df_merged <- left_join(df_students, df_absence, by = c("year", "prefecture"))
#不登校者数割合を追加
df_final <- df_merged %>%
  mutate(truancy_rate = (n_truants / n_students) * 100)
#クリーニングしたデータを保存
saveRDS(df_final, file = here("data", "cleaned", "cleaned_absence_data.rds"))
