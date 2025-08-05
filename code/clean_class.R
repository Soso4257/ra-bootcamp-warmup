# 必要なライブラリを読み込む
library(tidyverse)
library(here)
# 保存したRDSファイルを読み込む
list_class_data <- readRDS(here("data", "original", "class_data_eng.rds"))
# リスト内の各データフレームから "total" 列を削除する
list_no_total <- map(list_class_data, function(df) {
  df %>%
    select(-total)
})

df_panel_sorted <- list_no_total %>%
#数値に全部直す
  map(~ .x %>% mutate(across(-prefecture, as.numeric))) %>%
  
  # 1. リスト内の各DFに、1から47までの都道府県IDを振る
  map(~ .x %>% mutate(pref_id = 1:n())) %>%
  # 2. リストを一つのDFにまとめ、year列を追加する
  bind_rows(.id = "year") %>%
  # 3. 都道府県IDと年度で並び替える
  arrange(pref_id, as.numeric(year))

df_long <- df_panel_sorted %>%
  pivot_longer(
    cols = -c(year, prefecture, pref_id),
    names_to = "class_size",
    values_to = "n_schools"
  )

#完成版コードの再確認
df_completed <- readRDS("data/raw/assignment1_data/課題2_完成版データ.rds")

#学級数×学校数を「平均値考慮列」を作って計算
df_calculated <- df_long %>%
  mutate(
    # class_sizeの特殊なルールに基づいて計算用の数値を作成
    class_size_numeric = case_when(
      class_size == "25" ~ 27.5,  # 25～30 の平均値
      class_size == "31" ~ 33.5,  # 31～36 の平均値
      class_size == "37" ~ 39.5,  # 37～42 の平均値
      class_size == "43" ~ 45.5,  # 43～48 の平均値
      class_size == "49" ~ 51.5,  # 49～54 の平均値
      class_size == "55" ~ 57.5,  # 55～60 の平均値
      # 上記の特殊なケース以外は、class_sizeの値をそのまま数値として使用
      TRUE ~ as.numeric(class_size)
    ),
    # 「学級数 × 学校数」を計算
    total_classes = class_size_numeric * as.numeric(n_schools)
  )

# 1. 年度、都道府県ID、都道府県名でグループ化する
df_final_class <- df_calculated %>%
  group_by(year, pref_id, prefecture) %>%
  # 2. グループごとに合計学級数を計算
  summarise(
    total_class_num = sum(total_classes, na.rm = TRUE),
    .groups = "drop" # グループ化を解除
  ) %>%
  # 3. 【重要】都道府県IDと年度で並び替える
  arrange(pref_id, year)

# 1. 年度、都道府県ID、都道府県名でグループ化する
df_final_class <- df_calculated %>%
  group_by(year, pref_id, prefecture) %>%
  # 2. グループごとに合計学級数を計算
  summarise(
    total_class_num = sum(total_classes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # 3. 都道府県IDと年度で並び替える
  arrange(pref_id, year) %>%
  # 4. 列を選択して順番を整える
  select(prefecture, year, total_class_num)

# 5. 完成したデータをdata/cleanedフォルダに保存
saveRDS(df_final_class, file = here("data", "cleaned", "cleaned_class_data.rds"))

#多分、どこか間違ってる。
