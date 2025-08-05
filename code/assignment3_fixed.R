# パッケージの読み込み
library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(tidyr)
library(purrr)
library(here)

# 定数定義
NON_NUMERIC_COLS <- c("prefecture_name", "municipality_name", "municipality_id")
POPULATION_COLS <- c("male", "female", "total", "household", "moving_in", 
                     "birth", "increase_total", "moving_out", "mortality", 
                     "decrease_total", "change", "natural", "social")

# 生データの読み込みと前処理
load_population_data <- function() {
  read_csv(here("assignment23_data", "raw", "population_raw.csv"), 
           show_col_types = FALSE) |>
    separate(municipality_name,
             into = c("prefecture_name_copy", "municipality_name"),
             sep = "-") |>
    select(-prefecture_name_copy) |>
    mutate(
      year = as.numeric(str_remove(year, "year_")),
      municipality_id = str_sub(municipality_id, start = 2, end = 6)
    ) |>
    mutate(across(c(municipality_name), ~na_if(., ""))) |>
    mutate(across(c(municipality_name), ~na_if(., "\u001f"))) |>
    mutate(municipality_name = na_if(municipality_name, "NA")) |>
    drop_na(municipality_name) |>
    mutate(across(-any_of(NON_NUMERIC_COLS), as.numeric))
}

# 市町村変換データの読み込み
load_municipality_converter <- function() {
  read_xlsx(here("assignment23_data", "raw", "municipality_converter_jp.xlsx")) |>
    mutate(across(starts_with("id_"), as.character))
}

# 2020年基準市町村IDリストの取得
get_2020_ids <- function(adjust_df) {
  adjust_df |>
    distinct(id_muni2020) |>
    pull(id_muni2020)
}

# 市町村名-ID対応表の作成
create_name_mapping <- function(df_pop_raw) {
  df_pop_raw |>
    group_by(municipality_id) |>
    filter(year == max(year)) |>
    ungroup() |>
    distinct(municipality_id, .keep_all = TRUE) |>
    select(municipality_id, municipality_name, prefecture_name)
}

# 対象市町村IDの取得
get_target_ids <- function(id_2020, adjust_df) {
  adjust_df |>
    filter(id_muni2020 == id_2020) |>
    select(starts_with("id_"), -id_muni2020) |>
    pivot_longer(cols = everything(), names_to = NULL, values_to = "id") |>
    bind_rows(tibble(id = id_2020)) |>
    distinct(id) |>
    drop_na() |>
    pull(id)
}

# 市町村データの統合
aggregate_municipality_data <- function(id_2020, df_pop_raw, adjust_df) {
  target_ids <- get_target_ids(id_2020, adjust_df)
  
  df_pop_raw |>
    filter(municipality_id %in% target_ids) |>
    group_by(year) |>
    summarise(across(all_of(POPULATION_COLS), ~sum(., na.rm = TRUE)),
              .groups = "drop") |>
    mutate(municipality_id = id_2020, .before = 1)
}

# メイン処理関数
process_population_data <- function() {
  # データ読み込み
  df_pop_raw <- load_population_data()
  adjust_df <- load_municipality_converter()
  list_2020_id <- get_2020_ids(adjust_df)
  df_name_id <- create_name_mapping(df_pop_raw)
  
  # データ統合
  df_pop <- map(list_2020_id, aggregate_municipality_data, 
                df_pop_raw, adjust_df) |>
    bind_rows()
  
  # 最終結果
  df_pop |>
    left_join(df_name_id, by = "municipality_id") |>
    select(municipality_id, municipality_name, prefecture_name, year, everything())
}

# 実行
df_population_adjusted_1 <- process_population_data()

#秒数22秒（-7秒）,NAが一部残ったまま
#10時間経過のため一旦ここまで