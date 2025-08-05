# df_pop_raw を作成する完全なコード（最終修正版）
df_pop_raw <- readr::read_csv(
  here::here("assignment23_data", "raw", "population_raw.csv")
) |>
  separate(
    municipality_name,
    into = c("prefecture_name_copy", "municipality_name"),
    sep = "-"
  ) |>
  select(-prefecture_name_copy) |>
  mutate(year = str_remove(year, "year_")) |>
  mutate(year = as.numeric(year)) |>
  mutate(municipality_id = stringr::str_sub(municipality_id, start = 2, end = 6)) |>
  mutate(across(c(municipality_name), ~na_if(., ""))) |>
  mutate(across(c(municipality_name), ~na_if(., "\u001f"))) |>
  mutate(municipality_name = na_if(municipality_name, "NA")) |>
  tidyr::drop_na(municipality_name) |>
  # 数値に変換しない列のリストに 'municipality_id' を追加
  mutate(across(-any_of(c("prefecture_name", "municipality_name", "municipality_id")), as.numeric))

# Merge Data 以降のコード
adjust_df <- readxl::read_xlsx(
  here::here(
    "assignment23_data", "raw",
    "municipality_converter_jp.xlsx"
  )
) |>
  mutate(across(starts_with("id_"), as.character))

df_municipality_id_2020 <- adjust_df |>
  distinct(id_muni2020) |>
  rename(
    municipality_id = id_muni2020
  )

list_2020_id <- df_municipality_id_2020 |>
  pull(municipality_id)

# 【修正案】より頑健なdf_name_idの作成コード
df_name_id <- df_pop_raw |>
  # municipality_idでグループ化
  group_by(municipality_id) |>
  # 各IDの中で、最も新しい年(最新)のデータを1行だけ抽出
  filter(year == max(year)) |>
  # グループ化を解除
  ungroup() |>
  # 同一年で重複がある場合に備え、IDごとに最初の1行だけを残す
  distinct(municipality_id, .keep_all = TRUE) |>
  # 必要な列だけを選択
  select(municipality_id, municipality_name, prefecture_name)

adjust_municipality_id <- function(id_n, df_pop_raw, adjust_df) {
  list_id <- adjust_df |>
    filter(id_muni2020 == id_n) |>
    select(starts_with("id_"), -id_muni2020) |>
    pivot_longer(
      cols = everything(),
      names_to = NULL,
      values_to = "id"
    ) |>
    bind_rows(tibble(id = id_n)) |>
    distinct(id) |>
    drop_na() |>
    pull()
  
  output_data <- df_pop_raw |>
    filter(municipality_id %in% list_id) |>
    reframe(
      across(
        c(male, female, total, household, moving_in, birth, increase_total, 
          moving_out, mortality, decrease_total, change, natural, social),
        ~sum(., na.rm = TRUE)
      ),
      .by = year
    ) |>
    mutate(
      municipality_id = id_n,
      .before = 1
    )
  
  return(output_data)
}

df_pop <- purrr::map(list_2020_id, adjust_municipality_id, df_pop_raw, adjust_df) |>
  bind_rows()

df_population_adjusted <- df_pop |>
  left_join(df_name_id, by = "municipality_id") |>
  select(
    municipality_id, municipality_name, prefecture_name, year, everything()
  )
#秒数29秒