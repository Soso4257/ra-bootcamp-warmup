# --- Problem 1 ---

# 必要なライブラリを読み込む
library(tidyverse)

# サンプルデータを作成
df <- tibble(
  col1 = sample(c(NA, 1), size = 10, replace = TRUE)
)

# 元のデータを確認
print("元のデータ:")
print(df)

#修正前コード
df <- df %>%
  mutate(
    is_na = if_else(col1 == NA, TRUE, FALSE)
  )
print(df)

#『問題点』：is_na列がすべてNAになってしまうこと。
#=========コードの修正==========
df_fixed <- df %>%
  mutate(
    is_na = is.na(col1)
  )
print(df_fixed)
# is_na列（TRUE/FALSE）を 1/0 のダミー変数に変換する
df_dummy <- df_fixed %>%
  mutate(
    is_na_dummy = as.numeric(is_na)
  )

# 最終結果
print(df_dummy)

# --- Problem 2 ---
#修正前コード
df_population <- readr::read_csv(here::here("data", "raw", "assignment23_data", "raw", "population_ps2.csv"))
list_vars <- dplyr::select_vars(vars = names(df_population), contains("name"))

df_population %>%
  select(list_vars)

#『問題点』："now defunct."であり、現在のバージョンでは動作しないこと。
#=========コードの修正==========
df_population <- readr::read_csv(here::here("data", "raw", "assignment23_data", "raw", "population_ps2.csv"))
# select()の中で、直接 contains() を使って列を選択する
df_population_fixed <- df_population %>%
  select(contains("name"))
# 最終結果
print(df_population_fixed)

# --- Problem 3 ---
#修正前コード
library(tidyverse)
library(here)
df_population <- readr::read_csv(here::here("data", "raw", "assignment23_data", "raw", "population_ps2.csv"))
df_lm <- df_population |>
  arrange(city_name, year) |>
  mutate(
    change_rate = (log(population) - dplyr::lag(log(population), n = 20)),
    .by = city_id
  ) |>
  dplyr::filter(year == 2015)

lm(log(change_rate) ~ log(population), data = df_lm)

#『問題点』：log(change_late)はlogのlogを取っている？n = 20だと20年前との比較になっている？
#=========コードの修正==========
# 人口の変化率を計算し、2015年のデータに絞る
df_lm_fixed <- df_population %>%
  arrange(city_id, year) %>%
  mutate(
    change_rate = (log(population) - dplyr::lag(log(population), n = 1)),
    .by = city_id
  ) %>%
  ungroup() %>%
  dplyr::filter(year == 2015) %>%
  # NAの行を削除
  drop_na()

lm(change_rate ~ log(population), data = df_lm_fixed)

# --- Problem 4 ---
#修正前コード
set.seed(111)
df <- tibble(
  col_1 = seq(1, 10),
  col_2 = seq(11, 20) + rnorm(n = 10, 0, 1),
  col_3 = seq(30, 21) + rnorm(n = 10, 0, 1),
)

plot_col_12 <- ggplot(df, aes(x = col_1, y = col_2)) +
  geom_point() +
  theme_minimal()

plot_col_13 <- ggplot(df, aes(x = col_1, y = col_3)) +
  geom_point() +
  theme_minimal()

plot_col_23 <- ggplot(df, aes(x = col_2, y = col_3)) +
  geom_point() +
  theme_minimal()
#『問題点』：同じggplot関数なのに、修正する際には3箇所それぞれを変えなければならず、面倒
#=========コードの修正==========
# サンプルデータを作成
set.seed(111)
df <- tibble(
  col_1 = seq(1, 10),
  col_2 = seq(11, 20) + rnorm(n = 10, 0, 1),
  col_3 = seq(30, 21) + rnorm(n = 10, 0, 1)
)

# 1. 再利用可能なグラフ作成関数を定義する
create_scatter_plot <- function(data, x_var, y_var) {
  ggplot(data, aes(x = {{x_var}}, y = {{y_var}})) +
    geom_point() +
    theme_minimal()
}

# 2. 作成した関数を使って、3つのプロットを生成する
plot_col_12 <- create_scatter_plot(df, col_1, col_2)
plot_col_13 <- create_scatter_plot(df, col_1, col_3)
plot_col_23 <- create_scatter_plot(df, col_2, col_3)

# 3. 最初のプロットを表示して確認
print(plot_col_12)

# --- Problem 5 ---
#修正前コード
df <- tibble(
  a = seq(1, 10)
)

add_column <- function(df, is_name) {
  df <- df |>
    mutate(
      is_name = if_else(a > 5, TRUE, FALSE)
    )
  
  return(df)
}

add_column(df, "higher") |>
  dplyr::filter(higher == TRUE)
#『問題点』：よくわからなかった
#=========コードの修正==========
# サンプルデータを作成
df <- tibble(
  a = seq(1, 10)
)

# 1. 修正版の関数を定義
add_column <- function(df, new_col_name) {
  df %>%
    mutate(
      # {{}} と := を使って、動的に列名を指定する
      {{new_col_name}} := if_else(a > 5, TRUE, FALSE)
    )
}

# 2. 修正版の関数を呼び出す
add_column(df, higher) %>%
  dplyr::filter(higher == TRUE)

# --- Problem 6 ---
#修正前コード
set.seed(111)
df <- tibble(
  y = seq(101, 200),
  x_1 = seq(1, 100) + rnorm(n = 100, 0, 1),
  x_2 = sample(c(0,1), 100, replace = TRUE),
  x_3 = seq(50, 149) + rnorm(n = 100, 0, 1)
)

linear_models <- function() {
  
  model_1 <- lm_robust(y ~ x_1)
  model_2 <- lm_robust(y ~ x_1 + x_2)
  model_3 <- lm_robust(y ~ x_2)
  model_4 <- lm_robust(y ~ x_1 + x_2 + x_3)
  
  list_output <- list(
    model_1 = model_1,
    model_2 = model_2,
    model_3 = model_3,
    model_4 = model_4
  )
  
  return(list_output)
}
#『問題点』：修正（追加）が面倒
#=========コードの修正==========
# 必要なライブラリを読み込む
library(tidyverse)
library(estimatr) # lm_robust() を使うために必要

# サンプルデータを作成
set.seed(111)
df <- tibble(
  y = seq(101, 200) + rnorm(n = 100, 0, 1),
  x_1 = seq(1, 100) + rnorm(n = 100, 0, 1),
  x_2 = sample(c(0, 1), 100, replace = TRUE),
  x_3 = seq(50, 149) + rnorm(n = 100, 0, 1)
)

# 1. 修正版の関数を定義
linear_models_fixed <- function(data) {
  
  # a. モデル式の右辺（独立変数）を文字列として定義
  formulas_rhs <- c(
    "x_1",
    "x_1 + x_2",
    "x_2",
    "x_1 + x_2 + x_3"
  )
  
  # b. map()を使って、各モデル式で回帰分析を繰り返し実行
  model_list <- map(formulas_rhs, function(rhs) {
    # "y ~" と独立変数の文字列を結合して、完全なモデル式（文字列）を作成
    formula_str <- paste("y ~", rhs)
    
    # 文字列を本物の「モデル式」に変換して、lm_robust() を実行
    lm_robust(as.formula(formula_str), data = data)
  })
  
  return(model_list)
}

# 2. 修正版の関数を実行
list_of_models <- linear_models_fixed(df)

# 3. 最初のモデルの結果を確認
print(list_of_models[[1]])
