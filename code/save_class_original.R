# 必要なライブラリを読み込む
library(tidyverse)
library(here)
library(readxl)
class_data_folder <- here("data", "raw", "assignment1_data", "学級数")
# 指定したフォルダから、すべてのExcelファイル（.xlsx）のパスを取得
class_files <- list.files(path = class_data_folder,
                         pattern = ".xlsx",
                         full.names = TRUE)
# 全てのファイルを一括で読み込み、リストに格納する
list_class_data <- map(class_files, read_excel)

# 完成版データの確認
df_completed <- readRDS("data/raw/assignment1_data/課題2_完成版データ.rds")

# データの最初の6行を表示
head(df_completed)

# データの構造（列名、型など）を詳しく表示
str(df_completed)
# list_class_data の各要素に、対応する年度の名前を付ける
names(list_class_data) <- 2013:2022

# 名前の設定ができたか確認する
names(list_class_data)

# ヘッダーを整形するための【改良版】カスタム関数
clean_headers <- function(df) {
  # 1. 1行目を取得
  original_names <- as.character(df[1, ])
  
  # 2. 文字列から数字の部分だけを抽出する
  #    数字がない場合はNA（欠損値）になる
  numeric_names <- str_extract(original_names, "\\d+")
  
  # 3. 数字が抽出できた列はそれを、できなかった列（「都道府県」「計」など）は元の名前を使う
  new_names <- ifelse(!is.na(numeric_names), numeric_names, original_names)
  
  # 4. 新しい列名として設定
  colnames(df) <- new_names
  
  # 5. ヘッダーとして使った1行目を削除
  df <- df[-1, ]
  
  # 6. 整形後のデータフレームを返す
  return(df)
}

# map()を使って、改良版の関数を10個すべてのデータフレームに適用
list_class_data_cleaned <- map(list_class_data, clean_headers)

# 既存のリストの1列目の名前だけを修正する（年度→都道府県）
list_class_data_fixed <- map(list_class_data_cleaned, function(df) {
  # 1番目の列の名前を「都道府県」に設定する
  colnames(df)[1] <- "都道府県"
  # 修正したデータフレームを返す
  return(df)
})

# 日本語の列名を英語に変換
list_class_data_eng <- map(list_class_data_fixed, function(df) {
  df %>%
    rename(
      prefecture = "都道府県",
      total = "計"
    )
})

# 英語名に変換したリストをRDSファイルとして保存する
saveRDS(list_class_data_eng, file = here("data", "original", "class_data_eng.rds"))
