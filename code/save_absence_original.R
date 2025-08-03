# 必要なライブラリを読み込む
library(tidyverse)
library(here)
library(readxl)
# 生徒数データを読み込む
df_students <- read_excel(here("data", "raw", "assignment1_data" , "生徒数","生徒数.xlsx"))
# 不登校者数データが格納されているフォルダのパスを指定
absence_folder <- here("data", "raw", "assignment1_data", "不登校生徒数")

# 指定したフォルダから、すべてのExcelファイル（.xlsx）のパスを取得
list_files <- list.files(path = absence_folder,
                         pattern = ".xlsx",
                         full.names = TRUE)

# 取得したファイルパスのリストを元に、すべてのファイルを一括で読み込む
list_absence <- map(list_files, read_excel)
# df_studentsの列名を英語に変更
df_students <- df_students |>
  rename(
    prefecture = "都道府県",
    year = "年度",
    n_students = "生徒数"
  )
# list_absence内の全てのデータフレームの列名を英語に変更
list_absence <- map(list_absence, function(df) {
  df %>%
    rename(
      prefecture = "都道府県",
      n_truants = "不登校生徒数"
    )
})
# 名前を変更したデータオブジェクトをRDSファイルとして保存
saveRDS(df_students, file = here("data", "original", "students_renamed.rds"))
saveRDS(list_absence, file = here("data", "original", "absence_list_renamed.rds"))