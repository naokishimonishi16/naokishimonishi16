#学校給食の画像をダウンロードするウェブスクレイピングのコード
rm(list = ls())
library(rvest)
library(dplyr)
library(stringr)

#各小学校の名前
toshima_names <- c("komagome",
                  "sugamo",
                  "seiwa",
                  "nishisugamo",
                  "hosei",
                  "hoyu",
                  "asahi",
                  "ikebukurodaiichi",
                  "ikebukurodaisan",
                  "ikebukuro",
                  "minamiikebukuro",
                  "ikebukurohoncho",
                  "konan",
                  "nagasaki",
                  "kaname",
                  "shiinamachi",
                  "fujimidai",
                  "takamatsu",
                  "chihaya",
                  "sakura")

#各小学校のHPのurlがここに入る
toshima_urls <- c("","","","","","","","","","","","","","","","","","")

#スクレーピングの関数
scrape_toshima_school <- function(school_name, base_url, page_range) {
  
  # 内部関数：全角数字を半角に変換
  to_half_width <- function(x) {
    if (is.na(x) || length(x) == 0) return("")
    x <- chartr("０１２３４５６７８９", "0123456789", x)
    return(x)
  }
  
  # 設定・初期化
  save_dir <- school_name 
  global_count <- 0
  stop_flag <- FALSE
  
  # 閾値設定
  skip_threshold <- as.Date("2025-04-01")
  stop_threshold <- as.Date("2022-03-31")
  
  # 保存先ディレクトリがなければ新たに作成
  if (!dir.exists(save_dir)) {
    dir.create(save_dir)
    message(paste("ディレクトリ作成:", save_dir))
  }
  
  # 親ループ（一覧ページを巡回）
  for (i in page_range) {
    
    if (stop_flag) {
      message("指定年度以前のデータに到達したため、処理を終了します。")
      break
    }
    
    list_url <- paste0(base_url, i)
    message(paste("\n=== 一覧ページ巡回中: ", i, "ページ目 ==="))
    
    tryCatch({
      list_page <- read_html(list_url)
      
      # 記事のリンクを取得
      detail_links <- list_page %>% 
        html_nodes(".diary_card_list a") %>% 
        html_attr("href") %>% 
        unique()
      
      if (length(detail_links) == 0) {
        message("記事リンクが見つかりませんでした。ループを終了します。")
        break
      }
      
      message(paste("  ->", length(detail_links), "件の記事を処理します..."))
      
      # === 子ループ（詳細ページ） ===
      for (link in detail_links) {
        
        if (!str_detect(link, "^http")) {
          detail_url <- paste0("https://toshima.schoolweb.ne.jp", link)
        } else {
          detail_url <- link
        }
        
        Sys.sleep(1) # 待機
        
        try({
          detail_page <- read_html(detail_url)
          
          # 画像のURL取得
          target_imgs <- detail_page %>% 
            html_nodes(".diary_top_list_img img") %>% 
            html_attr("src")
          
          if (length(target_imgs) > 0) {
            
            # 以下、日付取得ロジック ---
            # 1. 投稿日時の取得（年情報のベースとして使用）
            post_time_text <- detail_page %>% html_nodes(".diary_top_date") %>% html_text() %>% head(1)
            base_year <- 9999 
            
            if (length(post_time_text) > 0) {
              year_match <- str_extract(post_time_text, "\\d{4}")
              if (!is.na(year_match)) {
                base_year <- year_match
              }
            }
            
            # タイトル取得と正規化
            title_text_raw <- detail_page %>% html_nodes(".diary_top_title") %>% html_text() %>% head(1)
            title_text <- to_half_width(title_text_raw)
            
            detected_year <- NA
            detected_mm <- NA
            detected_dd <- NA
            valid_date_found <- FALSE
            
            # === パターン1: タイトルから検索 ===
            # 【A】 YYYY/MM/DD
            match_full <- str_match(title_text, "(\\d{4})[\\./\\-](\\d{1,2})[\\./\\-](\\d{1,2})")
            
            if (!is.na(match_full[1,1])) {
              detected_year <- match_full[1,2]
              detected_mm   <- match_full[1,3]
              detected_dd   <- match_full[1,4]
              valid_date_found <- TRUE
            } else {
              # 【B】 MM/DD
              match_slash <- str_match(title_text, "(\\d{1,2})[\\/\\-](\\d{1,2})")
              
              if (!is.na(match_slash[1,1])) {
                detected_mm <- match_slash[1,2]
                detected_dd <- match_slash[1,3]
                valid_date_found <- TRUE
              } else {
                # 【C】 XX月 XX日 (スペース許容) 
                match_kanji <- str_match(title_text, "(\\d{1,2})月[\\s　]*(\\d{1,2})日")
                
                if (!is.na(match_kanji[1,1])) {
                  detected_mm <- match_kanji[1,2]
                  detected_dd <- match_kanji[1,3]
                  valid_date_found <- TRUE
                }
              }
            }
            
            # === パターン2: 本文から検索 ===
            # タイトルで見つからなかった場合のみ実行
            if (!valid_date_found) {
              body_text_raw <- detail_page %>% html_nodes(".cms_text") %>% html_text() %>% paste(collapse = " ")
              
              if (nchar(body_text_raw) > 0) {
                body_text <- to_half_width(body_text_raw)
                
                # 「XX月 XX日」を検索 (スペース許容)
                match_body <- str_match(body_text, "(\\d{1,2})月[\\s　]*(\\d{1,2})日")
                
                if (!is.na(match_body[1,1])) {
                  detected_mm <- match_body[1,2]
                  detected_dd <- match_body[1,3]
                  valid_date_found <- TRUE
                  message(paste0("  [補完] タイトル日付なし。本文から抽出: ", match_body[1,1]))
                }
              }
            }
            
            # 日付ロジック終了、以下保存処理 ---
            
            if (valid_date_found) {
              # 年の決定: 抽出できた年 または 投稿日時の年(base_year)
              target_year <- if(!is.na(detected_year)) detected_year else base_year
              
              mm <- sprintf("%02d", as.numeric(detected_mm))
              dd <- sprintf("%02d", as.numeric(detected_dd))
              
              if (target_year != 9999) {
                current_date_str <- paste0(target_year, "-", mm, "-", dd)
                current_date <- as.Date(current_date_str)
                
                if (!is.na(current_date)) {
                  # 【スキップ判定】
                  if (current_date > skip_threshold){
                    message(paste0("  [スキップ] ", current_date_str, " (2025年度以降)"))
                    next 
                  }
                  # 【停止判定】
                  if (current_date < stop_threshold){
                    message(paste0("★停止条件: ", current_date_str, " (2022年度以前)"))
                    stop_flag <- TRUE
                    break 
                  }
                }
              }
              
              yy <- str_sub(target_year, 3, 4)
              base_name_file <- paste0(school_name, "_", yy, mm, dd)
              
            } else {
              # 全て失敗
              global_count <- global_count + 1
              base_name_file <- paste0(school_name, "_unknown_", sprintf("%04d", global_count))
              message(paste0("  [日付不明] タイトル: ", str_trunc(title_text_raw, 20)))
            }
            
            # 画像保存処理
            img_sub_count <- 0
            for (src in target_imgs) {
              if (str_detect(src, "icon|button|logo|image_title")) next
              
              if (!str_detect(src, "^http")) {
                full_img_url <- paste0("https://toshima.schoolweb.ne.jp", src)
              } else {
                full_img_url <- src
              }
              img_sub_count <- img_sub_count + 1
              
              if (img_sub_count == 1) {
                current_file_name <- paste0(base_name_file, ".jpg")
              } else {
                current_file_name <- paste0(base_name_file, "_", img_sub_count, ".jpg")
              }
              
              check_path <- file.path(save_dir, current_file_name)
              dup_counter <- 1
              base_name_only <- tools::file_path_sans_ext(current_file_name)
              
              while (file.exists(check_path)) {
                current_file_name_dup <- paste0(base_name_only, "_dup", dup_counter, ".jpg")
                check_path <- file.path(save_dir, current_file_name_dup)
                dup_counter <- dup_counter + 1
                if (!file.exists(check_path)) {
                  current_file_name <- current_file_name_dup
                }
              }
              
              download.file(full_img_url, check_path, mode = "wb", quiet = TRUE)
              message(paste("    [保存]", current_file_name))
              
            }
          }
        }, silent = TRUE)
        
        if(stop_flag) break
      } 
      Sys.sleep(3) 
    }, error = function(e) {
      message(paste("エラー:", e$message))
      Sys.sleep(5)
    })
  }
  
  message(paste0("完了: ", school_name, " の処理が終わりました。"))
}


# 対象の画像を保存(各1~50ページまで)
for (i in 1:18) {
  scrape_toshima_school(toshima_names[i], toshima_urls[i], page_range = 1:50)
}
