library(shiny)
library(dplyr)
library(readr)
library(openxlsx)
library(jsonlite)
library(DT)

# --- 欄位名稱標準化函數 (容忍大小寫與 sp 各種變體) ---
standardize_colnames <- function(df) {
  c_names <- names(df)
  c_names_lower <- tolower(c_names)
  
  c_names[c_names_lower == "x1"] <- "X1"
  c_names[c_names_lower == "y1"] <- "Y1"
  c_names[c_names_lower == "x2"] <- "X2"
  c_names[c_names_lower == "y2"] <- "Y2"
  c_names[c_names_lower == "tag"] <- "TAG"
  c_names[c_names_lower == "b"] <- "b"
  c_names[c_names_lower %in% c("name", "sp", "sp.", "species")] <- "Name"
  
  names(df) <- c_names
  return(df)
}

# --- 核心函數：完整版 (保留 X2, Y2 象限邏輯) ---
core_keys <- c("X1", "Y1", "X2", "Y2", "TAG", "b", "Name")

process_and_mark <- function(df_target, df_compare) {
  anti_df <- anti_join(df_target, df_compare) %>% mutate(marks = "V")
  semi_df <- semi_join(df_target, df_compare) %>% mutate(marks = "")
  
  bind_rows(anti_df, semi_df) %>%
    mutate(
      quad_order = case_when(
        X2 == "1" & Y2 == "1" ~ 1,
        X2 == "1" & Y2 == "2" ~ 2,
        X2 == "2" & Y2 == "2" ~ 3,
        X2 == "2" & Y2 == "1" ~ 4,
        TRUE ~ 5
      )
    ) %>%
    arrange(quad_order, X1, Y1, X2, Y2, TAG, b, Name) %>%
    select(-quad_order)
}

# --- 1. 網頁介面 (UI) ---
ui <- fluidPage(
  titlePanel("資料比對工具"),
  
  sidebarLayout(
    sidebarPanel(
      # 給使用者的友善說明提示框
      div(class = "alert alert-info", 
          style = "padding: 15px; margin-bottom: 20px;",
          "💡 ", strong("說明："), "資料匯入時，資料須為.csv檔，且為utf-8編碼，欄位務必要包含 ", 
          strong("X1、Y1、X2、Y2、Tag、b"), " 以及 ", strong("Name (或 sp. 或 sp 或 species)"), 
          " 欄位，才能進行比對(欄位名稱的大小寫沒有差)。"
      ),
      
      fileInput("file1", "請上傳第一份檔案 (dt1)", accept = ".csv"),
      fileInput("file2", "請上傳第二份檔案 (dt2)", accept = ".csv"),
      
      verbatimTextOutput("summary_stats"),
      
      hr(),
      uiOutput("download_ui")
    ),
    
    mainPanel(
      h4("錯誤資料預覽 (僅摘要顯示錯誤的資料，阿彌陀佛)"),
      DTOutput("preview_table")
    )
  )
)

# --- 2. 背後運算邏輯 (Server) ---
server <- function(input, output, session) {
  
  app_data <- reactive({
    req(input$file1, input$file2)
    
    withProgress(message = '正在讀取並比對資料...', value = 0, {
      
      incProgress(0.2, detail = "讀取原始檔案...")
      dt1_raw <- read_csv(input$file1$datapath, col_types = cols(.default = "c")) %>% 
        mutate(across(everything(), trimws))
      dt2_raw <- read_csv(input$file2$datapath, col_types = cols(.default = "c")) %>% 
        mutate(across(everything(), trimws))
      
      dt1 <- standardize_colnames(dt1_raw)
      dt2 <- standardize_colnames(dt2_raw)
      
      incProgress(0.2, detail = "檢查欄位完整性...")
      required_cols <- c("X1", "Y1", "X2", "Y2", "TAG", "b", "Name")
      missing_dt1 <- setdiff(required_cols, names(dt1))
      missing_dt2 <- setdiff(required_cols, names(dt2))
      
      shiny::validate(
        need(length(missing_dt1) == 0, paste("提示：第一份檔案 (dt1) 缺少必要欄位：", paste(missing_dt1, collapse = ", "))),
        need(length(missing_dt2) == 0, paste("提示：第二份檔案 (dt2) 缺少必要欄位：", paste(missing_dt2, collapse = ", ")))
      )
      
      sp_variants <- c("sp", "sp.", "species")
      msg1 <- if(any(tolower(names(dt1_raw)) %in% sp_variants)) "💡 偵測到 dt1 含有物種縮寫欄位，已自動標準化為 Name。\n" else ""
      msg2 <- if(any(tolower(names(dt2_raw)) %in% sp_variants)) "💡 偵測到 dt2 含有物種縮寫欄位，已自動標準化為 Name。\n" else ""
      change_msg <- paste0(msg1, msg2)
      
      incProgress(0.3, detail = "比對資料中...")
      qall1 <- process_and_mark(dt1, dt2) %>% rename(marks_dt1 = marks)
      qall2 <- process_and_mark(dt2, dt1) %>% rename(marks_dt2 = marks)
      
      b12 <- merge(qall1, qall2, by = 0, all = TRUE, sort = FALSE, suffixes = c("_dt1", "_dt2"))
      
      incProgress(0.3, detail = "計算統計數據...")
      dt1_rows <- nrow(dt1)
      dt2_rows <- nrow(dt2)
      same_rows <- sum(qall1$marks_dt1 == "", na.rm = TRUE)
      diff_dt1 <- sum(qall1$marks_dt1 == "V", na.rm = TRUE)
      diff_dt2 <- sum(qall2$marks_dt2 == "V", na.rm = TRUE)
      
      list(
        full_data = b12,
        stats = c(dt1_rows, dt2_rows, same_rows, diff_dt1, diff_dt2),
        msg = change_msg
      )
    })
  })
  
  output$summary_stats <- renderText({
    res <- app_data()$stats
    msg <- app_data()$msg
    paste0(
      msg, if(msg != "") "\n" else "",
      "第一份檔案 (dt1) 總筆數：", res[1], " 筆\n",
      "第二份檔案 (dt2) 總筆數：", res[2], " 筆\n\n",
      "兩份檔案相同的資料：", res[3], " 筆\n",
      "dt1 錯誤的資料：", res[4], " 筆\n",
      "dt2 錯誤的資料：", res[5], " 筆"
    )
  })
  
  # --- 優化：網頁預覽表格同步精準上色 ---
  output$preview_table <- renderDT({
    df <- app_data()$full_data
    filtered_df <- df %>% 
      filter((!is.na(marks_dt1) & marks_dt1 == "V") | (!is.na(marks_dt2) & marks_dt2 == "V"))
    
    cols_dt1 <- grep("_dt1$", names(filtered_df), value = TRUE)
    diff_cols <- c()
    
    # 1. 建立隱藏的判斷欄位 (True 代表資料有差異)
    for (cx in cols_dt1) {
      cy <- sub("_dt1$", "_dt2", cx)
      
      if (cy %in% names(filtered_df)) {
        diff_name <- paste0("diff_flag_", cx)
        diff_cols <- c(diff_cols, diff_name)
        
        filtered_df[[diff_name]] <- (filtered_df[[cx]] != filtered_df[[cy]]) | 
          (is.na(filtered_df[[cx]]) & !is.na(filtered_df[[cy]])) | 
          (!is.na(filtered_df[[cx]]) & is.na(filtered_df[[cy]]))
        
        # 避免 NA 造成判斷錯誤
        filtered_df[[diff_name]][is.na(filtered_df[[diff_name]])] <- FALSE
      }
    }
    
    # 2. 初始化 DT 表格，並隱藏剛剛建立的判斷欄位 (DT 的 targets 是從 0 開始算)
    hide_targets <- match(diff_cols, names(filtered_df)) - 1
    
    d_table <- datatable(filtered_df, 
                         rownames = FALSE, # 關閉最左側的列號，確保隱藏欄位對齊正確
                         options = list(
                           pageLength = 10, 
                           scrollX = TRUE,
                           columnDefs = list(
                             list(visible = FALSE, targets = hide_targets)
                           )
                         ))
    
    # 3. 根據隱藏欄位的 True/False 來決定要不要塗上黃色底色
    for (cx in cols_dt1) {
      cy <- sub("_dt1$", "_dt2", cx)
      diff_name <- paste0("diff_flag_", cx)
      
      if (diff_name %in% diff_cols) {
        d_table <- d_table %>%
          formatStyle(cx, valueColumns = diff_name,
                      backgroundColor = styleEqual(c(TRUE, FALSE), c('#FFFF99', 'transparent'))) %>%
          formatStyle(cy, valueColumns = diff_name,
                      backgroundColor = styleEqual(c(TRUE, FALSE), c('#FFFF99', 'transparent')))
      }
    }
    
    return(d_table)
  })
  
  output$download_ui <- renderUI({
    if (is.null(input$file1) || is.null(input$file2)) {
      return(tags$button(class = "btn btn-secondary", disabled = NA, "下載 Excel 比對結果 (請先上傳csv utf-8資料)"))
    }
    
    df <- app_data()$full_data
    
    wb <- createWorkbook()
    addWorksheet(wb, "比對結果")
    writeData(wb, "比對結果", df)
    
    header_style_dt1 <- createStyle(fgFill = "#d8d8d8", textDecoration = "bold")
    header_style_dt2 <- createStyle(fgFill = "#98c4c8", textDecoration = "bold")
    
    for (i in 1:ncol(df)) {
      col_name <- names(df)[i]
      if (grepl("_dt1$", col_name) || col_name == "marks_dt1") {
        addStyle(wb, "比對結果", style = header_style_dt1, rows = 1, cols = i, gridExpand = TRUE)
      } else if (grepl("_dt2$", col_name) || col_name == "marks_dt2") {
        addStyle(wb, "比對結果", style = header_style_dt2, rows = 1, cols = i, gridExpand = TRUE)
      }
    }
    
    highlight_style <- createStyle(fgFill = "#FFFF99")
    cols_x <- grep("_dt1$", names(df), value = TRUE)
    
    for (cx in cols_x) {
      cy <- sub("_dt1$", "_dt2", cx)
      
      if (cy %in% names(df)) {
        diff_rows <- which(
          (df[[cx]] != df[[cy]]) | 
            (is.na(df[[cx]]) & !is.na(df[[cy]])) | 
            (!is.na(df[[cx]]) & is.na(df[[cy]]))
        ) + 1
        
        if (length(diff_rows) > 0) {
          col_idx_x <- which(names(df) == cx)
          col_idx_y <- which(names(df) == cy)
          
          addStyle(wb, "比對結果", style = highlight_style, rows = diff_rows, cols = col_idx_x)
          addStyle(wb, "比對結果", style = highlight_style, rows = diff_rows, cols = col_idx_y)
        }
      }
    }
    
    temp_file <- tempfile(fileext = ".xlsx")
    saveWorkbook(wb, temp_file, overwrite = TRUE)
    
    raw_data <- readBin(temp_file, "raw", file.info(temp_file)$size)
    b64_str <- jsonlite::base64_enc(raw_data)
    data_uri <- paste0("data:application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;base64,", b64_str)
    
    tags$a(href = data_uri, download = paste0("Compare_Result_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
           class = "btn btn-success", "下載 Excel 比對結果")
  })
}

shinyApp(ui = ui, server = server)