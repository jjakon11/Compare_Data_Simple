library(shiny)
library(dplyr)
library(readr)
library(openxlsx)
library(jsonlite)

# --- 新增：欄位名稱標準化函數 (容忍大小寫與 sp 替換) ---
standardize_colnames <- function(df) {
  c_names <- names(df)
  c_names_lower <- tolower(c_names)
  
  # 不管原本長怎樣，只要轉小寫後符合，就強制換成我們程式碼看得懂的標準名稱
  c_names[c_names_lower == "x1"] <- "X1"
  c_names[c_names_lower == "y1"] <- "Y1"
  c_names[c_names_lower == "x2"] <- "X2"
  c_names[c_names_lower == "y2"] <- "Y2"
  c_names[c_names_lower == "tag"] <- "TAG"
  c_names[c_names_lower == "b"] <- "b"
  c_names[c_names_lower %in% c("name", "sp")] <- "Name" # 將 name 或 sp 都統一為 Name
  
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
      fileInput("file1", "請上傳第一份檔案 (dt1)", accept = ".csv"),
      fileInput("file2", "請上傳第二份檔案 (dt2)", accept = ".csv"),
      
      verbatimTextOutput("summary_stats"),
      
      hr(),
      uiOutput("download_ui")
    ),
    
    mainPanel(
      h4("錯誤資料預覽 (僅摘要顯示錯誤的資料，阿彌陀佛)"),
      tableOutput("preview_table")
    )
  )
)

# --- 2. 背後運算邏輯 (Server) ---
server <- function(input, output, session) {
  
  app_data <- reactive({
    req(input$file1, input$file2)
    
    # 在讀取檔案並去空白後，立刻套用 standardize_colnames 函數
    dt1 <- read_csv(input$file1$datapath, col_types = cols(.default = "c")) %>% 
      mutate(across(everything(), trimws)) %>% 
      standardize_colnames()
    
    dt2 <- read_csv(input$file2$datapath, col_types = cols(.default = "c")) %>% 
      mutate(across(everything(), trimws)) %>% 
      standardize_colnames()
    
    qall1 <- process_and_mark(dt1, dt2) %>% rename(marks_dt1 = marks)
    qall2 <- process_and_mark(dt2, dt1) %>% rename(marks_dt2 = marks)
    
    b12 <- merge(qall1, qall2, by = 0, all = TRUE, sort = FALSE)
    
    dt1_rows <- nrow(dt1)
    dt2_rows <- nrow(dt2)
    same_rows <- sum(qall1$marks_dt1 == "", na.rm = TRUE)
    diff_dt1 <- sum(qall1$marks_dt1 == "V", na.rm = TRUE)
    diff_dt2 <- sum(qall2$marks_dt2 == "V", na.rm = TRUE)
    
    list(
      full_data = b12,
      stats = c(dt1_rows, dt2_rows, same_rows, diff_dt1, diff_dt2)
    )
  })
  
  output$summary_stats <- renderText({
    res <- app_data()$stats
    paste0(
      "第一份檔案 (dt1) 總筆數：", res[1], " 筆\n",
      "第二份檔案 (dt2) 總筆數：", res[2], " 筆\n\n",
      "兩份檔案相同的資料：", res[3], " 筆\n",
      "dt1 有錯的資料筆數：", res[4], " 筆\n",
      "dt2 有錯的資料筆數：", res[5], " 筆"
    )
  })
  
  output$preview_table <- renderTable({
    df <- app_data()$full_data
    filtered_df <- df %>% 
      filter((!is.na(marks_dt1) & marks_dt1 == "V") | (!is.na(marks_dt2) & marks_dt2 == "V"))
    head(filtered_df, 50)
  })
  
  # --- Excel 欄位名稱與單格錯值上色邏輯 ---
  output$download_ui <- renderUI({
    if (is.null(input$file1) || is.null(input$file2)) {
      return(tags$button(class = "btn btn-secondary", disabled = NA, "下載 Excel 比對結果 (請先上傳資料)"))
    }
    
    df <- app_data()$full_data
    
    wb <- createWorkbook()
    addWorksheet(wb, "比對結果")
    writeData(wb, "比對結果", df)
    
    header_style_dt1 <- createStyle(fgFill = "#d8d8d8", textDecoration = "bold")
    header_style_dt2 <- createStyle(fgFill = "#98c4c8", textDecoration = "bold")
    
    for (i in 1:ncol(df)) {
      col_name <- names(df)[i]
      if (grepl("\\.x$", col_name) || col_name == "marks_dt1") {
        addStyle(wb, "比對結果", style = header_style_dt1, rows = 1, cols = i, gridExpand = TRUE)
      } else if (grepl("\\.y$", col_name) || col_name == "marks_dt2") {
        addStyle(wb, "比對結果", style = header_style_dt2, rows = 1, cols = i, gridExpand = TRUE)
      }
    }
    
    highlight_style <- createStyle(fgFill = "#FFFF99")
    cols_x <- grep("\\.x$", names(df), value = TRUE)
    
    for (cx in cols_x) {
      cy <- sub("\\.x$", ".y", cx)
      
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

# --- 啟動 App ---
shinyApp(ui = ui, server = server)