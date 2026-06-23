library(shiny)
library(dplyr)
library(readr)

# --- 核心函數保持不變 ---
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
      # 換回 uiOutput，準備接收我們特製的 HTML 下載按鈕
      uiOutput("download_ui")
    ),
    
    mainPanel(
      h4("差異資料預覽 (僅顯示標記為 marks為V 的資料，沒有全部喔)"),
      tableOutput("preview_table")
    )
  )
)

# --- 2. 背後運算邏輯 (Server) ---
server <- function(input, output, session) {
  
  app_data <- reactive({
    req(input$file1, input$file2)
    
    dt1 <- read_csv(input$file1$datapath, col_types = cols(.default = "c")) %>% 
      mutate(across(everything(), trimws))
    dt2 <- read_csv(input$file2$datapath, col_types = cols(.default = "c")) %>% 
      mutate(across(everything(), trimws))
    
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
      "dt1 獨有的差異資料：", res[4], " 筆\n",
      "dt2 獨有的差異資料：", res[5], " 筆"
    )
  })
  
  output$preview_table <- renderTable({
    df <- app_data()$full_data
    filtered_df <- df %>% 
      filter((!is.na(marks_dt1) & marks_dt1 == "V") | (!is.na(marks_dt2) & marks_dt2 == "V"))
    head(filtered_df, 50)
  })
  
  # --- 使用上次樹高 App 的 Data URI 絕招 ---
  output$download_ui <- renderUI({
    # 如果還沒上傳檔案，顯示反灰的假按鈕
    if (is.null(input$file1) || is.null(input$file2)) {
      return(tags$button(class = "btn btn-secondary", disabled = NA, "下載比對結果 (請先上傳資料)"))
    }
    
    df <- app_data()$full_data
    
    # 利用 capture.output 安全地把 DataFrame 轉成 CSV 格式的文字
    csv_lines <- capture.output(write.csv(df, row.names = FALSE))
    csv_str <- paste(csv_lines, collapse = "\n")
    
    # 加上 BOM 標記 (%EF%BB%BF) 並把文字編碼成網址格式
    data_uri <- paste0("data:text/csv;charset=utf-8,%EF%BB%BF", URLencode(csv_str, reserved = TRUE))
    
    # 產出一個偽裝成按鈕的超連結
    tags$a(href = data_uri, download = paste0("Compare_Result_", format(Sys.Date(), "%Y%m%d"), ".csv"),
           class = "btn btn-default", "下載比對結果")
  })
}
# --- 啟動 App ---
shinyApp(ui = ui, server = server)
