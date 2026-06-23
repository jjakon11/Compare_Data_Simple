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
      
      # 新增：顯示統計資訊的區塊
      verbatimTextOutput("summary_stats"),
      
      uiOutput("download_ui")
    ),
    
    mainPanel(
      # 修改標題提示，讓使用者知道這只是預覽
      h4("差異資料預覽 (僅顯示標記為 marks為V 的資料，沒有全部喔)"),
      tableOutput("preview_table")
    )
  )
)

# --- 2. 背後運算邏輯 (Server) ---
server <- function(input, output, session) {
  
  # 建立一個 reactive 來處理並儲存所有需要的資料與統計數字
  app_data <- reactive({
    req(input$file1, input$file2)
    
    dt1 <- read_csv(input$file1$datapath, col_types = cols(.default = "c"), locale = locale(encoding = "Big5")) %>% 
      mutate(across(everything(), trimws))
    dt2 <- read_csv(input$file2$datapath, col_types = cols(.default = "c"), locale = locale(encoding = "Big5")) %>% 
      mutate(across(everything(), trimws))
    
    qall1 <- process_and_mark(dt1, dt2) %>% rename(marks_dt1 = marks)
    qall2 <- process_and_mark(dt2, dt1) %>% rename(marks_dt2 = marks)
    
    b12 <- merge(qall1, qall2, by = 0, all = TRUE, sort = FALSE)
    
    # 計算各項統計數字
    dt1_rows <- nrow(dt1)
    dt2_rows <- nrow(dt2)
    # 相同資料：在 qall1 中 marks 為空白的數量
    same_rows <- sum(qall1$marks_dt1 == "", na.rm = TRUE)
    # 差異資料：在各自的結果中 marks 為 "V" 的數量
    diff_dt1 <- sum(qall1$marks_dt1 == "V", na.rm = TRUE)
    diff_dt2 <- sum(qall2$marks_dt2 == "V", na.rm = TRUE)
    
    # 將所有算好的結果打包回傳
    list(
      full_data = b12,
      stats = c(dt1_rows, dt2_rows, same_rows, diff_dt1, diff_dt2)
    )
  })
  
  # 新增：將統計數字輸出到網頁畫面上
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
  
  # 修改：渲染表格時，只篩選出有 V 的資料來顯示
  output$preview_table <- renderTable({
    df <- app_data()$full_data
    
    # 篩選條件：只要 dt1 或是 dt2 其中一邊的標記是 "V" 就抓出來
    # 使用 !is.na 避免合併時產生的 NA 造成篩選錯誤
    filtered_df <- df %>% 
      filter((!is.na(marks_dt1) & marks_dt1 == "V") | (!is.na(marks_dt2) & marks_dt2 == "V"))
    
    # 為了避免網頁卡頓，預覽最多只顯示前 50 筆差異
    head(filtered_df, 50)
  })
  
  output$download_ui <- renderUI({
    req(app_data())
    downloadButton("downloadData", "下載比對結果")
  })
  
  # 維持不變：下載的檔案依然是完整的 full_data (包含所有 V 與空白的資料)
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Compare_Result_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(app_data()$full_data, file, row.names = FALSE, fileEncoding = "Big5")
    }
  )
}

# --- 啟動 App ---
shinyApp(ui = ui, server = server)