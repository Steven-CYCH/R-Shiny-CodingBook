library(shiny)
library(openxlsx)
library(rhandsontable)
# library(shinylive)
# library(httpuv)

# shinylive::export(appdir = "C:\\Users\\15334\\Desktop\\Module\\R Shiny 工具庫\\CodingBook產生器",
#                   destdir = "C:\\Users\\15334\\Desktop\\Module\\R Shiny 工具庫\\CodingBook產生器\\site")

ui <- fluidPage(
  tags$head(tags$style(HTML(".btn-danger { background-color: #d9534f; border-color: #d43f3a; }"))),
  
  titlePanel("CodeBook 產生器 (SPSS Syntax)"),
  
  sidebarLayout(
    sidebarPanel(
      width = 5,
      
      # 1. 讀取
      div(style = "background-color: #e3f2fd; padding: 10px; border-radius: 5px; border: 1px solid #90caf9;",
          h4("📂 接續工作", style = "margin-top: 0px;"),
          fileInput("load_rds", "選擇上次的 .rds 暫存檔", accept = c(".rds"))
      ),
      hr(),
      
      # 2. 設定
      h4("📝 步驟 1：變數設定"),
      fluidRow(
        column(4, textInput("var_name", "Variable (變數名)", placeholder = "例如：id")),
        column(8, textInput("var_label", "Label (中文標籤)", placeholder = "例如：編號"))
      ),
      hr(),
      
      # 3. 編碼
      h4("🔢 步驟 2：輸入編碼"),
      helpText("👇 左邊選模組，右邊顯示結果"),
      fluidRow(
        column(3, 
               tags$label("快速模組"),
               actionButton("btn_01_no_yes", "0 = 無，1 = 有", class = "btn-info btn-sm", style="width:100%; margin-bottom:5px; text-align: center;"),
               actionButton("btn_sex", "0 = 女，1 = 男", class = "btn-info btn-sm", style="width:100%; margin-bottom:5px; text-align: center;"),
               actionButton("btn_agr", "1 = 非常不同意，2 = 不同意，3 = 普通，4 = 同意，5 = 非常同意", class = "btn-info btn-sm", style="width:100%; margin-bottom:5px; text-align: center;"),
               actionButton("btn_clear", "❌ 清空", class = "btn-default btn-sm", style="width:100%; margin-top:5px;")
        ),
        column(9, rHandsontableOutput("hot_input"))
      ),
      br(),
      selectInput("var_type", "Measure (測量尺度)", 
                  choices = c("連續 (Scale)" = "SCALE", 
                              "類別 (Nominal)" = "NOMINAL", 
                              "順序 (Ordinal)" = "ORDINAL",
                              "不分析 (None)" = "NONE"), 
                  selected = "SCALE"),
      br(),
      actionButton("add_btn", "➡️ 加入右方總表 (並清空輸入)", class = "btn-primary btn-lg", style="width:100%;"),
      br(), hr(),
      
      # 4. 存檔輸出
      h4("💾 存檔與輸出"),
      div(style = "background-color: #f9f9f9; padding: 15px; border-radius: 5px; border: 1px solid #ddd;",
          fluidRow(
            column(6, downloadButton("save_rds", "💾 儲存進度 (.rds)", class = "btn-warning", style = "width: 100%;")),
            column(6, downloadButton("download_excel", "📥 下載 Excel", class = "btn-success", style = "width: 100%;"))
          ),
          br(),
          # helpText("提示：新加入的變數會排在 Excel 的最上方。")
      )
    ),
    
    mainPanel(
      width = 7,
      h3("📋 預覽與編輯 CodeBook"),
      rHandsontableOutput("hot_preview")
    )
  )
)

server <- function(input, output, session) {
  
  default_df <- data.frame(數值 = c("", ""), 說明 = c("", ""), stringsAsFactors = FALSE)
  
  values <- reactiveValues(
    hot_data = default_df,
    final_df = data.frame(
      Variable = character(), Label = character(), Value = character(), ValueLabel = character(), 
      Syntax_Var = character(), Syntax_Type = character(), Syntax_Val = character(), stringsAsFactors = FALSE
    ),
    pending_row = NULL,
    refresh_trigger = 0 
  )
  
  # 讀取
  observeEvent(input$load_rds, {
    req(input$load_rds)
    tryCatch({
      df_loaded <- readRDS(input$load_rds$datapath)
      values$final_df <- df_loaded
      showNotification("進度讀取成功！", type = "message")
    }, error = function(e) { showNotification("讀取失敗，檔案格式錯誤", type = "error") })
  })
  
  # 儲存
  output$save_rds <- downloadHandler(
    filename = function() { paste("Project_Save_", Sys.Date(), ".rds", sep = "") },
    content = function(file) { saveRDS(values$final_df, file) }
  )
  
  # 重置
  reset_inputs <- function() {
    updateTextInput(session, "var_name", value = "")
    updateTextInput(session, "var_label", value = "")
    updateSelectInput(session, "var_type", selected = "SCALE")
    values$hot_data <- default_df
    values$refresh_trigger <- values$refresh_trigger + 1
  }
  
  # 渲染表格
  output$hot_input <- renderRHandsontable({
    req(values$refresh_trigger >= 0) 
    rhandsontable(values$hot_data, rowHeaders = NULL) %>%
      hot_col("數值", type = "text") %>% hot_col("說明", type = "text") %>%
      hot_table(minSpareRows = 1, highlightCol = TRUE, highlightRow = TRUE, height = 200)
  })
  
  output$hot_preview <- renderRHandsontable({
    if (is.null(values$final_df) || nrow(values$final_df) == 0) return(NULL)
    rhandsontable(values$final_df, rowHeaders = NULL) %>%
      hot_col("Variable", type = "text") %>% hot_col("Label", type = "text") %>%
      hot_col("Value", type = "text") %>% hot_col("ValueLabel", type = "text") %>%
      hot_col("Syntax_Var", type = "text", colWidths = 180) %>%
      hot_col("Syntax_Type", type = "text", colWidths = 180) %>% 
      hot_col("Syntax_Val", type = "text", colWidths = 200) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
  
  observeEvent(input$hot_preview, {
    new_df <- hot_to_r(input$hot_preview)
    if (!is.null(new_df) && nrow(new_df) > 0) { values$final_df <- new_df }
  })
  
  # 按鈕與邏輯
  observeEvent(input$btn_01_no_yes, {
    values$hot_data <- data.frame(數值 = c("0", "1"), 說明 = c("無", "有"), stringsAsFactors = FALSE)
    updateSelectInput(session, "var_type", selected = "NOMINAL")
  })
  observeEvent(input$btn_sex, {
    values$hot_data <- data.frame(數值 = c("0", "1"), 說明 = c("女", "男"), stringsAsFactors = FALSE)
    updateSelectInput(session, "var_type", selected = "NOMINAL")
  })
  observeEvent(input$btn_agr, {
    values$hot_data <- data.frame(數值 = c("1", "2", "3", "4", "5"), 說明 = c("非常不同意", "不同意", "普通", "同意", "非常同意"), stringsAsFactors = FALSE)
    updateSelectInput(session, "var_type", selected = "NOMINAL")
  })
  observeEvent(input$btn_clear, { reset_inputs() })
  
  observe({
    req(input$hot_input)
    temp_df <- hot_to_r(input$hot_input)
    has_data <- any(!is.na(temp_df) & temp_df != "")
    if (has_data && input$var_type == "SCALE") {
      updateSelectInput(session, "var_type", selected = "NOMINAL")
    }
  })
  
  observeEvent(input$add_btn, {
    req(input$var_name)
    current_codes <- hot_to_r(input$hot_input)
    current_codes <- current_codes[(!is.na(current_codes$數值) & current_codes$數值 != "") | 
                                     (!is.na(current_codes$說明) & current_codes$說明 != ""), ]
    spss_var_label <- paste0("VARIABLE LABELS ", input$var_name, " '", input$var_label, "'.")
    spss_role_level <- if (input$var_type == "NONE") paste0("VARIABLE ROLE /NONE ", input$var_name, ".") else paste0("VARIABLE LEVEL ", input$var_name, " (", input$var_type, ").")
    need_val_labels <- input$var_type %in% c("NOMINAL", "ORDINAL")
    
    new_rows <- data.frame()
    if (!need_val_labels) {
      new_rows <- data.frame(Variable = input$var_name, Label = input$var_label, Value = "", ValueLabel = "", Syntax_Var = spss_var_label, Syntax_Type = spss_role_level, Syntax_Val = "", stringsAsFactors = FALSE)
    } else {
      if (nrow(current_codes) == 0) {
        new_rows <- data.frame(Variable = input$var_name, Label = input$var_label, Value = "", ValueLabel = "", Syntax_Var = spss_var_label, Syntax_Type = spss_role_level, Syntax_Val = "", stringsAsFactors = FALSE)
      } else {
        syntax_val_col <- paste0("ADD VALUE LABELS ", input$var_name, " ", current_codes$數值, " '", current_codes$說明, "'.")
        new_rows <- data.frame(Variable = input$var_name, Label = input$var_label, Value = current_codes$數值, ValueLabel = current_codes$說明, Syntax_Var = spss_var_label, Syntax_Type = spss_role_level, Syntax_Val = syntax_val_col, stringsAsFactors = FALSE)
      }
    }
    
    if(input$var_name %in% values$final_df$Variable) {
      values$pending_row <- new_rows
      showModal(modalDialog(
        title = "⚠️ 變數名稱重複", paste0("變數 '", input$var_name, "' 已經存在。確定要覆蓋嗎？"),
        footer = tagList(modalButton("取消"), actionButton("confirm_overwrite", "是的，我要覆蓋", class = "btn-danger"))
      ))
    } else {
      # === 關鍵修改：新資料(new_rows) 放前面，舊資料(final_df) 放後面 ===
      values$final_df <- rbind(new_rows, values$final_df)
      reset_inputs() 
      showNotification("已新增 (置頂)！", type = "message")
    }
  })
  
  observeEvent(input$confirm_overwrite, {
    req(values$pending_row) 
    target_var <- values$pending_row$Variable[1]
    values$final_df <- values$final_df[values$final_df$Variable != target_var, ]
    # === 關鍵修改：覆蓋時，也把該變數移到最上方 ===
    values$final_df <- rbind(values$pending_row, values$final_df)
    removeModal()     
    reset_inputs()    
    values$pending_row <- NULL 
    showNotification(paste("已覆蓋並移至頂端:", target_var), type = "warning")
  })
  
  # === 下載 Excel (修正顯示邏輯) ===
  output$download_excel <- downloadHandler(
    filename = function() { paste("CodeBook_Final_", Sys.Date(), ".xlsx", sep = "") },
    content = function(file) {
      display_df <- values$final_df
      original_vars <- values$final_df$Variable # 使用原始變數清單進行比對
      
      if (nrow(display_df) > 1) {
        for (i in 2:nrow(display_df)) {
          # 比對邏輯：只要這一行跟上一行的變數名稱一樣，就是重複的內容
          if (original_vars[i] == original_vars[i-1]) {
            display_df$Variable[i] <- ""
            display_df$Label[i] <- ""
            display_df$Syntax_Var[i] <- "" 
            display_df$Syntax_Type[i] <- "" 
          }
        }
      }
      write.xlsx(display_df, file)
    }
  )
}

shinyApp(ui, server)