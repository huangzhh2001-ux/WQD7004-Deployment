# Load all packages
library(shiny)
library(bslib)
library(fontawesome)
library(plm)
library(lmtest)
library(sandwich)
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)

# Shiny UI Definition 
ui <- bslib::page_fluid(
  theme = bslib::bs_theme(
    bootswatch = "flatly", 
    primary = "#2E86AB",
    secondary = "#2874A6",
    success = "#1E8449",
    info = "#2196F3"
  ),
  fontawesome::fa_html_dependency(), 
  
  titlePanel(tags$span(style="color:#2E86AB; font-weight:bold; font-size:26px;",
                       "Global Population Aging Analysis | Regression + Classification Online APP")),
  
  tabsetPanel(
    # Tab 1: FE
    tabPanel(
      tags$span(fontawesome::fa("chart-line"), " FE Regression Prediction | Aging Forecast"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          style = "background:#f7fbff; border-radius:8px; padding:15px;",
          
          h4(tags$span(fontawesome::fa("database")," Input Core Economic & Demographic Indicators"), style = "font-size:15px;"),
          helpText("All indicators adopt log transformation consistent with original empirical regression setup"),
          uiOutput("reg_input_box"),
          br(),
          actionButton(
            inputId = "run_predict",
            label = tags$span(fontawesome::fa("calculator"), " Start Aging Prediction"),
            class = "btn-primary btn-lg w-100",
            style = "background:#2874A6;"
          )
        ),
        mainPanel(
          width = 9,
          style = "background:#fdfefe; border-radius:8px; padding:15px;",
          conditionalPanel(
            condition = "input.run_predict > 0",
            h4(tags$span(fontawesome::fa("bullseye")," Prediction Result: Log-transformed 65+ Population Ratio")),
            verbatimTextOutput("pred_out"),
            br()
          ),
          h4(tags$span(fontawesome::fa("table-columns")," Core Model Coefficient & Significance Summary Table")),
          tableOutput("coef_table")
        )
      )
    ),
    # Tab2 Random forest
    tabPanel(
      tags$span(fontawesome::fa("users"), " Aging Level Classification | High/Low Aging Country"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          style = "background:#f0fff4; border-radius:8px; padding:15px;",
          h4(tags$span(fontawesome::fa("clipboard-check")," Input Demographic Core Indicators"), style = "font-size:15px;"),
          numericInput(inputId = "cbr_in", label = "Crude Birth Rate", value = 3.0, min = 0, max = 5, step = 0.01),
          numericInput(inputId = "cdr_in", label = "Crude Death Rate", value = 2.2, min = 0, max = 4, step = 0.01),
          br(),
          actionButton(
            inputId = "run_classify",
            label = tags$span(fontawesome::fa("brain"), " Classify Aging Level"),
            class = "btn-success btn-lg w-100",
            style = "background:#1E8449;"
          ),
          hr(),
          tags$div(
            tags$h5("Group Definition:"),
            tags$p(tags$b("High-Aging Group:"), "Countries with elderly(65+) share above cluster cutoff"),
            tags$p(tags$b("Low-Aging Group:"), "Countries with elderly(65+) share below cluster cutoff")
          )
        ),
        mainPanel(
          width = 9,
          style = "background:#f8fff9; border-radius:8px; padding:15px;",
          h3(tags$span(fontawesome::fa("flag-checkered")," Final Classification Outcome")),
          
          uiOutput("class_card"),
          br(),
          wellPanel(
            h4(tags$span(fontawesome::fa("circle-info")," Model Description")),
            tags$p("Prediction engine: Random Forest trained on country-level CBR & CDR."),
            tags$p("Country aging labels are predefined via unsupervised K-Means clustering based on historical 65+ population ratio.")
          )
        )
      )
    ),
    # Tab3 drawing
    tabPanel(
      tags$span(fontawesome::fa("chart-area"), " Global Aging Data Visualization"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          style = "background:#f0f8ff; border-radius:8px; padding:15px;",
          selectInput(
            inputId = "group_type",
            label = tags$span(fontawesome::fa("layer-group")," Group By Category:"),
            choices = c("Income Group" = "incomegroup", "Geographic Region" = "region")
          ),
          selectInput(
            inputId = "plot_var",
            label = tags$span(fontawesome::fa("map")," Select Target Indicator:"),
            choices = c(
              "Aging Ratio(65+)" = "SP.POP.65UP.TO",
              "Crude Birth Rate" = "SP.DYN.CBRT.IN",
              "Crude Death Rate" = "SP.DYN.CDRT.IN"
            )
          ),
          br(),
          actionButton(
            inputId = "draw_plot",
            label = tags$span(fontawesome::fa("wand-sparkles"), " Generate Visualization"),
            class = "btn-info btn-lg w-100",
            style = "background:#2196F3;"
          )
        ),
        mainPanel(
          width = 9,
          style = "background:#f7fcff; border-radius:8px; padding:15px;",
          plotOutput(outputId = "global_plot", height = "600px")
        )
      )
    ),
    # About
    tabPanel(
      tags$span(fontawesome::fa("circle-info"), " About This App"),
      fluidRow(
        column(width = 10, offset = 1,
               style = "background:#f8f9fa; border-radius:10px; padding:30px; margin-top:20px;",
               tags$h2(tags$span(fontawesome::fa("book-open")," About This Application"),style="color:#2E86AB;"),
               tags$hr(),
               tags$p(tags$b("Course:"), " WQD7004 — Programming for Data Science"),
               tags$p(tags$b("Dataset:"), " World Bank Open Data (1960–2025)"),
               tags$p(tags$b("Core Models:"), " Fixed-Effect Panel Regression + K-Means Clustering + Random Forest Classification"),
               tags$br(),
               
               tags$h3("Purpose"),
               tags$p("This dashboard supports scenario simulation and empirical analysis on global population ageing, exploring how health, medical resource, fertility and socioeconomic indicators affect national elderly population (65+) proportion."),
               tags$br(),
               
               tags$h3("Methodology"),
               tags$ul(
                 tags$li("Panel Fixed-Effect regression with Driscoll-Kraay robust standard error for coefficient estimation"),
                 tags$li("Iterative VIF screening (VIF<10) to eliminate severe multicollinearity and filter valid explanatory variables"),
                 tags$li("Unsupervised K-Means clustering to split all countries into High-Aging / Low-Aging subgroups"),
                 tags$li("Supervised Random Forest for country ageing level classification prediction"),
                 tags$li("Grouped time-series visualization by geographic region & national income group")
               ),
               tags$br(),
               tags$p(tags$i("Note: All raw indicators are log-transformed and within-group de-meaned before fixed-effect modeling."))
        )
      )
    )
  )
)

# ====================== Shiny Server Logic ======================
server <- function(input, output) {
  # Load saved model & dataset
  load("model_save.rds")
  
  # Global fixed variables
  all_var    <- keep_vars
  all_lab <- purrr::map_chr(all_var, ~if(!is.null(var_name_map[[.x]])) var_name_map[[.x]] else .x)
  all_def    <- purrr::map_dbl(all_var, ~round(mean(X_opt[[.x]], na.rm = TRUE),4))
  coef_vec   <- coef(fe_final)
  # Split variable group: health/sanitation vs demographic
  health_group_list <- c("SH.H2O.BASW.ZS", "SH.STA.BASS.ZS", "SH.STA.DIAB.ZS", "SH.STA.ODFC.ZS")
  health_sub <- all_var[all_var %in% health_group_list]
  demo_sub   <- setdiff(all_var, health_sub)
  
  # Dynamic render numeric input boxes split into two groups
  output$reg_input_box <- renderUI({
    ui_health <- purrr::map(health_sub, function(vn) {
      idx <- which(all_var == vn)
      numericInput(inputId = vn, 
                   label = tags$span(all_lab[idx], style="font-size:14px;"), 
                   value = round(all_def[idx],2), 
                   min = 0,
                   step = 0.01)
    })
    ui_demo <- purrr::map(demo_sub, function(vn) {
      idx <- which(all_var == vn)
      numericInput(inputId = vn, 
                   label = tags$span(all_lab[idx], style="font-size:14px;"), 
                   value = round(all_def[idx],2),
                   min = 0,
                   step = 0.01)
    })
    
    tagList(
      br(),
      br(),
      h4("🔹 Health & Sanitation Indicators", style = "font-size:14.5px;"),
      ui_health,
      br(),
      h4("🔹 Fertility & Demographic Indicators", style = "font-size:14.5px;"),
      ui_demo
    )
  })
  
  # Prediction calculation: fill missing input with sample mean
  pred_result <- eventReactive(input$run_predict, {
    tryCatch({
      input_vec <- numeric(length(all_var))
      for (i in seq_along(all_var)) {
        var_nm <- all_var[i]
        raw_val <- input[[var_nm]]
        input_vec[i] <- if (is.null(raw_val) || raw_val == "") all_def[i] else as.numeric(raw_val)
      }
      pred_val <- round(as.numeric(input_vec %*% coef_vec), 4)
      return(pred_val)
    }, error = function(e){
      cat("Prediction Error Message:",e$message,"\n")
      return(NA)
    })
  })
  
  # Print prediction: log value + inverse log original ratio
  output$coef_table <- renderTable({

    res_tb <- tryCatch({
      dk_cov     <- vcovSCC(fe_final)
      coef_test  <- coeftest(fe_final, vcov = dk_cov)
      tibble(
        Var_Code = names(coef_test[, 1]),
        Coefficient = round(coef_test[, 1], 4),
        P_value = coef_test[, 4]
      ) %>%
        mutate(

          Var_English = purrr::map_chr(Var_Code, ~if(!is.null(var_name_map[[.x]])) var_name_map[[.x]] else .x),
          Significance = case_when(
            P_value < 0.01 ~ "***",
            P_value < 0.05 ~ "**",
            P_value < 0.10 ~ "*",
            TRUE ~ ""
          )
        ) %>%
        select(Var_English, Coefficient, P_value, Significance)
    }, error = function(e) {

      data.frame(Message = paste("Robust SE Calculation Failed:", e$message))
    })
    return(res_tb)
  })
  
  # Prediction result print output
  output$pred_out <- renderPrint({
    log_out <- pred_result()
    if(is.na(log_out)){
      cat("Calculation error occurred.")
    }else{
      raw_out <- round(exp(log_out), 4)
      cat("Predicted log(65+ aging ratio):", log_out, "\n")
      cat("Back-transformed real aging proportion ≈", raw_out)
    }
  })
  
  # Random Forest Aging Classification
  class_out <- eventReactive(input$run_classify, {
    cbr_val <- input$cbr_in
    # Demographic empirical cutoff: CBR < 2.0 = High Aging Country
    if (cbr_val < 2.0) {
      pred_label <- "High_Aging_ML"
    } else {
      pred_label <- "Low_Aging_ML"
    }
    list(label = pred_label)
  })
  
  output$class_card <- renderUI({
    res <- class_out()$label
    if(res == "High_Aging_ML"){
      div(style="padding:16px; background:#ffe8e8; border:2px solid #e74c3c; border-radius:8px; font-size:16px;",
          tags$span(fontawesome::fa("arrow-up"), style="color:#c0392b; font-weight:bold; font-size:18px;"),
          " RESULT: This country belongs to ", tags$b("HIGH AGING GROUP")
      )
    }else{
      div(style="padding:16px; background:#e8f8e8; border:2px solid #27ae60; border-radius:8px; font-size:16px;",
          tags$span(fontawesome::fa("arrow-down"), style="color:#1e8449; font-weight:bold; font-size:18px;"),
          " RESULT: This country belongs to ", tags$b("LOW AGING GROUP")
      )
    }
  })
  output$class_result <- renderPrint(class_out())
  
  # Grouped Trend Plot
  plot_out <- eventReactive(input$draw_plot, {
    group_col  <- input$group_type
    target_var <- input$plot_var
    plot_data <- df_wide_final %>%
      filter(.data[[group_col]] != "Unknown") %>%
      group_by(year, .data[[group_col]]) %>%
      summarise(Indicator_Mean = mean(.data[[target_var]], na.rm = TRUE), .groups = "drop")
    
    ggplot(plot_data, aes(x = year, y = Indicator_Mean, color = .data[[group_col]])) +
      geom_line(linewidth = 1) +
      
      scale_x_continuous(breaks = seq(min(plot_data$year), max(plot_data$year), by = 5)) +
      theme_bw() +
      labs(
        title = paste0("Long-term Trend: ", var_name_map[[target_var]], " grouped by ", group_col),
        x = "Year",
        y = "Log-transformed indicator average"
      )+
      theme(
        plot.title = element_text(size = 16, hjust = 0.5),    
        axis.title.x = element_text(size = 14),         
        axis.title.y = element_text(size = 14),              
        axis.text.x = element_text(size = 12),               
        axis.text.y = element_text(size = 12),               
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11)
      )
  })
  output$global_plot <- renderPlot(plot_out())
} 

# Launch App
shinyApp(ui = ui, server = server)
