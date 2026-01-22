install.packages("plotly")
library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(readr)

# Dane z pobranego z Colaba pliku csv
url <- "https://raw.githubusercontent.com/awilk16-stack/airline-analysis/refs/heads/main/raport_kompletny_wszystkie_cechy.csv"
df <- read_csv(url)

ui <- fluidPage(
  titlePanel("Analiza modeli szyfrowania"),
  tags$head(tags$style(HTML(".tab-content { padding-top: 20px; }"))),
  
  tabsetPanel(
    tabPanel("Jakość i czas", plotlyOutput("metricsPlot", height = "600px")),
    tabPanel("Macierz pomyłek", plotlyOutput("confusionPlot", height = "650px")),
    tabPanel("Istotność cech", plotlyOutput("importancePlot", height = "800px"))
  )
)

server <- function(input, output) {
  
  #1 wykres
  output$metricsPlot <- renderPlotly({
    metrics_list <- c("Dokładność (Accuracy)", "Precision", "Recall", "F1-score")
    models_names <- c("Standard", "Szyfrowanie HE", "DP")
    col_mapping <- c("MLP_Standard", "Neural_Net_HE_Szyfrowany", "Neural_Net_DP_Opacus")
    
    fig <- plot_ly()
    
    for(m in metrics_list) {
      m_row <- df %>% filter(`Metryka / Cecha` == m)
      vals <- as.numeric(m_row[1, col_mapping])
      
      fig <- fig %>% add_trace(
        x = models_names, 
        y = vals,
        name = m, 
        type = 'scatter', 
        mode = 'lines+markers+text',
        text = round(vals, 3), 
        textposition = "top center",
        cliponaxis = FALSE
      )
    }
    
    time_row <- df %>% filter(`Metryka / Cecha` == "Czas wykonania (sek)")
    time_vals <- as.numeric(time_row[1, col_mapping])
    
    fig <- fig %>% add_trace(
      x = models_names, 
      y = time_vals,
      name = "Czas (s)", 
      type = 'scatter', 
      mode = 'lines+markers+text',
      text = round(time_vals, 1),
      textposition = "bottom center",
      line = list(dash = 'dot', color = 'black'), 
      yaxis = "y2"
    )
    
    fig %>% layout(
      title = list(text = "1. Porównanie jakości modeli i czasu przetwarzania", y = 0.95),
      margin = list(t = 120, b = 100, l = 150, r = 150),
      xaxis = list(
        title = "Model", 
        offset = 50,
        categoryorder = "array",
        categoryarray = models_names
      ),
      yaxis = list(title = "Wartość metryki", range = c(0.85, 1.05)),
      yaxis2 = list(
        overlaying = "y", 
        side = "right", 
        title = "Czas (sekundy)", 
        range = c(0, 160), 
        showgrid = FALSE
      ),
      legend = list(orientation = 'h', x = 0.5, xanchor = 'center', y = -0.2),
      template = "plotly_white"
    )
  })
  
  #2 wykres
  output$confusionPlot <- renderPlotly({
    cm_order <- c("True Negative (TN)", "False Positive (FP)", "False Negative (FN)", "True Positive (TP)")
    labels <- c("TN", "FP", "FN", "TP")
    blue_shades <- c('#08306b', '#2171b5', '#6baed6', '#deebf7')
    
    domains <- list(
      list(x = c(0, 0.3), y = c(0.1, 0.9)),
      list(x = c(0.35, 0.65), y = c(0.1, 0.9)),
      list(x = c(0.7, 1), y = c(0.1, 0.9))
    )
    
    col_names <- c("MLP_Standard", "Neural_Net_HE_Szyfrowany", "Neural_Net_DP_Opacus")
    display_names <- c("Standard", "Szyfrowanie HE", "DP")
    
    fig <- plot_ly()
    for(i in 1:3) {
      vals <- sapply(cm_order, function(m) as.numeric(df[df$`Metryka / Cecha` == m, col_names[i]]))
      
      fig <- fig %>% add_pie(
        labels = labels, values = vals, hole = 0.75,
        name = display_names[i], domain = domains[[i]],
        marker = list(colors = blue_shades, line = list(color = 'white', width = 1)),
        title = list(text = display_names[i], font = list(size = 14)),
        sort = FALSE
      )
    }
    
    fig %>% layout(
      title = list(text = "2. Porównanie błędnych klasyfikacji", y = 0.95),
      margin = list(t = 120, b = 20),
      showlegend = TRUE, template = "plotly_white"
    )
  })
  
  #3 wykres
  output$importancePlot <- renderPlotly({
    #odfiltrowanie wartości, które nie są potrzebne
    not_features <- c("Dokładność (Accuracy)", "Czas wykonania (sek)", "Precision", 
                      "Recall", "F1-score", "True Negative (TN)", "False Positive (FP)", 
                      "False Negative (FN)", "True Positive (TP)")
    
    feat_data <- df %>% filter(!(`Metryka / Cecha` %in% not_features))
    
    fig <- plot_ly(feat_data, orientation = 'h') %>%
      add_trace(y = ~`Metryka / Cecha`, x = ~MLP_Standard, name = 'Standard', type = 'bar', marker = list(color = '#636EFA')) %>%
      add_trace(y = ~`Metryka / Cecha`, x = ~Neural_Net_HE_Szyfrowany, name = 'Szyfrowanie HE', type = 'bar', marker = list(color = '#00CC96')) %>%
      add_trace(y = ~`Metryka / Cecha`, x = ~Neural_Net_DP_Opacus, name = 'DP', type = 'bar', marker = list(color = '#EF553B')) %>%
      layout(
        title = list(text = "3. Ranking istotności cech modelu", y = 0.98),
        margin = list(t = 120, l = 150, r = 50, b = 50),
        barmode = 'group',
        xaxis = list(title = "Wartość wskaźnika"),
        yaxis = list(title = "", autorange = "reversed"),
        legend = list(x = 1, y = 1.02, xanchor = 'right', yanchor = 'bottom', orientation = 'h'),
        template = "plotly_white"
      )
  })
}

shinyApp(ui = ui, server = server)