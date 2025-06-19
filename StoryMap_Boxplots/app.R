if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, ggplot2, scales, tidyverse, ggiraph, rsconnect, openxlsx, shinyWidgets, bs4Dash)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load data
data_mets <- read.xlsx("data/BugMetrics_4app.xlsx")

data_mets_long <- 
  data_mets %>%
  pivot_longer(names_to = "MetricName",
               values_to = "MetricValue",
               cols = -c(1:4)) %>%
  mutate(AREA_NM = gsub("_", " ", AREA_NM)) %>%
  filter(!is.na(DistCat3))

metric_perf <- read.xlsx("data/Metric19Performance.xlsx")
metric_perf_f <- 
  metric_perf %>%
  filter(Metric_Name %in% data_mets_long$MetricName) %>%
  mutate(DE = round(DE, 1))

# UI
# UI
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .custom-select {
        font-size: 18px !important;
        width: 200px !important;
        margin: 0 auto;
        display: block;
      }
      .white-background {
        background-color: white;
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 0 5px rgba(0,0,0,0.1);
      }
      .center-vertical {
      display: flex;
      flex-direction: column;
      justify-content: center;
      height: 100%;
      min-height: 700px; /* Match plot height or adjust as needed */
  }
      .sidebar-white {
        background-color: white !important;
        padding: 20px;
        border-radius: 8px;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-white",
      tags$div(
        tags$div(
          style = "text-align: center; font-size: 36px; font-weight: bold; margin-bottom: 15px;",
          "Select A Metric"
        ),
        tags$div(
          class = "custom-select",
          selectInput(
            inputId = "metric_sel",
            label = NULL,
            choices = unique(data_mets_long$MetricName),
            selected = unique(data_mets_long$MetricName)[1],
            selectize = FALSE,
            width = "200px"
          )
        ),
        br(),
        uiOutput("metric_panel")  # Moved here
      )
    ),
    
    mainPanel(
      girafeOutput("metric_boxplot", height = "1100px", width = "800px")
    )
 )
)



# Server
server <- function(input, output, session) {
  
  output$metric_boxplot <- renderGirafe({
    plot_data <- data_mets_long %>% filter(MetricName == input$metric_sel)
    
    # Step 1: Compute summary stats
    stats <- plot_data %>%
      group_by(DistCat3) %>%
      summarise(
        mean = round(mean(MetricValue, na.rm = TRUE), 1),
        median = round(median(MetricValue, na.rm = TRUE), 1),
        q1 = round(quantile(MetricValue, 0.25, na.rm = TRUE), 1),
        q3 = round(quantile(MetricValue, 0.75, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      mutate(
        tooltip = paste0(
          "<b>", DistCat3, "</b><br>",
          "Mean: ", mean, "<br>",
          "Median: ", median, "<br>",
          "Q1: ", q1, "<br>",
          "Q3: ", q3
        )
      )
    
    # Step 2: Merge tooltip info back to plot_data
    plot_data <- left_join(plot_data, stats, by = "DistCat3")
    
    # Step 3: Create interactive plot
    p <- ggplot(plot_data, aes(x = DistCat3, y = MetricValue, fill = DistCat3)) +
      geom_boxplot_interactive(
        aes(tooltip = tooltip),
        outlier.shape = NA, color = "black", alpha = 1, linewidth = 2
      ) +
      labs(y = "Scaled Metric Value (0–100)") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(0, 100)) +
      scale_fill_manual(values = c("#009E73", "#0072B2", "#D55E00")) +
      theme_bw() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(size = 20, color = "black", face = "bold"),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 18, color = "black"),
        legend.position = "none",
        panel.grid.major.y = element_line(color = "grey90", size = 0.3)
      )
    
    girafe(ggobj = p, width_svg = 10, height_svg = 12)
  })
  
  
  
  output$metric_panel <- renderUI({
    dir <- metric_perf_f$Direction[metric_perf_f$Metric_Name == input$metric_sel]
    de <- metric_perf_f$DE[metric_perf_f$Metric_Name == input$metric_sel]
    
    de <- paste0("DE: ", de, "%")
    
    tagList(
      div(
        style = "width: 60%; margin: 0 auto;",  # Adjust width as needed
        panel(
          title = "Direction of Effect",
          status = ifelse(dir == "Positive", "success", ifelse(dir == "Negative", "danger", "info")),
          solidHeader = TRUE,
          collapsible = FALSE,
          div(
            style = "font-size: 24px; font-weight: bold; color: #2c3e50; padding: 6px; text-align: center;",
            dir
          )
        )
      ),
      div(
        style = "width: 60%; margin: 20px auto 0 auto;",  # Adds spacing between panels
        panel(
          title = "DE Score",
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          div(
            style = "font-size: 24px; font-weight: bold; color: #2c3e50; padding: 6px; text-align: center;",
            de
          )
        )
      )
    )
  })
  
  
}

# Run the app
shinyApp(ui = ui, server = server)
