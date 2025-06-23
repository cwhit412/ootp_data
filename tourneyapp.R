
#need to fix sorting of tables
#default when click name to checkbox
# Load data
requireNamespace('shiny')
requireNamespace('dplyr')
requireNamespace('ggplot2')
requireNamespace('DF')


#put path to file here, then can run all
data <- read.csv('path')


# Calculate max tournaments per team
max_tourneys <- data %>%
  group_by(team) %>%
  summarise(total = sum(totaltourney)) %>%
  summarise(max_total = max(total)) %>%
  pull(max_total)

ui <- fluidPage(
  titlePanel("OOTP PT 26 Tournament Performance Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("team", "Select Team(s) (optional):",
                     choices = sort(unique(data$team)),
                     selected = NULL,
                     multiple = TRUE,
                     options = list(placeholder = 'Select one or more teams...')),
      
      uiOutput("tier_ui"),
      uiOutput("title_ui"),
      
      selectInput("cycle", "Select PTCS Cycle:",
                  choices = c("All", sort(unique(data$ptcscycle)))),
      
      checkboxInput("group_by_tier", "Group by Tier", value = FALSE),
      checkboxInput("group_by_title", "Group by Title", value = FALSE),
      checkboxInput("group_by_cycle", "Group by PTCS Cycle", value = FALSE),
      
      numericInput("minTourneyBox", "Minimum Tournaments:", value = 5, min = 1, max = max_tourneys, step = 1),
      sliderInput("minTourneySlider", NULL, min = 1, max = max_tourneys, value = 5)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary Table", DTOutput("summaryTable")),
        tabPanel("Top Teams by Wins", plotOutput("topTeamsPlot")),
        tabPanel("Performance Over Time", plotOutput("teamPerformancePlot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Sync slider and numeric box
  observeEvent(input$minTourneySlider, {
    updateNumericInput(session, "minTourneyBox", value = input$minTourneySlider)
  })
  
  observeEvent(input$minTourneyBox, {
    updateSliderInput(session, "minTourneySlider", value = input$minTourneyBox)
  })
  
  output$tier_ui <- renderUI({
    available <- data
    if (!is.null(input$team)) {
      available <- available %>% filter(team %in% input$team)
    }
    selectInput("tier", "Select Tier (optional):",
                choices = c("All", sort(unique(available$tier))),
                selected = "All")
  })
  
  output$title_ui <- renderUI({
    available <- data
    if (!is.null(input$team)) {
      available <- available %>% filter(team %in% input$team)
    }
    if (!is.null(input$tier) && input$tier != "All") {
      available <- available %>% filter(tier == input$tier)
    }
    selectInput("title", "Select Title (optional):",
                choices = c("All", sort(unique(available$title))),
                selected = "All")
  })
  
  filtered_data <- reactive({
    df <- data
    
    if (!is.null(input$team) && length(input$team) > 0) {
      df <- df %>% filter(team %in% input$team)
    }
    
    if (!is.null(input$tier) && input$tier != "All") {
      df <- df %>% filter(tier == input$tier)
    }
    
    if (!is.null(input$title) && input$title != "All") {
      df <- df %>% filter(title == input$title)
    }
    
    if (input$cycle != "All") {
      df <- df %>% filter(ptcscycle == as.numeric(input$cycle))
    }
    
    df
  })
  
  summary_data <- reactive({
    df <- filtered_data()
    group_vars <- c()
    
    if (is.null(input$team) || length(input$team) > 1) {
      group_vars <- c(group_vars, "team")
    }
    if (input$group_by_tier) {
      group_vars <- c(group_vars, "tier")
    }
    if (input$group_by_title) {
      group_vars <- c(group_vars, "title")
    }
    if (input$group_by_cycle) {
      group_vars <- c(group_vars, "ptcscycle")
    }
    
    df %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        `Total Tournaments` = sum(totaltourney),
        `Average Rounds Won` = round(mean(avgroundswon, na.rm = TRUE), 2),
        `Total Wins` = sum(numberwins),
        .groups = "drop"
      ) %>%
      filter(`Total Tournaments` >= input$minTourneyBox) %>%
      arrange(desc(`Total Wins`), desc(`Average Rounds Won`))
  })
  
  output$summaryTable <- renderDT({
    datatable(summary_data(), selection = "single", options = list(pageLength = 25))
  })
  
  observeEvent(input$summaryTable_rows_selected, {
    selected_row <- input$summaryTable_rows_selected
    if (length(selected_row)) {
      selected_data <- summary_data()[selected_row, ]
      
      if ("team" %in% colnames(selected_data)) {
        updateSelectizeInput(session, "team", selected = selected_data$team)
      }
      if ("tier" %in% colnames(selected_data)) {
        updateSelectInput(session, "tier", selected = selected_data$tier)
        updateCheckboxInput(session, "group_by_tier", value = TRUE)
      }
      if ("title" %in% colnames(selected_data)) {
        updateSelectInput(session, "title", selected = selected_data$title)
        updateCheckboxInput(session, "group_by_title", value = TRUE)
      }
      if ("ptcscycle" %in% colnames(selected_data)) {
        updateSelectInput(session, "cycle", selected = as.character(selected_data$ptcscycle))
        updateCheckboxInput(session, "group_by_cycle", value = TRUE)
      }
    }
  })
  
  output$topTeamsPlot <- renderPlot({
    df <- summary_data()
    if ("team" %in% names(df)) {
      top_teams <- df %>% top_n(10, wt = `Total Wins`)
      ggplot(top_teams, aes(x = reorder(team, `Total Wins`), y = `Total Wins`)) +
        geom_col(fill = "#3B82F6") +
        coord_flip() +
        labs(title = "Top 10 Teams by Wins", x = "Team", y = "Wins") +
        theme_minimal(base_size = 14)
    }
  })
  
  output$teamPerformancePlot <- renderPlot({
    df <- filtered_data()
    
    df_sum <- df %>%
      group_by(team, ptcscycle) %>%
      summarise(
        total_tournaments = sum(totaltourney),
        avg_rounds_won = round(mean(avgroundswon, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      filter(total_tournaments >= input$minTourneyBox)
    
    if (!is.null(input$team) && length(input$team) == 1) {
      df_sum <- df_sum %>% filter(team == input$team)
    } else {
      top_teams <- summary_data() %>%
        top_n(10, wt = `Total Wins`) %>%
        pull(team)
      df_sum <- df_sum %>% filter(team %in% top_teams)
    }
    
    ggplot(df_sum, aes(x = ptcscycle, y = avg_rounds_won, color = team)) +
      geom_line(size = 1.2) +
      geom_point() +
      labs(title = "Average Rounds Won Over Time",
           x = "PTCS Cycle", y = "Avg Rounds Won") +
      theme_minimal(base_size = 14)
  })
  
}

shinyApp(ui, server)
