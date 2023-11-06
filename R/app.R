#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# 0. Midterm scheduling

# 1. Clean up the table column names X
# 2. Allow multiple brief title keywords X
# 3. Create a histogram of the phase
# 4. Organize files.
# 5. Fix the Phase plot
# 6. Plot the concurrent studies (adding a feature/capability).

# Steps to adding a feature:
# 1. Specify the feature.
#   - What does it do?
#   - What will be shown?
# 2. Specify the interface
#   - Where should it go?
#   - Does it enhance the user experience?
# 3. Implement the feature without the UI
# 4. Integrate the feature.

source("ct-util.R")
max_num_studies <- 1000

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Clinical Trials Query"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("brief_title_kw", "Brief title keywords"),

      # Problem 3: Add a drop-down so that queries can be subset-ted on sponsor type
      selectInput("sponsor_type", "Sponsor Type:",
                  c('Both', sponsor_types_u$lead_or_collaborator)),
      
      selectInput("top_n_conditions", "Top N Trial Conditions to Query",
                  choices=seq(4, 16), selected=6)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        id = "tabs",
        type = "tabs",
        tabPanel("Phase", plotOutput("phase_plot"), value="phase"),
        tabPanel("Concurrent", plotOutput("concurrent_plot"), value="concurrent"),
        tabPanel("Trial Conditions", plotOutput("trial_conditions_plot"), value="conditions"),
        tabPanel("Trial DB Search", dataTableOutput("trial_search_table"), value="search")
       ),
      
      uiOutput("summary_table")
    )
  )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  phase_table = reactiveVal()
  phase_plot = reactiveVal()
  concurrent_table = reactiveVal()
  concurrent_plot = reactiveVal()
  trial_conditons_table = reactiveVal()
  trial_conditons_plot = reactiveVal()
  trial_data = reactiveVal()
  
  listen_to <- reactive({
    list(input$brief_title_kw, input$sponsor_type, input$top_n_conditions)
  })
  observeEvent(listen_to(), {
  # get_studies = reactive({
    if (input$brief_title_kw != "") {
      si = input$brief_title_kw |>
           strsplit(",") |>
           unlist() |>
           trimws()
      ret = query_kwds(STUDIES, si, "brief_title", match_all = TRUE)
    } else {
      ret = STUDIES
    }


    # P3: join with sponsors table and filter sponsor type input
    if (input$sponsor_type != 'Both') {
      sponsor_type <- input$sponsor_type
      
      studies_df <- SPONSORS |>
        select(-c(id, agency_class)) |>
        dplyr::inner_join(y=ret, by = 'nct_id', copy=TRUE) |>
        dplyr::rename(sponsor_name=name) |>
        dplyr::filter(lead_or_collaborator == sponsor_type) |>
        head(max_num_studies) |>
        collect()

    } else {
      studies_df <- ret |>
        head(max_num_studies) |>
        collect()
    }

    study_phases <- plot_phase_histogram(studies_df=studies_df)
    phase_table(study_phases$study_phase_df)
    phase_plot(study_phases$phase_plot)

    concurrent_studies <- plot_concurrent_studies(studies_df=studies_df)
    concurrent_table(concurrent_studies$concurrent_df)
    concurrent_plot(concurrent_studies$concurrent_line_plot)
  
    top_n_conditions <- as.numeric(input$top_n_conditions)
    trial_conditions <- plot_study_conditions_histogram(
      studies_df=studies_df, top_n=top_n_conditions)
    trial_conditons_table(trial_conditions$summ_df)
    trial_conditons_plot(trial_conditions$study_cond_plot)

    trial_df <- studies_df |>
      select(nct_id, brief_title, start_date, completion_date) |>
      rename(`NCT ID` = nct_id, `Brief Title` = brief_title,
             `Start Date` = start_date, `Completion Date` = completion_date)
    trial_data(trial_df)

  }, ignoreNULL = FALSE)
  
  output$phase_table <-  renderDataTable({
    phase_table()
  })
  
  output$phase_plot = renderPlot({
    phase_plot()
  })
  
  output$concurrent_table = renderDataTable({
    concurrent_table()
  })
  
  output$concurrent_plot = renderPlot({
    concurrent_plot()
  })
  

  # Problem 2
  # Add a new tab that gives a histogram showing the conditions that trials in a query are examining.
  output$trial_conditons_table = renderDataTable({
    trial_conditons_table()
  })
  
  output$trial_conditions_plot = renderPlot({
    trial_conditons_plot()
  })
  
  output$trial_search_table = renderDataTable({
    trial_data()
  })
  
  output$summary_table <- renderUI({
    tab_name <- input$tabs
    if (tab_name == "phase") {
      DT::renderDataTable(phase_table())
    } else if (tab_name == "concurrent") {
      DT::renderDataTable(concurrent_table())
    } else if (tab_name == "conditions") {
      DT::renderDataTable(trial_conditons_table())
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
