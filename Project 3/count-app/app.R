library(shiny)
library(tidyverse)
list.files("R", full.names = TRUE) |> purrr::walk(source)

ui = fluidPage(
    titlePanel("Count Regression Toolkit"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Upload CSV File", accept = ".csv"),
            uiOutput("response_ui")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Data Preview",
                        tableOutput("preview")
                ),
                tabPanel("Count Summary",
                        verbatimTextOutput("summary_stats"),
                        plotOutput("count_plot")
                )
            )
        )
    )
)


server = function(input, output, session){
    # Load Data
    data = reactive({
        req(input$file)
        read_csv(input$file$datapath)
    })

    output$response_ui <- renderUI({
    req(data())
    
    selectInput(
      "response",
      "Select Response Variable (Count)",
      choices = names(data())[sapply(data(), is.numeric)]
    )
    })

    # Data Preview
    output$preview = renderTable({
        req(data())
        head(data())
    })

     # Summary stats
    output$summary_stats <- renderPrint({
        req(data(), input$response)
        
        s <- get_count_summary(data(), input$response)
        
        cat("Mean:", s$mean, "\n")
        cat("Variance:", s$variance, "\n")
        cat("Min:", s$min, "\n")
        cat("Max:", s$max, "\n")
        cat("Proportion of zeros:", s$zero_prop, "\n")
    })

    # Plot
    output$count_plot = renderPlot({
    req(data(), input$response)
    
    df <- data()
    y <- df[[input$response]]
    
    ggplot(df, aes(x = y)) +
      geom_bar(fill = "steelblue") +
      labs(
        title = "Count Distribution",
        x = "Count Value",
        y = "Frequency"
      ) +
      theme_minimal()
  })
}

shinyApp(ui, server)

