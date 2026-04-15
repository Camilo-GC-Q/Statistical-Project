library(shiny)
library(tidyverse)

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
      choices = names(data())
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
    
    y <- data()[[input$response]]
    
    # Remove NA
    y <- y[!is.na(y)]
    
    cat("Count Summary:\n\n")
    cat("Mean:", mean(y), "\n")
    cat("Variance:", var(y), "\n")
    cat("Min:", min(y), "\n")
    cat("Max:", max(y), "\n")
    cat("Proportion of Zeros:", mean(y == 0), "\n")
    })

    # Plot
    output$count_plot <- renderPlot({
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

