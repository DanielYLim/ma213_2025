library(shiny)

# ---- UI ----
ui <- fluidPage(
  titlePanel("CLT Demonstration for p-hat"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Sample Size (n):", min=5, max=500, value=10, step=5),
      sliderInput("p", "Population Proportion (p):", min=0.01, max=1, 
                  value=0.25, step=0.01),
      actionButton("simulate", "Run Simulation"),
      hr()
    ), mainPanel(plotOutput("histPlot"))))

# ---- Server ----
server <- function(input, output) {
  
  # Reactive function to create population
  population_result <- reactive({
    # TODO: add N (pop_size) to input - keep same pop. size as display code
    pop_size <- 250000
    possible_entries <- c(rep("support", round(input$p * pop_size)), 
                          rep("not", round((1 - input$p) * pop_size)))
    return(possible_entries)
  })
  
  # Function to sample phat
  sample_get_phat_fn <- function(n, possible_entries) {     
    sampled_entries <- sample(possible_entries, size = n, replace = TRUE)
    phat = sum(sampled_entries == "support")/n
    return(phat)
  }
  
  observeEvent(input$simulate, {
    output$histPlot <- renderPlot({
      # TODO: add K to input - keep same # simulations as display code
      K <- 10000  # Number of simulations 
      possible_entries <- population_result()  # Get updated population
      
      # Run simulation:
      Sample_proportions <- replicate(K, sample_get_phat_fn(input$n, 
                                                            possible_entries))
      
      # Plot histogram:
      # TODO: update visuals (color, etc) to be same as display code
      ggplot(data = data.frame(Sample_proportions), 
             aes(x = Sample_proportions)) +
        geom_histogram(bins = input$n, fill = "blue", alpha = 0.5, 
                       color = "black") +
        labs(title = "Histogram of Sample Proportions (p-hat)",
             x = "Sample Proportion (p-hat)",
             y = "Frequency") + 
        xlim(c(0, 1)) + 
        theme_minimal()
    })
  })
}