library(shiny)
library(cowplot)
source("Lec16DemoFunctions.R")

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
    pop_size <- 250000
    possible_entries <- c(rep("support", round(input$p * pop_size)), 
                          rep("not", round((1 - input$p) * pop_size)))
    possible_entries <- sample(possible_entries)
    return(possible_entries)
  })
  
  observeEvent(input$simulate, {
    output$histPlot <- renderPlot({
      K <- 1000  # Number of simulations 
      population <- population_result()  # Get updated population
      
      # Run simulation
      simulation <- replicate(K, sample_get_phat_fn(population, input$n))
      
      SE_phat <- sqrt((input$p*(1-input$p))/K)
      normal_dist <- rnorm(K, mean=input$p, sd=SE_phat)
      comparison <- create_comparison_data(simulation, normal_dist, K)
      
      # Create histogram plot
      hist <- ggplot(data = data.frame(simulation), 
                     aes(x = simulation)) +
        geom_histogram(bins=100, alpha=0.5, color=4, fill="white") + 
        labs(title = "Histogram of p-hat values",
             x = "Sample Proportion (p-hat)",
             y = "Frequency") + 
        xlim(c(0,1))
      
      # Create density plot
      dens <- ggplot(data=comparison, aes(x=values, color=source, fill=source)) +
        geom_density(lwd=1, alpha=0.25) +
        labs(title = "Distribution comparison",
             x = "Sample Proportion (p-hat)",
             y = "Density") +
        theme(legend.position = c(0.8, 0.8)) +
        xlim(c(0,1))
      
      # Display histogram & density plots together
      plot_grid(hist, dens)
    })
  })
}
