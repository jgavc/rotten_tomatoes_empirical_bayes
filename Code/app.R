library(shiny)
library(tidyverse)

#Loading Data
modeling_data <- readRDS("D:/Portfolio Projects/rotten_tomatoes_empirical_bayes/Data/Processed/mixed_beta_output.RData")
posterior_stats <- readRDS("D:/Portfolio Projects/rotten_tomatoes_empirical_bayes/Data/Processed/posterior_stats.RData")
load("D:/Portfolio Projects/rotten_tomatoes_empirical_bayes/Data/Processed/movie_df.RData")
mixed_beta_output <- readRDS("D:/Portfolio Projects/rotten_tomatoes_empirical_bayes/Data/Processed/mixed_beta_output.RData")
critic_posterior_stats <- posterior_stats$critic_posterior_stats
mm_critic <- mixed_beta_output$mm_critic

#Loading Function
helper_functions <- readRDS("D:/Portfolio Projects/rotten_tomatoes_empirical_bayes/Data/Functions/fit_mixed_beta_helpers.RData")
sample_from_stored_posterior <- helper_functions$sample_from_stored_posterior
mix_beta_density <- helper_functions$mix_beta_density
posterior_params_mixture <- helper_functions$posterior_params_mixture

ui <- fluidPage(
  pageWithSidebar(
    headerPanel('Empirical Bayes Rating Adjustment'),
    sidebarPanel(
      numericInput('rating', 'Tomatometer Rating', 50, min = 0, max = 100),
      numericInput('num_reviews', 'Number of Critic Reviews', 10, min = 1, max = 1000),
    ),
    mainPanel(
      textOutput("sample1"),
      plotOutput('plot1')
    )
  ))

server <- function(input, output, server) {
  posterior <- reactive(posterior_params_mixture(round(input$rating/100*input$num_reviews),input$num_reviews,
                                                 mm_critic$pi,mm_critic$alpha, mm_critic$beta))
  sample <- reactive(sample_from_stored_posterior(w1 = posterior()$w[1], a1 = posterior()$post_alpha[1], b1 = posterior()$post_beta[1],
                                                  a2 = posterior()$post_alpha[2], b2 = posterior()$post_beta[2], S = 10000))
  df <- reactive(tibble(draws = sample()))
  quantiles <- reactive(quantile(df() |> pull(draws),c(.025,.975)))
  exp_value <- reactive(mean(sample())*100)
  change <- reactive(exp_value() - input$rating)
  line_color <- reactive(ifelse(change() > 0,"green","red"))
  
  print_posterior <- reactive(print(posterior))
  output$sample1 <- renderText(quantiles())
  output$plot1 <- renderPlot(ggplot(data = df(), aes(x = draws*100)) + 
                               geom_density() +
                               geom_vline(xintercept = quantiles()[1:2]*100, color = "red", linetype = "dashed") +
                               xlim(0,100) +
                               theme_minimal() +
                               labs(x = "Adjusted Tomatometer", title = "Adjusted Tomatometer Density",
                                    subtitle = "Red lines are 95% credible interval") +
                               annotate("segment", x = input$rating, xend = exp_value(), y = 0,
                                        arrow = arrow(length = unit(.2,"cm")), color = line_color()))
  observe({
    cat("exp_value:", exp_value(), "\n")
    cat("input rating:", input$rating, "\n")
    cat("change:", change(), "\n\n")
  })
                               
}

shinyApp(ui, server)