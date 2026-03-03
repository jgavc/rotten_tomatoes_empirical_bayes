library(shiny)
library(tidyverse)
library(DT)

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
      numericInput('rating', 'Tomatometer Rating', 90, min = 0, max = 100),
      numericInput('num_reviews', 'Number of Critic Reviews', 10, min = 1, max = 1000),
      br(),
      tags$hr(),
      tags$p(
        "With fewer reviews, ratings shrink toward the typical rating distribution.",
        style = "font-size: 12px; color: #666;"
      )
      
    ),
    mainPanel(
      # --- Top summary "hero" area ---
      fluidRow(
        column(
          12,
          wellPanel(
            tags$div(
              style = "
      display:flex;
      gap: 24px;
      align-items: stretch;
      justify-content: space-between;
    ",
              
              # Block 1
              tags$div(
                style = "flex:1; min-width: 0;",
                tags$div("Adjusted Tomatometer", style = "font-size:12px; color:#666; margin-bottom:6px;"),
                tags$div(textOutput("adj_rating_big"),
                         style = "font-size:30px; font-weight:700; line-height:1;")
              ),
              
              # Block 2
              tags$div(
                style = "flex:1; min-width: 0;",
                tags$div("95% credible interval", style = "font-size:12px; color:#666; margin-bottom:6px;"),
                tags$div(textOutput("ci_big"),
                         style = "font-size:30px; font-weight:700; line-height:1;")
              ),
              
              # Block 3 (right-aligned feels nice)
              tags$div(
                style = "flex:1; min-width: 0; text-align:right;",
                tags$div("Change vs raw", style = "font-size:12px; color:#666; margin-bottom:6px;"),
                tags$div(htmlOutput("delta_big"),
                         style = "font-size:30px; font-weight:700; line-height:1;")
              )
            )
          )
        )
      ),
      
      # --- Plot ---
      fluidRow(
        column(
          12,
          plotOutput("plot1", height = "380px")
        )
      )
    )))

server <- function(input, output, server) {
  posterior <- reactive(posterior_params_mixture(round(input$rating/100*input$num_reviews),input$num_reviews,
                                                 mm_critic$pi,mm_critic$alpha, mm_critic$beta))
  sample <- reactive(sample_from_stored_posterior(w1 = posterior()$w[1], a1 = posterior()$post_alpha[1], b1 = posterior()$post_beta[1],
                                                  a2 = posterior()$post_alpha[2], b2 = posterior()$post_beta[2], S = 10000))
  df <- reactive(tibble(draws = sample()))
  quantiles <- reactive(quantile(df() |> pull(draws),c(.025,.975)))
  exp_value <- reactive(mean(sample())*100)
  output$stat_table <- renderDT(tibble(exp = round(exp_value(),2), lower = round(quantiles()[1]*100,2), upper = round(quantiles()[2]*100,2)),
                                colnames = c("New Rating","2.5%","97.5%"), options = list(searching = FALSE, lengthChange = FALSE,
                                                                                                    paging = FALSE, info = FALSE),
                                rownames = FALSE
                                )
  output$adj_rating_big <- renderText(round(exp_value(),2))
  output$ci_big <- renderText(str_c("(",round(quantiles()[1]*100,2),",",round(quantiles()[2]*100,2),")"))
  
  change <- reactive(exp_value() - input$rating)
  line_color <- reactive(ifelse(change() > 0,"green","red"))
  output$delta_big <- renderUI({
    delta <- exp_value() - input$rating
    sign <- ifelse(delta >= 0, "+", "")
    col  <- ifelse(delta >= 0, "green", "red")
    HTML(sprintf("<span style='color:%s;'>%s%.2f pts</span>", col, sign, delta))
  })
  
  print_posterior <- reactive(print(posterior))
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