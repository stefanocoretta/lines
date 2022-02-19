#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#  http://shiny.rstudio.com/
#

library(shiny)
library(tibble)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- navbarPage(

  # Application title
  "A linear model",

  tabPanel(
    "Continuous",
    sidebarLayout(
      sidebarPanel(
        splitLayout(
          cellWidths = "50%",
          sliderInput("n",
                      "Sample size:",
                      min = 50,
                      max = 250,
                      value = 150,
                      step = 10),
          sliderInput("error",
                      "Measurement error (SD):",
                      min = 0,
                      max = 1,
                      value = 0.5,
                      step = 0.1)
        ),

        splitLayout(
          cellWidths = "50%",
          sliderInput("intercept",
                "Intercept:",
                min = -5,
                max = 5,
                value = 0),
          sliderInput("slope",
                "Slope:",
                min = -1,
                max = 3,
                step = 0.1,
                value = 1)
        ),
        checkboxInput("raw",
                      "Show raw data",
                      FALSE)
      ),

      mainPanel(
        plotOutput("continuous")
      )
    )
  ),
  tabPanel(
    "Categorical",
    sidebarLayout(
      sidebarPanel(
        sliderInput("n",
                    "Sample size:",
                    min = 10,
                    max = 200,
                    value = 100,
                    step = 2),
        checkboxInput("raw",
                      "Show raw data",
                      FALSE),
        h3("Group A"),
        sliderInput("intercept",
                    "Intercept (A):",
                    min = -5,
                    max = 5,
                    value = 0),
        sliderInput("slope",
                    "Slope (A):",
                    min = -1,
                    max = 3,
                    step = 0.1,
                    value = 1),
        h3("Group B"),
        checkboxInput("group_b",
                      "Include Group B?",
                      FALSE),
        sliderInput("intercept_b",
                    "Intercept (B):",
                    min = -5,
                    max = 5,
                    value = 0),
        sliderInput("slope_b",
                    "Slope (B):",
                    min = -1,
                    max = 3,
                    step = 0.1,
                    value = 1),
        h3("Model formula"),
        checkboxInput("interactions",
                      "Include the interaction X*Group?",
                      FALSE),
      ),

      mainPanel(
        plotOutput("categorical")
      )
    )
  ),
  tabPanel(
    "Continuous + Categorical",
    sidebarLayout(
      sidebarPanel(
        sliderInput("n",
                    "Sample size:",
                    min = 10,
                    max = 200,
                    value = 100,
                    step = 2),
        checkboxInput("raw",
                      "Show raw data",
                      FALSE),
        h3("Group A"),
        sliderInput("intercept",
                    "Intercept (A):",
                    min = -5,
                    max = 5,
                    value = 0),
        sliderInput("slope",
                    "Slope (A):",
                    min = -1,
                    max = 3,
                    step = 0.1,
                    value = 1),
        h3("Group B"),
        checkboxInput("group_b",
                      "Include Group B?",
                      FALSE),
        sliderInput("intercept_b",
                    "Intercept (B):",
                    min = -5,
                    max = 5,
                    value = 0),
        sliderInput("slope_b",
                    "Slope (B):",
                    min = -1,
                    max = 3,
                    step = 0.1,
                    value = 1),
        h3("Model formula"),
        checkboxInput("interactions",
                      "Include the interaction X*Group?",
                      FALSE),
      ),

      mainPanel(
        plotOutput("lines")
      )
    )
  )
)

server <- function(input, output) {

  the_limits <- c(-5, 5)
  the_seq <- seq(-5, 5)
  the_seq_1 <- seq(-5, 5, by = 0.01)

  output$continuous <- renderPlot({
    set.seed(8788)

    n <- input$n
    the_intercept <- input$intercept
    the_slope <- input$slope
    the_error <- input$error

    x <- sample(the_seq_1, n)
    y <- the_intercept + the_slope * x + rnorm(n, 0, the_error)

    tib <- tibble(
      x = x,
      y = y
    )

    ggplot(tib, aes(x, y)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    {if (input$group_b) aes(colour = group) } +
    {if (input$raw) geom_point(size = 5, alpha = 0.5) } +
    geom_abline(intercept = input$intercept, slope = input$slope, size = 2) +
    geom_point(aes(x = 0, y = input$intercept), size = 5, colour = "red") +
    {if (input$group_b & !input$interactions) geom_abline(intercept = input$intercept_b, slope = input$slope, size = 2) } +
    {if (input$group_b & input$interactions) geom_abline(intercept = input$intercept_b, slope = input$slope_b, size = 2) } +
    {if (input$group_b) geom_point(aes(x = 0, y = input$intercept_b), size = 5, colour = "blue") } +
    scale_x_continuous(breaks = the_seq, limits = the_limits) +
    scale_y_continuous(breaks = the_seq, limits = the_limits) +
    labs(x = "X", y = "Y") +
    coord_fixed() +
    theme_minimal() +
    theme(legend.position = "none")},

    height = 600, res = 100
  )

  output$categorical <- renderPlot({
    set.seed(8788)
    the_limits <- c(-5, 5)
    the_seq <- seq(-5, 5)

    n <- input$n
    x <- sample(seq(-5, 5, by = 0.01), n)
    y_a <- input$intercept + input$slope * x[1:(n/2)] + rnorm(n/2, 0, 0.5)
    if (input$group_b) {
      y_b <- input$intercept_b + input$slope_b * x[((n/2)+1):n] + rnorm(n/2, 0, 0.5)
    } else {
      y_b <- input$intercept + input$slope * x[((n/2)+1):n] + rnorm(n/2, 0, 0.5)
    }

    group <- rep(c("A", "B"), each = n/2)
    tib <- tibble(
      x = x,
      y = c(y_a, y_b),
      group = group
    )

    ggplot(tib, aes(x, y)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = 0, linetype = "dashed") +
      {if (input$group_b) aes(colour = group) } +
      {if (input$raw) geom_point(size = 5, alpha = 0.5) } +
      geom_abline(intercept = input$intercept, slope = input$slope, size = 2) +
      geom_point(aes(x = 0, y = input$intercept), size = 5, colour = "red") +
      {if (input$group_b & !input$interactions) geom_abline(intercept = input$intercept_b, slope = input$slope, size = 2) } +
      {if (input$group_b & input$interactions) geom_abline(intercept = input$intercept_b, slope = input$slope_b, size = 2) } +
      {if (input$group_b) geom_point(aes(x = 0, y = input$intercept_b), size = 5, colour = "blue") } +
      scale_x_continuous(breaks = the_seq, limits = the_limits) +
      scale_y_continuous(breaks = the_seq, limits = the_limits) +
      labs(x = "X", y = "Y") +
      coord_fixed() +
      theme_minimal() +
      theme(legend.position = "none")},

    height = 600, res = 100
  )


}

# Run the application
shinyApp(ui = ui, server = server)
