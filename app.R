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
  "Linear models illustrated",
  tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #1b9e77}")),
  tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #d95f02}")),

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
                      "SD:",
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
                      TRUE)
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
        sliderInput("n_cat",
                    "Sample size:",
                    min = 50,
                    max = 250,
                    value = 150,
                    step = 10),
        splitLayout(
          cellWidths = "50%",
          sliderInput("mean_a",
                      "Mean A:",
                      min = -5,
                      max = 5,
                      value = 0),
          sliderInput("sd_a",
                      "SD A:",
                      min = 0,
                      max = 2,
                      value = 1,
                      step = 0.25)
        ),
        splitLayout(
          cellWidths = "50%",
          sliderInput("mean_b",
                      "Mean B:",
                      min = -5,
                      max = 5,
                      value = 0),
          sliderInput("sd_b",
                      "SD B:",
                      min = 0,
                      max = 2,
                      value = 1,
                      step = 0.25)
        )
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
        splitLayout(
          cellWidths = "50%",
          sliderInput("n_cont_cat",
                      "Sample size:",
                      min = 50,
                      max = 250,
                      value = 150,
                      step = 10),
          sliderInput("error_cont_cat",
                      "SD:",
                      min = 0,
                      max = 1,
                      value = 0.5,
                      step = 0.1)
        ),
        splitLayout(
          cellWidths = "50%",
          sliderInput("int_a",
                      "Intercept A:",
                      min = -5,
                      max = 5,
                      value = 0),
          sliderInput("slope_a",
                      "Slope A:",
                      min = -1,
                      max = 3,
                      step = 0.1,
                      value = 1)
        ),
        splitLayout(
          cellWidths = "50%",
          sliderInput("int_b",
                      "Intercept B:",
                      min = -5,
                      max = 5,
                      value = 0),
          sliderInput("slope_b",
                      "Slope B:",
                      min = -1,
                      max = 3,
                      step = 0.1,
                      value = 1)
        ),
        checkboxInput("raw_cont_cat",
                      "Show raw data",
                      FALSE),
      ),

      mainPanel(
        plotOutput("cont_cat")
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
    # Axes
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    # Show raw data
    {if (input$raw) geom_point(size = 3, alpha = 0.3) } +
    # Regression line
    geom_abline(intercept = the_intercept, slope = the_slope, size = 2) +
    # Intercept
    geom_segment(
      x = -Inf, y = the_intercept, xend = 0, yend = the_intercept,
      linetype = "dotted", colour = "#1b9e77"
    ) +
    # Slope
    geom_segment(
      x = 1, y = -Inf, xend = 1, yend = the_intercept + the_slope,
      linetype = "dotted", colour = "#d95f02"
    ) +
    geom_segment(
      x = 0, y = the_intercept, xend = 1, yend = the_intercept,
      linetype = "dotted", colour = "#d95f02"
    ) +
    geom_segment(
      x = -Inf, y = the_intercept + the_slope, xend = 1, yend = the_intercept + the_slope,
      linetype = "dotted", colour = "#d95f02"
    ) +
    geom_segment(
      x = 1, y = the_intercept, xend = 1, yend = the_intercept + the_slope,
      size = 1, colour = "#d95f02"
    ) +
    # Intercept and slope points
    geom_point(aes(x = 0, y = the_intercept), size = 5, colour = "#1b9e77") +
    geom_point(aes(x = 1, y = the_intercept + the_slope), size = 5, colour = "#d95f02") +
    # Plot settings
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

    n <- input$n_cat
    mean_a <- input$mean_a
    sd_a <- input$sd_a
    mean_b <- input$mean_b
    sd_b <- input$sd_b

    y_a <- rnorm(n/2, mean_a, sd_a)
    y_b <- rnorm(n/2, mean_b, sd_b)
    group <- rep(c("A", "B"), each = n/2)

    tib <- tibble(
      y = c(y_a, y_b),
      group = group
    )

    ggplot(tib, aes(group, y, colour = group)) +
      geom_jitter(width = 0.2, size = 5, alpha = 0.5) +
      scale_y_continuous(breaks = the_seq, limits = the_limits) +
      scale_color_manual(values = c("#1f78b4", "#33a02c")) +
      theme_minimal() +
      theme(legend.position = "none")},

    height = 600, res = 100
  )

  output$cont_cat <- renderPlot({
    set.seed(8788)

    n_cc <- input$n_cont_cat
    n_2 <- input$n_cont_cat / 2
    the_intercept_a <- input$int_a
    the_slope_a <- input$slope_a
    the_intercept_b <- input$int_b
    the_slope_b <- input$slope_b
    the_error <- input$error_cont_cat

    x <- sample(the_seq_1, n_cc)
    y_a <- the_intercept_a + the_slope_a * x[1:n_2] + rnorm(n_2, 0, the_error)
    y_b <- the_intercept_b + the_slope_b * x[(n_2+1):n_cc] + rnorm(n_2, 0, the_error)

    tib <- tibble(
      x = x,
      y = c(y_a, y_b),
      group = rep(c("A", "B"), each = n_cc/2)
    )

    ggplot(tib, aes(x, y, colour = group)) +
      # Axes
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = 0, linetype = "dashed") +
      # Raw data
      {if (input$raw_cont_cat) geom_point(size = 5, alpha = 0.5) } +
      # Regression line A
      geom_abline(intercept = the_intercept_a, slope = the_slope_a, size = 2) +
      geom_point(aes(x = 0, y = the_intercept_a), size = 5, colour = "#a6611a") +
      # Regression line B
      geom_abline(intercept = the_intercept_b, slope = the_slope_b, size = 2) +
      geom_point(aes(x = 0, y = the_intercept_b), size = 5, colour = "#018571") +
      # Plot settings
      scale_x_continuous(breaks = the_seq, limits = the_limits) +
      scale_y_continuous(breaks = the_seq, limits = the_limits) +
      scale_colour_manual(values = c("#a6611a", "#018571")) +
      labs(x = "X", y = "Y") +
      coord_fixed() +
      theme_minimal() +
      theme(legend.position = "none")},

    height = 600, res = 100
  )


}

# Run the application
shinyApp(ui = ui, server = server)
