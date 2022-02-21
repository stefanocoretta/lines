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
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #1b9e77}")),
  tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #d95f02}")),
  tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: #1b9e77}")),
  tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: #d95f02}")),
  tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$']]}
            });
            </script >
            ")),

  tabPanel(
    "Continuous",
    sidebarLayout(
      sidebarPanel(
        h3("Continuous predictor X"),
        h4("Instructions"),
        p("You can adjust the intercept and slope values below to see what happens to the regression line (the solid black line)."),
        p("Note that the sample size and SD settings do not affect the regression line. Rather, those settings can be changed to generate raw data based on the given intercept and slope values."),
        h4("Formula"),
        p(withMathJax("$$y \\sim \\beta_0 + \\beta_1 x$$")),
        tags$ul(
          tags$li("$y$: outcome variable"),
          tags$li("$\\beta_0$: intercept"),
          tags$li("$\\beta_1$: slope"),
          tags$li("$x$: predictor X")
        ),
        h4("Settings"),

        splitLayout(
          cellWidths = "50%",
          sliderInput("intercept",
                      withMathJax("Intercept $\\beta_0$:"),
                      min = -5,
                      max = 5,
                      value = 0),
          sliderInput("slope",
                      "Slope $\\beta_1$:",
                      min = -3,
                      max = 3,
                      step = 0.5,
                      value = 1)
        ),

        p("You can adjust the following settings to generate random data based on the given intercept and slope values."),
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
        checkboxInput("raw",
                      "Show generated data",
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
        h3("Categorical predictor Group"),
        h4("Instructions"),
        p("You can adjust the intercept and slope values below to see what happens to the means of Group A and B (the big diamonds)."),

        sliderInput("n_cat",
                    "Sample size:",
                    min = 50,
                    max = 250,
                    value = 150,
                    step = 10),
        selectInput("coding",
                    "Coding:",
                    c("Treatment/dummy" = "treat", "Sum/effect" = "sum")),
        splitLayout(
          cellWidths = "50%",
          sliderInput("int_cat",
                      "Intercept:",
                      min = -5,
                      max = 5,
                      value = 0),
          sliderInput("slope_cat",
                      "Slope:",
                      min = -3,
                      max = 3,
                      step = 0.5,
                      value = 0)
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
        h3("Continuous and categorical predictors"),

        h4("Formula"),
        p(withMathJax("$$y \\sim \\beta_0 + \\beta_1 x + \\beta_2 \\cdot group + \\beta_3 x \\cdot group$$")),
        tags$ul(
          tags$li("$y$: outcome variable"),
          tags$li("$\\beta_0$: mean Y of Group A"),
          tags$li("$\\beta_1$: effect of X in Group A"),
          tags$li("$x$: predictor X"),
          tags$li("$\\beta_2$: Group B mean Y - Group A mean Y"),
          tags$li("$group$: predictor Group"),
          tags$li("$\\beta_3$: effect of X in Group B - effect of X in Group A")
        ),

        h4("Settings"),
        splitLayout(
          cellWidths = "50%",
          sliderInput("beta_0",
                      "$\\beta_0$:",
                      min = -5,
                      max = 5,
                      step = 0.5,
                      value = 0),
          sliderInput("beta_1",
                      "$\\beta_1$:",
                      min = -3,
                      max = 3,
                      step = 0.1,
                      value = 1)
        ),
        splitLayout(
          cellWidths = "50%",
          sliderInput("beta_2",
                      "$\\beta_2$:",
                      min = -3,
                      max = 3,
                      step = 0.1,
                      value = 0),
          sliderInput("beta_3",
                      "$\\beta_3$:",
                      min = -3,
                      max = 3,
                      step = 0.1,
                      value = 0)
        ),

        p("You can adjust the following settings to generate random data based on the given parameter values."),
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
        checkboxInput("raw_cont_cat",
                      "Show generated data",
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
    the_intercept <- input$int_cat
    the_slope <- input$slope_cat

    tib <- tibble(
      group = rep(c("A", "B"), each = n/2),
      group_treat = rep(c(0, 1), each = n/2),
      group_sum = rep(c(1, -1), each = n/2),
      y_treat = the_intercept + (the_slope * group_treat) + rnorm(n, 0, 0.5),
      y_sum = the_intercept + (the_slope * group_sum) + rnorm(n, 0, 0.5)
    )

    if (input$coding == "treat") {
      tib$y <- tib$y_treat
    } else if (input$coding == "sum") {
      tib$y <- tib$y_sum
      the_a <- the_intercept + the_slope
      the_b <- the_intercept - the_slope
    }

    ggplot(tib) +
      geom_jitter(aes(group, y), width = 0.2, size = 3, alpha = 0.3) +
      {if (input$coding == "treat") geom_point(x = "A", y = the_intercept, size = 10, colour = "#1b9e77", shape = 18) } +
      {if (input$coding == "treat") geom_point(x = "B", y = the_intercept + the_slope, size = 10, colour = "#d95f02", shape = 18) } +
      {if (input$coding == "sum") geom_point(x = "A", y = the_a, size = 10, colour = "#d95f02", shape = 18) } +
      {if (input$coding == "sum") geom_point(x = "B", y = the_b, size = 10, colour = "#d95f02", shape = 18) } +
      {if (input$coding == "sum") geom_point(x = 1.5, y = the_intercept, size = 5, colour = "#1b9e77", shape = 16) } +
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
    the_beta_0 <- input$beta_0
    the_beta_1 <- input$beta_1
    the_beta_2 <- input$beta_2
    the_beta_3 <- input$beta_3
    group <- rep(c("A", "B"), each = n_2)
    group_treat <- rep(c(0, 1), each = n_2)
    the_error <- input$error_cont_cat

    x <- sample(the_seq_1, n_cc)
    y <- the_beta_0 +
      the_beta_1 * x +
      the_beta_2 * group_treat +
      the_beta_3 * x * group_treat +
      rnorm(n_2, 0, the_error)

    tib <- tibble(
      x = x,
      y = y,
      group = group
    )

    ggplot(tib, aes(x, y, colour = group)) +
      # Axes
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = 0, linetype = "dashed") +
      # Raw data
      {if (input$raw_cont_cat) geom_point(size = 3, alpha = 0.3) } +
      # Regression line A
      geom_abline(intercept = the_beta_0, slope = the_beta_1, size = 2) +
      # Regression line B
      geom_abline(intercept = the_beta_0 + the_beta_2, slope = the_beta_1 + the_beta_3, size = 2) +
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
