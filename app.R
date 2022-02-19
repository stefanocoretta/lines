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
ui <- fluidPage(

  # Application title
  titlePanel("A linear model"),

  # Sidebar with a slider input for number of bins
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
            value = 1),
      checkboxGroupInput("groups",
                 "Groups:",
                 c("Include groups" = "grp",
                 "Include interaction" = "int"
                 )
                 ),
      sliderInput("intercept_b",
            "Intercept (Group B):",
            min = -5,
            max = 5,
            value = 0),
      sliderInput("slope_b",
            "Slope (Group B):",
            min = -1,
            max = 3,
            step = 0.1,
            value = 1),
    ),

    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("linePlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$linePlot <- renderPlot({
    n <- input$n
    x <- sample(seq(-5, 5, by = 0.01), n)
    y_a <- input$intercept + input$slope * x[1:(n/2)] + rnorm(n/2, 0, 1)
    if ("grp" %in% input$groups) {
    y_b <- input$intercept_b + input$slope_b * x[((n/2)+1):n] + rnorm(n/2, 0, 1)
    } else {
    y_b <- input$intercept + input$slope * x[((n/2)+1):n] + rnorm(n/2, 0, 1)
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
    {if ("grp" %in% input$groups) aes(colour = group) } +
    {if (input$raw) geom_point(size = 5, alpha = 0.5) } +
    geom_abline(intercept = input$intercept, slope = input$slope, size = 2) +
    geom_point(aes(x = 0, y = input$intercept), size = 5, colour = "red") +
    {if ("grp" %in% input$groups & !("int" %in% input$groups)) geom_abline(intercept = input$intercept_b, slope = input$slope, size = 2) } +
    {if ("grp" %in% input$groups & "int" %in% input$groups) geom_abline(intercept = input$intercept_b, slope = input$slope_b, size = 2) } +
    {if ("grp" %in% input$groups) geom_point(aes(x = 0, y = input$intercept_b), size = 5, colour = "blue") } +
    scale_x_continuous(breaks = seq(-5, 5), limits = c(-5, 5)) +
    scale_y_continuous(breaks = seq(-5, 5), limits = c(-5, 5)) +
    labs(x = element_blank(), y = element_blank()) +
    coord_fixed() +
    theme_minimal() +
    theme(legend.position = "none")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
