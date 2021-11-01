library(tidyverse)
library(shiny)

pageWithSidebar(
  headerPanel('Random variable distributions'),
  sidebarPanel(
    selectInput('dist', 'Distribution',
                c("Bernoulli", "Normal", "Poisson")),
    h2("Distribution parameters:"),
    uiOutput('dist_params')
  ),
  mainPanel(
    plotOutput('cdf_plot'),
    plotOutput('density_plot')
  )
)
