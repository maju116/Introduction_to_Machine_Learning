function(input, output, session) {

  output$dist_params <- renderUI({
    req(input$dist)
    if (input$dist == "Bernoulli") {
      div(
        numericInput("p", "Probability", 0.5, min = 0, max = 1, step = 0.01)
      )
    } else if (input$dist == "Normal") {
      div(
        numericInput("u", "Mean", 0, step = 0.01),
        sliderInput("sd", "Standard deviation", value = 1, min = 0.1, max = 4, step = 0.1, animate = TRUE)
      )
    } else if (input$dist == "Poisson") {
      div(
        sliderInput("lambda", "Lambda", value = 1, min = 0.01, max = 10, step = 0.1, animate = TRUE)
      )
    }
  })

  dist_data <- reactive({
    req(input$dist)
    if (input$dist == "Bernoulli") {
      req(input$p)
      x <- 0:1
      density <- c(1 - input$p, input$p)
      cdf <- cumsum(density)
    }
    else if (input$dist == "Normal") {
      req(input$u)
      req(input$sd)
      x <- seq(input$u - 5, input$u + 5, by = 0.01)
      density <- dnorm(x, input$u, input$sd)
      cdf <- pnorm(x, input$u, input$sd)
    } else if (input$dist == "Poisson") {
      req(input$lambda)
      x <- 0:10
      density <- dpois(x, input$lambda)
      cdf <- ppois(x, input$lambda)
    }
    tibble(
      x = x, density = density, cdf = cdf
    )
  })

  output$cdf_plot <- renderPlot({
    req(input$dist)
    if (input$dist %in% c("Bernoulli", "Poisson")) {
      base_plot <- ggplot(dist_data(), aes(x, cdf)) + geom_bar(stat="identity")
    } else {
      base_plot <- ggplot(dist_data(), aes(x, cdf)) + geom_line()
    }
    base_plot + labs(x = "VALUE", y = "PROBABILITY", title = "CDF") +
      theme(plot.title = element_text(hjust = 0.5))
  })

  output$density_plot <- renderPlot({
    req(input$dist)
    if (input$dist %in% c("Bernoulli", "Poisson")) {
      base_plot <- ggplot(dist_data(), aes(x, density)) + geom_bar(stat="identity") +
        ggtitle("MASS")
    } else {
      base_plot <- ggplot(dist_data(), aes(x, density)) + geom_line() +
        ggtitle("DENSITY")
    }
    base_plot + labs(x = "VALUE", y = "PROBABILITY") +
      theme(plot.title = element_text(hjust = 0.5))
  })

}
