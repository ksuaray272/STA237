library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Sampling Distribution of the Sample Mean"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "dist", "Choose a distribution:",
        choices = c(
          "Uniform",
          "Exponential/Gamma",
          "Bernoulli",
          "Geometric",
          "Poisson"
        )
      ),
      
      conditionalPanel(
        condition = "input.dist == 'Uniform'",
        numericInput("min", "Minimum (a):", value = 0),
        numericInput("max", "Maximum (b):", value = 1)
      ),
      
      conditionalPanel(
        condition = "input.dist == 'Exponential/Gamma'",
        radioButtons(
          "expType", "Choose distribution type:",
          choices = c(
            "Exponential" = "exp",
            "Gamma (shape α, rate λ)" = "gamma"
          )
        ),
        
        conditionalPanel(
          condition = "input.expType == 'exp'",
          numericInput("rate", "Rate (λ):", value = 1, min = 0.001)
        ),
        
        conditionalPanel(
          condition = "input.expType == 'gamma'",
          numericInput("shape", "Shape (α):", value = 2, min = 0.001),
          numericInput("rate2", "Rate (λ):", value = 1, min = 0.001)
        )
      ),
      
      conditionalPanel(
        condition = "input.dist == 'Bernoulli'",
        numericInput(
          "prob", "Success probability (p):",
          value = 0.5, min = 0, max = 1
        )
      ),
      
      conditionalPanel(
        condition = "input.dist == 'Geometric'",
        numericInput(
          "geom_prob", "Success probability (p):",
          value = 0.5, min = 0.001, max = 1
        ),
        helpText("Counts failures before the first success: 0, 1, 2, …")
      ),
      
      conditionalPanel(
        condition = "input.dist == 'Poisson'",
        numericInput("lambda", "Rate (λ):", value = 3, min = 0.001)
      ),
      
      plotOutput("pdfPlot", height = "200px"),
      hr(),
      
      sliderInput("n", "Sample size (n):", min = 1, max = 150, value = 1),
      
      checkboxInput(
        "show_normal", "Overlay normal approximation", value = TRUE
      ),
      
      checkboxInput(
        "fixed_seed", "Use fixed random seed (123)", value = TRUE
      ),
      
      conditionalPanel(
        condition = "!input.fixed_seed",
        actionButton("resample", "Resample")
      )
    ),
    
    mainPanel(
      plotOutput("distPlot", height = "450px"),
      verbatimTextOutput("summaryText")
    )
  )
)

server <- function(input, output) {
  
  simulation <- reactive({
    # Clicking Resample triggers a new simulation when the fixed seed is off.
    input$resample
    
    if (isTRUE(input$fixed_seed)) {
      set.seed(123)
    }
    
    n <- input$n
    sim_size <- 1000
    
    sample_means <- switch(
      input$dist,
      
      "Uniform" = replicate(
        sim_size,
        mean(runif(n, input$min, input$max))
      ),
      
      "Exponential/Gamma" = {
        if (input$expType == "exp") {
          replicate(sim_size, mean(rexp(n, rate = input$rate)))
        } else {
          replicate(
            sim_size,
            mean(rgamma(n, shape = input$shape, rate = input$rate2))
          )
        }
      },
      
      "Bernoulli" = replicate(
        sim_size,
        mean(rbinom(n, size = 1, prob = input$prob))
      ),
      
      "Geometric" = replicate(
        sim_size,
        mean(rgeom(n, prob = input$geom_prob))
      ),
      
      "Poisson" = replicate(
        sim_size,
        mean(rpois(n, lambda = input$lambda))
      )
    )
    
    example_sample <- switch(
      input$dist,
      
      "Uniform" = runif(n, input$min, input$max),
      
      "Exponential/Gamma" = {
        if (input$expType == "exp") {
          rexp(n, rate = input$rate)
        } else {
          rgamma(n, shape = input$shape, rate = input$rate2)
        }
      },
      
      "Bernoulli" = rbinom(n, size = 1, prob = input$prob),
      
      "Geometric" = rgeom(n, prob = input$geom_prob),
      
      "Poisson" = rpois(n, lambda = input$lambda)
    )
    
    list(
      sample_means = sample_means,
      xbar = mean(example_sample)
    )
  })
  
  output$pdfPlot <- renderPlot({
    if (input$dist == "Uniform") {
      validate(need(input$max > input$min, "Maximum must exceed minimum."))
      
      x <- seq(input$min - 0.5, input$max + 0.5, length.out = 200)
      df <- data.frame(x = x, y = dunif(x, input$min, input$max))
      
      ggplot(df, aes(x, y)) +
        geom_line(color = "#0073C2FF", linewidth = 1.2) +
        labs(
          title = "Population Distribution (Uniform)",
          x = "x",
          y = "Density"
        ) +
        theme_minimal(base_size = 13)
      
    } else if (input$dist == "Exponential/Gamma") {
      if (input$expType == "exp") {
        x <- seq(0, qexp(0.995, rate = input$rate), length.out = 200)
        y <- dexp(x, rate = input$rate)
        title <- "Population Distribution (Exponential)"
      } else {
        x <- seq(
          0,
          qgamma(0.995, shape = input$shape, rate = input$rate2),
          length.out = 200
        )
        y <- dgamma(x, shape = input$shape, rate = input$rate2)
        title <- "Population Distribution (Gamma)"
      }
      
      df <- data.frame(x = x, y = y)
      
      ggplot(df, aes(x, y)) +
        geom_line(color = "#0073C2FF", linewidth = 1.2) +
        labs(title = title, x = "x", y = "Density") +
        theme_minimal(base_size = 13)
      
    } else if (input$dist == "Bernoulli") {
      df <- data.frame(
        x = 0:1,
        y = dbinom(0:1, size = 1, prob = input$prob)
      )
      
      ggplot(df, aes(x = factor(x), y = y)) +
        geom_col(fill = "#0073C2FF", width = 0.4) +
        labs(
          title = "Population Distribution (Bernoulli)",
          x = "Outcome",
          y = "Probability"
        ) +
        coord_cartesian(ylim = c(0, 1)) +
        theme_minimal(base_size = 13)
      
    } else if (input$dist == "Geometric") {
      k_max <- max(10, qgeom(0.995, prob = input$geom_prob))
      x <- 0:k_max
      df <- data.frame(x = x, y = dgeom(x, prob = input$geom_prob))
      
      ggplot(df, aes(x = factor(x), y = y)) +
        geom_col(fill = "#0073C2FF", width = 0.7) +
        labs(
          title = "Population Distribution (Geometric)",
          x = "Failures before first success",
          y = "Probability"
        ) +
        theme_minimal(base_size = 13)
      
    } else {
      k_max <- max(10, qpois(0.995, lambda = input$lambda))
      x <- 0:k_max
      df <- data.frame(x = x, y = dpois(x, lambda = input$lambda))
      
      ggplot(df, aes(x = factor(x), y = y)) +
        geom_col(fill = "#0073C2FF", width = 0.7) +
        labs(
          title = "Population Distribution (Poisson)",
          x = "Count",
          y = "Probability"
        ) +
        theme_minimal(base_size = 13)
    }
  })
  
  output$distPlot <- renderPlot({
    sample_means <- simulation()$sample_means
    
    if (input$dist == "Uniform") {
      theo_mean <- (input$min + input$max) / 2
      theo_sd <- (input$max - input$min) / sqrt(12)
      
    } else if (input$dist == "Exponential/Gamma") {
      if (input$expType == "exp") {
        theo_mean <- 1 / input$rate
        theo_sd <- 1 / input$rate
      } else {
        theo_mean <- input$shape / input$rate2
        theo_sd <- sqrt(input$shape) / input$rate2
      }
      
    } else if (input$dist == "Bernoulli") {
      theo_mean <- input$prob
      theo_sd <- sqrt(input$prob * (1 - input$prob))
      
    } else if (input$dist == "Geometric") {
      p <- input$geom_prob
      theo_mean <- (1 - p) / p
      theo_sd <- sqrt(1 - p) / p
      
    } else {
      theo_mean <- input$lambda
      theo_sd <- sqrt(input$lambda)
    }
    
    sampling_sd <- theo_sd / sqrt(input$n)
    
    p <- ggplot(
      data.frame(sample_mean = sample_means),
      aes(x = sample_mean)
    ) +
      geom_histogram(
        aes(y = after_stat(density)),
        bins = 30,
        fill = "#0073C2FF",
        color = "white",
        alpha = 0.7
      ) +
      labs(
        title = paste(
          "Sampling Distribution of the Sample Mean (n =", input$n, ")"
        ),
        x = "Sample Mean",
        y = "Density"
      ) +
      theme_minimal(base_size = 14)
    
    if (isTRUE(input$show_normal) && sampling_sd > 0) {
      x_vals <- seq(
        min(sample_means, theo_mean - 4 * sampling_sd),
        max(sample_means, theo_mean + 4 * sampling_sd),
        length.out = 500
      )
      
      normal_df <- data.frame(
        x = x_vals,
        y = dnorm(x_vals, mean = theo_mean, sd = sampling_sd)
      )
      
      p <- p +
        geom_line(
          data = normal_df,
          aes(x = x, y = y),
          color = "red",
          linewidth = 1.2,
          linetype = "dashed"
        )
    }
    
    p
  })
  
  output$summaryText <- renderPrint({
    cat("Distribution:", input$dist, "\n")
    cat("Sample size:", input$n, "\n")
    cat("Simulation size: 1000\n")
    cat(
      "Fixed seed:",
      if (isTRUE(input$fixed_seed)) "123" else "Off",
      "\n"
    )
    cat(
      "Sample mean (x̄) for one sample of size n:",
      format(simulation()$xbar, digits = 5),
      "\n"
    )
    
    if (input$dist == "Uniform") {
      cat("a =", input$min, ", b =", input$max, "\n")
      
    } else if (input$dist == "Exponential/Gamma") {
      if (input$expType == "exp") {
        cat("Exponential with rate λ =", input$rate, "\n")
      } else {
        cat(
          "Gamma with shape α =", input$shape,
          "and rate λ =", input$rate2, "\n"
        )
      }
      
    } else if (input$dist == "Bernoulli") {
      cat("Bernoulli with p =", input$prob, "\n")
      
    } else if (input$dist == "Geometric") {
      cat("Geometric with success probability p =", input$geom_prob, "\n")
      cat("Counts failures before the first success.\n")
      
    } else {
      cat("Poisson with rate λ =", input$lambda, "\n")
    }
  })
}

shinyApp(ui = ui, server = server)
