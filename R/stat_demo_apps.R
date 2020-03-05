#' Some stats demos
#'
#' @param demo Which demo to show? (Partial matching supported)
#'
#' @details
#' \subsection{paired ttest}{
#' Shows a paired vs un-paired ttest, and how these differences are
#' affected by the correlation.
#' }
#'
#' \subsection{truncated correlation}{
#' Demo of how truncation affects correlations and doesn't affect MSE.
#' }
#'
#' \subsection{berksons paradox}{
#' Demo of how How sampling bias can affect estimated correlations and effects.
#' See related \href{https://www.youtube.com/watch?v=FUD8h9JpEVQ}{Numberphile video}.
#' }
#'
#' @export
stat_demo_apps <- function(demo = c("paired ttest", "truncated correlation", "berksons paradox")) {
  demo <- match.arg(demo)
  switch (demo,
          "paired ttest" = .demo_paired_ttest(),
          "truncated correlation" = .demo_truncated_correlation(),
          "berksons paradox" = .demo_berksons_paradox()
  )
}


.demo_paired_ttest <- function(){
  .check_namespace("shiny", "MASS", "patchwork", "ggplot2")

  .tidy_ttest <- function(data, var.equal = FALSE, paired = FALSE){
    res <- t.test(data[,1],data[,2], var.equal = var.equal, paired = paired)

    data.frame(
      t.value = res$statistic,
      df = as.integer(res$parameter),
      SE = res$stderr,
      p.value = res$p.value
    )
  }

  ui <- shiny::fluidPage(

    # Application title
    shiny::titlePanel("Paired T-Tests"),

    # Sidebar with a slider input for number of bins
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::sliderInput('r','Correlation',min = -1, max = 0.9999, value = 0.7, step = 0.1),
        shiny::numericInput('d','Cohens D',value = 0.3, min = 0, step = 0.1),
        shiny::numericInput('n','N',value = 30, min = 4, step = 1)
      ),

      # Show a plot of the generated distribution
      shiny::mainPanel(
        shiny::tableOutput('results'),
        shiny::plotOutput('plot')
      )
    )
  )

  # Define server logic required to draw a histogram
  server <- function(input, output) {

    data <- shiny::reactive({
      as.data.frame(MASS::mvrnorm(
        n = input$n,
        mu = c(input$d, 0),
        Sigma = matrix(c(1, input$r, input$r, 1), 2),
        empirical = TRUE
      ))
    })

    ## Table ##
    output$results <- shiny::renderTable(rownames = TRUE, {
      res_p <- .tidy_ttest(data(), paired = TRUE)
      res_up <- .tidy_ttest(data(), var.equal = TRUE)

      res <- rbind(res_p, res_up)
      rownames(res) <- c('Paired','Indapendant')
      res
    })

    ## Plot ##
    output$plot <- shiny::renderPlot({
      corr_plot <- ggplot2::ggplot(data(), ggplot2::aes(V1, V2)) +
        ggplot2::geom_point() +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "X1", y = "X2")

      sbs_data <- rbind(
        data.frame(y = data()[,1],
                   x = "X1",
                   id = seq_along(data()[,1])),
        data.frame(y = data()[,2],
                   x = "X2",
                   id = seq_along(data()[,2]))
      )

      tplot <- ggplot2::ggplot(sbs_data,
                               ggplot2::aes(
                                 .data$x,
                                 .data$y,
                                 group = .data$id,
                                 color = factor(.data$id)
                               )) +
        ggplot2::geom_line() +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = 'none') +
        ggplot2::labs(x = "", y = "")


      patchwork::wrap_plots(tplot, corr_plot)
    })
  }

  # Run the application
  shiny::runApp(list(ui = ui, server = server))
}

.demo_truncated_correlation <- function(){
  .check_namespace("shiny", "MASS", "ggplot2")

  ploting_list <- list(ggplot2::geom_point(),
                       ggplot2::coord_cartesian(xlim = c(-4, 4), ylim = c(-3, 3)),
                       ggplot2::labs(x = "X", y = "Y"),
                       ggplot2::theme_bw(),
                       ggplot2::theme(legend.position = 'none'))

  ui <- shiny::fluidPage(

    # Application title
    shiny::titlePanel("Truncated Correlations"),

    # Sidebar with a slider input for number of bins
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::sliderInput('r','Correlation',min = -1,max = 1,step = 0.1, value = 0.7),
        shiny::sliderInput('lims','Set cutoff',min = -4,max = 4,step = 0.1, value = c(-3,3))
      ),

      # Show a plot of the generated distribution
      shiny::mainPanel(
        shiny::plotOutput("fullPlot"),
        shiny::plotOutput("trimPlot")
      )
    )
  )

  # Define server logic required to draw a histogram
  server <- function(input, output) {

    ## make data ##
    corr_data <- shiny::reactive({
      as.data.frame(MASS::mvrnorm(
        n = 200,
        mu = c(0, 0),
        Sigma = matrix(c(1, input$r, input$r, 1), 2),
        empirical = TRUE
      ))
    })


    data <- shiny::reactive({
      df <- corr_data()

      df$in_range <-
        input$lims[1] < df[[1]] &
        df[[1]] < input$lims[2]

      df
    })

    ## plot 1 ##
    output$fullPlot <- shiny::renderPlot({
      rr <- cor(data()[,1:2])[2]
      rr <- round(rr,2)

      se <- sd(residuals(lm(V2 ~ V1, data())))

      ggplot2::ggplot(data(), ggplot2::aes(V1, V2, color = in_range)) +
        ploting_list +
        ggplot2::scale_color_manual(values = c('red','blue')) +
        ggplot2::labs(title = paste0("r = ", rr),
                      subtitle = paste0("MSE = ", round(se,2)))
    })

    ## plot 2 ##
    output$trimPlot <- renderPlot({
      rr <- cor(data()[data()$in_range,1:2])[2]
      rr <- round(rr,2)

      se <- sd(residuals(lm(V2 ~ V1, data()[data()$in_range,1:2])))

      ggplot2::ggplot(data()[data()$in_range,], ggplot2::aes(V1, V2)) +
        ploting_list +
        ggplot2::labs(title = paste0("Truncated r = ", rr),
                      subtitle = paste0("MSE = ", round(se,2)))
    })
  }

  # Run the application
  shiny::runApp(list(ui = ui, server = server))
}


.demo_berksons_paradox <- function(){
  # .check_namespace("shiny", "MASS", "ggplot2")

  ui <- shiny::fluidPage(

    # Application title
    shiny::titlePanel("Berkson's Paradox"),

    # Sidebar with a slider input for number of bins
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::sliderInput('r','True Slope',min = -1,max = 1,step = 0.1, value = 0.3),
        shiny::sliderInput('xcutoff','Cutoff on X',min = -4,max = 4,step = 0.1, value = 1),
        shiny::sliderInput('ycutoff','Cutoff on Y',min = -4,max = 4,step = 0.1, value = 1)
      ),

      # Show a plot of the generated distribution
      shiny::mainPanel(
        shiny::plotOutput("outPlot")
      )
    )
  )

  # Define server logic required to draw a histogram
  server <- function(input, output) {

    ## make data ##
    data <- shiny::reactive({
      as.data.frame(MASS::mvrnorm(
        n = 1000,
        mu = c(0, 0),
        Sigma = matrix(c(1, input$r, input$r, 1), 2),
        empirical = TRUE
      ))
    })

    ## plot 1 ##
    output$outPlot <- shiny::renderPlot({
      plot_data <- data()
      plot_data$is_in <-
        plot_data$V1 > input$xcutoff |
        plot_data$V2 > input$ycutoff

      cut_data <- plot_data[plot_data$is_in, 1:2]

      cut_rr <- cor(cut_data)[2]

      cap <- sprintf("True slope = %.2f;\nSample slope = %.2f", input$r, cut_rr)

      ggplot2::ggplot(plot_data, ggplot2::aes(V1, V2, color = is_in)) +
        ggplot2::geom_point(alpha = 0.4, shape = 16) +
        ggplot2::geom_smooth(ggplot2::aes(group = 1),
                             color = "black",
                             method = "lm", se = TRUE) +
        ggplot2::geom_smooth(ggplot2::aes(group = 1), data = cut_data,
                             color = "red2",
                             method = "lm", se = TRUE) +


        ggplot2::scale_color_manual(values = c('black','red')) +
        ggplot2::coord_cartesian(xlim = c(-4, 4), ylim = c(-3, 3)) +
        ggplot2::labs(x = "X", y = "Y") +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = 'none') +
        ggplot2::labs(title = cap)
    })
  }

  # Run the application
  shiny::runApp(list(ui = ui, server = server))

}
