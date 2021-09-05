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
#' See related [Numberphile video](https://www.youtube.com/watch?v=FUD8h9JpEVQ).
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

    res <- t.test(data[[1]],data[[2]], var.equal = var.equal, paired = paired)

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
        shiny::sliderInput('r','Correlation',min = -1, max = 0.9999, value = 0.7, step = 0.05),
        shiny::numericInput('d','Cohens D',value = 0.3, min = 0, step = 0.05),
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
        Sigma = matrix(c(1, 0, 0, 1), 2),
        empirical = TRUE
      ))
    })

    ## Table ##
    output$results <- shiny::renderTable(rownames = TRUE, digits = 3, {
      df <- .add_corr(data(), input$r)

      res_p <- .tidy_ttest(df, paired = TRUE)
      res_up <- .tidy_ttest(df, var.equal = TRUE)

      res <- rbind(res_p, res_up)
      rownames(res) <- c('Paired','Indapendant')
      res
    })

    ## Plot ##
    output$plot <- shiny::renderPlot({
      df <- .add_corr(data(), input$r)

      corr_plot <- ggplot2::ggplot(df, ggplot2::aes(V1, V2)) +
        ggplot2::geom_point() +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "X1", y = "X2")

      sbs_data <- rbind(
        data.frame(y = df[,1],
                   x = "X1",
                   id = seq_along(df[,1])),
        data.frame(y = df[,2],
                   x = "X2",
                   id = seq_along(df[,2]))
      )

      tplot <- ggplot2::ggplot(sbs_data,
                               ggplot2::aes(
                                 .data$x,
                                 .data$y,
                                 group = .data$id,
                                 color = factor(.data$id)
                               )) +
        ggplot2::geom_line(alpha = 0.5) +
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
  .check_namespace("shiny", "MASS", "patchwork", "ggplot2")

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
        shiny::sliderInput('r','Correlation',min = -1,max = 1,step = 0.05, value = 0.7),
        shiny::sliderInput('lims','Set cutoff',min = -4,max = 4,step = 0.05, value = c(-3,3)),
        shiny::numericInput('n','N',value = 200, min = 4, step = 1)
      ),

      # Show a plot of the generated distribution
      shiny::mainPanel(
        shiny::plotOutput("thePlot")
      )
    )
  )

  # Define server logic required to draw a histogram
  server <- function(input, output) {

    ## make data ##
    corr_data <- shiny::reactive({
      as.data.frame(MASS::mvrnorm(
        n = input$n,
        mu = c(0, 0),
        Sigma = matrix(c(1, 0, 0, 1), 2),
        empirical = TRUE
      ))
    })


    data <- shiny::reactive({
      df <- .add_corr(corr_data(),input$r)

      df$in_range <-
        input$lims[1] < df[[1]] &
        df[[1]] < input$lims[2]

      df
    })

    ## plot 1 ##
    output$thePlot <- shiny::renderPlot({
      m1 <- lm(V2 ~ V1, data())
      rr1 <- cor(data()[,1:2])[2]
      se1 <- sd(residuals(m1))
      b1 <- coef(m1)[2]

      m2 <- lm(V2 ~ V1, data()[data()$in_range,1:2])
      rr2 <- cor(data()[data()$in_range,1:2])[2]
      se2 <- sd(residuals(m2))
      b2 <- coef(m2)[2]


      p1 <- ggplot2::ggplot(data(), ggplot2::aes(V1, V2, color = in_range)) +
        ggplot2::scale_color_manual(values = c('red','blue')) +
        ggplot2::labs(title = paste0("r = ", round(rr1, 2)),
                      subtitle = paste0("b = ", round(b1, 3) ,", RMSE = ", round(se1, 2)))


      p2 <- ggplot2::ggplot(data()[data()$in_range,], ggplot2::aes(V1, V2)) +
        ggplot2::labs(title = paste0("Truncated r = ", round(rr2, 2)),
                      subtitle = paste0("b = ", round(b2, 3) ,", RMSE = ", round(se2, 2)))

      p1 / p2 & ploting_list
    })
  }

  # Run the application
  shiny::runApp(list(ui = ui, server = server))
}

.demo_berksons_paradox <- function(){
  .check_namespace("shiny", "MASS", "ggplot2")

  ui <- shiny::fluidPage(

    # Application title
    shiny::titlePanel("Berkson's Paradox"),

    # Sidebar with a slider input for number of bins
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::sliderInput('r','True Slope',min = -1,max = 1,step = 0.05, value = 0.3),
        shiny::numericInput('n','N',value = 1000, min = 4, step = 1),
        shiny::selectInput("cuttype", "Cutoff Type",
                           c("Independent" = "independent",
                             "Sum" = "joint")),
        shiny::conditionalPanel(
          condition = "input.cuttype == 'independent'",
          shiny::sliderInput('xcutoff','Cutoff on X',min = -4,max = 4,step = 0.1, value = 1),
          shiny::sliderInput('ycutoff','Cutoff on Y',min = -4,max = 4,step = 0.1, value = 1)
        ),
        shiny::conditionalPanel(
          condition = "input.cuttype == 'joint'",
          shiny::sliderInput('jointsum','Cuttoff on X + Y',min = -4,max = 4,step = 0.1, value = 0)
        )

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
        n = input$n,
        mu = c(0, 0),
        Sigma = matrix(c(1, 0, 0, 1), 2),
        empirical = TRUE
      ))
    })

    ## plot 1 ##
    output$outPlot <- shiny::renderPlot({
      plot_data <- .add_corr(data(), input$r)

      if (input$cuttype == 'independent') {
        plot_data$is_in <-
          plot_data$V1 > input$xcutoff |
          plot_data$V2 > input$ycutoff
      } else {
        plot_data$is_in <-
          plot_data$V1 + plot_data$V2 > input$jointsum
      }

      cut_data <- plot_data[plot_data$is_in, 1:2]

      cut_rr <- cor(cut_data)[2]

      cap <- sprintf("True slope = %.2f;\nSample slope = %.2f", input$r, cut_rr)

      ggplot2::ggplot(plot_data, ggplot2::aes(V1, V2, color = is_in)) +
        ggplot2::geom_point(alpha = 0.4, shape = 16) +
        ggplot2::geom_smooth(ggplot2::aes(group = 1),
                             formula = y ~ x,
                             color = "blue2",
                             method = "lm", se = TRUE) +
        ggplot2::geom_smooth(ggplot2::aes(group = 1), data = cut_data,
                             formula = y ~ x,
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


# Utils -------------------------------------------------------------------

#' @keywords internal
.add_corr <- function(df, rr) {
  m1 <- mean(df[[1]])
  m2 <- mean(df[[2]])

  X <- df[[1]]
  e <- df[[2]]

  Se <- sqrt(1 - rr^2)

  df[[2]] <- rr * X + Se * e

  df[[1]] <- scale(df[[1]], TRUE, FALSE) + m1
  df[[2]] <- scale(df[[2]], TRUE, FALSE) + m2

  df
}
