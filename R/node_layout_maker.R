#' Shiny app for manual Node layout
#'
#' @param object Object to get the node names from. Passed to \code{\link[semPlot]{semPlotModel}}. Can also be a vector of names, or a number representing the number of nodes.
#' @param snap_to_grid Should nodes be auto snapped to the grid?
#'
#' @return A matrix with a row per node, and column representing x and y. This matrix can be passed as-is to \code{\link[semPlot]{semPaths}} via \code{layout=}.
#'
#' @example examples/examples.node_layout_maker.R
#'
#' @export
node_layout_maker <- function(object, snap_to_grid = TRUE) {
  UseMethod("node_layout_maker")
}

#' @rdname node_layout_maker
#' @export
node_layout_maker.character <- function(object, snap_to_grid = TRUE) {
  .check_namespace("shiny", "ggplot2")

  #### Setup ####
  dat <- data.frame(labs = object,
                    x = seq_along(object),
                    y = seq_along(object))


  #### UI ####
  ui <- shiny::fluidPage(
    shiny::titlePanel("Drag and Drop Nodes to position them..."),

    shiny::plotOutput(
      "distPlot",
      click = "plot_click",
      brush = shiny::brushOpts("plot_brush", opacity = 0, resetOnNew = TRUE)
    ),

    shiny::actionButton("is_done", label = "Save")
  )

  #### Server ####
  server <- function(input, output) {
    ## Make Data Reactive
    vals <- shiny::reactiveValues(dat = dat)

    shiny::observeEvent(input$plot_brush, {
      ## First Click ##
      first_x <- input$plot_click$x
      first_y <- input$plot_click$y


      ## Last Click ##
      # get both clicks
      last_x <- c(input$plot_brush$xmin, input$plot_brush$xmax)
      last_y <- c(input$plot_brush$ymin, input$plot_brush$ymax)

      # remove first click
      last_x <- last_x[which.max(abs(last_x - first_x))]
      last_y <- last_y[which.max(abs(last_y - first_y))]


      ## Which node was selected? (first click) ##
      # Distance from first click
      ds <- mapply(
        FUN = .distance_xy,
        vals$dat$x,
        vals$dat$y,
        MoreArgs = list(x2 = first_x, y2 = first_y),
        SIMPLIFY = TRUE
      )

      # Only close to nodes
      ds[ds > sqrt(0.5)] <- Inf


      ## Set new x,y ##
      if (!all(ds == Inf)) {
        # Find closest
        i <- which.min(ds)[1]

        # Round?
        if (snap_to_grid) {
          last_x <- round(last_x)
          last_y <- round(last_y)
        }

        # Set as new x,y
        vals$dat$x[i] <- last_x
        vals$dat$y[i] <- last_y
      }
    })

    ## Plot grid ##
    output$distPlot <- shiny::renderPlot({

      ggplot2::ggplot(vals$dat, ggplot2::aes(x, y, label = labs)) +
        ggplot2::geom_label() +
        ggplot2::scale_x_continuous(breaks = seq_along(object), minor_breaks = NULL) +
        ggplot2::scale_y_continuous(breaks = seq_along(object), minor_breaks = NULL) +
        ggplot2::coord_cartesian(
          xlim = c(1,length(object)) + c(-1, 1),
          ylim = c(1,length(object)) + c(-1, 1),
          expand = FALSE
        )

    })

    ## When done ##
    shiny::observeEvent(input$is_done, {
      m <- as.matrix(vals$dat[, 2:3])
      colnames(m) <- c("x", "y")
      rownames(m) <- object
      stopApp(m)
    })
  }


  #### Run app ####
  shiny::runApp(list(ui = ui, server = server))
}

#' @rdname node_layout_maker
#' @export
node_layout_maker.numeric <- function(object, snap_to_grid = TRUE) {
  if (length(object) == 1) {
    object <- paste0("Node", seq_len(object))
  } else {
    object <- as.character(object)
  }
  node_layout_maker(object, snap_to_grid = snap_to_grid)
}

#' @rdname node_layout_maker
#' @export
node_layout_maker.default <- function(object, snap_to_grid = TRUE) {
  .check_namespace("semPlot")
  labs <- semPlot::semPlotModel(object)@Vars$name
  node_layout_maker(labs, snap_to_grid = snap_to_grid)
}



#' @keywords internal
.distance_xy <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2)
}

