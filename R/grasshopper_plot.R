#' @title grasshopper_plot
#'
#' @description A function to display data that is discribed by three categorical values.
#'   Essentially, a grouped heatmap.  For example, one could plot the changes in the
#'   levels (value) of particular proteins (variable 1) in different cell types
#'   (variable 2) to a particular stimulus (variable 3).
#'
#'   Note: since this uses ggplot2, parameters passed to xaxis, yaxis, and group_by
#'   should be unquoted.
#'
#' @param df A data frame with three categorical variables and one response
#'   value.
#' @param xaxis Category to plot along the x-axis.
#' @param yaxis Category to plot along the y-axis.
#' @param group_by Category to group the values by.
#' @param point_size Size to use for plotting values.  Default: 3.
#' @param shape Shape to use for the plotting values.  To see acceptable values
#'   run the function view_shapes().  Default: 'square'
#' @param do_label Should each point be labeled with the grouping variable
#'   value? Currently non-functional.  Default: FALSE
#' @param plot_title: Default: NULL
#' @param show_value_guide Should a colorbar legend be displayed?  Default: FALSE.
#' @param value_guide_title: Title to display for the colorbar (if displayed).
#'   Default: "value"
#'
#' @import rlang
#' @import ggplot2
#' @importFrom dplyr pull
#' @importFrom plyr mapvalues
#'
#' @return
#' @export
#'
#' @examples
grasshopper_plot <- function(df,
                             xaxis,
                             yaxis,
                             group_by,
                             fill_by,
                             point_size = 3,
                             shape = "square",
                             do_label = FALSE,
                             main_title = NULL,
                             show_value_guide = FALSE,
                             value_guide_title = NULL){

  group_by <- enquo(group_by)
  xaxis <- enquo(xaxis)
  yaxis <- enquo(yaxis)
  fill_by <- enquo(fill_by)

  group_levels <- df %>% pull(!!group_by)

  num_col <- group_levels %>%
    unique() %>%
    length() %>%
    sqrt() %>%
    ceiling()
  num_row <- group_levels %>%
    unique() %>%
    length() %>%
    sqrt() %>%
    floor()

  df$x_level <- mapvalues(x = group_levels,
                          from = unique(group_levels),
                          to = rep(1:num_col, num_row))
  df$y_level <- mapvalues(x = group_levels,
                          from = unique(group_levels),
                          to = rep(1:num_row, num_col))
  midpoint <- df %>% pull(!!fill_by) %>% mean()

  p <- df %>%
    ggplot(aes(
      y = y_level,
      x = x_level,
      color = !!fill_by,
      size = point_size)) +
    geom_point(shape = shape) +
    facet_grid(cols = vars(!!xaxis), rows = vars(!!yaxis), scales = "free") +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          strip.background = element_blank(),
          strip.text.y = element_text(angle = 0,hjust = 0),
          legend.position = "none") +
    scale_color_gradient2(low = "blue",
                          mid = "white",
                          high = "red",
                          midpoint = midpoint) +
    labs(title = main_title) +
    guides(color = FALSE,
           shape = FALSE)

  # if (do_label) {
  #   first_row <- df %>% select(!!xaxis) %>% unique() %>% slice(1)
  #   first_col <- df %>% select(!!yaxis) %>% unique() %>% slice(1)
  #   things_to_label <- data.frame(x_level = first_row,
  #                                 y_level = first_col,
  #                                 label_data = unique(group_levels))
  #   colnames(things_to_label) <- c("x_level", "y_level", "value")
  #   print(things_to_label)
  #   p <- p + geom_text(data = things_to_label,
  #                      label = "something")
  # }
  p
}

#' @title view_shapes
#'
#' @description Shows the shapes that can be used with grasshopper_plot
#'
#' @return ggplot of shapes and their names
#' @export
#'
#' @import ggplot2
#'
#' @examples
view_shapes <- function(){
  shape_names <- c(
    "circle", paste("circle", c("open", "filled", "cross", "plus", "small")), "bullet",
    "square", paste("square", c("open", "filled", "cross", "plus", "triangle")),
    "diamond", paste("diamond", c("open", "filled", "plus")),
    "triangle", paste("triangle", c("open", "filled", "square")),
    paste("triangle down", c("open", "filled")),
    "plus", "cross", "asterisk"
  )

  shapes <- data.frame(
    shape_names = shape_names,
    x = c(1:7, 1:6, 1:3, 5, 1:3, 6, 2:3, 1:3),
    y = -rep(1:6, c(7, 6, 4, 4, 2, 3))
  )

  ggplot(shapes, aes(x, y)) +
    geom_point(aes(shape = shape_names), fill = "red", size = 5) +
    geom_text(aes(label = shape_names), nudge_y = -0.3, size = 3.5) +
    scale_shape_identity() +
    scale_x_continuous(NULL, breaks = NULL) +
    scale_y_continuous(NULL, breaks = NULL)
}


