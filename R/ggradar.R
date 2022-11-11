#' ggradar
#'
#' @param plot.data dataframe comprising one row per group
#' @param base.size text size
#' @param font.radar text font family
#' @param values.radar values to print at minimum, 'average', and maximum gridlines
#' @param axis.labels  names of axis labels if other than column names supplied via plot.data
#' @param grid.values values at which grid line is plotted, has to be uneven
#' @param centre.y value of y at centre of plot
#' @param plot.extent.x.sf controls relative size of plot horizontally
#' @param plot.extent.y.sf controls relative size of plot vertically
#' @param x.centre.range controls axis label alignment
#' @param label.centre.y whether value of y at centre of plot should be labelled
#' @param grid.line.width width of gridline
#' @param gridline.linetype line type of minimum gridline
#' @param gridline.colour colour of gridlines
#' @param grid.label.size text size of gridline label
#' @param gridline.label.offset displacement to left/right of central vertical axis
#' @param label.gridline whether or not to label the gridlines
#' @param axis.label.offset vertical displacement of axis labels from maximum grid line, measured relative to circle diameter
#' @param axis.label.size text size of axis label
#' @param axis.line.colour colour of axis line
#' @param group.line.width line width of group
#' @param group.point.size point size of group
#' @param group.colours colour of group
#' @param background.circle.colour colour of background circle/radar
#' @param background.circle.transparency transparency of background circle/radar
#' @param plot.legend whether to include a plot legend
#' @param legend.title title of legend
#' @param plot.title title of radar plot
#' @param legend.text.size text size in legend
#' @param legend.position position of legend, valid values are "top", "right", "bottom", "left"
#' @param fill whether to fill polygons
#' @param fill.alpha if filling polygons, transparency values
#'
#' @import ggplot2
#' @importFrom dplyr filter row_number bind_rows
#' @importFrom magrittr %>%
#' 
#' @return a ggplot object
#'
#' @name ggradar-package
#'
#' @export
#'
#' @source
#' Most of the code is from \url{http://rstudio-pubs-static.s3.amazonaws.com/5795_e6e6411731bb4f1b9cc7eb49499c2082.html}.
#'
#' @examples
#' \dontrun{
#' library(ggradar)
#' library(dplyr)
#' library(scales)
#' library(tibble)
#'
#' mtcars_radar <- mtcars %>%
#'   as_tibble(rownames = "group") %>%
#'   mutate_at(vars(-group), rescale) %>%
#'   tail(4) %>%
#'   select(1:10)
#' mtcars_radar
#' ggradar(mtcars_radar)
#' }
ggradar <- function(plot.data,
                    base.size = 15,
                    font.radar = "sans",
                    values.radar = paste0(round(grid.values*100,0),"%"),
                    axis.labels = colnames(plot.data)[-1],
                    grid.values = c(0,0.5,1),
                    centre.y = grid.min - ((1 / 9) * (grid.values[length(grid.values)] - grid.values[1])),
                    plot.extent.x.sf = 1,
                    plot.extent.y.sf = 1.2,
                    x.centre.range = 0.02 * (grid.values[length(grid.values)] - centre.y),
                    label.centre.y = FALSE,
                    grid.line.width = 0.5,
                    gridline.linetype = rep("longdash",length(grid.values)),
                    gridline.colour = rep("grey",length(grid.values)),
                    grid.label.size = 6,
                    gridline.label.offset = -0.1 * (grid.values[length(grid.values)] - centre.y),
                    label.gridline = rep(TRUE,length(grid.values)),
                    axis.label.offset = 1.15,
                    axis.label.size = 5,
                    axis.line.colour = "grey",
                    group.line.width = 1.5,
                    group.point.size = 6,
                    group.colours = NULL,
                    background.circle.colour = "#D7D6D1",
                    background.circle.transparency = 0.2,
                    plot.legend = if (nrow(plot.data) > 1) TRUE else FALSE,
                    legend.title = "",
                    plot.title = "",
                    legend.text.size = 14,
                    legend.position = "left",
                    fill = FALSE,
                    fill.alpha = 0.5) {
  # Additional code
  grid.max <- grid.values[length(grid.values)]
  grid.min <- grid.values[1]
  
  plot.data <- as.data.frame(plot.data)

  if(!is.factor(plot.data[, 1])) {
    plot.data[, 1] <- as.factor(as.character(plot.data[, 1]))
  }

  var.names <- colnames(plot.data)[-1] # Short version of variable names
  # axis.labels [if supplied] is designed to hold 'long version' of variable names
  # with line-breaks indicated using \n

  # calculate total plot extent as radius of outer circle x a user-specifiable scaling factor
  plot.extent.x <- (grid.max + abs(centre.y)) * plot.extent.x.sf
  plot.extent.y <- (grid.max + abs(centre.y)) * plot.extent.y.sf

  # Check supplied data makes sense
  if (length(axis.labels) != ncol(plot.data) - 1) {
    stop("'axis.labels' contains the wrong number of axis labels", call. = FALSE)
  }
  if (min(plot.data[, -1]) < centre.y) {
    stop("plot.data' contains value(s) < centre.y", call. = FALSE)
  }
  if (max(plot.data[, -1]) > grid.max) {
    stop("'plot.data' contains value(s) > grid.max", call. = FALSE)
  }

  ### Convert supplied data into plottable format
  # (a) add abs(centre.y) to supplied plot data
  # [creates plot centroid of 0,0 for internal use, regardless of min. value of y
  # in user-supplied data]
  plot.data.offset <- plot.data
  plot.data.offset[, 2:ncol(plot.data)] <- plot.data[, 2:ncol(plot.data)] + abs(centre.y)
  # print(plot.data.offset)
  # (b) convert into radial coords
  group <- NULL
  group$path <- CalculateGroupPath(plot.data.offset)

  # print(group$path)
  # (c) Calculate coordinates required to plot radial variable axes
  axis <- NULL
  axis$path <- CalculateAxisPath(var.names, grid.min + abs(centre.y), grid.max + abs(centre.y))
  # print(axis$path)
  # (d) Create file containing axis labels + associated plotting coordinates
  # Labels
  axis$label <- data.frame(
    text = axis.labels,
    x = NA,
    y = NA
  )
  # print(axis$label)
  # axis label coordinates
  n.vars <- length(var.names)
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / n.vars)
  axis$label$x <- sapply(1:n.vars, function(i, x) {
    ((grid.max + abs(centre.y)) * axis.label.offset) * sin(angles[i])
  })
  axis$label$y <- sapply(1:n.vars, function(i, x) {
    ((grid.max + abs(centre.y)) * axis.label.offset) * cos(angles[i])
  })
  # print(axis$label)
  # (e) Create Circular grid-lines + labels
  # caclulate the cooridinates required to plot circular grid-lines for three user-specified
  # y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
  gridline <- lapply(1:length(grid.values), list)
  for (i in 1:length(grid.values)){
    gridline[[i]]$path <- funcCircleCoords(c(0, 0), grid.values[i] + abs(centre.y), npoints = 360)
    gridline[[i]]$label <- data.frame(
      x = gridline.label.offset, y = grid.values[i] + abs(centre.y),
      text = as.character(grid.values[i]), labels = values.radar[i]
    )
  }

  ### Start building up the radar plot

  # Declare 'theme_clear', with or without a plot legend as required by user
  # [default = no legend if only 1 group [path] being plotted]
  theme_clear <- theme_bw(base_size = base.size) +
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.key = element_rect(linetype = "blank")
    )

  if (plot.legend == FALSE) legend.position = "none"

  # Base-layer = axis labels + plot extent
  # [need to declare plot extent as well, since the axis labels don't always
  # fit within the plot area automatically calculated by ggplot, even if all
  # included in first plot; and in any case the strategy followed here is to first
  # plot right-justified labels for axis labels to left of Y axis for x< (-x.centre.range)],
  # then centred labels for axis labels almost immediately above/below x= 0
  # [abs(x) < x.centre.range]; then left-justified axis labels to right of Y axis [x>0].
  # This building up the plot in layers doesn't allow ggplot to correctly
  # identify plot extent when plotting first (base) layer]

  # base layer = axis labels for axes to left of central y-axis [x< -(x.centre.range)]
  base <- ggplot(axis$label) + xlab(NULL) + ylab(NULL) + coord_equal() +
    geom_text(
      data = subset(axis$label, axis$label$x < (-x.centre.range)),
      aes(x = x, y = y, label = text), size = axis.label.size, hjust = 1, family = font.radar
    ) +
    scale_x_continuous(limits = c(-1.5 * plot.extent.x, 1.5 * plot.extent.x)) +
    scale_y_continuous(limits = c(-plot.extent.y, plot.extent.y))

  # ... + circular grid-lines at 'min', 'mid' and 'max' y-axis values
  for (i in 1:length(grid.values)){
    base <- base + geom_path(
      data = gridline[[i]]$path, aes(x = x, y = y),
      lty = gridline.linetype[i], colour = gridline.colour[i], size = grid.line.width
    )
  }

  # + axis labels for any vertical axes [abs(x)<=x.centre.range]
  base <- base + geom_text(
    data = subset(axis$label, abs(axis$label$x) <= x.centre.range),
    aes(x = x, y = y, label = text), size = axis.label.size, hjust = 0.5, family = font.radar
  )
  # + axis labels for any vertical axes [x>x.centre.range]
  base <- base + geom_text(
    data = subset(axis$label, axis$label$x > x.centre.range),
    aes(x = x, y = y, label = text), size = axis.label.size, hjust = 0, family = font.radar
  )
  # + theme_clear [to remove grey plot background, grid lines, axis tick marks and axis text]
  base <- base + theme_clear
  #  + background circle against which to plot radar data
  base <- base + geom_polygon(
    data = gridline[[length(grid.values)]]$path, aes(x, y),
    fill = background.circle.colour,
    alpha = background.circle.transparency
  )

  # + radial axes
  base <- base + geom_path(
    data = axis$path, aes(x = x, y = y, group = axis.no),
    colour = axis.line.colour,size = grid.line.width
  )

  theGroupName <- names(group$path[1])

  # ... + group (cluster) 'paths'
  base <- base + geom_path(
    data = group$path, aes_string(x = "x", y = "y", group = theGroupName, colour = theGroupName),
    size = group.line.width
  )

  # ... + group points (cluster data)
  base <- base + geom_point(data = group$path, aes_string(x = "x", y = "y", group = theGroupName, colour = theGroupName), size = group.point.size)

  # ... + group (cluster) fills
  if(fill == TRUE) {
    base <- base + geom_polygon(data = group$path, aes_string(x = "x", y = "y", group = theGroupName, fill = theGroupName), alpha = fill.alpha)
  }

  # ... + amend Legend title
  if (plot.legend == TRUE) base <- base + labs(colour = legend.title, size = legend.text.size)

  # ... + grid-line labels (max; mid; min)
  if(sum(label.gridline)>=0){
    gridLabelData <- bind_rows(lapply(gridline,FUN=function(x) x$label)) %>% 
      filter(row_number() %in% which(label.gridline)) 
    base <- base + geom_text(aes(x = x, y = y, label = labels), 
                             data = gridLabelData, 
                             inherit.aes=FALSE,
                             size = grid.label.size * 0.8, hjust = 1, family = font.radar)
  }

  #base <- base + geom_text(aes(x = x, y = y, label = values.radar[1]), data = gridline[[1]]$label, size = grid.label.size * 0.8, hjust = 1, family = font.radar)
  #base <- base + geom_text(aes(x = x, y = y, label = values.radar[2]), data = gridline[[2]]$label, size = grid.label.size * 0.8, hjust = 1, family = font.radar)
  #base <- base + geom_text(aes(x = x, y = y, label = values.radar[3]), data = gridline[[3]]$label, size = grid.label.size * 0.8, hjust = 1, family = font.radar)
  # ... + centre.y label if required [i.e. value of y at centre of plot circle]
  if (label.centre.y == TRUE) {
    centre.y.label <- data.frame(x = 0, y = 0, text = as.character(centre.y))
    base <- base + geom_text(aes(x = x, y = y, label = text), data = centre.y.label, size = grid.label.size, hjust = 0.5, family = font.radar)
  }

  if (!is.null(group.colours)) {
    colour_values <- rep(group.colours, 100)
  } else {
    colour_values <- rep(c(
      "#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051",
      "#00D1C1", "#FFAA91", "#B4A76C", "#9CA299", "#565A5C", "#00A04B", "#E54C20"
    ), 100)
  }

  base <- base + theme(legend.key.width = unit(3, "line")) + theme(text = element_text(
    size = 20,
    family = font.radar
  )) +
    theme(legend.text = element_text(size = legend.text.size), legend.position = legend.position) +
    theme(legend.key.height = unit(2, "line")) +
    scale_colour_manual(values = colour_values) +
    theme(text = element_text(family = font.radar)) +
    theme(legend.title = element_blank())


  if(isTRUE(fill)) {
    base <- base +
      scale_fill_manual(values = colour_values, guide = "none")
  }

  if(legend.title != "") {
    base <- base + theme(legend.title = element_text())
  }

  if (plot.title != "") {
    base <- base + ggtitle(plot.title)
  }

  return(base)
}
