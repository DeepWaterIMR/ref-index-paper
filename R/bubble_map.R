### Map to get an overview of the data

bubble_map <- function(x, label = expression(paste("Density (kg ", nmi^-2, ")")), zero_color = "grey", point_stroke = 0.3, ncol = 6, by_year = TRUE, filled_circles = TRUE, axis_text = FALSE, axis_title = FALSE, legend_rows = 1, base_size = 10) {
  
  # Guess columns
  
  tmp <- ggOceanMaps::guess_coordinate_columns(x)
  x[[names(tmp)[1]]] <- x[[tmp[1]]]
  x[[names(tmp)[2]]] <- x[[tmp[2]]]
  
  x$year <- x[[grep("year", colnames(x), ignore.case = TRUE, value = TRUE)[1]]]
  
  # Colors
  
  color_breaks <- 
    sort(unique(c(0, unname(round_any(quantile(x$value[x$value != 0], c(seq(0,1,0.25), 0.99,0.995)), 1, floor)), ceiling(max(x$value)))))
  
  color_breaks <- color_breaks[color_breaks <= (ceiling(max(x$value)) + 1)]
  
  # Map
  
  basemap(x, base_size = base_size, expand.factor = 1.05) + 
    ggspatial::geom_spatial_point(
      data = x %>% filter(value == 0), 
      aes(x = lon, y = lat, shape = value == 0), 
      color = zero_color, size = 1,
      crs = 4326, stroke = 2*point_stroke) + {
        if(filled_circles) 
          ggspatial::geom_spatial_point(
            data = x %>% filter(value > 0) %>% arrange(-value), 
            aes(x = lon, y = lat, size = value, fill = value,
                shape = value == 0), alpha = 0.4,
            crs = 4326, stroke = point_stroke) 
      } + {
        if(!filled_circles) 
          ggspatial::geom_spatial_point(
            data = x %>% filter(value > 0) %>% arrange(-value), 
            aes(x = lon, y = lat, size = value, color = value,
                shape = value == 0), alpha = 0.4,
            crs = 4326, stroke = point_stroke) 
      } +
    labs(
      fill = label,
      color = label,
      size = label
    ) +
    scale_shape_manual(values = c(`TRUE` = 3, `FALSE` = 21), 
                       guide = "none") +
    scale_size(
      breaks = color_breaks,
      limits = c(0, ceiling(max(x$value)) + 1),
      range = c(1, 14)
    ) + {
      if(filled_circles)
        binned_scale(
          aesthetics = "fill",
          scale_name = "stepsn",
          # trans = "sqrt",
          palette = function(x) c(viridis::turbo(length(color_breaks)-1)),
          limits = c(0, ceiling(max(x$value)) + 1),
          breaks = color_breaks,
          show.limits = TRUE
        )
    } + {
      if(!filled_circles)
        binned_scale(
          aesthetics = "color",
          scale_name = "stepsn",
          # trans = "sqrt",
          palette = function(x) c(viridis::turbo(length(color_breaks)-1)),
          limits = c(0, ceiling(max(x$value)) + 1),
          breaks = color_breaks,
          show.limits = TRUE
        )
    } +
    guides(
      fill = guide_legend(
        nrow = legend_rows, byrow = legend_rows > 1,
        keywidth=0.01, keyheight=0.01, default.unit="inch"), 
      color = guide_legend(
        nrow = legend_rows, byrow = legend_rows > 1,
        keywidth=0.01, keyheight=0.01, default.unit="inch"), 
      size = guide_legend(
        nrow = legend_rows, byrow = legend_rows > 1,
        override.aes = 
          list(shape = c(3, rep(21, length(color_breaks) - 1))),
        keywidth=0.01, keyheight=0.01, default.unit="inch")
    ) + {
      if(by_year) facet_wrap(~year, ncol = ncol)
    } + 
    {if(!axis_title) theme(axis.title = element_blank())} +
    {if(!axis_text) theme(axis.text = element_blank(),
                         axis.ticks = element_blank())} +
    theme(legend.spacing.x = unit(0.01, 'cm'),
          legend.spacing.y = unit(0.01, 'cm'),
          legend.text = element_text(margin = margin(0,0,0,0)),
          legend.position = "bottom")
  
}