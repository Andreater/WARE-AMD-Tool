################# RISK ##################
##### empty
empty_risk_plot <- function(combs){
  temp = combs %>% 
    filter(!log.comb.or <= (quantiles[1] - risk_plot_lims_expand) & !log.comb.or >= (quantiles[2] + risk_plot_lims_expand)) # set bar lims
  
  temp %>% 
    ggplot() +
    aes(x = log.comb.or, y = index) +
    geom_line(aes(color = log.comb.or,
                  size = risk_plot_geom_line_size)) +
    scale_colour_gradient2(low = colorpalette[1],
                           mid = colorpalette[2],
                           high = colorpalette[3],
                           midpoint = (range(temp$log.comb.or)[1] + range(temp$log.comb.or)[2]) / 2) +
    theme_minimal() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y = element_blank(),
          axis.title.y=element_blank(),
          legend.position = "none",
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
}

###### with subject
risk_plot <- function(combs, calculated_risk) {
  
  if (log(calculated_risk) >= (quantiles[[2]] + risk_plot_lims_expand)) {
    calculated_risk = exp(quantiles[[2]] + risk_plot_lims_expand)
  } else if (log(calculated_risk) <= quantiles[[1]] - risk_plot_lims_expand) {
    calculated_risk = exp(quantiles[[1]] - risk_plot_lims_expand)
  } else { 
    calculated_risk = calculated_risk 
  }
  
  temp = combs %>% 
    filter(!log.comb.or <= (quantiles[1] - risk_plot_lims_expand) & !log.comb.or >= (quantiles[2] + risk_plot_lims_expand))
  
  temp %>% 
    ggplot() +
    aes(x = log.comb.or, y = index) +
    geom_line(aes(color = log.comb.or,
                  size = risk_plot_geom_line_size)) +
    scale_colour_gradient2(low = colorpalette[1],
                           mid = colorpalette[2],
                           high = colorpalette[3],
                           midpoint = (range(temp$log.comb.or)[1] + range(temp$log.comb.or)[2]) / 2) +
    geom_point(aes(x = log(calculated_risk), y = 1), 
               color = "black",
               shape = risk_plot_geom_point_shape,
               size = risk_plot_geom_point_size) +
    theme_minimal() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y = element_blank(),
          axis.title.y=element_blank(),
          legend.position = "none",
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
}

################# PIES ##################
##### empties
empty_pie_chart <- function() {

  df = data.frame(Class = c("Equal"),
                  value = c(100))
  
  pie3D(x       = df$value,
        col     = c(colorpalette[4]),
        border  = pie3d_border,
        start   = pie3d_start,
        radius  = pie3d_radius,
        explode = pie3d_explode,
        theta   = pie3d_theta,
        shade   = pie3d_shade,
        main    = "Relative risk",
        mar     = pie3d_mar)
}

###### with subject
pie_chart_casi <- function(calculated_risk, intorno) {
  calculated_risk = log(calculated_risk)
  
  greater = sum(df_casi$log.comb.or > calculated_risk + intorno)
  less    = sum(df_casi$log.comb.or < calculated_risk - intorno)
  equal   = sum(df_casi$log.comb.or < calculated_risk + intorno & df_casi$log.comb.or > calculated_risk - intorno) 
  
  df = data.frame(Class = c("Greater", "Less", "Equal"),
                  value = c(greater, less, equal))
  df = df %>% 
    filter(value > 0)
  
  colvec <- select_colors(df)
  
  pie3D(x       = df$value,
        col     = colvec,
        border  = pie3d_border,
        start   = pie3d_start,
        radius  = pie3d_radius,
        explode = pie3d_explode,
        theta   = pie3d_theta,
        shade   = pie3d_shade,
        main    = "Relative risk in AMD population",
        mar     = pie3d_mar)
}


pie_chart_ctrl <- function(calculated_risk, intorno) {
  calculated_risk = log(calculated_risk)
  
  greater = sum(df_ctrl$log.comb.or > calculated_risk + intorno)
  less    = sum(df_ctrl$log.comb.or < calculated_risk - intorno)
  equal   = sum(df_ctrl$log.comb.or < calculated_risk + intorno & df_ctrl$log.comb.or > calculated_risk - intorno) 
  
  df = data.frame(Class = c("Greater", "Less", "Equal"),
                  value = c(greater, less, equal))
  
  df = df %>% 
    filter(value > 0)
  
  colvec <- select_colors(df)
  
  pie3D(x       = df$value,
        col     = colvec,
        border  = pie3d_border,
        start   = pie3d_start,
        radius  = pie3d_radius,
        explode = pie3d_explode,
        theta   = pie3d_theta,
        shade   = pie3d_shade,
        main    = "Relative risk in general population",
        mar     = pie3d_mar)
}

################# LEGEND ###########
draw_legend <- function(){
  df <- data.frame(Class = c("Greater", "Less", "Equal"),
                   Dummy = c("dummy1", "dummy2", "dummy3"))
  
  dummy_plot <- ggplot(df, aes(Dummy, fill = Class)) +
    geom_bar() +
    scale_fill_manual(values = c(colorpalette[4], colorpalette[3], colorpalette[1])) +
    guides(fill = guide_legend(nrow = 1)) + 
    labs(fill='Colors legend:') +
    theme(legend.key.size = unit(legend_key_size, "cm"),
          legend.text = element_text(size = legend_text_size),
          legend.title = element_text(size = legend_text_size))
  
  legend <- get_legend(dummy_plot)
  
  grid.newpage()
  grid.draw(legend)
}

################# DENSITY ##################
##### empties ----
empty_density_plot_cases <- function(calculated_risk){
  x <- df_casi$log.comb.or
  y <- density(x, n = 2^12,
               adjust = 0.9)
  
  ggplot(data.frame(x = y$x, y = y$y), aes(x, y)) +
    annotate("rect",
             xmin = min(y$x), xmax = quantiles[1],
             ymin = min(y$y), ymax = max(y$y),
             alpha = risk_zone_shade_alpha,
             fill = colorpalette[1]) +
    annotate("rect",
             xmin = quantiles[2], xmax = max(y$x),
             ymin = min(y$y), ymax = max(y$y),
             alpha = risk_zone_shade_alpha,
             fill = colorpalette[3]) +
    geom_line(colour = "grey25",
              size = 1.2) + 
    geom_segment(aes(xend = x, yend = 0, colour = x)) + 
    scale_color_gradient2(low = colorpalette[1],
                          mid = colorpalette[2],
                          high = colorpalette[3]) +
    geom_vline(xintercept = quantiles[1], linetype = 5) +
    geom_vline(xintercept = quantiles[2], linetype = 5) +
    annotate(geom = "text",
             x = quantiles[1] - low_risk_text_dist_annot_cases,
             y = y_text_annot_dens,
             label = "Low risk\nzone",
             size = text_size_annot) +
    annotate(geom = "text",
             x = quantiles[2] + high_risk_text_dist_annot_cases,
             y = y_text_annot_dens,
             label = "High risk\nzone",
             size = text_size_annot) +
    theme_classic() +
    theme(text = element_text(size = density_text_size),
          legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks = NULL,
                       limits = lims) +
    scale_y_continuous(breaks = NULL) +
    labs(y = "Frequency",
         x = "Risk",
         title = "Risk distribution in AMD population")
}

empty_density_plot_ctrl <- function(calculated_risk){
  x <- df_ctrl$log.comb.or
  y <- density(x, n = 2^12,
               adjust = 0.9)
  
  ggplot(data.frame(x = y$x, y = y$y), aes(x, y)) +
    annotate("rect",
             xmin = min(x)-0.26, xmax = quantiles[1],
             ymin = min(y$y), ymax = max(y$y),
             alpha = risk_zone_shade_alpha,
             fill = colorpalette[1]) +
    annotate("rect",
             xmin = quantiles[2], xmax = max(y$x),
             ymin = min(y$y), ymax = max(y$y),
             alpha = risk_zone_shade_alpha,
             fill = colorpalette[3]) +
    geom_line(colour = "grey25",
              size = 1.2) + 
    geom_segment(aes(xend = x, yend = 0, colour = x)) + 
    scale_color_gradient2(low = colorpalette[1],
                          mid = colorpalette[2],
                          high = colorpalette[3]) +
    geom_vline(xintercept = quantiles[1], linetype = 5) +
    geom_vline(xintercept = quantiles[2], linetype = 5) +
    annotate(geom = "text",
             x = quantiles[1] - low_risk_text_dist_annot_ctrl,
             y = y_text_annot_dens,
             label = "Low risk\nzone",
             size = text_size_annot) +
    annotate(geom = "text",
             x = quantiles[2] + high_risk_text_dist_annot_ctrl,
             y = y_text_annot_dens,
             label = "High risk\nzone",
             size = text_size_annot) +
    theme_classic() +
    theme(text = element_text(size = density_text_size),
          legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks = NULL,
                       limits = lims) +
    scale_y_continuous(breaks = NULL) +
    labs(y = "Frequency",
         x = "Risk",
         title = "Risk distribution in general population")
}

##### with subject ----
density_plot_cases <- function(calculated_risk){
  
  if (log(calculated_risk) > 7) {
    calculated_risk = exp(7)
  }
  
  x <- df_casi$log.comb.or
  y <- density(x, n = 2^12,
               adjust = 0.9)
  
  ggplot(data.frame(x = y$x, y = y$y), aes(x, y)) +
    annotate("rect",
             xmin = min(y$x), xmax = quantiles[1],
             ymin = min(y$y), ymax = max(y$y),
             alpha = risk_zone_shade_alpha,
             fill = colorpalette[1]) +
    annotate("rect",
             xmin = quantiles[2], xmax = max(y$x),
             ymin = min(y$y), ymax = max(y$y),
             alpha = risk_zone_shade_alpha,
             fill = colorpalette[3]) +
    geom_line(colour = "grey25",
              size = 1.2) + 
    geom_segment(aes(xend = x, yend = 0, colour = x)) + 
    scale_color_gradient2(low = colorpalette[1],
                          mid = colorpalette[2],
                          high = colorpalette[3]) +
    geom_vline(xintercept = log(calculated_risk),
               size = 1.25,
               color = subj_line_col) +
    geom_vline(xintercept = quantiles[1], linetype = 5) +
    geom_vline(xintercept = quantiles[2], linetype = 5) +
    annotate(geom = "text",
             x = quantiles[1] - low_risk_text_dist_annot_cases,
             y = y_text_annot_dens,
             label = "Low risk\nzone",
             size = text_size_annot) +
    annotate(geom = "text",
             x = quantiles[2] + high_risk_text_dist_annot_cases,
             y = y_text_annot_dens,
             label = "High risk\nzone",
             size = text_size_annot) +
    annotate(geom = "text",
             x = log(calculated_risk) - subj_text_dist_annot,
             y = 0.15,
             label = "Subject -->",
             size = text_size_annot) +
    theme_classic() +
    theme(text = element_text(size = density_text_size),
          legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks = NULL,
                       limits = lims) +
    scale_y_continuous(breaks = NULL) +
    labs(y = "Frequency",
         x = "Risk",
         title = "Risk distribution in AMD population")
}

density_plot_ctrl <- function(calculated_risk){
  
  if (log(calculated_risk) > 7) {
    calculated_risk = exp(7)
  }
  
  x <- df_ctrl$log.comb.or
  y <- density(x, n = 2^12,
               adjust = 0.9)
  
  ggplot(data.frame(x = y$x, y = y$y), aes(x, y)) +
    annotate("rect",
             xmin = min(x)-0.26, xmax = quantiles[1],
             ymin = min(y$y), ymax = max(y$y),
             alpha = risk_zone_shade_alpha,
             fill = colorpalette[1]) +
    annotate("rect",
             xmin = quantiles[2], xmax = max(y$x),
             ymin = min(y$y), ymax = max(y$y),
             alpha = risk_zone_shade_alpha,
             fill = colorpalette[3]) +
    geom_line(colour = "grey25",
              size = 1.2) + 
    geom_segment(aes(xend = x, yend = 0, colour = x)) + 
    scale_color_gradient2(low = colorpalette[1],
                          mid = colorpalette[2],
                          high = colorpalette[3]) +
    geom_vline(xintercept = log(calculated_risk),
               size = 1.25,
               color = subj_line_col) +
    geom_vline(xintercept = quantiles[1], linetype = 5) +
    geom_vline(xintercept = quantiles[2], linetype = 5) +
    annotate(geom = "text",
             x = quantiles[1] - low_risk_text_dist_annot_ctrl,
             y = y_text_annot_dens,
             label = "Low risk\nzone",
             size = text_size_annot) +
    annotate(geom = "text",
             x = quantiles[2] + high_risk_text_dist_annot_ctrl,
             y = y_text_annot_dens,
             label = "High risk\nzone",
             size = text_size_annot) +
    annotate(geom = "text",
             x = log(calculated_risk) - subj_text_dist_annot,
             y = 0.15,
             label = "Subject -->",
             size = text_size_annot) +
    theme_classic() +
    theme(text = element_text(size = density_text_size),
          legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks = NULL,
                       limits = lims) +
    scale_y_continuous(breaks = NULL) +
    labs(y = "Frequency",
         x = "Risk",
         title = "Risk distribution in general population")
}