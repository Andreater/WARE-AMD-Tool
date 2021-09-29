empty_plot <- function(combs){
  combs %>% 
    ggplot() +
    aes(x = log.comb.or, y = index) +
    geom_line(aes(color = log.comb.or,
                  size = 5)) +
    scale_colour_gradient2(low = "green2",
                           mid = "yellow2",
                           high = "red2") +
    geom_segment(aes(x = 0, xend = 0, y = 0.975, yend = 1.025)) +
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

risk_plot <- function(combs, calculated_risk) {
  
  combs %>% 
    ggplot() +
    aes(x = log.comb.or, y = index) +
    geom_line(aes(color = log.comb.or,
                  size = 5)) +
    scale_colour_gradient2(low = "green2",
                           mid = "yellow2",
                           high = "red2")+
    geom_point(aes(x = log(calculated_risk), y = 1), 
               color = "black",
               shape = 18,
               size = 3) +
    geom_segment(aes(x = 0, xend = 0, y = 0.975, yend = 1.025)) +
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