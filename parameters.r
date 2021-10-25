# ui parameters
plot1_height = 110
pie_legend_height = 100
radiobox_height = 340
pie_plot_height = 425
density_plot_height = 475
plotbox_height = 975

# server parameters
plot_dim = 100
round_to_decimals = 4
intorno_dim = 0.15

# plots parameters
colorpalette <- c("olivedrab2", "#ccd93d", "orangered2", "grey75")
subj_line_col = "red3"
y_text_annot_dens = 0.09
text_size_annot = 5.25
subj_text_dist_annot = 0.8
high_risk_text_dist_annot = 1
low_risk_text_dist_annot = 0.75
risk_zone_shade_alpha = 0.4
risk_plot_geom_line_size = 10
risk_plot_geom_point_shape = 18
risk_plot_geom_point_size = 4
legend_text_size = 16
legend_key_size = 2
pie3d_border = "grey25"
pie3d_start = 1.76
pie3d_radius = 1.5
pie3d_explode = 0.1
pie3d_theta = pi/4
pie3d_shade = 0.5
pie3d_mar = c(1,1,1,1)
density_text_size = 15
lims <- c(-9,7)
quantiles = quantile(df_ctrl$log.comb.or, probs = c(0.05, 0.99))
