plot_group <- function(df, n_page, groups_var, plots_var, timeseries_var) {
  if (is.na(groups_var)) groups_var <- "Any"
  d_ply(df, groups_var, plot_subgroup, n_page, plots_var, timeseries_var,
      .progress = "text")
}