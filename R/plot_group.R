plot_group <- function(df,
                       n_page = 1,
                       groups_var = NA,
                       plots_var = NA,
                       timeseries_var = "d.source",
                       all_vars = c("AreaCode", "ItemCode", "ElementCode", "d.source")) {
  # Everything field for files by
  df <- df %>% mutate(All = 1)
  
  if (any(is.na(groups_var))) groups_var <- "All"
  
  d_ply(
    df,
    groups_var,
    plot_subgroup,
    n_page,
    plots_var,
    timeseries_var,
    all_vars,
    .progress = "text"
  )
}