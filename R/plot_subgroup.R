# Plot a subgroup only when number of plotsin agroup is bigger than nyumber of plots per page
plot_subgroup <- function(df, n_page, plots_var, timeseries_var) {
  n_plots <-
    df %>%
    group_by_(.dots = plots_var) %>%
    n_groups
  
  # Split dataframe into groups
  if (n_plots > n_page) {
    df <-
      df %>%
      group_by_(.dots = plots_var) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      mutate(sub_page = rep(1:nrow(.), each = n_page)[1:nrow(.)]) %>%
      left_join(df, by = plots_var)
    
  } else {
    df <-
      df %>%
      mutate(sub_page = TRUE)
  }
  d_ply(df,
        .(sub_page),
        plot_page,
        plots_var = plots_var,
        timeseries_var = timeseries_var)
}