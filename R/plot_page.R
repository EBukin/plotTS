# Plotting one page
plot_page <-
  function(df, plots_var, timeseries_var) {
    page_plots_list <-
      dlply(df, plots_var, build_one_plot, timeseries_var)
    grid_arrange_shared_legend(plots_list = page_plots_list)
  }
