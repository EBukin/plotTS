


# TITLE:      Functions for plotting FAOSTAT data into .pdf files
# DESCRIPTION:This file contains three functions:
#				build_fs_plot :
#				write_plots_pdf_one :
#				plot_fs_df :
# AUTHOR:     Eduard Bukin, Javier Montero
# STARTED ON: 7/03/2016
# VERSION:    v1.0
# PURPOSE:



# Plotting function
build_one_plot <-
  function(data, timeseries_var, flagList = NA) {
    require(plyr)
    require(dplyr)
    require(tidyr)
    require(grid)
    require(ggplot2)
    require(ggthemes)
    require(scales)
    
    # one big IF statement to clean the resist against the breaking funciton when there is no data.
    if (nrow(data %>% filter(!is.na(Value))) == 0)
    {
      g <- ggplot(data.frame(x = c(1),
                             y = c(1)),
                  aes(x = x, y = y)) +
        annotate(
          geom = "text",
          x = 1,
          y = 1,
          label = paste0("There is no data")
        )
      
      
    } else {
      g <- ggplot(data = data)
      g <-
        g +
        aes_string(
          "Year",
          "Value",
          fill = timeseries_var,
          colour = timeseries_var,
          linetype = timeseries_var
        )
      
      # To avoid error: "geom_path: Each group consists of only one observation. Do you need to adjust the group aesthetic?
      # We check, if there is only one observation in each group
      many_obs <-
        data %>%
        filter(!is.na(Value)) %>%
        group_by_(.dots = timeseries_var) %>%
        summarise(num_obs = n()) %>%
        mutate(num_obs = ifelse(num_obs <= 1, FALSE, TRUE)) %>%
        .$num_obs %>%
        any()
      
      
      # If ther flags were pecified
      if (!all(is.na(flagList))) {
        g <- g + geom_point(aes(shape = Flag))
        g <- g + scale_shape_manual(values = flagList)
      } else {
        g <- g + geom_point()
      }
      
      # If there were only one observation in each group
      if (many_obs) {
        g <- g + geom_line()
      }
      
      # If the variable with units is provided we put them on the plot
      if ("Unit" %in% names(data)) {
        g <- g + ylab(paste0("Value ", unique(data$Unit)))
      } else {
        g <- g + ylab(paste0("Value"))
      }
    }
    ItemCode_t <- unique(data$ItemCode)
    ElementCode_t <- unique(data$ElementCode)
    # Titles
    g <-
      g +
      ggtitle(
        str_c(str_replace_na(c(ElementCode_t, " - ", unique(data$ElementName), ";\n",
              ItemCode_t, " - ", unique(data$ItemName), ";\n", 
              unique(data$AreaCode), " - ", unique(data$AreaName))), collapse = "")
      ) +
      scale_x_continuous(minor_breaks = seq(min(data$Year) , max(data$Year), 1),
                         breaks = pretty_breaks(n = (max(data$Year) - min(data$Year)) / 2)) +
      scale_y_continuous() +
      expand_limits(y = 0)
    
    g <- g + xlab("Year")
    g + 
      # theme_excel() + 
      # scale_colour_excel(palette = "new") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_vline(xintercept = 2016)
    
  }