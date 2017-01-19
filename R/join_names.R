
#' Join AreaName, ItemName and ElementName + Unit to a data frame with the respective codes (AreaCode, ItemCode, ElementCode)
#' 
#' The function joins the labels AreaName, ItemName and ElementName to a dataframe with the respective codes.
#' All labels are sourced from the folder specified in the path. 
#' 
#' @param df is a FAOSTAT structured data frame with at least the columns: AreaCode, ItemCode and ElementCode.
#' @param mapping_tbl_path is the path of the folder with the following mapping tables: "item_names.csv", "element_names.csv" and "faostat_areas.csv"
join_names <-
  function(df, 
           reference_tbl = "./data/references.rdata") {
    
    load(reference_tbl)
    
    to_remove <- c("AreaName", "ItemName", "ElementName", "Unit")
    
    l_ply(to_remove, 
          function(x) {
            if (x %in% colnames(df))
            {
              df <<- df %>% select_(.dots = str_c("-",x))
            }
          })
    
    to_add <- c("AreaCode", "ElementCode", "ItemCode")
    l_ply(to_add, 
          function(x) {
            if (x %in% colnames(df))
            {
              df <<- 
                df %>% 
                mutate_(.dots = setNames(str_c("as.character(", x, ")"), x)) %>% 
                left_join(eval(parse(text = x)), by = x)
            }
          })
    
    to_order <- c("AreaCode", "AreaName", "ItemCode", "ItemName", "ElementCode", "ElementName", "Unit")
    df[,c(to_order[to_order %in% names(df)], 
          names(df)[!names(df) %in% to_order])]
      
  }
