tblsLoad <- 
  function(connection, table.schematic = NULL, unlist = FALSE) {
    
    require(RODBC)
    require(dplyr)
    
    if(is.logical(unlist) == FALSE) {
      stop('Unlist must be of class logical')
    }
    
    if(is.character(table.schematic) == FALSE & is.null(table.schematic) == FALSE) {
      stop('Table schematic input must be text or left NULL')
    }
    
    tbls <-  # load all table details into a df
      sqlTables(odbc.textio)
    
    if(is.null(table.schematic) == FALSE) {
      tbls <-
        tbls %>% filter(TABLE_SCHEM %in% table.schematic)
    }
    
    list.tbls <-
      Map(
        function(tbl)
          sqlQuery(odbc.textio,
                   paste0(
                     'SELECT * FROM ['
                     , tbl
                     , ']'
                   )
          ),
        tbls$TABLE_NAME
      )
    
    if(unlist == TRUE) {
      list.tbls %>% list2env(.GlobalEnv) %>% return()
    } else if(unlist == FALSE) {
      return(list.tbls)
    }
    
    # to unlist the tables in returned list use list2env(list.tbls, .GlobalEnv)
    
  }