inBE <- 
  function(input, connection = odbc.textio, verbose = FALSE) {
    
    if(is.character(input) == FALSE) {
      stop('Input must be of class character')
    }
    
    if(is.logical(verbose) == FALSE) {
      stop('Verbose must be of class logical')
    }
    
    query <- 
      paste0(
        "SELECT def.Word word, def.Usage usage, def.Definition definition, cat.Category category FROM WordDefinition def LEFT JOIN WordCategory cat ON def.Word = cat.Word WHERE def.word IN ("
        , paste0("'",paste(c(input), collapse = "','"),"'")
        , ")"
      )
    
    results <-
      sqlQuery(connection, query)
    
    if(verbose == FALSE) {
      ifelse(nrow(results) == 0, FALSE, TRUE) %>% return()
    } else if(verbose == TRUE) {
      results %>% return()
    }
    
  }
