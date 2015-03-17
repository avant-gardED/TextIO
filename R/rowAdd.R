rowAdd <-
  function(
    channel, df, tablename) 
  {
    
    require(RODBC)
    require(dplyr)
    
    if(df %>% is.data.frame() == FALSE) {
      stop('Data to be appended to table in database isn\'t of class data.frame')
    }
    
    k.tbl.exist <-  # check if tablename exists in connection
      tablename %in% sqlTables(channel)$TABLE_NAME
    
    if(k.tbl.exist == FALSE) {  # break if tablename doesn't exist
      stop('Table does not exist in the connection specified')
    }
    
    k.channel.table.ncol <-  # num columns in dest table 
      sqlQuery(channel, paste('SELECT * FROM', tablename), max = 1) %>%
      ncol()
    
    if((ncol(df) != k.channel.table.ncol)) {  # check if input has same dim as dest
      stop('Input does not have the same number of columns as destination table')
    }
    
    sqlSave(  # add rows to tableA=-
      channel = channel,
      dat = df,
      tablename = tablename,
      append = TRUE  # add rows
      , colnames = FALSE
      , rownames = FALSE
      , addPK = FALSE
      , fast = TRUE
    )
    
  }