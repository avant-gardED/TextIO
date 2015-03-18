paraSplit <-
  function(paragraph, split = '[.]') {
    
    require(dplyr)
    require(stringr)
    
    if(is.character(paragraph) == FALSE) {
      stop('The paragraph input must be of class character.')
    }
    
    para <-
      paragraph %>% tolower()
    
    sents <- 
      para %>%
      tolower() %>%
      str_split(pattern = '[.] *') %>%
      unlist()
    
    words <-
      sapply(sents, function(x) str_count(x, '\\S+'))
    
    chars <-  # count the number of characters in a sentence
      sapply(sents, nchar)
    
    if(length(words) != length(sents) | length(sents) != length(chars)) {
      stop('There is an error in the input string.')
    }
    
    paragraph.split <-
      list(
        sentences = sents
        , n.word = words
        , n.char = chars
      )
    
    return(paragraph.split)
    
  }