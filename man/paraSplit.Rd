\name{paraSplit}
\alias{paraSplit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Split a paragraph into constituent sentences}
%%  ~~function to do ... ~~

\description{Split the sentences present in a paragraph into sentences and provide some very basic summary information about each sentence.}
%%  ~~ A concise (1-5 lines) description of what the function does. ~~

\usage{
parasplit(paragraph, split = '[.]')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{paragraph}{
a string
}
  \item{split}{
what to split the paragraph string on
}
}
\details{}
%%  ~~ If necessary, more details than the description above ~~

\value{list of sentences with summary statistics included in the list object
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{}
%% ~put references to the literature/web site here ~

\author{Nate Giraldi, avant-gardED}
%%  ~~who you are~~

\note{}
%%  ~~further notes~~


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--  or do  help(data=index)  for the standard data sets.

## The function is currently defined as
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
