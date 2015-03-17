\name{inBE}
\alias{inBE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Check for the existence of a word in the basic english dictionary}
%%  ~~function to do ... ~~

\description{Check for the existence of a word in the basic english dictionary}
%%  ~~ A concise (1-5 lines) description of what the function does. ~~

\usage{
inBE(input, connection = odbc.textio, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{the string to check for}
%%     ~~Describe \code{input} here~~

  \item{connection}{an odbc connection}
%%     ~~Describe \code{connection} here~~

  \item{verbose}{return extra details}
%%     ~~Describe \code{verbose} here~~

}
\details{}
%%  ~~ If necessary, more details than the description above ~~

\value{either boolean if verbose = FALSE or a data.frame if verbose = TRUE}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

\references{}
%% ~put references to the literature/web site here ~

\author{Nate Giraldi, avant-gardED}
%%  ~~who you are~~

\note{}
%%  ~~further notes~~


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{}
%% ~~objects to See Also as \code{\link{help}}, ~~~

\examples{}
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (input, connection = odbc.textio, verbose = FALSE) 
{
    if (is.character(input) == FALSE) {
        stop("Input must be of class character")
    }
    if (is.logical(verbose) == FALSE) {
        stop("Verbose must be of class logical")
    }
    query <- paste0("SELECT def.Word word, def.Usage usage, def.Definition definition, cat.Category category FROM WordDefinition def LEFT JOIN WordCategory cat ON def.Word = cat.Word WHERE def.word IN (", 
        paste0("'", paste(c(input), collapse = "','"), "'"), 
        ")")
    results <- sqlQuery(connection, query)
    if (verbose == FALSE) {
        ifelse(nrow(results) == 0, FALSE, TRUE) \%>\% return()
    }
    else if (verbose == TRUE) {
        results \%>\% return()
    }
  }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
