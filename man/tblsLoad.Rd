\name{tblsLoad}
\alias{tblsLoad}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Load all tables from an ODBC connection}
%%  ~~function to do ... ~~

\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tblsLoad(connection, table.schematic = NULL, unlist = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{connection}{an ODBC connection
%%     ~~Describe \code{connection} here~~
}
  \item{table.schematic}{a string specifying the type of table schematic to load. if NULL all tables will be loaded.
%%     ~~Describe \code{table.schematic} here~~
}
  \item{unlist}{if TRUE all tables will be loaded as data.frames into the global environment. additionally, if true do not assign the output of this function to a variable. if false the tables will be returned in a list object.
%%     ~~Describe \code{unlist} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Nate Giraldi, avant-gardED}
%%  ~~who you are~~

\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (connection, table.schematic = NULL, unlist = FALSE) 
{
    require(RODBC)
    require(dplyr)
    if (is.logical(unlist) == FALSE) {
        stop("Unlist must be of class logical")
    }
    if (is.character(table.schematic) == FALSE & is.null(table.schematic) == 
        FALSE) {
        stop("Table schematic input must be text or left NULL")
    }
    tbls <- sqlTables(odbc.textio)
    if (is.null(table.schematic) == FALSE) {
        tbls <- tbls \%>\% filter(TABLE_SCHEM \%in\% table.schematic)
    }
    list.tbls <- Map(function(tbl) sqlQuery(odbc.textio, paste0("SELECT * FROM [", 
        tbl, "]")), tbls$TABLE_NAME)
    if (unlist == TRUE) {
        list.tbls \%>\% list2env(.GlobalEnv) \%>\% return()
    }
    else if (unlist == FALSE) {
        return(list.tbls)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
