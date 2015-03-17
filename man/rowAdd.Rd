\name{rowAdd}
\alias{rowAdd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Insert a row into an SQL table}
%%  ~~function to do ... ~~

\description{Insert the rows of a data.frame object into a database table present in the specified ODBC connection}
%%  ~~ A concise (1-5 lines) description of what the function does. ~~

\usage{
rowAdd(channel, df, tablename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{channel}{
an odbc connection
}
  \item{df}{
the data frame to be inserted
}
  \item{tablename}{
a string specifying the table in the odbc connection for the rows to be inserted into
}
}
\details{}
%%  ~~ If necessary, more details than the description above ~~

\value{nothing will be returned from this function as it inserts rows into a table
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
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (channel, df, tablename) 
{
    require(RODBC)
    require(dplyr)
    if (df \%>\% is.data.frame() == FALSE) {
        stop("Data to be appended to table in database isn't of class data.frame")
    }
    k.tbl.exist <- tablename \%in\% sqlTables(channel)$TABLE_NAME
    if (k.tbl.exist == FALSE) {
        stop("Table does not exist in the connection specified")
    }
    k.channel.table.ncol <- sqlQuery(channel, paste("SELECT * FROM", 
        tablename), max = 1) \%>\% ncol()
    if ((ncol(df) != k.channel.table.ncol)) {
        stop("Input does not have the same number of columns as destination table")
    }
    sqlSave(channel = channel, dat = df, tablename = tablename, 
        append = TRUE, colnames = FALSE, rownames = FALSE, addPK = FALSE, 
        fast = TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
