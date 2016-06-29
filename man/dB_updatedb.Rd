\name{dB_updatedb}
\alias{dB_updatedb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
update SQLite database
}
\description{
Writing / Updating SQLite database which is used for downloading data with the SWC calibration tool (shiny app). 
}
\usage{
dB_updatedb(stations, variables, path2data, inCloud, write.csv, return_data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stations}{
character vector, station to update, e.g. c("B1", "B2", "B3"), defualt: c("B1","B2","B3","P1","P2","P3","I1","I3","M1","M2","M3","M4","M5","M6","M7","S2", "S4", "S5", "XS1", "XS6", "SF1", "SF2", "SF3", "SF4", "SF5")
}
  \item{variables}{
character, which variables to read, e.g "TOTAL" for unqueried data set, "METEO" for meteorological data, "SWC" for soil moisture, "TSoil" for soil temperature
}
  \item{inCloud}{
character, path where database is stored, if NULL database will be stored in package directory of \emph{SMCcalibration} where it is needed for plugging in the calibration tool, default: ownCloud directory data, download link.
}
  \item{write.csv}{
boolean, if TRUE data is written in .csv files station wise, default: FALSE
}
  \item{return_data}{
boolean, if TRUE data is returned, default: TRUE
}
 }

\value{
List containing timer series zoo objects names as argument stations

SQLite data base in th inCloud folder; if write.csv is TRUE, station wise .csv files (pretty format, to read with readr::read_csv or data.table::fread)
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Johannes Brenner, \email{johannesbrenner@gmail.com}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ RSQLite }
\keyword{ Mazia }% __ONLY ONE__ keyword per line
