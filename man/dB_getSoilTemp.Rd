\name{dB_getSoilTemp}
\alias{dB_getSoilTemp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get soil temperature from EURAC micro-meteo station (Mazia/Matsch)
%%  ~~function to do ... ~~
}
\description{
Retrieve soil temperature data from EURAC micro-meteo station located in Mazia/Matsch
}
\usage{
dB_getSoilTemp(path2files, header.file, station, station_nr, aggregation, minVALUE=-30, maxVALUE=30, clear_raw_data=FALSE, write.csv=FALSE, path2write)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path2files}{
  path to meteo files
  }
  \item{header.file}{
  header file, absolute path and file name
  }
   \item{station}{
  character, name of micro-meteo station, e.g. "B"
  }
  \item{station_nr}{
  integer, number of micro-meteo station, e.g. 2
  }
  \item{aggregation}{
  character, time aggregation applied. "n" no aggregation of raw data,"h": hourly, "d": daily
  }
  \item{minVALUE}{
  numeric, minimum value of soil temperature for filtering, default = -30 degC
  }
  \item{maxVALUE}{
  numeric, maximum value of soil temperature for filtering, default = +30 degC
  }
  \item{clear_raw_data}{
  boolean, TRUE: clearing of raw data, not yet implemented, default = FALSE
  }
  \item{write.csv}{
  boolean, default = FALSE; TRUE: .csv file is written to path2write, FALSE: no .csv file is written
  }
   \item{path2write}{
  path data should be written to
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\enumerate{
  \item zoo object containing processed data
  \item file output containing processed data, .csv format
}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Johannes Brenner, \email{Johannes.Brenner@eurac.edu}
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
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
