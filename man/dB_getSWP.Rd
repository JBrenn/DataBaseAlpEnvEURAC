\name{dB_getSWP}
\alias{dB_getSWP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get regular soil water potential time series from EURAC micro-meteo station (Mazia/Matsch)
%%  ~~function to do ... ~~
}
\description{
Retrieve regular soil potential time series from EURAC micro-meteo station located in Mazia/Matsch
}
\usage{
dB_getSWP(path2data, station="B2", aggregation="n", minVALUE=-2000, maxVALUE=2000, clear_raw_data=FALSE, 
          write.csv=FALSE, path2write="./")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path2data}{
  path to meteo data
  }
   \item{station}{
  character, name of micro-meteo station, default "B2"
  }
  \item{aggregation}{
  character, time aggregation applied. "n" no aggregation of raw data,"h": hourly, "d": daily
  }
  \item{minVALUE}{
  numeric, minimum value of soil temperature for filtering, default = -2000 hPa
  }
  \item{maxVALUE}{
  numeric, maximum value of soil temperature for filtering, default = 2000 hPa
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
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Johannes Brenner, \email{Johannes.Brenner@eurac.edu}
}
\note{
Only station measuring soil water potential is station B2
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
