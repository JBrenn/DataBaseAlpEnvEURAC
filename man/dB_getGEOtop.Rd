\name{dB_getGEOtop}
\alias{dB_getGEOtop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get meteorological data from micro-meteo station (Mazia/Matsch) in GEOtop format
}
\description{
Create GEOtop meteo input file from raw micro-meteo station (Mazia/Matsch) for a specific time frame
}
\usage{
dB_getGEOtop(path2files, header.file, station, READvar = c("Rain","WindSpeed","WindDirection","RH","Temp","SolarRadiation"), var2geotop = c("Rain","WindSpeed","WindDirection","RH","Temp","SolarRadiation", "CloudTrans"), GEOtopVAR= c("Iprec", "WindSp", "WindDir", "RelHum", "AirT", "SWglobal", "CloudTrans"), mkreg=FALSE,  aggr_time, cut_date=FALSE, time_window, file.name, skipwinter=FALSE, TempThresh=-2, tz="Etc/GMT-1")
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
  station specification, e.g. "B2"
  }
  \item{READvar}{
  variables to selecting for retrieving data
  }
  \item{var2geotop}{
  variables included in GEOtop input file (previous names)
  }
  \item{GEOtopVAR}{
  variables included in GEOtop input file (GEOtop names) 
  }
  \item{mkreg}{
  make time series data regular
  }
  \item{aggr_time}{
  character, time aggregation applied. "n" no aggregation of raw data,"h": hourly, "d": daily
  }
  \item{cut_date}{
  boolean, TRUE: data is retrieved for specific time period; FALSE: data is retrieved for time period available; default = FALSE
  }
  \item{time_window}{
  as.POSIXct vector, start and end datetime for the period data should be retrieved
  }
  \item{file.name}{
  name of file written out
  }
  \item{skipwinter}{
  boolean, set winter precipitation NA; adjustable by the argument \command{TempThresh}, see below
  }
  \item{TempThresh}{
  air temperature threshold for setting precipitation NA
  }
  \item{tz}{
  timezone spezification
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
