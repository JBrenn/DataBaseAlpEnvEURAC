\name{dB_getMETEO}
\alias{dB_getMETEO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get meteorological data from micro-meteo station (Mazia/Matsch)
}
\description{
GEt raw micro-meteo station data (Mazia/Matsch)
}
\usage{
dB_getMETEO(path2files, header.file, station, cols = c("Rain","WindSpeed","WindDirection","RH","Temp","SolarRadiation"), plot=FALSE, pdf.name, mkreg=FALSE)
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
  \item{cols}{
  variables to selecting for retrieving data, default: c("Rain","WindSpeed","WindDirection","RH","Temp"","SolarRadiation")
  }
  \item{plot}{
   boolean, default = FALSE; if TRUE: zoo object is visualized in pdf
  }
  \item{pdf.name}{
  name of pdf file
  }
  \item{mkreg}{
  boolean, default = FALSE; if TRUE: make time series data regular
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
