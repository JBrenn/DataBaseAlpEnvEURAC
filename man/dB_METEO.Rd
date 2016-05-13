\name{dB_getMETEO}
\alias{dB_getMETEO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get regular meteorological data time series from micro-meteo station (Mazia/Matsch)
}
\description{
GEt raw micro-meteo station data (Mazia/Matsch)
}
\usage{
dB_getMETEO(path2data, station,  cols = c("Tip_Precip_Tot","Wind_Speed","Wind_Dir","RH","T_Air","SR_Sw"), 
            plot=FALSE, pdf.name)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path2data}{
  path to meteo data
  }
  \item{station}{
  station specification, e.g. "B2"
  }
  \item{cols}{
  variables to selecting for retrieving data, default: c("Tip_Precip_Tot","Wind_Speed","Wind_Dir","RH","T_Air","SR_Sw")
  }
  \item{plot}{
   boolean, default = FALSE; if TRUE: zoo object is visualized in pdf
  }
  \item{pdf.name}{
  name of pdf file
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\value{
\enumerate{
  \item zoo object containing processed data
  \item file output containing processed data in .csv format, if write.csv = TRUE
}
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
