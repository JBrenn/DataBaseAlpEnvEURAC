\name{dB_readZRX}
\alias{dB_readZRX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
read ZRX data file (WISKI, batch download)
}
\description{
read ZRX data file (Province Bozen/Bolzano, WISKI, batch download); general quality check | min and max, variable dependent; hourly aggregation (mean - sum) possible}
\usage{
dB_readZRX(file, do.hourly=FALSE, do.quality=FALSE, chron=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
  ZRX file name (absolute path)
  }
  \item{do.hourly}{
  boolean, TRUE: hourly aggregation of data, variable specific
  }
  \item{do.quality}{
  boolean, TRUE: general quality check (min - max)
  }
  \item{chron}{
  boolean, TRUE: chron object for datetime representation, FALSE: POSIX object for datetime representation
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
LIST
\item{data}{list, zoo time series object for each station}
\item{meta}{data frame, station meta data}
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
