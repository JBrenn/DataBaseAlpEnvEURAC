\name{dB_readZRX2station}
\alias{dB_readZRX2station}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
process ZRX data files (WISKI, batch download) with function \command{readZRX}
}
\description{
process ZRX data files (Province of Bozen/Bolzano, WISKI, batch download) with function \command{readZRX}: general quality check | min - max variable dependent, hourly aggregation (mean - sum) possible. Return .csv file for each station containing available variables.
}
\usage{
dB_readZRX2station(files, write_csv, output_path, do.hourly, do.quality, chron)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{files}{
  ZRX files names (absolute paths)
  }
  \item{write_csv}{
  boolean, TRUE: .csv data files for each station are written
  }
  \item{output_path}{
  path to which .csv files are written
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
List of zoo objects, for each station available variables are summarized in a zoo object
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
