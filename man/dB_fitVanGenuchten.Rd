\name{dB_fitVanGenuchten}
\alias{dB_fitVanGenuchten}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fit soil water retention curve to observed data
}
\description{
Fit soil water retention curve (van Genuchten model) to observed data \emph"L-BFGS-B" method in \emph{optim} function}
\usage{
dB_fitVanGenuchten(data, par0, theta_res, theta_sat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  observed data matrix, first column soil water content in m³/m³, second column soil water pressure head in hPa (cm water column)
}
 \item{par0}{
  named vector, containing starting value for van Genuchten parameters alpha and n
}
 \item{theta_res}{
  residual water content
}
 \item{theta_sat}{
  saturated water content
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
see \emph{?optim} value paragraph
}

\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
