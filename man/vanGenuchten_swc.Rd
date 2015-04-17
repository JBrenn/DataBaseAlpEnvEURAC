\name{vanGenuchten_swc}
\alias{vanGenuchten_swc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
van Genuchten model / soil water retention curve}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
vanGenuchten_swc(par, psi, theta_res, theta_sat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{par}{
name vector containing paramters to be optimized (alpha and N)
}
 \item{psi}{
water paressure head (in hPa, cm water column)
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
Volumetric soil water content
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
