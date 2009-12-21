\name{distGPS}
\alias{distGPS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Compute physical distance between point using GPS coordinates }
\description{
  The function read a data frame with the colomn "nom","lat" and "lon" (respectively, the name of the sample, the longitude in degree.decimal and lattitude in degree.decimal) and compute the distance between point in kilometers.
}
\usage{
distGPS(input)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{A data frame with the colomn "nom","lat" and "lon" (respectively, the name of the sample, the longitude in degree.decimal and lattitude in degree.decimal)}
}
\details{
  Require the SoDA package.
}
\value{
  The output is a distance matrix with row and colomn names corresponding to the "nom" colomns of the input data frame.
}
\author{ Pierre Lefeuvre }

\examples{
input <- data.frame(nom=c("a","b","c"),lat=c(-21,18,12),lon=c(32,16,-5))
distGPS(input)
}
