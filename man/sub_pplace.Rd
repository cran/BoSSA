\name{sub_pplace}
\alias{sub_pplace}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
subsets a pplace object
}
\description{
subset a pplace object based on the placement_id, the name of the placement or a regular expression of the name of the placement
}
\usage{
sub_pplace(pplace, placement_id = NULL, ech_id = NULL, ech_regexp = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pplace}{
the pplace object to subset
}
  \item{placement_id}{
a vector of the placement_id to subset
}
  \item{ech_id}{
a vector of the names of the placement to subset
}
  \item{ech_regexp}{
a regular expression of the name of the placement to subset
}
}
\details{
Both the multiclass and the placement table are subseted. All the other compartiments of the list are left untouch.
}
\value{
another pplace object
}
\author{
pierre lefeuvre
}
\examples{

data(pplace)

###
sub1 <- sub_pplace(pplace,placement_id=1:5)

###
id <- c("GWZHISEQ01:514:HMCLFBCXX:2:1108:1739:60356_90",
"GWZHISEQ01:514:HMCLFBCXX:2:1114:13665:31277_80")
sub2 <- sub_pplace(pplace,ech_id=id)

###
sub3 <- sub_pplace(pplace,ech_regexp="^HWI")
}
