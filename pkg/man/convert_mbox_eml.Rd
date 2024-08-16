\name{convert_mbox_eml}
\alias{convert_mbox_eml}
\title{Convert E-Mails From mbox Format To eml Format}
\description{  
  Convert e-mails from mbox (i.e., several mails in a single box)
  format to eml (i.e., every mail in a single file) format.
}
\usage{
convert_mbox_eml(mbox, dir, format = "mbox", delim = NULL)
}
\arguments{
  \item{mbox}{a character string or connection describing the mbox
    location.}
  \item{dir}{a character string describing the output directory.}
  \item{format}{see \code{\link{MBoxSource}}.}
  \item{delim}{see \code{\link{MBoxSource}}.}
}
\value{
  No explicit return value.
  As a side product the directory \code{dir} contains the e-mails in eml
  format.
}
\seealso{
  \url{https://www.loc.gov/preservation/digital/formats/fdd/fdd000388.shtml}.
}
\author{Ingo Feinerer and Kurt Hornik}
