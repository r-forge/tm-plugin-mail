\name{convert_mbox_eml}
\alias{convert_mbox_eml}
\title{Convert E-Mails From mbox Format To eml Format}
\description{  
  Convert e-mails from mbox (i.e., several mails in a single box)
  format to eml (i.e., every mail in a single file) format.
}
\usage{
convert_mbox_eml(mbox, dir)
}
\arguments{
  \item{mbox}{A character or connection describing the mbox location.}
  \item{dir}{A character describing the output directory.}
}
\value{
  No explicit return value. As a side product the directory
  \code{dir} contains the e-mails in eml format.
}
\author{Ingo Feinerer}
