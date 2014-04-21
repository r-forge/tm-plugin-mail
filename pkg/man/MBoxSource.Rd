\name{MBoxSource}
\alias{MBoxSource}
\title{Mailbox Source}
\description{
  Create a mailbox source.
}
\usage{
MBoxSource(mbox, encoding = "")
}
\arguments{
  \item{mbox}{A character string giving the path or \acronym{URL} to a mailbox
    stored in \dQuote{mbox} format.}
  \item{encoding}{A character string describing the current encoding. It is
    passed to \code{\link{iconv}} to convert the input to \code{"UTF-8"}.}
}
\details{
  A \emph{mailbox source} interprets each e-mail stored in the mailbox as a
  document.
}
\value{
  An object inheriting from \code{MBoxSource}, \code{\link[tm]{SimpleSource}},
  and \code{\link[tm]{Source}}.
}
\seealso{
  \code{\link{Encoding}} and \code{\link{iconv}} on encodings.
}
