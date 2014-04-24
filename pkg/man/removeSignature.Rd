\name{removeSignature}
\alias{removeSignature}
\alias{removeSignature.MailDocument}
\title{Remove E-Mail Signatures}
\description{
  Remove signature lines from an e-mail message.
}
\usage{
\method{removeSignature}{MailDocument}(x, \dots)
}
\arguments{
  \item{x}{A mail document.}
  \item{\dots}{the argument \code{marks} giving a character of signature
    identifications marks (in form of regular expression patterns). Note that
    the official signature start mark \code{-- } (dash dash blank) is always
    considered.}
}
\seealso{
  \code{\link{removeCitation}} to remove e-mail citations, and
  \code{\link{removeMultipart}} to remove non-text parts from multipart
  e-mail messages.
}
\examples{
require("tm")
newsgroup <- system.file("mails", package = "tm.plugin.mail")
news <- VCorpus(DirSource(newsgroup), readerControl = list(reader = readMail))
news[[7]]
removeSignature(news[[7]], marks = "^[+]-*[+]$")
}
\author{Ingo Feinerer}
