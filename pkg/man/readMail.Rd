\name{readMail}
\alias{readMail}
\title{Read In an E-Mail Document}
\description{
  Return a function which reads in an electronic mail document.
}
\usage{
readMail(DateFormat = character())
}
\arguments{
  \item{DateFormat}{A character vector giving date-time formats for the
    \dQuote{Date} header field in the mail document.  By default, the
    \dQuote{basic} formats of RFC 5322 are tried.}
}
\details{
  Formally this function is a function generator, i.e., it returns a
  function (which reads in a mail document) with a well-defined
  signature, but can access passed over arguments (e.g., the
  \dQuote{Date} header format) via lexical scoping.
}
\value{
  A \code{function} with the following formals:
  \describe{
    \item{\code{elem}}{a named list with the component \code{content}
      which must hold the document to be read in.}
    \item{\code{language}}{a string giving the language.}
    \item{\code{id}}{a character giving a unique identifier for the
      created text document.}
  }
  The function returns a \code{\link{MailDocument}} representing the
  text and metadata extracted from \code{elem$content}.  The argument
  \code{id} is used as fallback if no corresponding metadata entry is
  found in \code{elem$content}.
}
\seealso{
  \code{\link[tm]{Reader}} for basic information on the reader
  infrastructure employed by package \pkg{tm}.

  \code{\link{strptime}} for date-time format specifications.

  RFC 5322 (\url{https://www.rfc-editor.org/rfc/rfc5322}).
}
\examples{
require("tm")
newsgroup <- system.file("mails", package = "tm.plugin.mail")
news <- VCorpus(DirSource(newsgroup),
                readerControl = list(reader = readMail))
inspect(news)
}
\author{Ingo Feinerer}
