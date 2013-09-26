\name{MailDocument}
\alias{MailDocument}
\alias{as.PlainTextDocument.MailDocument}
\title{E-Mail Document}
\description{
  Construct an object representing an electronic mail document.
}
\usage{
MailDocument(x, author = character(0),
             datetimestamp = as.POSIXlt(Sys.time(), tz = "GMT"),
             description = character(0), header = character(0),
             heading = character(0), id = character(0),
             origin = character(0), language = character(0),
             localmetadata = list())
}
\arguments{
  \item{x}{Object of class \code{list} containing the content.}
  \item{author}{Object of class \code{character} containing
    the author names.}
  \item{datetimestamp}{Object of class \code{POSIXlt} containing the
    date and time when the document was written.}
  \item{description}{Object of class \code{character} containing
    additional text information.}
  \item{header}{Object of class \code{character} containing the mail
    header.}
  \item{heading}{Object of class \code{character} containing
    the title or a short heading.}
  \item{id}{Object of class \code{character} containing
    an identifier.}
  \item{origin}{Object of class \code{character} containing
    information on the source and origin of the text.}
  \item{language}{Object of class \code{character} containing
    the language of the text (preferably in \acronym{ISO} 639-2
    format).}
  \item{localmetadata}{Object of class \code{list} containing
    local meta data in form of tag-value pairs.}
}
\seealso{
  \code{\link[tm:PlainTextDocument]{PlainTextDocument}}
}
\author{Ingo Feinerer}
