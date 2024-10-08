\name{MailDocument}
\alias{MailDocument}
\title{E-Mail Documents}
\description{
  Create electronic mail documents.
}
\usage{
MailDocument(x,
             author = character(),
             datetimestamp = as.POSIXlt(Sys.time(), tz = "GMT"),
             description = character(),
             header = character(),
             heading = character(),
             id = character(),
             language = character(),
             origin = character(),
             \dots,
             meta = NULL)
}
\arguments{
  \item{x}{a character vector giving the text content.}
  \item{author}{a character vector or an object of class
    \code{\link{person}} giving the author names.}
  \item{datetimestamp}{an object of class \code{\link{POSIXt}} or a
    character string giving the creation date/time information.  If a
    character string, exactly one of the \acronym{ISO} 8601 formats
    defined by \url{https://www.w3.org/TR/NOTE-datetime} should be used.
    See \code{\link[NLP]{parse_ISO_8601_datetime}} in package \pkg{NLP}
    for processing such date/time information.
  }
  \item{description}{a character string giving a description.}
  \item{header}{a character vector or list giving the mail header
    information.}
  \item{heading}{a character string giving the title or a short heading.}
  \item{id}{a character string giving a unique identifier.}
  \item{language}{a character string giving the language (preferably as
    \acronym{IETF} language tags, see \link[NLP]{language} in package
    \pkg{NLP}).}
  \item{origin}{a character string giving information on the source and
    origin.}
  \item{\dots}{user-defined document metadata tag-value pairs.}
  \item{meta}{a named list or \code{NULL} (default) giving all
    metadata.  If set, all other metadata arguments are ignored.}
}
\value{
  An object inheriting from \code{MailDocument},
  \code{\link[tm]{PlainTextDocument}},
  and \code{\link[tm]{TextDocument}}.
}
\author{Ingo Feinerer and Kurt Hornik}
