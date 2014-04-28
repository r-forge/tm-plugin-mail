\name{MailDocument}
\alias{MailDocument}
\title{E-Mail Documents}
\description{
  Create electronic mail documents.
}
\usage{
MailDocument(x,
             author = character(0),
             datetimestamp = as.POSIXlt(Sys.time(), tz = "GMT"),
             description = character(0),
             header = character(0),
             heading = character(0),
             id = character(0),
             origin = character(0),
             language = character(0),
             \dots,
             meta = NULL)
}
\arguments{
  \item{x}{A character giving the text content.}
  \item{author}{a character giving the author names.}
  \item{datetimestamp}{an object of class \code{POSIXlt} giving the creation
    date and time.}
  \item{description}{a character giving a description.}
  \item{header}{a character giving the mail header.}
  \item{heading}{a character giving the title or a short heading.}
  \item{id}{a character giving a unique identifier.}
  \item{origin}{a character giving information on the source and origin.}
  \item{language}{a character giving the language (preferably as \acronym{IETF}
    language tags).}
  \item{\dots}{user-defined document metadata tag-value pairs.}
  \item{meta}{a named list or \code{NULL} (default) giving all metadata. If set
    all other metadata arguments are ignored.}
}
\value{
  An object inheriting from \code{MailDocument},
  \code{\link[tm]{PlainTextDocument}}, and \code{\link[tm]{TextDocument}}.
}
\author{Ingo Feinerer}
