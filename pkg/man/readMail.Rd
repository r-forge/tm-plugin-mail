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
    \dQuote{basic} formats of
    \href{https://www.rfc-editor.org/rfc/rfc5322}{RFC 5322} are tried.}
}
\details{
  Formally this function is a function generator, i.e., it returns a
  function (which reads in a mail document) with a well-defined
  signature, but can access passed over arguments (e.g., the
  \dQuote{Date} header format) via lexical scoping.

  In version 0.3.0 of the \pkg{tm.plugin.mail} package, the reader code
  was switched to use the Python \pkg{email} library via CRAN package
  \CRANpkg{reticulate}.  Compared to previous versions, this allows to
  \itemize{
    \item handle textual message bodies in character sets other than
    US-ASCII and the use of base64 or quoted-printable transfer
    encodings
    (\href{https://www.rfc-editor.org/rfc/rfc2045}{RFC 2045})
    \item handle non-US-ASCII text data in message header fields
    (\href{https://www.rfc-editor.org/rfc/rfc2047}{RFC 2047})
    \item correctly handle the metadata in structured header fields
    (\href{https://www.rfc-editor.org/rfc/rfc5322}{RFC 5322})
  }
  
  For messages using the Multipurpose Internet Mail Extensions (MIME)
  extensions, the texts extracted from the messages are the (suitably
  decoded) bodies when using the \sQuote{text/plain} or
  \sQuote{text/html} content types, or the body parts using these
  types when using \sQuote{multipart/mixed} or
  \sQuote{multipart/alternative} (see
  \href{https://www.rfc-editor.org/rfc/rfc2046}{RFC 2046} for more
  information).
  Non-MIME messages are treated like \sQuote{text/plain}.
  The extracted texts are represented as character vectors with length
  the number of extracted body parts and names giving the MIME
  \emph{subtype} (\code{"plain"} or \code{"html"}).

  This allows text mining applications to flexibly handle HTML content
  \dQuote{as appropriate} by filtering on the names of the content of
  the \code{\link{MailDocument}} objects.

  In case the Python processing fails or its results cannot be
  transferred to R (in particular, when text body parts contain embedded
  NULs), the reader falls back to simple header field processing
  appropriate for unstructered headers, and/or extracting no text.
  Information about problems is provided in the \code{problems} element
  of the metadata.
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
  
  \href{https://www.rfc-editor.org/rfc/rfc5322}{RFC 5322},
  \href{https://www.rfc-editor.org/rfc/rfc2045}{RFC 2045},
  \href{https://www.rfc-editor.org/rfc/rfc2045}{RFC 2045},
  \href{https://www.rfc-editor.org/rfc/rfc2047}{RFC 2047}.
}
\examples{
require("tm")
newsgroup <- system.file("mails", package = "tm.plugin.mail")
news <- VCorpus(DirSource(newsgroup),
                readerControl = list(reader = readMail))
inspect(news)
## Use the high-level content and metadata accessors from package 'NLP':
require("NLP")
content(news[[2]])
meta(news[[2]])
## Processed header fields of the message.
meta(news[[2]])$header
}
\author{Ingo Feinerer and Kurt Hornik}
