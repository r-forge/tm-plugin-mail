\name{MBoxSource}
\alias{MBoxSource}
\title{Mailbox Source}
\description{
  Create a mailbox source.
}
\usage{
MBoxSource(mbox, format = "mbox", delim = NULL)
}
\arguments{
  \item{mbox}{a character string giving the path or \acronym{URL} to a
    mailbox stored in \dQuote{mbox} format.}
  \item{format}{a character string giving the mbox format to use, with
    possible values \code{"mbox"} (default), \code{"mboxo"}, and
    \code{"mboxrd"}.}
  \item{delim}{a character string giving a regexp to use for finding the
    \sQuote{From } lines delimiting the messages, or \code{NULL}
    (default), which provides suitable regexps according to the mbox
    format.}
}
\details{
  A \emph{mailbox source} interprets each e-mail stored in the mailbox
  as a document.

  \sQuote{Mbox} is a generic term for a family of related file formats
  used for holding collections of email messages.  The messages are
  stored in a single mailbox text file separated by lines starting with
  the four characters \sQuote{From} followed by a space (the so-called
  \sQuote{From } lines) and the sender's email address.

  Clearly, there will be a problem if the message bodies contain lines
  which also start with \sQuote{From} followed by a space.  There are
  four common variants of the mbox format to deal with this problem: in
  \emph{mboxo} and \emph{mboxrd} such lines get a greater-than sign
  prepended, whereas in \emph{mboxcl} and \emph{mboxcl2} a
  \sQuote{Content-Length:} header field is used to record the message 
  lengths.  For more information, see
  \url{https://en.wikipedia.org/wiki/Mbox} and
  \url{https://www.loc.gov/preservation/digital/formats/fdd/fdd000383.shtml}
  which in turn points to
  \url{https://www.loc.gov/preservation/digital/formats/fdd/fdd000384.shtml}
  and
  \url{https://www.loc.gov/preservation/digital/formats/fdd/fdd000385.shtml}
  for the \emph{mboxo} and \emph{mboxrd} extensions.

  The above LoC web page suggests that the \sQuote{From } lines are
  always of the form
  \sQuote{From \emph{sender} \emph{date} \emph{moreinfo}}
  where \emph{sender} is one word without spaces or tabs and \emph{date}
  (the delivery date of the message) always contains exactly 24
  characters in Standard C asctime format.  Thus, for the \emph{mbox}
  format, the default delimiter regexp for \sQuote{From } lines actually
  matches this form (with some timezone variants).  For the \emph{mboxo}
  and \emph{mboxrd} variants, the default delimiter regexp is
  \code{"^From "}.

  The \code{\link[tm]{getElem}()} method for class \code{MBoxSource}
  strips the prepended greater-than signs for the \emph{mboxo} and
  \emph{mboxrd} formats.
}
\value{
  An object inheriting from \code{MBoxSource},
  \code{\link[tm]{SimpleSource}},
  and \code{\link[tm]{Source}}.
}
\author{Ingo Feinerer and Kurt Hornik}
