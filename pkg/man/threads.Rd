\name{threads}
\alias{threads}
\title{E-Mail Threads}
\description{
  Extract threads (i.e., chains of messages on a single subject) from
  e-mail documents.
}
\usage{
threads(x)
}
\arguments{
  \item{x}{A corpus consisting of e-mails (\code{MailDocument}s).}
}
\value{
  A list with the two named components \code{ThreadID} and
  \code{ThreadDepth}, listing a thread and the level of replies for each
  mail in the corpus \code{x}.
}
\details{
  This function uses a one-pass algorithm for extracting the thread
  information by inspecting the \dQuote{References} header.  Some mails
  (e.g., reply mails appearing before their corresponding base mails)
  might not be tagged correctly. 
}
\examples{
require("tm")
newsgroup <- system.file("mails", package = "tm.plugin.mail")
news <- Corpus(DirSource(newsgroup),
               readerControl = list(reader = readMail))
sapply(news, ID)
lapply(news,
       function(x) grep("References", attr(x, "Header"), value = TRUE))
threads(news)
}
\author{Ingo Feinerer}
