## Author: Ingo Feinerer

readMail <- structure(
function(DateFormat = "%d %B %Y %H:%M:%S")
{
    stopifnot(is.character(DateFormat))

    format <- DateFormat
    function(elem, language, id) {
        mail <- elem$content

        ## The header is separated from the body by a blank line.
        ## <http://en.wikipedia.org/wiki/E-mail#Internet_e-mail_format>
        index <- Position(function(e) e == "", mail)

        header <- mail[1L : (index - 1L)]
        content <- mail[(index + 1L) : length(mail)]

        ## Parse message header.
        ## See RFC 5322 <https://tools.ietf.org/html/rfc5322>.
        ## Section 2.2 of RFC 5322 says that
        ##   Header fields are lines beginning with a field name,
        ##   followed by a colon (":"), followed by a field body, and
        ##   terminated by CRLF.  A field name MUST be composed of
        ##   printable US-ASCII characters (i.e., characters that have
        ##   values between 33 and 126, inclusive), except colon.
        ## For convenience, let us also trim leading spaces in the field
        ## values.

        ## Note that RFC 5322 Section 3.6 gives the limits on the min
        ## and max numbers of times fields may occur in the header, and
        ## there are fields which may occur arbitrarily often.  So we
        ## must parse into a *list*.

        pat <- "^([\041-\071\073-\176]+): *(.*)"
        ind <- grepl(pat, header, perl = TRUE)
        header <- split(header, cumsum(ind))
        header <- unlist(lapply(header, paste, collapse = ""),
                         use.names = FALSE)
        header <- split(sub(pat, "\\2", header, perl = TRUE),
                        sub(pat, "\\1", header, perl = TRUE))

        datetimestamp <-
            strptime(header$Date,
                     format = format, tz = "GMT")
        mid <- header$"Message-ID"

        MailDocument(content,
                     header$From,
                     datetimestamp,
                     character(0),
                     header,
                     header$Subject,
                     if (length(mid)) mid[1L] else id,
                     language,
                     header$Newsgroups)
    }
}, class = c("FunctionGenerator", "function"))
