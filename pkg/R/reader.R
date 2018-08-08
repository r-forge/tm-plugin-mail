## Author: Ingo Feinerer

readMail <- structure(
function(DateFormat = character())
{
    stopifnot(is.character(DateFormat))

    function(elem, language, id) {
        mail <- elem$content

        ## The header is separated from the body by a blank line.
        ## <http://en.wikipedia.org/wiki/E-mail#Internet_e-mail_format>
        index <- Position(function(e) e == "", mail)

        header <- mail[1L : (index - 1L)]
        content <- mail[(index + 1L) : length(mail)]

        header <- parse_RFC_5322_message_header(header)

        datetimestamp <-
            parse_RFC_5322_date_time(header$Date, DateFormat)
        mid <- header$"Message-ID"

        MailDocument(content,
                     header$From,
                     datetimestamp,
                     character(),
                     header,
                     header$Subject,
                     if (length(mid)) mid[1L] else id,
                     language,
                     header$Newsgroups)
    }
}, class = c("FunctionGenerator", "function"))

parse_RFC_5322_message_header <-
function(x)
{
    ## Parse message header.
    ## See RFC 5322 <https://tools.ietf.org/html/rfc5322>.
    ## Section 2.2 of RFC 5322 says that
    ##   Header fields are lines beginning with a field name, followed
    ##   by a colon (":"), followed by a field body, and terminated by
    ##   CRLF.  A field name MUST be composed of printable US-ASCII
    ##   characters (i.e., characters that have values between 33 and
    ##   126, inclusive), except colon. 
    ## For convenience, let us also trim leading spaces in the field
    ## values.
    
    ## Note that RFC 5322 Section 3.6 gives the limits on the min and
    ## max numbers of times fields may occur in the header, and there
    ## are fields which may occur arbitrarily often.  So we must parse
    ## into a *list*.
    
    p <- "^([\041-\071\073-\176]+): *(.*)"
    i <- grepl(p, x, perl = TRUE)
    y <- split(x, cumsum(i))
    ## Unfold.
    y <- unlist(lapply(y, paste, collapse = ""), use.names = FALSE)
    split(sub(p, "\\2", y, perl = TRUE),
          sub(p, "\\1", y, perl = TRUE))
}

parse_RFC_5322_date_time <-
function(x, format = character())
{
    ## "Basic" formats, see <https://tools.ietf.org/html/rfc5322>,
    ## Section "3.3.  Date and Time Specification".
    if(!length(format))
        format <- c("%a, %d %B %Y %H:%M:%S %z",
                    "%d %B %Y %H:%M:%S %z")
    ## However, the abbreviated weekday names are in English, so for %a
    ## we must use a C locale:
    lc <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "C")
    on.exit(Sys.setlocale("LC_TIME", lc))

    y <- strptime(x, format[1L], tz = "GMT")
    i <- which(is.na(y))
    if(length(i)) {
        for(f in format[-1L]) {
            y[i] <- strptime(x[i], f, tz = "GMT")
            i <- which(is.na(y))
            if(!length(i)) break
        }
    }

    y
}
