readMail <- structure(
function(DateFormat = character())
{
    stopifnot(is.character(DateFormat))

    function(elem, language, id) {
        bad <- character()
        msg <- tryCatch(.py_msg_from_content(elem$content),
                        error = identity)
        if(inherits(msg, "error"))
            bad <- c(msg = conditionMessage(msg))
        else {
            hdr <- tryCatch(.py_get_hdr_from_msg(msg),
                            error = identity)
            if(inherits(hdr, "error"))
                bad <- c(hdr = conditionMessage(hdr))
        }
        if(length(bad)) {
            ## Parsing or getting headers failed: let's get the headers
            ## ourselves.
            msg <- elem$content
            idx <- Position(function(e) e == "", msg)
            hdr <- msg[seq_len(idx - 1L)]
            hdr <- parse_RFC_5322_message_header(hdr)
            ## Leave the body empty (too much work to reliably extract
            ## *text* from it).
            bdy <- character()
        } else {
            bdy <- tryCatch(.py_get_bdy_from_msg(msg),
                            error = identity)
            if(inherits(bdy, "error")) {
                bad <- c(bdy = conditionMessage(bdy))
                bdy <- character()
            }
        }

        dts <- parse_RFC_5322_date_time(hdr$Date, DateFormat)
        mid <- hdr$"Message-ID"
        MailDocument(bdy,
                     hdr$From,
                     dts,
                     character(), 
                     hdr,
                     hdr$Subject,
                     if(length(mid)) mid[1L] else id,
                     language,
                     hdr$Newsgroups,
                     problems = bad)
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

    ## Raw headers should be all ASCII, but not everone complies ...
    x <- iconv(x, to = "UTF-8", sub = "byte")
    
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

## <NOTE>
## Decoded MIME body parts (and maybe also header fields?) may contain
## embedded NULs, which R cannot handle, and for which conversion via
## py_to_r() throws an "Embedded NUL in string." error.
## This cannot easily be changed, as it is not clear what should happen
## with the NULs by default (drop or replace).  We thus try to catch
## such errors and if they occur replace the NULs by <00>.
## The code for doing this is based on suggestions/explanations by
## Tomasz Kalinowski, whose help on this is greatly appreciated.
## </NOTE>

## The startup code does a variant of
##   .py_nul_str <- import_builtins(convert = FALSE$chr(0L)

.py_str_to_r_chr <-
function(s)
    py_to_r(s$replace(.py_nul_str, "<00>"))

.py_msg_get_content <-
function(e)
{
    ## Call e$get_content(), but handle embedded NUL errors.
    ## Note that we could simply always call
    ##   .py_str_to_r_chr(r_to_py(e)$get_content())
    ## without fancy error catching.
    y <- tryCatch(e$get_content(), error = identity)
    if(inherits(y, "error")) {
        m <- conditionMessage(y)
        if(m != "Embedded NUL in string.") {
            ## Unfortunately, no classed error.
            stop(m, call. = FALSE)
        }
        y <- .py_str_to_r_chr(r_to_py(e)$get_content())
    }
    y
}

.py_msg_get_values <-
function(e)    
{
    ## See above for comments.
    y <- tryCatch(as.list(e$values()), error = identity)
    if(inherits(y, "error")) {
        m <- conditionMessage(y)
        if(conditionMessage(m) != "Embedded NUL in string.") {
            stop(m, call. = FALSE)
        }
        y <- lapply(iterate(r_to_py(e)$values()),
                    .py_str_to_r_chr)
    }
    y
}
    
.py_msg_from_content <-
function(x)
{
    .py_email$message_from_bytes(charToRaw(paste(x, collapse = "\n")),
                                 policy = .py_email$policy$default)
}

.py_get_hdr_from_msg <-
function(msg)
{
    hdr <- .py_msg_get_values(msg)
    names(hdr) <- msg$keys()
    hdr
}

.py_get_bdy_from_msg <-
function(msg)
{
    one <- function(e) {
        cc <- e$get_content_charset()
        ## Avoid
        ##   LookupError: unknown encoding: X-UNKNOWN
        ## errors:
        if(!is.null(cc) &&
           inherits(tryCatch(.py_codecs$lookup(cc),
                             error = identity),
                    "error"))
            e$set_charset("us-ascii")
        ## Ideally we'd get encoding with surrogateescapes ...
        ct <- e$get_content_type()
        if(ct == "text/plain")
            c(plain = .py_msg_get_content(e))
        else if(ct == "text/html")
            c(html  = .py_msg_get_content(e))
        else
            character()
    }
    if(msg$is_multipart()) {
        bdy <- lapply(iterate(msg$iter_parts()), one)
        structure(unlist(bdy), names = unlist(lapply(bdy, names)))
    }
    else
        one(msg)
}

## Legacy reader, modulo doing everything in bytes.
readMailLegacy <- structure(
function(DateFormat = character())
{
    stopifnot(is.character(DateFormat))

    function(elem, language, id) {
        msg <- elem$content
        idx <- Position(function(e) e == "", msg)
        hdr <- msg[seq_len(idx - 1L)]
        bdy <- msg[seq.int(idx + 1L, length(msg))]
        hdr <- parse_RFC_5322_message_header(hdr)
        dts <- parse_RFC_5322_date_time(hdr$Date, DateFormat)
        mid <- hdr$"Message-ID"
        MailDocument(bdy,
                     hdr$From,
                     dts,
                     character(), 
                     hdr,
                     hdr$Subject,
                     if(length(mid)) mid[1L] else id,
                     language,
                     hdr$Newsgroups)
    }
},
class = c("FunctionGenerator", "function"))

## Simple reader for extracting the raw bytes of each message for
## subsequent experimenting.
readMailBytes <- structure(
function() {
    function(elem, language, id) elem$content
},
class = c("FunctionGenerator", "function"))
