convert_mbox_eml <-
function(mbox, dir)
{
    dir.create(dir, recursive = TRUE)
    content <- readLines(mbox)
    counter <- start <- end <- 1L
    nMessages <- sum(startsWith(content, "From "))
    fmt <- paste0("%0", nchar(nMessages), "d")
    needWrite <- FALSE
    for (i in seq_along(content)) {
        if (startsWith(content[i], "From ")) {
            end <- i - 1L
            if (needWrite && (start <= end)) {
                con <- file(file.path(dir, sprintf(fmt, counter)))
                writeLines(content[start : end], con)
                close(con)
                needWrite <- FALSE
                counter <- counter + 1L
            }
            start <- i
            needWrite <- TRUE
        }
    }
    if (needWrite) {
        con <- file(file.path(dir, sprintf(fmt, counter)))
        writeLines(content[start : length(content)], con)
        close(con)
    }
    invisible(TRUE)
}

## Remove e-mail citations beginning with >
removeCitation <-
function(x, removeQuoteHeader)
    UseMethod("removeCitation", x)

removeCitation.character <-
function(x, removeQuoteHeader = FALSE)
{
    citations <- grep("^[[:blank:]]*>", x, useBytes = TRUE)
    if (removeQuoteHeader) {
        headers <- grep("wrote:$|writes:$", x)
        ## A quotation header must immediately preceed a quoted part,
        ## possibly with one empty line in between.
        headers <- union(headers[(headers + 1L) %in% citations],
                         headers[(headers + 2L) %in% citations])
        citations <- union(headers, citations)
    }
    if (length(citations)) x[-citations] else x
}

removeCitation.MailDocument <-
    content_transformer(removeCitation.character)

## Remove non-text parts from multipart e-mail messages
removeMultipart <-
function(x)
    UseMethod("removeMultipart", x)

removeMultipart.character <-
function(x)
{
    ## <http://en.wikipedia.org/wiki/Multipart_message#Multipart_Messages>
    ## We are only interested in text/plain parts
    i <- grep("^Content-Type: text/plain", x, useBytes = TRUE)
    r <- character(0)
    k <- 2L
    for (j in i) {
        end <- if (k <= length(i)) i[k] - 1L else length(x)
        content <- x[j : end]
        ## Find boundary (starting with "--")
        ## In most cases the boundary is just one line before the
        ## Content-Type header.
        start <- j - 1L
        while (j > 0L) {
            if (startsWith(x[j], "--")) {
                start <- j
                break
            }
            else
                j <- j - 1L
        }
        index <- grep(x[start], content, useBytes = TRUE, fixed = TRUE)
        index <- if (length(index)) (index[1L] - 1L) else length(content)
        content <- content[1L : index]
        ## Now remove remaining headers
        index <- grep("^$", content, useBytes = TRUE)
        index <- if (length(index)) (index[1L] + 1L) else 1L
        r <- c(r, content[index : length(content)])
        k <- k + 1L
    }

    if (!length(r)) x else r
}

removeMultipart.MailDocument <-
    content_transformer(removeMultipart.character)

# Remove e-mail signatures
removeSignature <-
function(x, marks)
    UseMethod("removeSignature", x)

removeSignature.character <-
function(x, marks = character(0))
{
    ## "---" is often added to Sourceforge mails
    ## "___" and "***" are also common, i.e.,
    ## marks <- c("^_{10,}", "^-{10,}", "^[*]{10,}")

    ## "-- " is the official correct signature start mark
    marks <- c("^-- $", marks)

    n <- length(x)

    signatureStart <- n + 1L
    for (m in marks)
        signatureStart <-
            min(grep(m, x, useBytes = TRUE), signatureStart)

    if (signatureStart <= n)
        x[-(signatureStart : n)]
    else
        x
}
removeSignature.MailDocument <-
    content_transformer(removeSignature.character)

get.thread.id <-
function(parentID, ht)
{
    threadID <- NA_character_
    threadLevel <- 2L

    if (!identical(parentID, "")
        && (length(parentID) != 0)
        && is.numeric(id <- ht[[parentID]][1L]))
        threadID <- as.integer(id)

    if (!identical(parentID, "")
        && (length(parentID) != 0)
        && is.numeric(id <- ht[[parentID]][2L]))
        threadLevel <- as.integer(id + 1L)

    list(threadID = threadID, threadLevel = threadLevel)
}

## Compute thread IDs and thread depth (based on heuristics).
## See <http://www.jwz.org/doc/threading.html> for a more complete and
## correct approach.
threads <- function(x)
{
    ## Hash table storing (thread ID, thread level) for a given message
    ## ID.
    ht <- new.env()
    tid <- 1L
    threadIDs <- threadLevels <- integer(length(x))
    for (i in seq_along(x)) {
        messageID <- meta(x[[i]], "id")
        header <- meta(x[[i]], "header")
        parentID <- header$"In-Reply-To"
        refID <- sub(">.*", ">", header$"References")

        ## Generate new thread
        if (!length(parentID) && !length(refID)) {
            ht[[messageID]] <- c(tid, 1L)
            threadIDs[i] <- tid
            threadLevels[i] <- 1L
            tid <- tid + 1L
        }
        ## Use existing thread
        else {
            info <- get.thread.id(refID, ht)
            if (!is.na(info$threadID)) {
                ht[[messageID]] <- c(info$threadID, info$threadLevel)
                threadIDs[i] <- info$threadID
                threadLevels[i] <- info$threadLevel
                next
            }

            parentID <- unique(parentID)
            if (length(parentID) > 1L) {
                for (i in 1:length(parentID)) {
                    info <- get.thread.id(parentID[[i]], ht)
                    if (!is.na(info$threadID))
                        next
                }
            } else
                info <- get.thread.id(parentID, ht)

            if (is.na(info$threadID)) {
                ## The message is a reply to some other message, but the
                ## parent e-mail is not available --- for instance, it
                ## could be a follow-up to a discussion from a separate
                ## mailing list.  Create a new thread.
                ht[[messageID]] <- c(tid, 1L)
                threadIDs[i] <- tid
                threadLevels[i] <- 1L
                tid <- tid + 1L
            }
            else {
                ht[[messageID]] <- c(info$threadID, info$threadLevel)
                threadIDs[i] <- info$threadID
                threadLevels[i] <- info$threadLevel
            }
        }
    }
    list(ThreadID = threadIDs, ThreadDepth = threadLevels)
}
