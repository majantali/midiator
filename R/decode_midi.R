
from_midi <- function(file = "sample.mid")
{
    n <- file.size(file)
    x <- readBin(file, what = "raw", n = n + 2)
    stopifnot(n == length(x))
    structure(x, class = "midi")
}

print.midi <- function(x, ...)
{
    cbind(bits = bits(as.vector(x)),
          hex = sprintf("%02x", as.integer(x)),
          char = rawToChar(x, multiple = TRUE),
          int = sprintf("%4d", as.integer(x))) |> print(quote = FALSE, ...)
}


toLength <- function(x)
{
    ## interpret multiple bytes as a length. There may be a more direct way
    stopifnot(length(x) <= 4)
    z <- as.integer(rev(x))
    ans <- 0L
    for (i in seq_along(z)) ans <- ans + 256^(i-1) * z[i]
    ans
}

## split a "midi" object into multiple tracks


extract_track <- function(x)
{
    ## x is a raw vector that should start with the track header
    ## "MTrk". This function checks for validity (header and footer
    ## consistency) and returns (a) extracted track data and (b) total
    ## length used up
    header <- rawToChar(x[1:4])
    if (header != "MTrk") stop("Invalid header: ", header)
    track_len <- toLength(x[5:8]) - 4
    track <- x[8 + seq_len(track_len)]
    footer <- x[8 + track_len + (2:4)]
    if (toLength(footer) != 16723712)
    {
        print()
        stop("Unexpected footer: ", as.character(footer))
    }
    list(header = header,
         track_len = track_len,
         track = structure(track, class = c("track", "midi")),
         footer = footer,
         total = 12 + track_len)
}


split_midi <- function(x)
{
    headerA <- head(x, 4) |> rawToChar()
    if (headerA != "MThd") stop("Invalid header: ", headerA)
    x <- tail(x, -4)
    headerB <- head(x, 4) |> toLength()
    if (headerB != 6) stop("Unexpected header length (was expecting 6): ", headerB)
    x <- tail(x, -4)
    type <- head(x, 2) |> toLength(); x <- tail(x, -2)
    ntracks <- head(x, 2) |> toLength(); x <- tail(x, -2)
    speed <- head(x, 2) |> toLength(); x <- tail(x, -2)
    ## FIXME: there can be a different interpretation speed if the
    ## first byte is negative, which e do not consider here.
    tracks <- vector(mode = "list", length = ntracks)
    for (i in seq_len(ntracks))
    {
        tracks[[i]] <- extract_track(x)
        x <- tail(x, -tracks[[i]]$total)
    }
    if (length(x) > 0) stop("Unexpected residual content in x")

    list(headerA = headerA, headerB = headerB,
         type = type, ntracks = ntracks, speed = speed,
         tracks = tracks)
}



extract_events <- function(track)
{
    ## A track consists of multiple events, each with variable
    ## length. We extract them one by one, until there are no more.
    n <- length(track)
    ans <- list()
    i <- 1L; count <- 1L
    status <- NULL
    while (i < n) {
        ## print(i)
        timestamp <- decode_varlen(track, i)
        i <- timestamp[[2]]
        status <- decode_event(track, i, running_status = status)
        status$timestamp <- timestamp[[1]]
        ans[[count]] <- status
        count <- count + 1
        i <- i + status$bytes
        ## str(status)
    }
    ans
}

