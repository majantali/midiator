
## The main content of a MIDI file are in tracks, which consist of
## events one after the other, each consisting of some bytes (which
## can vary). Events can be complicated. Writing events is easier
## because we can restrict our attention to a limited subset of
## features. To read them, we need to account for many possibilities.

## In generic form, an event consists of
## 
## <timestamp>   mandatory but variable length (1-4 bytes)
##
## <status>      single byte but possibly missing (in which case it is 
##               the same as for the previous event). MUST be in the range
##               80-FF (if not, the status for this event has been omitted,
##               and must be taken from the previous one)
##
## <data>        number of bytes and interpretation depends on status; each
##               byte must normally be between 00-FF (i.e., 0-127) for events
##               whose status may be omitted.
##
## The timestamp has a well defined variable-length format which is
## unfortunately sequential (and not vectorizable) by nature. We will
## implement it in R for now, but this is something that may be useful
## to implement in C eventually.
##
## To read status codes, we need knowledge of all possible values, but
## the useful ones are limited in number. Although not explicitly
## guaranteed in the specs I have read so far, it must be true that
## the bytes following a status code cannot be a status code. We will
## see.


decode_varlen <- function(x, i, maxlen = 4L)
{
    ## We will do this using a pass-by-reference philosophy so that it
    ## is easier to re-implement in C: x is a raw array of bytes, and
    ## i is a location where the timestamp starts. We will return the
    ## decoded timestamp value as well as the index to the next byte.
    T <- 0L
    j <- 0L
    while (TRUE) {
        if (x[[i + j]] < 0x80) {
            T <- T * 0x80 + as.integer(x[[i + j]])
            return(c(T, i + j + 1))
        }
        else {
            T <- T * 0x80 + as.integer(x[[i + j]] & as.raw(0x7f))
            j <- j + 1L
            if (j >= maxlen) stop("Exceeded 'maxlen' bytes without reaching end")
        }
    }
}

## logical ops are bitwise, comparison is possible
## as.integer(x[1:10] & as.raw(0x7f))

.status_map <- c(NA, NA, NA, NA, NA, NA, NA,
                 "NoteOff",           # 8
                 "NoteOn",            # 9
                 "AfterTouch",        # A
                 "ControlChange",     # B
                 "ProgramChange",     # C
                 "ChannelPressure",   # D
                 "PitchWheel",        # E
                 "System")            # F

.databytes_map <- c(NA, NA, NA, NA, NA, NA, NA,
                    2,   # 8 (key | note + volume | velocity)
                    2,   # 9 (key + volume)
                    2,   # A (key + pressure value)
                    2,   # B (controller number + new value)
                    1,   # C (program number)
                    1,   # D (pressure value)
                    2,   # E (two 7-bit bytes to give 14-bit value)
                    NA)  # F (variable)



byte2status <- function(b)
{
    status <- as.integer(rawShift(b, -4))
    channel <- as.integer(b & as.raw(0x0f))
    if (status < 8) return(NULL) # b is not a status byte
    return(list(status = status,
                desc = .status_map[status],
                channel = channel))
}



decode_event <- function(x, i, running_status = NULL)
{
    ## using status information as returned by byte2status(), collect
    ## and interpret data bytes to obtain complete event
    ## information. running_status is ignored unless
    ## byte2status(x[[i]]) is NULL.

    status <- byte2status(x[[i]])
    use_running <- is.null(status)
    if (use_running)
        status <- running_status
    else
        i <- i + 1L # i now points to start of data block
    if (is.null(status)) stop("Invalid event data")

    ## placeholder values for when they are not applicable
    status$key <- NA
    status$value <- NA
    status$controller <- NA
    status$comment <- ""
    
    ## do the simple ones first
    if (status$desc == "NoteOff") {
        status$key <- as.integer(x[[i]])
        status$value <- as.integer(x[[i+1]])
        status$bytes <- 2L + (1L - use_running)
    }
    else if (status$desc == "NoteOn") {
        status$key <- as.integer(x[[i]])
        status$value <- as.integer(x[[i+1]])
        status$bytes <- 2L + (1L - use_running)
    }
    else if (status$desc == "AfterTouch") {
        status$key <- as.integer(x[[i]])
        status$value <- as.integer(x[[i+1]])
        status$bytes <- 2L + (1L - use_running)
    }
    else if (status$desc == "ControlChange") {
        status$controller <- as.integer(x[[i]])
        status$value <- as.integer(x[[i+1]])
        status$bytes <- 2L + (1L - use_running)
        ## some controllers/values are special (channel mode):
        ## (122, 0)   - local control off
        ## (122, 127) - local control on
        ## (123, 0)   - all notes off
        ## (124, 0)   - omni mode off
        ## (125, 0)   - omni mode on
        ## (126, M)   - mono mode on (poly off), M = number of channels
        ## (127, 0)   - poly mode on (mono off)
        ## but we will ignore these special interpretations
        ##
        ## There is a long table of other messages which should be
        ## made available as a lookup table. This will be more
        ## important when writing MIDI.
    }
    else if (status$desc == "ProgramChange") {
        status$value <- as.integer(x[[i]])
        status$bytes <- 1L + (1L - use_running)
    }
    else if (status$desc == "ChannelPressure") {
        status$value <- as.integer(x[[i]])
        status$bytes <- 1L + (1L - use_running)
    }
    else if (status$desc == "PitchWheel") {
        status$value <- as.integer(x[[i]]) + 0x80 * as.integer(x[[i+1]])
        status$bytes <- 2L + (1L - use_running)
    }
    else if (status$desc == "System") {
        ## this one has many possibilities...

        if (status$channel == 0) {
            ## system exclusive (proprietary)... just read on until we
            ## encounter 0xf7
            STOPCODE <- as.raw(0xf7)
            count <- 0L
            while (x[[i + count]] != STOPCODE) count <- count + 1
            status$comment <- "SystemExclusive"
            status$bytes <- count + 1L + (1L - use_running)
        }
        else if (status$channel == 2) {
            status$comment <- "SystemSongPositionPointer"
            status$value <- as.integer(x[[i]]) + 0x80 * as.integer(x[[i+1]])
            status$bytes <- 2L + (1L - use_running)
        }
        else if (status$channel == 3) {
            status$comment <- "SystemSongSelect"
            status$value <- as.integer(x[[i]])
            status$bytes <- 1L + (1L - use_running)
        }
        else if (status$channel == 6) {
            status$comment <- "SystemTuneRequest"
            status$bytes <- (1L - use_running)
        }
        else if (status$channel == 7) {
            status$comment <- "SystemEscapeF7"
            status$bytes <- (1L - use_running)
            stop("Unexpected F7 byte --- not sure how to process remaining data")
        }
        else if (status$channel == 8) {
            status$comment <- "SystemTimingClock"
            status$bytes <- (1L - use_running)
        }
        else if (status$channel == 10) {
            status$comment <- "SystemStartSequence"
            status$bytes <- (1L - use_running)
        }
        else if (status$channel == 11) {
            status$comment <- "SystemContinueSequence"
            status$bytes <- (1L - use_running)
        }
        else if (status$channel == 12) {
            status$comment <- "SystemStopSequence"
            status$bytes <- (1L - use_running)
        }
        else if (status$channel == 14) {
            status$comment <- "SystemActiveSensing"
            status$bytes <- (1L - use_running)
        }
        else if (status$channel == 15) {
            status$comment <- "SystemMetaEvent"

            ## This has further subtypes, but general format is
            ## FF <type> <length> <bytes>

            status$type <- as.integer(x[[i]])
            vlen <- decode_varlen(x, i + 1)
            status$raw <- x[vlen[[2]] - 1L + seq_len(vlen[[1]])]
            status$value <- rawToChar(status$raw, multiple = TRUE) |> paste(collapse = "")
            status$bytes <- (1L - use_running) + vlen[[1]] + (vlen[[2]] - i)
        }
        else stop("Encountered undefined system message with channel = ", status$channel)
    }
    status
}
