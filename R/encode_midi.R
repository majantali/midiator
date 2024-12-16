
## encode an integer as raw bytes (MSB first) 

fromLength <- function(x, bytes = 4)
{
    x <- as.integer(x)
    ans <- raw(bytes)
    i <- 0L
    while (i < bytes) {
        i <- i + 1L
        ans[[i]] <- as.raw(x %% 256L)
        x <- x %/% 256L
    }
    rev(ans)
}



encode_track <- function(track)
{
    ## 'track' is a raw sequence containing a single track. Now all we
    ## need to do is to add suitable header and footer bytes. The only
    ## non-trivial step is to encode the length of the track
    ## (including the stop footer) 

    c(charToRaw("MTrk"),                   # track header
      fromLength(length(track) + 4L),      # track length
      track,                               # track data
      as.raw(c(0L, 255L, 47L, 0L)))        # track footer
}



encode_midi <- function(tracks, speed = 160L)
{
    ## 'tracks' is a list containing one or more raw track data

    ans <- c(charToRaw("MThd"),
             fromLength(6L),                # length of MIDI header (always 6)
             fromLength(1L, bytes = 2),     # we will only do type 1
             fromLength(length(tracks),     # number of tracks
                        bytes = 2),
             fromLength(speed, bytes = 2))  # ticks per quarter note
    for (x in tracks)
        ans <- c(ans, encode_track(x))
    ans
}
