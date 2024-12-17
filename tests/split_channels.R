
## An example where a Type-0 MIDI file with a single track but
## multiple channels is split into separate tracks for each channel


library(midiator)

if (requireNamespace(fluidsynth))
    midifile <- fluidsynth::demo_midi()
else
    stop("This test requires the fluidsynth package")

x <- from_midi(midifile) |> split_midi()

str(x) ## single track with multiple channels

e <- extract_events(x$tracks[[1]]$track)

d <- sapply(c("status", "desc", "channel", "what", "value", "timestamp"),
            function(s) sapply(e, getElement, s),
            simplify = FALSE) |> do.call(what = data.frame)

d$ctime <- cumsum(d$timestamp) # actual event times (not increments)

## drop all System events because we don't support them yet
d <- subset(d, desc != "System")

(channel.vals <- sort(unique(d$channel)))

new_tracks <-
    lapply(channel.vals,
           function(i) {
               cat("Processing channel: ", i, fill = TRUE)
               dd <- subset(d, channel == i)
               rownames(dd) <- NULL
               dd$timestamp <- c(0, diff(dd$ctime))
               ## include status only if different event (channel is same)
               dd$inc_status <- 
                   c(TRUE,
                     tail(dd$desc, -1) != head(dd$desc, -1))
               with(dd,
                    mapply(midi_event,
                           timestamp, desc, channel,
                           what = what, value = value,
                           include.status = inc_status,
                           SIMPLIFY = FALSE)) |> unlist()
           })


encode_midi(new_tracks, speed = x$speed) |> writeBin(con = "multi-track.mid")

from_midi("multi-track.mid") |> split_midi() |> str()


