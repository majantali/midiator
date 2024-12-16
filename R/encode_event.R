
## Mainly three types of events: midi, system, meta (also sysex). We
## mainly care about
##
## * MIDI events, which are mostly note-on / note-off events, and
##   controller messages (instruments etc). Status = 8x-Ex
##
## * Some system events (F1-FE) ? 
## 
## * Some meta events (FF <type> <len> <data>): comments / tempo? / ?
##
## * We will ignore SYSEX messages (FF <len> <data>) when exporting to MIDI


## Midi events have one or two parameters, in one or two bytes. The
## meanings depend on the event type. We will always call one of the
## parametes 'value', and the other one (if needed) 'what'. We will
## always represent them as integers (usually in the range 0-127, but
## sometimes the value can be larger, and must be split into two 7-bit
## numbers).


.type_map <- c("NoteOff" = 0x80,
               "NoteOn" = 0x90,
               "PolyPressure" = 0xA0,
               "ControlChange" = 0xB0,
               "ProgramChange" = 0xC0,
               "ChannelPressure" = 0xD0,
               "PitchWheel" = 0xE0,
               "System" = 0xF0)

encode_varlen_7bit <- function(value)
{
    ## 0x80 == 128 = 2^7
    value <- as.integer(value)
    if (value < 0x80) value
    else c(encode_varlen_7bit(value %/% 0x80L),
           value %% 0x80L)
}

encode_varlen <- function(value)
{
    e <- encode_varlen_7bit(value)
    n <- length(e)
    if (n > 1) e[-n] <- e[-n] + 0x80L
    as.raw(e)
}


midi_event <- function(timestamp = 0L,
                       type = c("NoteOff", "NoteOn", "PolyPressure",
                                "ControlChange", "ProgramChange",
                                "ChannelPressure", "PitchWheel", "System"),
                       channel = 0L,
                       status = as.raw(.type_map[[type]] + channel),
                       what = NA_integer,
                       value,
                       prev.status = 0L,
                       include.status = (status == prev.status))
{
    type <- match.arg(type)
    if (type == "System") stop("Use sys_event() or meta_event() for System and Meta events")
    ans <-
        list(timestamp = encode_varlen(timestamp),
             status = status,
             data = switch(type,
                           NoteOff = ,
                           NoteOn = ,
                           PolyPressure = ,
                           ControlChange = {
                               as.raw(c(what, value))
                           },
                           ProgramChange = ,
                           ChannelPressure = {
                               as.raw(value)
                           },
                           PitchWheel = {
                               as.raw(c(value %% 128L, value %/% 128L))
                           }))
    if (include.status)
        with(ans, c(timestamp, status, data))
    else
        with(ans, c(timestamp, data))
}


sys_event <- function(...) raw(0)

meta_event <- function(...) raw(0)
