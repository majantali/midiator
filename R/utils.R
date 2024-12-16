

## User-friendly helper functions to generate codes

.note_map <- c("C" = 1, "D" = 3, "E" = 5, "F" = 6, "G" = 8, "A" = 10, "B" = 12,
               "CB" = 0, "DB" = 2, "EB" = 4, "FB" = 5, "GB" = 7, "AB" = 9, "BB" = 11,
               "C#" = 2, "D#" = 4, "E#" = 6, "F#" = 7, "G#" = 9, "A#" = 11, "B#" = 13)

.note_map_indian <-
    c("s" = 1, "R" = 2, "r" = 3, "G" = 4, "g" = 5, "m" = 6, "M" = 7,
      "p" = 8, "D" = 9, "d" = 10, "N" = 11, "n" = 12)




key <- function(note, octave, offset = 0)
{
    ## note is 1:12, octave is 0, +-1, +-2, ...
    ## (1, 0) is middle C === 60 === 0x3C
    if (is.character(note)) note <- .note_map[toupper(note)]
    59 + note + octave * 12
}

key_indian <- function(note, octave, offset = 0)
{
    key(.note_map_indian[note], octave, offset = offset)
}


## controllers are a bit complicated because (a) not all meanings are
## clear (yet) and many have two values (LSB and MSB) with different
## controller number. The following function gives a mapping from
## description to controller number for selected controllers

controller_code <-
    function(desc = c("ModulationWheel", "PortamentoTime", "ChannelVolume",
                      "Pan", "ExpressionController", # have LSB / MSB

                      ## toggles (on = 127 / off = 0)
                      "Sustain", "Portamento", "Sustenuto", "SoftPedal", "LocalControl",

                      "PortamentoControl",
                      "ResetAllControllers", "AllNotesOff"),

             which = c("MSB", "LSB"))
{
    which <- match.arg(which)
    desc <- match.arg(desc)
    LSB <- which == "LSB"
    switch(desc,

           ModulationWheel = 1L + 32L * LSB,
           PortamentoTime = 5L + 32L * LSB,
           ChannelVolume = 7L + 32L * LSB,
           Pan = 10L + 32L * LSB,
           ExpressionController = 11L + 32L * LSB,

           Sustain = 64L,
           Portamento = 65L,
           Sustenuto = 66L,
           SoftPedal = 67L,
           LocalControl = 122L,

           PortamentoControl = 84L,
           ResetAllControllers = 121L,
           AllNotesOff = 123L,
           
           0)
}


program_code <- function(instrument)
{
    inames  <-  c("acoustic grand",
                  "bright acoustic",
                  "electric grand",
                  "honky-tonk",
                  "electric piano 1",
                  "electric piano 2",
                  "harpsichord",
                  "clav",
                  "celesta",
                  "glockenspiel",
                  "music box",
                  "vibraphone",
                  "marimba",
                  "xylophone",
                  "tubular bells",
                  "dulcimer",
                  "drawbar organ",
                  "percussive organ",
                  "rock organ",
                  "church organ",
                  "reed organ",
                  "accordion",
                  "harmonica",
                  "concertina",
                  "acoustic guitar (nylon)",
                  "acoustic guitar (steel)",
                  "electric guitar (jazz)",
                  "electric guitar (clean)",
                  "electric guitar (muted)",
                  "overdriven guitar",
                  "distorted guitar",
                  "guitar harmonics",
                  "acoustic bass",
                  "electric bass (finger)",
                  "electric bass (pick)",
                  "fretless bass",
                  "slap bass 1",
                  "slap bass 2",
                  "synth bass 1",
                  "synth bass 2",
                  "violin",
                  "viola",
                  "cello",
                  "contrabass",
                  "tremolo strings",
                  "pizzicato strings",
                  "orchestral harp",
                  "timpani",
                  "string ensemble 1",
                  "string ensemble 2",
                  "synthstrings 1",
                  "synthstrings 2",
                  "choir aahs",
                  "voice oohs",
                  "synth voice",
                  "orchestra hit",
                  "trumpet",
                  "trombone",
                  "tuba",
                  "muted trumpet",
                  "french horn",
                  "brass section",
                  "synthbrass 1",
                  "synthbrass 2",
                  "soprano sax",
                  "alto sax",
                  "tenor sax",
                  "baritone sax",
                  "oboe",
                  "english horn",
                  "bassoon",
                  "clarinet",
                  "piccolo",
                  "flute",
                  "recorder",
                  "pan flute",
                  "blown bottle",
                  "shakuhachi",
                  "whistle",
                  "ocarina",
                  "lead 1 (square)",
                  "lead 2 (sawtooth)",
                  "lead 3 (calliope)",
                  "lead 4 (chiff)",
                  "lead 5 (charang)",
                  "lead 6 (voice)",
                  "lead 7 (fifths)",
                  "lead 8 (bass+lead)",
                  "pad 1 (new age)",
                  "pad 2 (warm)",
                  "pad 3 (polysynth)",
                  "pad 4 (choir)",
                  "pad 5 (bowed)",
                  "pad 6 (metallic)",
                  "pad 7 (halo)",
                  "pad 8 (sweep)",
                  "fx 1 (rain)",
                  "fx 2 (soundtrack)",
                  "fx 3 (crystal)",
                  "fx 4 (atmosphere)",
                  "fx 5 (brightness)",
                  "fx 6 (goblins)",
                  "fx 7 (echoes)",
                  "fx 8 (sci-fi)",
                  "sitar",
                  "banjo",
                  "shamisen",
                  "koto",
                  "kalimba",
                  "bagpipe",
                  "fiddle",
                  "shanai",
                  "tinkle bell",
                  "agogo",
                  "steel drums",
                  "woodblock",
                  "taiko drum",
                  "melodic tom",
                  "synth drum",
                  "reverse cymbal",
                  "guitar fret noise",
                  "breath noise",
                  "seashore",
                  "bird tweet",
                  "telephone ring",
                  "helicopter",
                  "applause",
                  "gunshot") |> tolower()

    icodes <- structure(0:127, names = inames)
    w <- which(startsWith(inames, tolower(instrument)))
    if (length(w) == 1) icodes[w]
    else if (length(w) == 0) stop("no matching instrument found")
    else if (length(w) > 1) stop("multiple matches found: ",
                                 paste(inames[w], collapse = ", "))
}



