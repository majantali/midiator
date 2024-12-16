
## Useful for inspection

.tobits <- function(x, size)
{
    m <- as.integer(x) |> matrix(nrow = size)
    apply(m[size:1, , drop = FALSE], 2, paste, collapse = "")
}

bits <- function(x) UseMethod("bits")

bits.default <- function(x) stop("Unsupported object")

bits.raw <- function(x)
{
    structure(rawToBits(x) |> .tobits(8), class = "bits")
}

bits.integer <- function(x)
{
    structure(intToBits(x) |> .tobits(32), class = "bits")
}

bits.numeric <- function(x)
{
    structure(numToBits(x) |> .tobits(64), class = "bits")
}

print.bits <- function(x, quote = FALSE, width = NULL, ...)
{
    if (is.null(width)) width <- max(nchar(x)) + 2
    print(as.vector(x), quote = quote, width = width, ...)
}
