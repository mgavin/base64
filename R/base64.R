# code header goes here
# things about copying the python library
# and implementing the rfc 4648

#' LETTERS and letters are from base::Constants
#' TODO: should probably change them to not rely on base, so the
#' package is more self-contained
b64alphabet <- c(LETTERS, letters, 0:9, "+", "/")

#' helper functions
`%not in%` <- function(a, b) {
  !(a %in% b)
}
#' @export
#' @param input hopefully a single character vector string.
#'              or a raw byte string
#'
#'
b64encode <- function(input) {
  if (typeof(input) %not in% c("character", "raw")) {
    return
  }
  ## output <- raw(0)
  ## input <- as.raw(unlist(strsplit(input, ''), use.names = FALSE))

  ## ## pretty sure you can take 3 bytes at a time
  ## if (length(input) %% 3 != 0) addPadding(input)

  ## ## then split them up into 4 characters in the 64 alphabet characters
  ## for (x in seq(from = 1, to = length(input), by = 3)) {

  ## }

  ## -------------------- copying from caTools::base64encode ---------------------
  nlen          <- nchar(input)
  input         <- writeBin(input, raw())
  length(input) <- nlen

  x       <- as.integer(input)
  ndbytes <- length(x)            # number of decoded bytes
  nblocks <- ceiling(ndbytes / 3) # number of blocks/groups
  nebytes <- 4 * nblocks          # number of encoded bytes

  if (ndbytes < 3 * nblocks) x[(ndbytes+1) : (3 * nblocks)] <- 0
  dim(x) <- c(3, nblocks)                     # reshape the data
  y      <- matrix(as.integer(0), 4, nblocks) # for the encoded data

  y[1,] <- bitwShiftR(x[1,], 2) # 6 highest bits of x[1, :]
  y[2,] <- bitwOr(bitwShiftL(x[1,], 4), bitwShiftR(x[2,], 4))
  y[3,] <- bitwOr(bitwShiftL(x[2,], 2), bitwShiftR(x[3,], 6))
  y[4,] <- x[3,]

  # trim numbers to lower 6 bits
  y <- bitwAnd(y, 63)

  # rearrange characters
  z <- b64alphabet[y+1]

  npbytes <- 3 * nblocks - ndbytes
  if (npbytes > 0) {
    z[(nebytes - npbytes + 1) : nebytes] <- "=" # pad with extra "="
  }

  # combine characters into one string
  output <- paste(z, collapse = "")
  invisible(output)
}

#' @export
#' @param input a character string composed of the Base64 character set
#' @param what the return value datatype
#' @param size the size of each character in the return value
#' @param validate validate something (python related)
#'
#' Decodes a Base64 string into (what)
b64decode <- function(input, what, sz = NA, validate = FALSE) {
  if (!is.character(input))
    stop("Input argument 'input' is supposed to be a string")

  if (length(input) == 1)
    input <- strsplit(input, '')[[1]]
  if (length(input) %% 4 != 0)
    warning("Length of base64 data (input) is not a multiple of 4.")

  # get lookup number of each character
  y <- match(input, c(b64alphabet, "=", "\r", "\n"), nomatch = -1) - 1

  if (any(y == -1))
    stop("Input string is not in Base64 format")

  if (any(y > 63))
    y <- y[y < 64] # remove padding and new line characters

  nebytes <- length(y)            # number of encoded bytes
  nblocks <- ceiling(nebytes / 4) # number of blocks/groups
  ndbytes <- 3 * nblocks          # number of decoded bytes

  # add padding if necessary
  if (nebytes < (4 * nblocks))
    y[(nebytes + 1) : (4*nblocks)] <- 0

  dim(y) <- c(4, nblocks)                    # shape into a matrix
  x      <- matrix(as.integer(0), 3, nblocks) # for the decoded data

  x[1,] <- bitwOr(bitwShiftL(y[1,], 2), bitwShiftR(y[2,], 4))
  x[2,] <- bitwOr(bitwShiftL(y[2,], 4), bitwShiftR(y[3,], 2))
  x[3,] <- bitwOr(bitwShiftL(y[3,], 6), y[4,])
  x     <- bitwAnd(x, 255)

  # remove padding
  if (nebytes %% 4  %in% c(2, 3))
    x <- x[1 : (ndbytes - (4 - (nebytes%%4)))]

  typelist <- c("logical",
                "integer",
                "double",
                "complex",
                "character",
                "raw",
                "numeric",
                "int")

  if (!is.character(what) || length(what) != 1 || what %not in% typelist)
    what <- typeof(what)
  if (is.na(sz))
    sz <- switch(match(what, typelist), 4, 4, 8, 16, 2, 1, 8, 4)

  r <- as.raw(x)
  if (what == "raw")
    return(r)
  if (what == "character") {
    rlen <- sz * ceiling(length(r) / sz)
    length(r) <- rlen
  }
  n <- length(r)

  if (n %% sz)
    stop("number of elements in 'r' is not a multiple of 'sz'")

  output <- readBin(r, what, n = n %/% sz, size = sz)
  if (what == "character")
    output <- paste(output, collapse = "")

  output
}

#' @export
urlsafe_b64encode <- function(input) {
  str <- b64encode(input)
  gsub("+", "-", gsub("/", "_", str, fixed = TRUE), fixed = TRUE)
}

#' @export
urlsafe_b64decode <- function(input, what) {
  str <- gsub("_", "/", gsub("-", "+", input, fixed = TRUE), fixed = TRUE)
  b64decode(str, what)
}

#' @export
b32encode <- function(){}

#' @export
b32decode <- function(){}

#' @export
b16encode <- function(){}

#' @export
b16decode <- function(){}

#' @export
a85encode <- function(){}

#' @export
a85decode <- function(){}

#' @export
b85encode <- function(){}

#' @export
b85decode <- function(){}
