# drop NULL items from a list
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

#' @export
#' @title Factor to character
#' @description Convert factor columns to character in data.frame.
#' @param x data.frame
fact2char <- function(x) {
    i <- sapply(x, is.factor)
    x[i] <- lapply(x[i], as.character)
    x
}

#' @export
#' @title Character to factor
#' @description Convert character columns to factor in data.frame.
#' @param x data.frame
char2fact <- function(x) {
    i <- sapply(x, is.character)
    x[i] <- lapply(x[i], as.factor)
    x
}

cast_to_short_table <- function( x, samp_n = 6, limit = 13, symbol = "." ){
    if( nrow(x) > limit ){
        dat1 <- head(x, n = samp_n)
        dat1[] <- lapply(dat1, format)
        dat2 <- tail(x, n = samp_n)
        dat2[] <- lapply(dat2, format)
        blank_df <- dat1[1,, drop = FALSE]
        blank_df[] <- symbol
        df_info_1 <- paste("Only the", samp_n, "first rows and the", samp_n, "last rows are showned.")
        dat <- rbind(dat1, blank_df, dat2)
    } else {
        dat <- x
        df_info_1 <- ""
    }
    row.names(dat) <- NULL
    attr(dat, "info") <- df_info_1
    dat
}