getOnlySurname <- function(a){
  b <- a
  for (i in seq_along(a)){
    tokens <- strsplit(a[i], " ")
    isupper <- grepl("^[[:upper:]'\\.]+$", tokens[[1]])
    tokens <- tokens[[1]][isupper]
    name <- paste(tokens, collapse= " ")
    b[i] <- name
  }
  b
}

dftouppercase <- function(df) {
  data.frame(lapply(df, function(v) {
    if (is.character(v)) return(toupper(v))
    else return(v)
  }))
}

erasename <- function(df) {
  data.frame(lapply(df, function(v) {
    if (is.character(v)) {
      v <- gsub(" [A-Z]\\.", "", v)
      return(toupper(v))
    }
    else return(v)
  }))
}