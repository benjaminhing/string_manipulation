library(stringr)

#Get string given reference string and end index position
string.by.pattern <- function (ref.string, pattern.search, end.position){
  start.position <- regexpr(pattern.search, ref.string)
  end.pos <- end.position
  split.string <-unlist(str_split(ref.string, ""))
  slice.string <- split.string[start.position:end.position]
  return(paste(slice.string, collapse = ""))
} 

# Get string given index
string.by.index <- function (ref.string, start.position, end.position){
  split.string <-unlist(str_split(ref.string, ""))
  slice.string <- split.string[start.position:end.position]
  return(paste(slice.string, collapse = ""))
}

# Get character given index
get.character <-  function (ref.string, index){
  ref.string <- unlist(str_split(ref.string, ""))
  return (ref.string[index])
}

# Get total number of characters in a string
string.length <- function(ref.string){
  ref.string <- unlist(str_split(ref.string, ""))
  return (length(ref.string))
}

# Get total number of words in a string
word.number <- function (ref.string){
  words <- unlist(str_split(ref.string))
  len <- length(words)
  return (len)
}

# Get indexes of all search strings
string.indexes <- function(ref.string, pattern.search){
  index.vec <- unlist(gregexpr(pattern.search, ref.string))
  return (index.vec)
}

# Get start index of a word in a string
string.index <- function(ref.string, pattern){
  a <- regexpr(pattern, ref.string)
  return(a[1])
}

#Get reverse string
reverse.string <- function(ref.string){
  ref.string <- unlist(str_split(ref.string, ""))
  rev.string <- rev(ref.string)
  return(paste(rev.string, collapse = ""))
}

