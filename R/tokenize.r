#' Tokenize list of tweets
#'
#' @param x list of documents
#' @param stopwords vector of words to remove
#' @param stem_language which language to use (if any) for stemming using \code{\link[SnowballC]{wordStem}}
#' @export
tokenize_twitter <- function(x, stopwords = NULL, stem_language = "porter") {
  tokenize_document(x, re_twitter, stopwords, strip_punct = TRUE, stem_language = stem_language)
}

#' Tokenize a document
#'
#' @param x list of documents
#' @param pattern regex pattern to generate tokens from
#' @param stopwords vector of words to remove
#' @param strip_punct whether or not to strip single punctuation tokens
#' @param stem_language which language to use (if any) for stemming using \code{\link[SnowballC]{wordStem}}
#'
#' @export
tokenize_document <- function(x, pattern, stopwords = NULL, strip_punct = TRUE, stem_language = NULL) {
  # tokenize using pattern
  if (is.null(pattern)) pattern <- re_twitter
  x <- regmatches(x, gregexpr(pattern, x, perl = TRUE))

  # clean text
  lapply(x, function(words) {
    # lowercase (non-emoticons only)
    has_emoticon <- grepl(re_emoticon, words, perl = TRUE)
    words[!has_emoticon] <- tolower(words[!has_emoticon])

    # strip punctuation
    if (strip_punct) {
      words[!has_emoticon] <- gsub("^[[:punct:]]$", "", words[!has_emoticon], perl = TRUE)
    }

    # remove stopwords
    if (is.character(stopwords)) {
      words <- words[!(words %in% stopwords)]
    }

    # stem words
    if (!is.null(stem_language)) {
      words <- SnowballC::wordStem(words, language = stem_language)
    }

    # trim empty words
    words <- words[nchar(words) > 0]

    # return words
    words
  })
}
