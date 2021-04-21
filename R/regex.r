# https://www.nltk.org/_modules/nltk/tokenize/casual.html#TweetTokenizer

#' Join regexes together with an "or"
#'
#' Simply concatenates regex strings with a "|" symbol.
#'
#' @param ... regex strings to join together
#' @export
join_regex <- function(...) paste(..., sep = "|")

# emoticons
re_emoticon <- paste0(
  "(?:",
  "[<>]?",
  "[:;=8]",                               # eyes
  "[\\-o\\*\\']?",                        # optional nose
  "[\\)\\]\\(\\[dDpP/\\:\\}\\{@\\|\\\\]", # mouth
  "|",
  "[\\)\\]\\(\\[dDpP/\\:\\}\\{@\\|\\\\]", # mouth
  "[\\-o\\*\\']?",                        # optional nose
  "[:;=8]",                               # eyes
  "[<>]?",
  "|",
  "<3",                                   # heart
  ")"
)

# html tags
re_html <- "<[^>\\s]+>"

# ASCII arrows
re_arrow <- "[\\-]+>|<[\\-]+"

# twitter mention
re_mention <-"(?:@[\\w_]+)"

# twitter hashtag
re_hashtag <- "(?:\\#+[\\w_]+[\\w\\'_\\-]*[\\w_]+)"

# email
re_email <- "[\\w.+-]+@[\\w-]+\\.(?:[\\w-]\\.?)+[\\w-]"

# phone number
re_phone <- paste0(
  "(?:",
  # (international)
  "(?:",
  "\\+?[01]",
  "[ *\\-.\\)]*",
  ")?",
  # (area code)
  "(?:",
  "[\\(]?",
  "\\d{3}",
  "[ *\\-.\\)]*",
  ")?",
  # exchange
  "\\d{3}",
  "[ *\\-.\\)]*",
  # base
  "\\d{4}",
  ")"
)

re_words <- paste(
  "(?:[^\\W\\d_](?:[^\\W\\d_]|['\\-_])+[^\\W\\d_])", # Words with apostrophes or dashes.
  "(?:[+\\-]?\\d+[,/.:-]\\d+[+\\-]?)",  # Numbers, including fractions, decimals.
  "(?:[\\w_]+)", # Words without apostrophes or dashes.
  "(?:\\.(?:\\s*\\.){1,})", # Ellipsis dots.
  "(?:\\S)", # Everything else that isn't whitespace.
  sep = "|"
)

re_urls <- paste0(
  # Capture 1: entire matched URL
  "(?:",
  "https?:",				# URL protocol and colon
  "(?:",
  "/{1,3}",				# 1-3 slashes
  "|",					#   or
  "[a-z0-9%]",				# Single letter or digit or '%'
  # (Trying not to match e.g. "URI::Escape")
  ")",
  "|",					#   or
  # looks like domain name followed by a slash:
  "[a-z0-9.\\-]+[.]",
  "(?:[a-z]{2,13})",
  "/",
  ")",
  "(?:",					# One or more:
  "[^\\s()<>{}\\[\\]]+",			# Run of non-space, non-()<>{}[]
  "|",					#   or
  "\\([^\\s()]*?\\([^\\s()]+\\)[^\\s()]*?\\)", # balanced parens, one level deep: (...(...)...)
  "|",
  "\\([^\\s]+?\\)",				# balanced parens, non-recursive: (...)
  ")+",
  "(?:",					# End with:
  "\\([^\\s()]*?\\([^\\s()]+\\)[^\\s()]*?\\)", # balanced parens, one level deep: (...(...)...)
  "|",
  "\\([^\\s]+?\\)",				# balanced parens, non-recursive: (...)
  "|",					#   or
  "[^\\s\`!()\\[\\]{};:\'\".,<>?]",	# not a space or one of these punct chars
  ")",
  "|",					# OR, the following to match naked domains:
  "(?:",
  "(?<!@)",			        # not preceded by a @, avoid matching foo@_gmail.com_
  "[a-z0-9]+",
  "(?:[.\\-][a-z0-9]+)*",
  "[.]",
  "(?:[a-z]{2,13})",
  "\\b",
  "/?",
  "(?!@)",			        # not succeeded by a @,
  # avoid matching "foo.na" in "foo.na@example.com"
  ")"
)

# all regexes
re_twitter <- paste(
  re_urls,
  re_phone,
  re_emoticon,
  re_html,
  re_arrow,
  re_mention,
  re_hashtag,
  re_email,
  re_words,
  sep = "|")
