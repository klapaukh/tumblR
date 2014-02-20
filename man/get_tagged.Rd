\name{get_tagged}
\alias{get_tagged}
\title{Get posts with a specific tag}
\usage{
get_tagged(tag, before, limit, filter)
}
\arguments{
  \item{tag}{The tag to search for}

  \item{before}{Latest post to show [default: now]}

  \item{limit}{Maximum number of results [default: 20]
  (1-20)}

  \item{filter}{Post format to return [ommit for HTML]
  (text / raw). May return Markdown if raw is selected.}
}
\description{
Gets posts with a given tag. featured_timestamp is not
supported. Return a list of posts. Posts do not all have
the same structure making them slightly harder to coerce
into a sensible structure.
}
\examples{
setup_tumblr_apikey("MyApiKey")
get_tagged("superbowl")
get_tagged("superbowl", filter="text")
}

