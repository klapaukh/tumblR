\name{get_iterated_tagged}
\alias{get_iterated_tagged}
\title{Get more than the limilt for tagged posts}
\usage{
get_iterated_tagged(tag, before, limit, filter)
}
\arguments{
  \item{tag}{The tag to search for}

  \item{before}{Latest post to show [default: now]}

  \item{limit}{Maximum number of results}

  \item{filter}{Post format to return [ommit for HTML]
  (text / raw). May return Markdown if raw is selected.}
}
\description{
Get more than 20 posts with a specific tag in one call. May
make multiple requests to the api.
}
\examples{
setup_tumblr_apikey("MyApiKey")
get_iterated_tagged("superbowl", limit=100)
}

