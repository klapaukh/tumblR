\name{setup_tumblr_apikey}
\alias{setup_tumblr_apikey}
\title{Store your API key for use with tumblR}
\usage{
setup_tumblr_apikey(consumer_key, credentials_file = NULL)
}
\arguments{
  \item{consumer_key}{Your api key}

  \item{credentials_file}{Optional argument of a file to
  save to}
}
\description{
This function stores the api key for use with Tumblr API
calls that require an api_key. Optionally it saves the key
to a file.
}
\examples{
setup_tumblr_apikey("myApiKeyIsVeryLong")
}

