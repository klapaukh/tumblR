\name{setup_tumblr_oauth}
\alias{setup_tumblr_oauth}
\title{Set up oauth to work with tumblR}
\usage{
setup_tumblr_oauth(consumer_secret, credentials_file = NULL)
}
\arguments{
  \item{consumer_secret}{Consumer secret provided by
  tumblr.}

  \item{credentials_file}{Optional argument of a file to
  save the consumer key and secret to.}
}
\description{
This function initialises oauth assuming that
\code{\link{setup_tumblr_apikey}} has been called already.
This allows access to tumblR api calls that require being
logged into a blog.
}
\examples{
setup_tumblr_apikey("myApiKeyIsVeryLong")
setup_tumblr_oauth("myConsumerSecretIsLongToo")
}

