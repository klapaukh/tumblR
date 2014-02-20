\name{do_tumblr_query}
\alias{do_tumblr_query}
\title{Send a request to tumblr}
\usage{
do_tumblr_query(blog = NULL, user = FALSE, query = list(),
  type = AUTH_NONE, url = NULL)
}
\arguments{
  \item{blog}{[optional] The blog to use}

  \item{user}{[optional] True to make a user query}

  \item{query}{[optional] arguments to the url}

  \item{type}{Authentication type to use. One AUTH_NONE,
  AUTH_API, AUTH_OAUTH}

  \item{url}{Query path}
}
\description{
General function for sending requests to the tumblr api.
}

