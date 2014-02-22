\name{get_draft_posts}
\alias{get_draft_posts}
\title{Get draft posts}
\usage{
get_draft_posts(blog, before_id, filter)
}
\arguments{
  \item{blog}{Blog to get draft posts for}

  \item{before_id}{Post id to start at}

  \item{filter}{Format to return posts. Ommit for HTML.
  Alternatives are "text" and "raw"}
}
\description{
Get draft posts for a user. This function requires OAuth.
}

