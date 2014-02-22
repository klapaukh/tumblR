\name{get_queued_posts}
\alias{get_queued_posts}
\title{Get your queued posts}
\usage{
get_queued_posts(blog, limit = 20, offset = 0, filter)
}
\arguments{
  \item{blog}{Blog to get queued posts for}

  \item{limit}{Number of posts to return (1-20)}

  \item{offset}{Post to start at}

  \item{filter}{Format to return posts in. Default is HTML.
  alternatives are "text" and "raw"}
}
\description{
Get the OAuthed user's queued posts. Returns a maximum of
20 posts. Require OAuth.
}

