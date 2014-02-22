\name{get_user_dashboard}
\alias{get_user_dashboard}
\title{Get user's dashboard}
\usage{
get_user_dashboard(limit, offset, type, since_id, reblog_info, notes_info)
}
\arguments{
  \item{limit}{Number of posts to return (1-20)}

  \item{offset}{Post to start at}

  \item{type}{Type of posts to return (omit for all) (text,
  photo, quote, link, chat, audio, video, answer"}

  \item{since_id}{Return posts after this id (used for
  pagination)}

  \item{reblog_info}{Return reblog information}

  \item{notes_info}{Return notes information}
}
\description{
Get a user's dashboard. Requires OAuth.
}

