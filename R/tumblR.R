#Tumblr doesn't always require authentican. In fact it has 3 levels
#none: Lets you see avatars
#api_key: Publically viewable inforamation
#oauth: Your own blog information / status

AUTH_NONE  = "AUTH_NONE"
AUTH_API   = "AUTH_API"
AUTH_OAUTH = "AUTH_OAUTH"

#We require somewhere to save variables that isn't the global
#scope. You cannot just use the current environment as it is locked 
#once this is treated like a package. So we need a new environment
#inside the global. This isn't exported and so can't be seen outside.
tumblRenv <- new.env(parent=.GlobalEnv)

#For the case of OAuth we need to tell httr where the find the
#site
tumblr = oauth_endpoint("request_token", "authorize", "access_token",
         base_url = "http://www.tumblr.com/oauth")

#Base url where all tumblr api calls live
tumblrApiBase = "api.tumblr.com/v2/"

#The start of time
origin="1970-01-01"

#' Store your API key for use with tumblR
#'
#' This function stores the api key for use with Tumblr API calls that
#' require an api_key. Optionally it saves the key to a file.
#'
#' @param consumer_key Your api key
#' @param credentials_file Optional argument of a file to save to
#' @export
#' @examples 
#' setup_tumblr_apikey("myApiKeyIsVeryLong")
setup_tumblr_apikey = function(consumer_key, credentials_file=NULL){
  assign("TUMBLR_CONSUMER_KEY",consumer_key, env=tumblRenv)
  if(!is.null(credentials_file)){
    save(consumer_key, file = credentials_file)
  }
}

#' Set up oauth to work with tumblR
#'
#' This function initialises oauth assuming that \code{\link{setup_tumblr_apikey}} 
#' has been called already. This allows access to tumblR api calls that require 
#' being logged into a blog. 
#' @param consumer_secret Consumer secret provided by tumblr.
#' @param credentials_file Optional argument of a file to save the consumer key and secret to. 
#' @export
#' @examples
#' setup_tumblr_apikey("myApiKeyIsVeryLong")
#' setup_tumblr_oauth("myConsumerSecretIsLongToo")
setup_tumblr_oauth = function(consumer_secret,
                               credentials_file=NULL) {
  if(!exists("TUMBLR_CONSUMER_KEY",env=tumblRenv)){
    stop("Need to call setup_tumblr_apikey before using this function")
  } 
  app = oauth_app("tumblr", key=consumer_key)
  Sys.setenv(TUMBLR_CONSUMER_SECRET=consumer_secret)

  token <- oauth1.0_token(tumblr, app)
  sig = sign_oauth1.0(app, token$oauth_token, token$oauth_token_secret)
  assign("sig",sig, env= tumblRenv)

  if (!is.null(credentials_file)) {
    save(consumer_key, consumer_secret, file=credentials_file)
  }
}

#' Get info about a blog
#'
#' Gets info about a tumblr blog. This requires a Tumblr API key.
#' This function must be called after \code{\link{setup_tumblr_api}}.
#'
#' @param blog The blog url to get information about
#' @export
#' @examples 
#' setup_tumblr_apikey("MyApiKey")
#' get_info("staff.tumblr.com")
get_info<- function(blog){
    url="info"
    resp = do_tumblr_query(blog=blog, type=AUTH_API, url=url)
    if(if_failed(resp)) return(resp)
    resp <- resp$blog
    resp <- parse_blog(resp) 
    make_response(resp) 
    return(resp)
}

#' Get an blogs avatar
#'
#' Get the avatar of a blog in a range of sizes. The allowed sizes are
#' 16, 24, 30, 40, 48 ,64, 96, 128, 512.
#'
#' @param blog The blog url 
#' @param size Size of the image to get (16, 24, 30, 40, 48 ,64, 96, 128, 512)
#' @export
#' @examples 
#' get_avatar("staff.tumblr.com")
#' get_avatar("staff.tumblr.com",512)
get_avatar <- function(blog,size=64){
  if(! (size %in% c(16,24,30,40,48,64,96,128,512))){
          stop("Size must be one of 16, 24, 30, 40, 48 ,64, 96, 128, 512")
  }
  url = "avatar/size"
  do_tumblr_query(blog=blog, url=url, type=AUTH_NONE)
}

#' Get a blogs likes
#'
#' Get up to 20 of a blogs likes. In order to get more than 20, the 
#' function must be called multiple times. This function requires an 
#' API key.
#'
#' @param blog The blog url
#' @param limit Number of likes to return (1-20)
#' @param Number of first liked post to start at
#' @export 
#' @seealso get_iterated_likes
#' @examples
#' setup_tumblr_apikey("MyApiKey")
#' get_likes("stuff.tumblr.com",limit=2,offset=0)
#' get_likes("stuff.tumblr.com",limit=2,offset=2)
get_likes <- function(blog,limit=20,offset=0){
  url="likes"
  query=list(limit=limit,offset=offset)
  do_tumblr_query(blog=blog, url=url, query=query, type=AUTH_API)
}

#' Get more than 20 likes in one call
#'
#' This function will return any number of likes. In doing so
#' it may more multiple requests to the tumblr api. 
#' This function requires an
#' API key. Unlike get_likes which returns a list of liked
#' posts and a count, this function returns the count of 
#' total number of liked posts as the attribute liked_count.
#'
#' @param blog The blog url
#' @param limit Number of likes to return (1-20)
#' @param Number of first liked post to start at
#' @export 
#' @seealso get_likes
#' @examples
#' setup_tumblr_apikey("MyApiKey")
#' get_iterated_likes("stuff.tumblr.com",limit=100,offset=0)
get_iterated_likes <- function(blog, limit=20,offset=0){
  likes = list()
  while(length(likes) < limit){
    toGet = min(limit - length(likes),20)
    l = get_likes(blog=blog, limit=toGet, offset=length(likes)+offset)
    if(if_failed(l)){
      if(length(likes) == 0){
        return(l)
      }else{
        return (likes)
      }
    }
    limit=min(limit, l$liked_count)
    likes = c(likes, l$liked_posts)
    attr(likes, "liked_count") <- l$liked_count
  }
  return(likes)
}


#' Get posts posted by a blog
#' 
#' Get the posts posted by a blog. This function requires an API key. 
#' At most 20 posts can be returned at one time. If more posts are required
#' the function will need to be called multiple times, or 
#' \code{\link{\get_iterated_posts}} can be called.
#'
#' @param The type of post of get [ommit to get all] (text,quote,link,answer,video,audio,photo,chat)
#' @param id The id of the post to get [ommit if not needed]
#' @param tag Only get posts with specified tag [Ommit if not needed]
#' @param limit Return up to limit many posts (1-20)
#' @param offset First post to get [0 if ommitted]
#' @param reblog_info Return reblog information [default: False] (True / False)
#' @param notes_info Return \code{note count} and \code{note} information [default: False] (True / False)
#' @param filter Post format to return [ommit for HTML] (text / raw). May return Markdown if raw is selected.
#' @export
#' @seealso get_iterated_posts
#' @examples 
#' setup_tumblr_apikey("MyApiKey")
#' get_posts("staff.tumblr.com", limit=20,offset=0)
#' get_posts("staff.tumblr.com", limit=20,offset=20)
#' get_posts("staff.tumblr.com", id=20)
#' get_posts("staff.tumblr.com", filter="raw")
#' get_posts("staff.tumblr.com", tag="NYFW")
get_posts <- function(blog,type,id, tag, limit, offset,reblog_info, notes_info,filter){
  url="posts"
        
  query=list()
  if(!missing(type)) query = c(query,type=type)
  if(!missing(id)) query = c(query,id=id)
  if(!missing(tag)) query = c(query,tag=tag)
  if(!missing(limit)) query = c(query,limit=limit)
  if(!missing(offset)) query = c(query,offset=offset)
  if(!missing(reblog_info)) query = c(query,reblog_info=reblog_info)
  if(!missing(notes_info)) query = c(query,notes_info=notes_info)
  if(!missing(filter)) query = c(query,filter=filter)

  response = do_tumblr_query(blog=blog, query=query, url=url, type=AUTH_API)
  
  if(if_failed(response)){
    return(response)
  }
  response$response
  blog = response$blog
  count = response$total_posts
  posts = response$posts

  response = list(total_posts=count, blog=parse_blog(blog), posts=parse_posts(posts))
  make_response(response)
  return(response)
}

#' Get more than the 20 posts at once
#' 
#' This function may make multiple requests to the 
#' tumblr api.
#'
#' @param limit The number of posts to return
#' @param offset The first post to start at
#' @param ... extra options to pass to get_posts
#' @export
#' @seealso get_posts
#' @examples
#' setup_tumblr_apikey("MyApiKey")
#' get_iterated_posts("staff.tumblr.com", limit=200,offset=0)
get_iterated_posts <- function(...,limit=20,offset=0){
  posts = list()
  while(length(posts) < limit){
    toGet = min(limit - length(posts),20)
    l = get_posts(limit=toGet, offset=length(posts)+offset,...)
    if(if_failed(l)){
      if(length(posts) == 0){
        return(l)
      }else{
        return (posts)
      }
    }
    limit=min(limit, l$total_posts)
    posts = c(posts, l$posts)
    attr(posts, "total_posts") <- l$total_posts
  }
  return(posts)
 
}

#' Get posts with a specific tag
#'
#' Gets posts with a given tag. featured_timestamp is not supported.  
#' Return a list of posts. Posts do not all have the same structure
#' making them slightly harder to coerce into a sensible structure.
#'
#' @param tag The tag to search for
#' @param before Latest post to show [default: now] 
#' @param limit Maximum number of results [default: 20] (1-20)
#' @param filter Post format to return [ommit for HTML] (text / raw). May return Markdown if raw is selected. 
#' @export
#' @examples
#' setup_tumblr_apikey("MyApiKey")
#' get_tagged("superbowl")
#' get_tagged("superbowl", filter="text")
get_tagged <-function(tag, before, limit, filter){
  if(missing(tag)) stop("A tag must be supplied")

  query= list(tag=tag)

  if(!missing(before)) query = c(query, before=before)
  if(!missing(filter)) query = c(query, filter=filter)
  if(!missing(limit)) query = c(query, limit=limit)

  result = do_tumblr_query(query=query,url="tagged",type=AUTH_API)  
  if(if_failed(result)){
    return(result)
  }

  return(result)
}

#' Send a request to tumblr
#' 
#' General function for sending requests to the tumblr api. 
#'
#' @param blog [optional] The blog to use
#' @param user [optional] True to make a user query
#' @param query [optional] arguments to the url
#' @param type Authentication type to use. One AUTH_NONE, AUTH_API, AUTH_OAUTH
#' @param url Query path
do_tumblr_query <- function(blog = NULL, user=FALSE, query=list(), type=AUTH_NONE, url=NULL){
  
  if(is.null(url)){
     stop("A url must be provided")
  }

  if(type == AUTH_API){
    if(!exists("TUMBLR_CONSUMER_KEY",env=tumblRenv)){
            stop("Need to call setup_tumblr_apikey before using this function")
    }else{
            query= c(query, api_key=get("TUMBLR_CONSUMER_KEY",env=tumblRenv))
            request = "http://"
    }
  }else{
          request = "https://"
  }

  request = paste(request, tumblrApiBase,sep="")

  if(user) {
          request <- paste(request, "user/",sep="")
  }else if(!is.null(blog)){
          request <- paste(request, "blog/",blog,"/",sep="")
  }

  request = paste(request, url, sep="") 

  result = NULL
  if(type == AUTH_OAUTH){
   if(!exists("sig",env=tumblRenv)){
           stop("Need to call setup_tumblr_oauth before using this fuction")
   }
    result = GET(url=request, query=query,sig = get("sig",env=tumblRenv)) 
  }else{
    result = GET(url=request, query=query) 
  }
  
  if(result$status_code != 200){
    stop(sprintf("Something went wrong with the request (%d: %s)", result$status_code, result$headers$statusmessage))
  }
  
  result = fromJSON(content(result,"text"),F)
  
  meta = result$meta
  make_meta(meta)
  response = result$response
  make_response(response)

  if(meta$status != 200){
    return(meta)
  }
  
  return(response)
}

#' Give an object the S3 class tumblR.Meta
#' 
#' Gives an object a class so that it can be marked
#' as an unsucessful query
#'
#' @param x Object to add the class to
make_meta <- function(x) {
  class(x) <- c(class(x), "tumblR.Meta")
}

#' Give an object the S3 class tubmlR.Response
#'
#' Give an object an S3 class so that it  can be
#' marked as a successful query
#' @param x Object to add the class too
make_response <- function(x) {
  class(x) <- c(class(x), "tumblR.Response")
}

#' Test if an query failed
#' 
#' Tests if a query was unsuccessful. Assumes an S3
#' class has been attached to tag it.
#' 
#' @param Response to test
if_failed <- function(x) {
  return(inherits(x, "tumblR.Meta"))
}

#' Parse a blog entry. 
#'
#' Just turns the updated field into a date
#'
#' @param parse_blog The blog repsonse
parse_blog <- function(x){
  x$updated = tumblr_to_date(x$updated)
  return(x)
}

#' Do something to parse the posts
#'
#' At the moment this function does nothing.
#'
#' @param Posts to parse
parse_posts <- function(x){
  return(x)
}

#' Turn a tumblr timestamp into a POSIXct
#'
#' Turns a tumblr timestamp into a POSIXct for 
#' ease of use
#' 
#' @param x A tumblr timestamp
#' @export
tumblr_to_date <-function(x){
  return(as.POSIXct(x,origin=origin))
}

#' tumblR a package for accessing tumblr
#'
#' The tumblR package provides commands for accessing the tumblr API
#'
#' @import httr ROAuth jsonlite
#' @docType package
#' @name tumblR
NULL
