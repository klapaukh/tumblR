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
#' This function initialised oauth assuming that \code{\link{setup_tumblr_apikey}}
#' has been called already. 
setup_tumblr_oauth = function(consumer_secret,
                               credentials_file=NULL) {
  
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
#' getInfo("staff.tumblr.com")
getInfo<- function(blog){
    url="info"
    resp = doTumblrQuery(blog=blog, type=AUTH_API, url=url)
    if(ifFailed(resp)) return(resp)
    resp <- resp$blog
    resp <- parseBlog(resp) 
    makeResponse(resp) 
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
#' getAvatar("staff.tumblr.com")
#' getAvatar("staff.tumblr.com",512)
getAvatar <- function(blog,size=64){
  if(! (size %in% c(16,24,30,40,48,64,96,128,512))){
          stop("Size must be one of 16, 24, 30, 40, 48 ,64, 96, 128, 512")
  }
  url = "avatar/size"
  doTumblrQuery(blog=blog, url=url, type=AUTH_NONE)
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
#' @examples
#' setup_tumblr_apikey("MyApiKey")
#' getLikes("stuff.tumblr.com",limit=2,offset=0)
#' getLikes("stuff.tumblr.com",limit=2,offset=2)
getLikes <- function(blog,limit=20,offset=0){
  url="likes"
  query=list(limit=limit,offset=offset)
  doTumblrQuery(blog=blog, url=url, query=query, type=AUTH_API)
}

#' Get posts posted by a blog
#' 
#' Get the posts posted by a blog. This function requires an API key. 
#' At most 20 posts can be returned at one time. If more posts are required
#' the function will need to be called multiple times. 
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
#' @examples 
#' setup_tumblr_apikey("MyApiKey")
#' getPosts("staff.tumblr.com", limit=20,offset=0)
#' getPosts("staff.tumblr.com", limit=20,offset=20)
#' getPosts("staff.tumblr.com", id=20)
#' getPosts("staff.tumblr.com", filter="raw")
#' getPosts("staff.tumblr.com", tag="NYFW")
getPosts <- function(blog,type,id, tag, limit, offset,reblog_info, notes_info,filter){
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

  response = doTumblrQuery(blog=blog, query=query, url=url, type=AUTH_API)
  
  if(ifFailed(response)){
    return(response)
  }
  response$response
  blog = response$blog
  count = response$total_posts
  posts = response$posts

  response = list(total_posts=count, blog=parseBlog(blog), posts=parsePosts(posts))
  makeResponse(response)
  return(response)
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
#' getTagged("superbowl")
#' getTagged("superbowl", filter="text")
getTagged <-function(tag, before, limit, filter){
  if(missing(tag)) stop("A tag must be supplied")

  query= list(tag=tag)

  if(!missing(before)) query = c(query, before=before)
  if(!missing(filter)) query = c(query, filter=filter)
  if(!missing(limit)) query = c(query, limit=limit)

  result = doTumblrQuery(query=query,url="tagged",type=AUTH_API)  
  if(ifFailed(result)){
    return(result)
  }

  return(result)
}

doTumblrQuery <- function(blog = NULL, user=NULL, query=list(), type=AUTH_NONE, url=NULL){
  
  if(is.null(url)){
     stop("A url must be provided")
  }

  if(type == AUTH_API){
    if(!exists("TUMBLR_CONSUMER_KEY",env=tumblRenv)){
            stop("Need to call setup_tumblr_apikey before using this method")
    }else{
            query= c(query, api_key=get("TUMBLR_CONSUMER_KEY",env=tumblRenv))
            request = "http://"
    }
  }else{
          request = "https://"
  }

  request = paste(request, tumblrApiBase,sep="")

  if(!is.null(user)) {
          request <- paste(request, "user/",sep="")
  }else if(!is.null(blog)){
          request <- paste(request, "blog/",blog,"/",sep="")
  }

  request = paste(request, url, sep="") 

  result = NULL
  if(type == AUTH_OAUTH){
    result = GET(url=request, query=query,sig = get("sig",env=tumblRenv)) 
  }else{
    result = GET(url=request, query=query) 
  }
  
  if(result$status_code != 200){
    warning(sprintf("Something went wrong with the request (%d: %s)", result$status_code, result$headers$statusmessage))
    return(NULL)
  }
  
  result = fromJSON(content(result,"text"),F)
  
  meta = result$meta
  makeMeta(meta)
  response = result$response
  makeResponse(response)

  if(meta$status != 200){
    return(meta)
  }
  
  return(response)
}

makeMeta <- function(x) {
  class(x) <- c(class(x), "tumblR.Meta")
}

makeResponse <- function(x) {
  class(x) <- c(class(x), "tumblR.Response")
}

ifFailed <- function(x) {
  return(inherits(x, "tumblR.Meta"))
}

parseBlog <- function(x){
  x$updated = as.POSIXct(x$updated, origin=origin)
  return(x)
}

parsePosts <- function(x){
  return(x)
}

#' tumblR a package for accessing tumblr
#'
#' The tumblR package provides commands for accessing the tumblr API
#'
#' @import httr ROAuth jsonlite
#' @docType package
#' @name tumblR
NULL
