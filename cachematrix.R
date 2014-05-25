## Put comments here that give an overall description of what your
## functions do

## Function returns a list which
## contains methods to access matrix and its inversed stored in the cache 
## $get, $set for matrix and $getInversed, $setInvesed for the inverse of matrix
makeCacheMatrix <- function(matrix = matrix()) {
  ## variable that will hold inversed value in
  ## funcation level environment
  inversed <- NULL
  
  ## update matrix in the cache
  set <- function(value) {
    matrix <<- value
    ## drop inversed value 'cause in that point 
    ## we can't guarantee that 'matrix' is equal to passed 'value'
    inversed <<- NULL
  }
  
  ## returns cached matrix
  get <- function() x
  
  ## update inversed value in the cache
  setInversed <- function(value) inversed <<- value
  
  ## returns cached inverse value
  getInversed <- function() inversed
  
  ## list of accessors
  list(set = set, get = get, setInversed = setInversed, getInversed = getInversed)
}

makeCache <- function(data, payloadLabel = "Payload") {
  ## variable that will hold inversed value in
  ## funcation level environmenta
  payload <- NULL
  
  ## update matrix in the cache
  set <- function(value) {
    data <<- value
    ## drop inversed value 'cause in that point 
    ## we can't guarantee that 'matrix' is equal to passed 'value'
    payload <<- NULL
  }
  
  ## returns cached matrix
  get <- function() data
  
  ## update inversed value in the cache
  setPayload <- function(value) payload <<- value
  
  ## returns cached inverse value
  getPayload <- function() payload
  
  ## helper function which create accessors names for
  ## payload stored in the cache
  makeLabel <- function(prefix) paste(prefix, payloadLabel, sep = "")
  
  ## list of accessors
  env <- new.env()
  
  env$set <- set
  env$get <- get
  assign(payloadLabel, envir=env)
  assign(makeLabel("get"), getPayload, envir=env)
  
  as.list(env)
}

cachePayloadMaker <- function(cache, payloadLabel, makePayload, ...) {
  ## lookup into cache for calculated payload
  getPayload <- paste("get", payloadLabel, sep="")
  payload <- cache[[getPayload]]()
  
  ## checking that cached value is exists
  if(is.null(payload) == FALSE) {
    ## if so use it
    message("returning cached payload")
  } else {
    ## otherwise, calculate payload using cached data
    payload <- makePayload(cache$get(), ...)
    ## store calculated payloaad into cache
    setPayload <- paste("set", payloadLabel, sep="")
    cache[[setPayload]](payload)
  }
  
  ## Return a matrix that is the inverse of 'matrix'
  payload
}

makeCacheMatrix <- function(matrix = matrix()) makeCache(matrix, "Inversed")

cacheSolve <- function(cachedMatrix, ...) cachePayloadMaker(cachedMatrix, "Inversed", solve)

x <- makeCacheMatrix(matrix(c(4, 7, 2, 6), 2, 2))
cacheSolve(x)