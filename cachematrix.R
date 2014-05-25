## Description:
##  Returns a list which
##  contains methods to access data and associated payload stored in the cache 
## Arguments:
##  data  object to cache
## Output:
##  list of cache accessors: 
##    $get, $set for data and 
##    $getPayload, $setPayload for the payload
makeCache <- function(data) {
  ## variable that will hold payload in
  ## funcations level environment (cache)
  payload <- list()
  
  ## update data in the cache
  set <- function(value) {
    data <<- value
    ## drop payload 'cause in that point we can't
    ## guarantee that 'data' is equal to passed 'value'
    payload <<- list()
  }
  
  ## returns cached data
  get <- function() data
  
  ## update payload value by name in the cache
  setPayload <- function(name, value) payload[[name]] <<- value
  
  ## returns cached payload by name
  getPayload <- function(name) payload[[name]]
  
  ## list of accessors
  list(get = get, set = set, getPayload = getPayload, setPayload = setPayload)
}

## Description:
##  Makes a function which consume the same set of arguments as
##  as original and stores evaluated data in cache to prevent
##  recalculation
## Arguments:
##  payloadLabel  label for distinguishing among the others
##  makePayload   function which produce data calculation
## Output:
##  described function
cachePayloadMaker <- function(payloadLabel, makePayload) {
  function(cache, ...) {
    ## lookup into cache for calculated payload
    payload <- cache$getPayload(payloadLabel)
    
    ## checking that cached value is exists
    if(is.null(payload) == FALSE) {
      ## if so use it and log that the
      ## data is retrieved from cache
      message("returning cached payload")
    } else {
      ## otherwise, calculate payload using cached data
      payload <- makePayload(cache$get(), ...)
      ## store calculated payloaad into cache
      cache$setPayload(payloadLabel, payload)
    }
    
    ## Return a matrix that is the inverse of 'matrix'
    payload
  }
}

## Description:
##  Constructs object which holds matrix and 
##  cached payload
## Arguments:
##  matrix  an instance of matrix
## Exceptions:
##  evaluation will stop if given object is 
##  not an instance of matrix
makeCacheMatrix <- function(matrix = matrix()) {
  if(is.matrix(matrix) == FALSE) {
    stop("given object is not an instance of matrix")
  }
  
  makeCache(matrix)
}

## Description:
##  Constructed function (from cachePayloadMaker) which
##  uses standard 'solve' function to evaluate and
##  stores evaluated payload in 'Inversed' cache item
cacheSolve <- cachePayloadMaker("Inversed", solve)

## Description:
##  Constructed function (from cachePayloadMaker) which
##  uses standard 'mean' function to evaluate and
##  stores evaluated payload in 'Mean' cache item
cacheMean <- cachePayloadMaker("Mean", mean)


## The provided code above still have a range of 
## not covered cases for e.g. evaluated payload 
## should also be recalculated in case function parameters
## are changed ...
## The rest of the script does some sanity testing
matrix <- makeCacheMatrix(matrix(c(4, 7, 2, 6), 2, 2))

cacheSolve(matrix)
cacheMean(matrix)
cacheSolve(matrix)
cacheMean(matrix)

matrix$set(matrix(1:9, 3, 3))
cacheMean(matrix)
cacheMean(matrix)