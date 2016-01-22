## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(Solve) m <<- solve(x) ## this function caches result of inversion 
  getSolve <- function() m  ## this function retreives inversed matrix from cach
  ## returning list of functions
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve() ## trying to get cached result into variable m
  if(!is.null(m)) {  ## if successful notify and return previously cached inversed matrix
    message("getting cached data")
    return(m)
  }
  data <- x$get()  ## get data to be inversed from the not cached matrix
  m <- solve(data, ...)   ## calculating inversed matrix
  x$setSolve(m) ## caching result of inversion
  m ## returning inversed matrix
}
