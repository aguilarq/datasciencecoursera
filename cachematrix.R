##The following functions complement each other in orther to generate 
##an invertible matrix and store it in a cache environment

#makecacheMatrix creates and displays a list of funcitons
#these are then used by cacheSolve in order to get or set the inverted
#matrix in cache

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y){
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) cache <<- inverse
  getmatrix <- function() cache
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

##cacheSolve computes the inverse of the special
#"matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated (and the matrix has not changed), then
#`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  cache <- x$getmatrix()
  if(!is.null(cache)) {
    message("getting cached matrix")
    return(cache)
  }
  data <- x$get()
  cache <- solve(data)
  x$setmatrix(cache)
  cache
}
