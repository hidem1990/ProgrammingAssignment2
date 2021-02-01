## R programming Assignment #2
## invert a matrix and cache the result. 
## if the result has been calculated, the function returns the cached result. 

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  get <- function() x
  setinv <- function(solve_answer) invMat <<- solve_answer
  getinv <- function() invMat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  invMat <- x$getinv()
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  data <- x$get()
  invMat <- solve(data, ...)
  x$setinv(invMat)
  invMat
}
