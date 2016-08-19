## Put comments here that give an overall description of what your functions do
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## initialize value of matrix inverse to NULL
  matinv <- NULL
  ## declare another function set where the value will be cached in 
  ## 1. Matrix is created for the first time. 2. changes made to cached matrix
  set <- function(y) {
    x <<- y
    ## change value of inverse of the matrix in case matrix was changed.
    matinv <<- NULL
  }
  ## gets value of inverse
  get <- function() x
  ## calculates inverse of non-singular matrix via solve function
  setInvMat <- function(solve) matinv <<- solve
  ## gets inverse 
  getInvMat <- function() matinv
  ## passes value of function makeCacheMatrix
  list(set = set, get = get, setInvMat = setInvMat, getInvMat = getInvMat)
}

## Write a short comment describing this function
## used to get the cache of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matinv <- x$getInvMat()
  ##if inverse matrix exists, it´s getting out of cache.
  if(!is.null(matinv)) {
    message("getting cached data")
    return(matinv)
  }
  ##ifinverse matrix is not there, it is calculated and then retrieved.
  data <- x$get()
  matinv <- solve(data, ...)
  x$setInvMat(matinv)
  matinv
}
