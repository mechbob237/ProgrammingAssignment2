## Two functions to cache an invertible matrix
## and solve its inverse.

## Takes an inverible matrix and
## returns: a list containing functions to:
## 1. set the matrix,
## 2. get the matrix,
## 3. set the inverse,
## 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv,getInv = getInv)
}

## Takes cached matrix from makeCacheMatrix,
## checks to see if the inverse has been solved,
## returns that if it has been solved, and if not,
## solves and returns inverse as cacheSolve

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInv(inv)
  inv
}
