## cache the inverse of a matrix rather than compute it repeatedly 
## Matrix inversion is usually a costly computation and there may be some benefit
## by caching the inverse of a matrix rather than compute it repeatedly.
## Following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix function creates matrix object that can cache its inverse.
## This Function will creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invdata <- NULL
  set <- function(y){
    x <<- y
    invdata <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(solveddata) invdata <<- solveddata
  getinvmatrix <- function() invdata
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)

}


## This function computes the inverse of the matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.If not it calculates 
## the inverse and store it in the cache using the setinvmatrix function.
## Note : This will work only for inversible matrix.

cacheSolve <- function(x, ...) {
  invdata <- x$getinvmatrix()
  if(!is.null(invdata)){
    message("getting cached data")
    return(invdata)
  }
  matrixdata <- x$get()
  invdata <- solve(matrixdata, ...)
  x$setinvmatrix(invdata)
  invdata
}
