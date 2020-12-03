# makeCacheMatrix function ----
## makeCacheMatrix is a function used to create a matrix that can cache its inverse
## Argument 'x' is a matrix
## Return a list of function to set the value of the matrix, get the value of the matrix, set the value of the inverse of the matrix and get the value of the matrix
makeCacheMatrix <- function(x = matrix()) {
  
  # initialize the inverted matrix 
  matInv <- NULL
  
  # set the matrix into another environment
  set <- function(mat) {
    x <<- mat
    matInv <<- NULL
  }
  
  # get the current matrix
  get <- function() x
  
  # set the inverted matrix into another environment
  setinverse <- function(solve) matInv <<- solve
  
  # get the current inverted matrix
  getinverse <- function() matInv
  
  # return the list of functions to set/get the matrix and its inverse
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve function ----
## cacheSolve is a function used to calculate the inverse of a Matrix; if the inverted matrix is already cached, it will get the cached object instead of calculating the inverse
## Argument 'x' which is a matrix
## Return a Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  # assign to mInv the current cached inverted matrix
  matInv <- x$getinverse()
  # if the inverse has ever been computed, return the existing object
  if(!is.null(matInv)) {
    message("getting cached data")
    return(matInv)
  }
  
  # otherwise get the matrix and calculate its inverse and return it
  data <- x$get()
  matInv <- solve(data, ...)
  x$setinverse(matInv)
  matInv
}

# Example ----
A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)
## Cache the matrix A ----
cacheA <- makeCacheMatrix(A)
## Get the inverse of matrix A ----
inverseA <- cacheSolve(cacheA)
inverseA
### return the inverse of the matrix
inverseA <- cacheSolve(cacheA)
### message to inform that the cached inverse matrix has been used and not recalculated

