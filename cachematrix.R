## In this Programming Assignment I took advantage of the scoping rules. As it can take a long time to perform 
## certain computations (especially in the loops), it may make sense to preserve the computation results and call 
## them rather then recompute them again. The below combination of the functions computes inverse of a matrix
## and stores it in the cache. When asked again to recalculate the inverse of the matrix, if the input matrix 
## did not change the result is retrieved from the cache rather than recalculated from the scratch.


## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix of the matrix
## 4. get the value of the inverse matrix of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(s) inv <<- s
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}

##### For testing
# B = matrix(c(2, 4, 3, 1, 5, 7, 2, 8, 9), nrow=3, ncol=3) 
# BInv <- solve(B) # computes the inverse matrix
# test <- B %*% BInv

# makeCacheMatrix(B)
# undebug(cacheSolve)
# undebug(makeCacheMatrix)
# cacheSolve(makeCacheMatrix(B))

# testMatrix <- makeCacheMatrix(B)
# testMatrix$get()                  # call list item "get" from object testMatrix, which actually means calling the function that writes "x"
# testMatrix$getInv()               # call list item "getInv" from list testMatrix, which shows variable "inv"
# cacheSolve(testMatrix)            # performs function cacheSolve on testMatrix, for the first time calculates inverse matrix, second time run 
                                    # calls the already once calculated inverse matrix from cache

