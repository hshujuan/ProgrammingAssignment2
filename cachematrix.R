
##############################################################################
# The following functions makeCacheMatrix and cacheSolve cache the inverse of 
#         a matrix.
#
# We want to cache the inverse of a matrix since matrix inversion is usually
#         a costly computation and there may be some benefit to 
#         caching the inverse of a matrix rather than computing it repeatedly

##############################################################################
# makeCacheMatrix: 
# This function creates a special "matrix" object that can cache its inverse.
# We assume that the matrix supplied is always invertible.
##############################################################################

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    
    inverseMatrix <<- NULL
  }
  get <- function()
    x
  setInverse <- function(inverse)
    inverseMatrix <<- inverse
  getInverse <- function()
    inverseMatrix
  ## Return a list containing functions to
  ## 1. set the matrix;
  ## 2. get the matrix;
  ## 3. set the inverse;
  ## 4. get the inverse.
  ## This list is used as the input to cacheSolve().
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}
##############################################################################
# cacheSolve: 
# This function computes the inverse of the special "matrix" returned by 
#      makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed),
#      then cacheSolve will retrieve the inverse from the cache.
##############################################################################

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  IM<- x$getInverse()
  if (!is.null(IM)) {
    message("getting cached data")
    return(IM)
  }
  data <- x$get()
  IM <- solve(data, ...)
  x$setInverse(IM)
  IM
  
}



#Testing the function 

#load the matrix
# set.seed(12)
# X <- matrix(rpois(25,5), nrow = 5)
# ## Generate the makeCacheMatrix object with x
# cX <- makeCacheMatrix(X)
# cX$get()
# [,1] [,2] [,3] [,4] [,5]
# [1,]    2    1    4    4    3
# [2,]    7    3    7    5    7
# [3,]    9    6    4    5    2
# [4,]    4    1    4    6    6
# [5,]    3    1    3    2    3

## Retrieve the inverse of matrix calculated using the cacheSolve function.
# cacheSolve(cX)
# invX <- cacheSolve(cX)
# [,1]       [,2]        [,3]        [,4]       [,5]
# [1,] -0.1505376 -1.0215054  0.13978495  0.06451613  2.3118280
# [2,] -0.1075269  1.6989247 -0.04301075 -0.09677419 -3.6344086
# [3,]  0.4193548 -0.2258065 -0.03225806 -0.32258065  0.7741935
# [4,]  0.2150538 -0.3978495  0.08602151  0.19354839  0.2688172
# [5,] -0.3763441  0.9462366 -0.15053763  0.16129032 -1.7204301

