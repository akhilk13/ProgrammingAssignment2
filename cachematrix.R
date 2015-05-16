#Matrix inversion is usually a costly computation and there may be some 
#benefit to caching the inverse of a matrix rather than compute it repeatedly.
#Following pair of functions help to cache the inverse of a matrix.

#Sample Run is written after the code.

# makeCacheMatrix creates a list containing the following functions
# 1. setMatrix : To set a new value of the matrix 
# 2. getMatrix : To get the present value of the matrix
# 3. setInverse : To set the value of inverse of the matrix
# 4. getInverse : To get the value of inverse of the matrix

#The argument x is assumed to be an invertible matrix 

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  # The inverse is initially unknown. So it is assigned NULL value.
  
  setMatrix <- function(y) {
    x <<- y
    #Value of matrix is updated and value of cached inverse is nullified
    inv <<- NULL
  }
  
  getMatrix <- function() x
  #Returns the stored matrix
  
  setInverse <- function(inv_Solve) inv <<- inv_Solve
  #Value of inverse of matrix is cached and stored in inv
  
  getInverse <- function() inv
  #value of cached inverse stored in inv is returned
  
  # Returns a list where each named element of the list is a function
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function. This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  #Stores the cached inverse in inv
  
  if(!is.null(inv)) {
    #This part is executed only if the inverse is already cached
    
    message("Getting cached Inverse of given matrix")
    
    #The function terminates after the return statement
    return(inv)
  }
  #This part is executed only if the inverse is not already cached
  data <- x$getMatrix()
  inv <- solve(data)
  #It computes the inverse and caches it using setInverse
  x$setInverse(inv)
  
  #It displays the required inverse on the console
  inv
}

# Sample run:

#> x<-matrix(1:9,nrow=3,ncol=3)
#
#> a=makeCacheMatrix(x)
#
#> a$getMatrix()
#[,1] [,2] [,3]
#[1,]    1    4    7
#[2,]    2    5    8
#[3,]    3    6    9
#
#> y<-matrix(1:4,nrow=2,ncol=2)
#
#> a$setMatrix(y)
#
#> a$getMatrix()
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4
#
#> cacheSolve(a)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#
#> cacheSolve(a)
#Getting cached Inverse of given matrix
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
