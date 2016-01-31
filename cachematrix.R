## Use  makeCacheMatrix function to assign and initialize the value of matrix 
## and its inverse in the cache.
## Use the cacheSolve function to calculate and returning the inverse value of  
## Matrix. If the inverse value of Matrix is null, then store the inverse value
## in the cache. If Inverse value of the matrix is not null, then return the value
## from the cache without spending time on calculation.

## This function is use to initialize, getting and setting value of
## matrix and its inverse 
makeCacheMatrix <- function(x = matrix()) {
  
# Initialize inverse to null
  i = NULL

# Set the value of inverse and matrix in global environment, so that it can be
# reference by various functions. We can do it bt using <<- assignment operator
  set = function(y) {
    x <<- y
    i <<- NULL
  }

# Get the value of matrix x from the environment
  get = function() x

# Set the value for inverse in global in global environment    
  setinverse = function(inverse) i <<- inverse 

# Get the value of inverse from global environment    
  getinverse = function() i
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function returns the inverse of matrix x
## It first check if the value of inverse matrix x exists in cache
## If it does not exists it, calculate
## If exists in cache, do not calculate. Return the value from cache

cacheSolve <- function(x, ...) {
        
  # Get the inverse value of X from the global environment
  i = x$getinverse()
  
  # if the the inverse value is set to a value which is not null
  # it means, inverse of matrix is the set.
  # Stop the further execution and Return the value
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  # Below code gets execute if inverse value of matrix is not set.
  # It gets the value of the matrix and using solve function
  # calculate the inverse of the matrix
  # Computing the inverse of a square matrix can be done with the solve function in R. 
  # For example, if X is a square invertible matrix, then solve(X) returns its inverse.
  matrixX = x$get()
  i = solve(matrixX, ...)
  
  # Set the ivnerse value of matrix X
  x$setinverse(i)
  
  # Return the invese value of matrix  X
  return(i)
}

#Test Results
#> a <- makeCacheMatrix(matrix(c(500,1,1,500),2,2))
#> cacheSolve(a)
#[,1]          [,2]
#[1,]  2.000008e-03 -4.000016e-06
#[2,] -4.000016e-06  2.000008e-03
#> cacheSolve(a)
#getting cached data
#[,1]          [,2]
#[1,]  2.000008e-03 -4.000016e-06
#[2,] -4.000016e-06  2.000008e-03
