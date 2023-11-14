## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a cache for a matrix, allowing the storage
## of both the original matrix and its inverse to improve efficiency
makeCacheMatrix <- function(x = matrix()) {
  # Initialize a variable to store the inverse matrix
  inverse <- NULL
  
  # Function to set the matrix
  setMatrix <- function(newMatrix) {
    x <<- newMatrix
    # Reset the cached inverse when the matrix is updated
    inverse <<- NULL
  }
  
  # Function to get the matrix
  getMatrix <- function() {
    x
  }
  
  # Function to set the inverse matrix to the cache
  setInverse <- function(newInverse) {
    inverse <<- newInverse
  }
  
  # Function to get the inverse matrix from the cache
  getInverse <- function() {
    inverse
  }
  
  # Return a list of functions for manipulating the matrix and its inverse
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes and returns the inverse of a matrix using a cache
## If the inverse has already been calculated and stored in the cache, it is returned directly
cacheSolve <- function(cacheMatrix, ...) {
  # Retrieve the matrix and its inverse from the cache
  matrix <- cacheMatrix$getMatrix()
  inverse <- cacheMatrix$getInverse()
  
  # If the inverse is not cached or the matrix has changed, compute the inverse
  if (is.null(inverse) || !identical(matrix, cacheMatrix$getMatrix())) {
    inverse <- solve(matrix, ...)
    # Cache the newly computed inverse
    cacheMatrix$setInverse(inverse)
  }
  
  # Return the cached inverse
  inverse
}
