## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  ## Check that we have a square matix
  if (!ncol(x) == nrow(x)){
    message("You need a matrix with the same rows and cols to do the inverse")
    return (NULL)
  }
  
  ## Initializing the inverseMatrix
  inverseMatrix <<- NULL
  
  ## Creation of the get & set methods for the matrix (x) and the inverse matrix (inverse)
  getOriginalMatrix <- function() x
  
  setOriginalMatrix <- function(originalMatrix) {
    x <<- originalMatrix
    inverseMatrix <<- NULL
    
  }
  
  getInverseMatrix <- function() inverseMatrix
  
  setInverseMatrix <- function(inverse){
    inverseMatrix <<- inverse
  } 
    
  ## The output of the function is a list with all the possibilities
  list(getOriginalMatrix = getOriginalMatrix, setOriginalMatrix = setOriginalMatrix,
       getInverseMatrix = getInverseMatrix,
       setInverseMatrix = setInverseMatrix)  
  
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {
  
  cachedInverse <- x$getInverseMatrix()
  
  if (!is.null(cachedInverse)){
    message('Getting matrix from cache')
    return (cachedInverse)
  }
  
  ## Return a matrix that is the inverse of 'x'
  message('Getting the inverse of the matrix')
  
  inverse <- solve(x$getOriginalMatrix())
  
  x$setInverseMatrix(inverse)
  
  
}


