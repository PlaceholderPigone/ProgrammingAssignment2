## Cachematrix creates a matrix and also caches its inverse, 
## to save processing time on calculating the inverse


## This function creates a list of functionas and lexically stored
## variables in order for the cachesolve to access the
## matrices to solve as well as to store the solved values

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL #clear solved value for new matrix
  
  setmatrix <- function(y){
   x <<- y
   inverse <<- NULL
   #Set current cached matrix and clear solved value
  }
  
  getmatrix <- function() {x} #return the cached matrix
  
  #Store the inputted solved matrix value
  setinverse <- function(ival) {inverse <<- ival}
  
  #return the solved matrix
  getinverse <- function() {inverse}
  
  #Puts all the functions in a list that contains the stored matrix and its solution
  list(setmatrix = setmatrix, 
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## CacheSolve interacts with Cachematrix's function list
## in order to check for a cached solution, and if not,
## it will calculate the solution and store it within the
## list with setinverse

cacheSolve <- function(x, ...) {
  #Set the local inverse variable based on whats stored in the cached matrix
  matrix.inverse <- x$getinverse()
  
  #First check if the matrix is already solved, if so, return solution
  if(!is.null(matrix.inverse)){
    message("getting cached data")
    return(matrix.inverse)
  }
  #Load the cached matrix data into the closure
  matrix.data <- x$getmatrix()
  #Solve the loaded matrix
  sm <- solve(matrix.data, ...) 
  #Store the inverse matrix 
  x$setinverse(sm) 
  #Return the inverse matrix
  sm
}
