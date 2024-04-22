# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse to NULL
  inv <- NULL
  
  # Function to set the matrix value and reset the inverse cache
  set <- function(y) {
    x <<- y   # Assign the input matrix y to the global variable x
    inv <<- NULL   # Reset the inverse cache to NULL since the matrix has changed
  }
  
  # Function to get the current matrix value
  get <- function() {
    x   # Return the current matrix value
  }
  
  # Function to set the cached inverse value
  setInverse <- function(inverse) {
    inv <<- inverse   # Assign the input inverse to the global variable inv
  }
  
  # Function to get the cached inverse value
  getInverse <- function() {
    inv   # Return the cached inverse value
  }
  
  # Return a list of functions to manipulate the matrix object
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Function to compute the inverse of the special "matrix" object returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  # Retrieve the cached inverse value
  inv <- x$getInverse()
  
  # If the cached inverse exists, return it
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  # If the cached inverse doesn't exist, compute it
  mat <- x$get()   # Get the current matrix value
  inv <- solve(mat, ...)   # Compute the inverse of the matrix
  
  # Cache the computed inverse
  x$setInverse(inv)
  
  # Return the computed inverse
  inv
}
