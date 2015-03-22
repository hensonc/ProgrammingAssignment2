## makeCacheMatrix will generate a square, invertable matrix
## cacheSolve will check for a cached version of the matrix and 
## generate one if needed.


## Creates a function with its own environment to cache results

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL #initialize the variable for cache
    set <- function(y) { #set the matrix, y being the variable passed into the function
      x <<- matrix(rnorm(y), y, y) #assign matrix to variable x
      m <<- NULL #assign m to be null
    }
    get <- function() x #make a function 'get' for makeCacheMatrix
    setsolve <- function(solve) m <<- solve #takes a value ('solve') and sets it to the value of 'm' in this environment
    getsolve <- function() m #returns the value of 'm' from this environment
    list(set = set, get = get, #store these methods in a list so that they can be accessed using $
         setsolve = setsolve,
         getsolve = getsolve)
}

## Checks for and then generates the inverse of the matrix

cacheSolve <- function(x, ...) {
  m <- x$getsolve()             #Assigns to 'm' the value from x environment
  message("Checking cache...")
  if(!is.null(m)) {             #If the 'x' environment exists, the function prints message and the value of m      
    message("Getting cached data")
    return(m)
  }
  message("Setting new cached value...")
  data <- x$get()       #If this 'x' has never been evaluted before, pull the x-matrix into 'data'
  m <- solve(data, ...) #Calculate the inverse of the matrix x by calling 'solve' on 'data'
  x$setsolve(m)         #Assign the result to the 'x' environment using the 'setsolve' function.
  m                     #display m
}
