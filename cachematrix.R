## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix initializes the x and m storage locations in the parent environ
## Then it generates the list of available functions for cacheSolve to use.

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL                  #initializing the value of m to null
     set <- function(y) {     #defining the set function
          x <<- y             #apply the matrix to object "x" in the parent environ
          m <<- NULL          #clears any prior values of m when a new matrix is assigned to "x"
     }
     get <- function() x      #defining the get function to retrieve the current value of "x" from parent environ	
     setsolve <- function(solve) m <<- solve   #defining the function to assign inverse of x to parent environ
     getsolve <- function() m     #definig the function to retrieve the inverse of x from parent environ
     
	 #Generating the return list of available functions to be used by the cacheSolve function. 
	 list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve completes the other function of makeCacheMatrix by using logic
## to either retrieve or calculate the inverse of the matrix input.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getsolve()              #retrieves the inverse of assign to m
     if(!is.null(m)) { 			   #logic to determine if inverse exists
          message("getting cached data")
          return(m)				   #if inverse exists prints the inverse matrix 
     }
     data <- x$get()			   #if no inverse exists, goes to retrieve the matrix
     m <- solve(data, ...)		   #Solves for the inverse of the matrix and assigns it to m.
     x$setsolve(m)      		   #Calls function to set the new "inverse value" into parent environ
     m							   #Returns the inverse value to be printed to the screen.
		
		
}

