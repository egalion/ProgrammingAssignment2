## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL   # remove m
  # function to set y to x in the parent environment
  set <- function(y) {  
	      x <<- y
      m <<- NULL
        }
    get <- function() x   # function to get the matrix value
    # set another value for the matrix in the parent environment
    setinverse <- function(inverse) m <<- inverse 
      getinverse <- function() m  # get the matrix of the value
      # create a list with the functions
      list(set = set, get = get,
	          setinverse = setinverse,
		         getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	  ## Return a matrix that is the inverse of 'x'
	  # get the matrix, from the makeCacheMatrix function
	  # if the inverse exists, print a message and return the value of the matrix
	  m <- x$getinverse()
  if(!is.null(m)) {
	      message("getting cached data")
      return(m)
        }
    # if not, get the original matrix from makeCacheMatrix
    # and calculate its inverse; output the inverse.
    data <- x$get()
    m <- solve(data, ...)
      x$setinverse(m)
      m
}

# test with
# m <- matrix(c(-1, -2, 1, 1), 2,2)
# x <- makeCacheMatrix(m)
# inv <- cacheSolve(x)
# m %*% inv








