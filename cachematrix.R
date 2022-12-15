## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## First we need to set the parameters. This can be done in the same way as in the example (makeVector)
## In order to make things easy to understand I am retaining most of the setup from the example function
## and only changing the name to, in my opinion, better describe what is being done

# The first function stores the inverse matrix and will be called by the second function, if the inverse was already calculated
# it will return the cached value, otherwise it will be used to store the newly calculated inverse

makeCacheMatrix <- function(x = matrix()) {
  #set the inverse to NULL
  inverse_matrix <- NULL
  set <- function() {
    x <<- y
    inverse_matrix <<- NULL
  }
  # specify the get function so that the matrix can be retrieved by the cacheSolve function
  get <- function() x
  setinverse <- function(inverse) inverse_matrix <<- inverse
  getinverse <- function() inverse_matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## Write a short comment describing this function
# The second function calls the first and checks if there is a stored inverse matrix
# in case it is not, it calculates it and then uses the first function to store it

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  # Start by checking if the inverse was already calculated
  # If so, then return the inverse_matrix object already stored in cache 
  # and write a message that the data is being retrieved and not calculated
  inverse_matrix <- x$getinverse()
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  # if the value is missing (NULL), we continue by assigning the input matrix 
  # to the MatrixToBeInverted object, this is actually retrieved from the current value
  # stored when the MakeCacheMatrix function was called
  MatrixToBeInverted <- x$get()
  # make the inverse of the matrix using the solve function
  inverse_matrix <- solve(MatrixToBeInverted, ...)
  # set the inverse_matrix
  x$setinverse(inverse_matrix)
  # return the inverse matrix
  inverse_matrix
}
