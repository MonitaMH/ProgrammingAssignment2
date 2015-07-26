makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    #Defining set function
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    #Defining get function
    get <- function() x
    #Defining get and set inverse functions
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    #Generating the list for all the available functions 
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

cacheSolve <- function(x, ...) {
  #Calling the getinv function in makeCacheMatrix
  m <- x$getinv()
  #Checking if the matrix is on cache or if R has to calculate it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #Calculate the inverse of a matrix for the first time
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}