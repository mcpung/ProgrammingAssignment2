## The two functions makeCacheMatrix and cacheSolve work in tandem to create the inverse of a matrix 
 # and store it in cache. If it already exists in cache then the cached version is retreived
 # If it does not exists in cache then it is created.

 # Both functions are based on the examples provided in the Assigment specification
 # the mean function is replaced with the solve function

## The function makeCacheMatrix inverses the input matrix
 # It uses the solve function to perform the inverse 
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve #solve is the function that inverts the matrix
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The function cacheSolve returns a matrix that is the inverse of x, an input matrix
 # The if statement tests if the inverse matrix exists in cache. If it exists 
 # then it's retreived from cache, or else it's created.
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}