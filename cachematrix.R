makeCacheMatrix <- function(x = matrix()) {
  # This sets the functions to invert the input matrix
  # The matrix is inverted using the solve() function
  # This assumes a square matrix
  
  
  m <- NULL
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(#set = set, 
    get = get,
       setinv = setinv,
       getinv = getinv)
  
}
cacheSolve <- function(x, ...) {
  #this function takes the output of makeCacheMatrix.
  #the first time it runs, it computes the inverse of the input matrix because
  #the output of x$getinv() from makeCacheMatrix is a NULL value
  #after running, x$getinv is assigned the inverted matrix so future calls of
  #this function will have the cached matrix inversion
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() #data is the original matrix
  m <- solve(data, ...) #inverted matrix calculation
  x$setinv(m) #(puts inverted matrix into x$getinv())
  m
}
