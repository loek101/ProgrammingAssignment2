# First I'll make a simple invertible matrix for testing

A <- matrix(c(1,2,2,0), 2, 2) 
solve(A)

# answer should be:
#     [,1] [,2]
#[1,] 0.0  0.50
#[2,] 0.5 -0.25

# This is the fuction that can cache the inverse of the given matrix
makeMat <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

# This is the function that skips calculating the inverse if
# that has already been done before. Input is the object created
# by the function makeMat (above).
cachesolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

# Demonstration that the functions work:

A2 <- makeMat(A) # The matrix A is inverted and stored in object
cachesolve(A2) # The inverted matrix is given
# The second time the function is called, it retrieves from cache
cachesolve(A2) 
