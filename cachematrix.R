## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeMatrix <- function(x = numeric(), rows, cols) {
  m <- NULL
  set <- function(y){
    matrix(y,ro,co)
  }
  get <- function() {
    matrix(x,rows,cols)
  }
  
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}