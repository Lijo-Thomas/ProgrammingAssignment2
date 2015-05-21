# PROGRAMMING ASSIGNMENT 2
# Caching the Inverse of a Matrix


makeCacheMatrix <- function(x = numeric(), rows, cols) {

# This function creates a special "matrix" object that can cache its inverse
#
#
# Args:   
#    x: the data vector
# rows: the desired number of rows
# cols: the desired number of columns
    
    
    m   <- NULL
    set <- function(y){
# This function is used to set the initial matrix using the arguments listed above       
        matrix(y,ro,co)
    }

    get <- function() {
# This function is used to obtain the current Matrix that has been created.
        matrix(x,rows,cols)
    }
    
    setinv <- function(inv) m <<- inv

# The setinv function is used to set the inverse of the matrix in the cache

    getinv <- function() m

# The getinv function is used to get the inverse of the matrix from the cache 
# and provide it to the cacheSolve function.

    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


#Cache Solve


cacheSolve <- function(x) {
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache
#
#   Arg:
#     x: The name of the matrix whose inverse has to be obtained
    
    m <- x$getinv()                         #if inverse of matrix was cached
    if(!is.null(m)) {
        message("getting cached data")
        return(m)                           # return cached value and exit program 
    }
    data <- x$get()                         # else add the matrix to data
    m <- solve(data)                        # use solve() to compute inverse
    x$setinv(m)                             # save the inverse of matrix in the cache
    m                                       # return inverse of matrix
}