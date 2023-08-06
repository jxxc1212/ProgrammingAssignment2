## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# 1.  `makeCacheMatrix`: This function creates a special "matrix" object
# that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  
  list(set = set, 
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

X <- matrix(1:4,nrow=2,ncol=2)
X2 <- makeCacheMatrix(X)

#test get()
X2$get()

#test set()
X2$set(matrix(5:8,nrow=2,ncol=2))

#test setsolve()
Xi <- solve(X)
X2$setsolve(Xi)

#test getsolve()
X2$getsolve()



## Write a short comment describing this function
# 2.  `cacheSolve`: This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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


#first run
cacheSolve(X2)
#second run
cacheSolve(X2)
