## The following pair of functions allow for the inverse of a matrix to be cached and returned
## on susequent calculations to improve processing speed and reduce the cost of this sometimes expensive operation

## The first function makeCacheMatrix creates a special list containing 4 functions
## The function initialises and provides mutator and accessor methods used in caching 
## and retuning the martix inversion   

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL    ##initiates object i to be used later in the code
  set <- function(y) {
    x <<- y
    i <<- NULL ##makes global environment object i NULL whenever x is reset
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse  ###UNSURE ON THIS LINE
  getinverse <- function() i    ##return i from parent environment
  
  ##creates special list containing getters and setters
    list(set = set, get=get, setinverse = setinverse, getinverse = getinverse) 
  
}


## This function checks the cache to see if the inverse of matrix x has already been calcualted
## If so, it return the invesre from the cache, if not, it computes the inverse and adds to cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() 
  if(!is.null(i)) {
    message("Retreiving inverse from cache")
    return(i)
  }
  j <- x$get()
  i <- solve(j, ...)
  x$setinverse(i)
  i
}


###Test Case 1

##k <- makeCacheMatrix(matrix(c(1,2,3, 0,1,4, 5,6,0),3,3))
##cacheSolve(k)
##cacheSolve(k)

##k <- makeCacheMatrix(matrix(c(1,3,3, 0,1,4, 5,6,0),3,3))
##cacheSolve(k)
##cacheSolve(k)




