# The two functions are used in cojunction to calculate the inverse
# of a matrix, or to retrieve the inverse from memory if its
# inverse has already been calculated before and stored.

# This function returns a list of 3 functions to 1: return the
# matrix x stored in the main function; 2: store the value of the
# input in a variable i in the main function; 3: return that value
# of i.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  inputval <- function() x
  cacheinverse <- function(inversemat) i <<- inversemat
  inversefromcache <- function() i
  list(inputval = inputval, cacheinverse = cacheinverse, inversefromcache = inversefromcache)
}


# This function first checks the value of i, stored with
# inversefromcache, to see if its NULL. If it's not, it simply returns
# a message and the value i, which is the inverse. if it's NULL,
# the function calculates the inverse and returns it.

cacheSolve <- function(x, ...) {
  i <- x$inversefromcache()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  inputmat <- x$inputval()
  i <- solve(inputmat, ...)
  x$cacheinverse(i)
  i
}
