## Put comments here that give an overall description of what your
## functions do

# these functions creates a wrapper which enables caching of
# costly computations on matrices, in this case inverse

## Write a short comment describing this function
# below functions creates setters and getters for 
# an initialized matrice. This function's variable
# inv cache the value of inverse when computed
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }
    get <- function() x
    set_inv <- function(inv_) inv <<- inv_
    get_inv <- function() inv
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## Write a short comment describing this function
# this function when called on matrix object initialized
# with previous function, enables reusing the cached value of inverse
# first it checks if inv variable is defined in the matrice 
# environment, if not it computes, save and return the value
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv = x$get_inv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$set_inv(inv)
    inv
}


a = matrix(rnorm(16), 4, 4)
print(a)
b = makeCacheMatrix(a)
print('Inverse before computing cache')
print(b$get_inv())
print(cacheSolve(b))
print('Inverse after computing cache')
print(b$get_inv())
print('Inverse calculation after computing cache')
print(cacheSolve(b))