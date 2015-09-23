# pairs of functions that cache the inverse of a matrix

# This function creaes a special matrix object that can cache its inverse
# set : set the matrix
# get : get the matrix
# setInverse : set the inverse of the matrix
# getInverse : retruns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
     m <- NULL
     set <- function(y) 
     {
          x <<- y
          m <<- NULL
     }  
     get <- function() x
     setInverse <- function(inverse) m <<- inverse
     getInverse <- function() m
     list(set = set, 
     	  get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}

# This function computes the inverse of the special matrix returned by makeCacheMatrix() function
# is.null() is used to check if the inverse has already been calculated ; if so cacheSolve() returns its inverse from the cache

cacheSolve <- function(x,...)
{
     m <- x$getInverse()
	 if(!is.null(m))
	 {
	      message("getting the inverse from the cache")
	      return(m)
	}
	mat <- x$get()
	m <- solve(mat)
	x$setInverse(m)
	m
}

## to test :

# x <- matrix(1:2,2,2)
# m = makeCacheMatrix(x)
# m$get()
#     [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# cacheSolve(m)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
# cacheSolve(m)
# getting the inverse from the cache
#     [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5






