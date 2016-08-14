# The main objective of these two functions is to cache matrix and its inverse
# to be accessibe from other functions.

# makeCacheMatrix function creates a special matrix object to cache matrix and its inverse.
# It has 2 get and 2 set functions for that.
# Matrix is cached in "m", the inverse matrix is cached in "m_inv".
# Inverse matrix should be calculated outside and cached calling setinv().
# Every time new matrix is set with set(), the variable where the inverse matrix cached before
# become empty, and to cache the inverse matrix again the setinv() should be called.

makeCacheMatrix <- function(x = matrix()) {
      m_inv <- NULL
      Set <- function(y) {
            x <<- y
            m_inv <<- NULL
      }
      Get <- function() x
      SetInv <- function(m_solved) m_inv <<- m_solved
      GetInv <- function() m_inv
      list(set = Set, get = Get,
           setinv = SetInv, getinv = GetInv)
}

# cacheSolve function returns the inverse matrix of the matrix cached
# by makeCacheMatrix. If there is no inverse matrix in cache, this function
# gets normal matrix from makeCacheMatrix object, computes its inverse and
# sets it back to makeCacheMatrix object, which caches it, so next time,
# if matrix is not changed, it will be taken from cache.

cacheSolve <- function(x, ...) {
      m <- x$getinv()
      if(!is.null(m)) {
            message("Getting the inverse matrix from cache!")
            return(m)
      }
      m <- solve(x$get(), ...)
      x$setinv(m)
      m
}

