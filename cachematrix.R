## Put comments here that give an overall description of what your
## functions do

##

## Write a short comment describing this function

## MakeChacheMatrix creates the instance of an object that contains a given matrix,  the inverse of this matrix and a set of functions
## used to get the matrix and set a new value for it in case it's needed.

makeCacheMatrix <- function(z = matrix()) {
  m_inv <- NULL
  set <- function(y){
    z <<- y
    m_inv <<- NULL
  }
  get <- function() z
  set_m_inv <- function(solve) m_inv <<- solve
  get_m_inv <- function() m_inv
  list(set = set, get = get, set_m_inv = set_m_inv, get_m_inv = get_m_inv)
}


## Write a short comment describing this function

## The CacheSolve function uses an argument that is returned by makeChacheMatrix and retrieves its inverse from the cached value
## stored in the makeChacheMatrix object environment. 
    
cacheSolve <- function(z, ...) {
        ## Return a matrix that is the inverse of 'x'
  m_inv <- z$get_m_inv()
  if(!is.null(m_inv)){
    message("getting cached data")
    return(m_inv)
  }
  data <- z$get()
  m_inv <- solve(data, ...)
  z$set_m_inv(m_inv)
  m_inv
}
