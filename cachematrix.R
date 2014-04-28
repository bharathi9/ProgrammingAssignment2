## The following code lets you store the inverse of a matrix in cache.
## If the inverse calculation has to be done again, it looks for the inverse 
## in the cache. If the inverse is available in cache, we get the cached inverse
## If not, the inverse is calculated and stored in cache again for future use.

## makeCacheMatrix is a function that creates a specail matrix object  
## that can cache its inverse. Basically it is creating a list of functions
## that can set the contents of the matrix, get the contents of the matrix
## set the inverse of the matrix and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix())
{
	inverse <- NULL
	## function to set the matrix and default value for inverse
      set <- function(y) 
	{
      	x <<- y
            inverse <<- NULL
      }
	## function to get the matrix
      get <- function() x
	## function to set the inverse of the matrix 
      setinverse <- function(m_inverse) inverse <<- m_inverse
	## function to get the inverse of the matrix
      getinverse <- function() inverse
	## return the list of these functions
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve function calculates the inverse of the specail matrix object 
## that is created by the makeCacheMatrix function above. It first checks 
## to see if the inverse has already been calculated. If it is, the it gets 
## the inverse from the cache. If not it solves for the inverse and stores it 
## in cache for later use. 

cacheSolve <- function(x, ...)
{
      ## get the inverse of 'x'
	m_inverse <- x$getinverse()
	## if it already exist in cache
	if(!is.null(m_inverse))
	{
		message("Getting cached inverse.")
		return(m_inverse)
	}
	## if it is not in cache, get the matrix  
	data <- x$get()
	## solve for the inverse
	m_inverse <- solve(data)
	## store it in cache
	x$setinverse(m_inverse)
	## return the inverse of the matrix
	m_inverse
}
  