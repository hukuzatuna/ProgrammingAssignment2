## These functions provide caching matrix inversion.
## This reduces the performance hit when performing
## repeated matrix inversions.
##
## Philip R. Moyer (hukuzatuna@gmail.com)
## September 2015

## This function creates and manages the list of 
## functions needed to manipulate the matrix
## inverse cache.

makeCacheMatrix <- function(x = matrix()) {
	cInvMatrix <<- NULL
  gx <<- x
	setMatrix <- function(nm) {
		gx <<- nm
		cInvMatrix <<- NULL
	}
	getMatrix <- function() gx
	setMatInv <- function(nm) cInvMatrix <<- nm
	getMatInv <- function() cInvMatrix
	list(
		setMatrix = setMatrix,
		getMatrix = getMatrix,
		setMatInv = setMatInv,
		getMatInv = getMatInv
	)
}


## This function returns the cached inverse of x,
## provided it exists.
##
## If there is no cached version of x, it calculates
## the inverse, caches the answer, then returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
	if (is.null(x$getMatInv())) {
		## Matrix inverse is not cached
		## print("Not using cached value")
    cSolution <- solve(as.matrix(x$getMatrix()))
    ## print("Intermediate solution:")
    ## print(cSolution)
    ## Store the solution
		x$setMatInv(cSolution)
	}

	## Matrix inverse is cached, either because it
	## already was, or because we just cached it
	rv <- x$getMatInv()

	## Explicitly return the return value
	return(rv)
}


