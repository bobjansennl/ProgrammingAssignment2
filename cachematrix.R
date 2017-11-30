## The cachSolve function inverts a matrix if that matrix had not been resolved before and
## has been stored in the cache that is created by the makeCacheMatrix function

## The makeCacheMatrix returns a list of functions that allow to get and set the matrix and
## to get and set the inverted matrix

makeCacheMatrix <- function( x = matrix() ) {
	m <- NULL
	set <- function( y ) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setmatrix <- function( matrix ) { m <<- matrix }
	getmatrix <- function() { m }
	list( set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix )
}

## The cacheSolve function takes a matrix as an argument and returns the matrix inverted.
## If the inversion was already cached it will return the matrix fro the cache else it 
## will process the invertion, store it in cache and return it

cacheSolve <- function( x, ... ) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getmatrix()
	if( !is.null( m ) ) {
		message( "getting cached data" )
		return( m )
	}
	data <- x$get()
	m <- solve( data )
	x$setmatrix( m )
	m
}
