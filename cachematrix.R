## Programming Assignment 2: Lexical Scoping 2015-11-22

## the function makeCacheMatrix creates square invertible matrix
        
makeCacheMatrix <- function(x = matrix()) {
        inver = NULL
		
        set = function(y) 	{
								x <<- y
								inver <<- NULL
							}
        get = function() x
		
        setinv = function(inverse) inver <<- inverse 
        getinv = function() inver
		
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## the function cacheSolve is supposed to compute the inverse of the matrix returned by makeCacheMatrix()

cacheSolve <- function(x, ...) {
       
        inver = x$getinv()
        
        # check for inverscalc 
			if (!is.null(inver))
				{
					message("findig cached data")
					return(inver)
				}
        
        # else - calculates inverse 
        mat.data = x$get()
        inver = solve(mat.data, ...)
        
        x$setinv(inver)
        
    return(inver)
}