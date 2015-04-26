#These two functions help avoiding time-consuming computations,
#in this case the inverse matrix of matrices with large dimensions.
#Before entering any matrix, remember two fundamental conditions that need
#to be satisfied to get the inverse of a matrix(otherwise, it does not exist).
	#1 - The matrix must be quadratic, i.e 2x2, 3x3,..., nxn (same number
		#of rows and columns)
	#2 - The determinant of the matrix must be different than 0. You can know 
		#this by using the command "det(name of the matrix)". I recommend to
		#check this before using the two following functions.

#This first function contains four functions (set, get, setinv and getinv) that
#will be used in the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
inversa <- NULL
	set <- function(y)
		{x <<- y
		inversa <<- NULL
		}
	get <- function() x
	setinv <- function(solve) inversa <<- solve
	getinv <- function() inversa
	list(set = set, get = get, setinv = setinv, getinv = getinv)

}


#The cacheSolve function gives the inverse of the matrix "x". If it is already
#computed, the function will take the value of the inverse from the cache. If not,
#it will compute the inverse of the matrix and save it in the cache. The output of
#this function is the inverse of the matrix (input).

cacheSolve <- function(x, ...) {
      inversa <- x$getinv()
	if(!is.null(inversa))
		{message("Getting cached data")
		return(inversa)
		}
	data <- x$get()
	inversa <- solve(data, ...)
	x$setinv(inversa)
	inversa

}
