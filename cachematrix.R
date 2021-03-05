## The function will set up a list where it sets and gets the value of the matrix, 
##and sets and gets the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv<<- NULL
                }
                get <- function() x
                setinverse <- function(inverse) inv <<- inverse
                getinverse <- function() inv
                #the list is set up 
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        }


##CacheSolve checks whether the inverse has been computed, if yes, it will get the stored value
##otherwise, the inverse will be computed

cacheSolve <- function(x, ...) {
                inv <- x$getinverse()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data, ...)
                x$setinverse(inv)
                inv
        }
        ## Return a matrix that is the inverse of 'x'
