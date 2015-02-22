makeCacheMatrix <- function(x=matrix()){
        inv <- NULL
        set <- function(y){
                matrix <<- y
                inv <<- NULL
        }
        get <- function(){
                matrix
        }
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        getInverse <- function() {
                inv
        }
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
         m <- solve(data) %*% data
        x$setInverse(m)
        m 
}
