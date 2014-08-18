## makeCacheMatrix returns a named list of 4 functions : set, get, setCache, getCache

makeCacheMatrix <- function(x = matrix()) {
        cache_value <- NULL
        # sets the matrix to use by passing it to the makeCacheMatrix
        # environment (x variable) and resets the cache value 'cache_value'
        set <- function(y) {
                x <<- y
                cache_value <<- NULL
        }
        # gets the matrix used either defined by the 'set' function or by
        # calling 'makeCacheMatrix' with a matrix
        get <- function() x
        # sets the cache value by passing it to the makeCacheMatrix environment
        setCache <- function(cache) cache_value <<- cache
        # returns the cache value
        getCache <- function() cache_value

        # returns a named list of the 4 functions : set, get, setCache, getCache
        list(set = set, get = get,
             setCache = setCache,
             getCache = getCache)
}

## cacheSolve actually computes the inverse of a square matrix by either calling
## the 'solve' function or by using the already cached value x should be a named
## list with functions attached as defined in makeCacheMatrix

cacheSolve <- function(x, ...) {
        my_inverse_matrix <- x$getCache()
        ## Is the result already cached ?
        if(!is.null(my_inverse_matrix)) {
                message("getting cached data")
                return(my_inverse_matrix)
        }
        ## Gets the matrix used
        data <- x$get()
        ## Actually does the inverse computation
        my_inverse_matrix <- solve(data, ...)
        ## Sets the cache
        x$setCache(my_inverse_matrix)
        ## Returns the inverse matrix
        my_inverse_matrix
}

####################
## Example of code
####################

## 1 : load the source file
# source("cachematrix.R")

## 2 : define a square matrix 'm1'
# m1<-matrix(c(1,2,3,4),nrow=2,ncol=2)

## 3 : attach the setters and getters functions to an object 't'
# t<-makeCacheMatrix(m1)

## 4 : Please verify that the get function returns you the same matrix
# t$get()

## 5 : get the inverse and stores the result in m2 matrix
# m2<-cacheSolve(t)
## 'm2' should be the same as m2<-matrix(c(-2,1,1.5,-0.5),nrow=2,ncol=2)
## 6 : Prints the inverse, using the cache (should print "getting cached data" on the console)
# cacheSolve(t)

## 7 : Use the same object 't' already created but setting with the 'm2' matrix
# t$set(m2)

## 8 : get the inverse of 'm2' by 'solving the cache'
# m3<-cacheSolve(t)
# cacheSolve(t)
## The resulting 'm3' matrix should be the same as 'm1'