## Put comments here that give an overall description of what your
## functions do

## ProgrammingAssignment2, R Programming, Week 3, Data Science
## Data Science course
## Assignment of two fuctions. The first function have 4 functions inside to load and 
## save outside of function environment necessary resulting data.
## The second function checks update of the calculation done previously and use results if any
## otherwise makes calculation. The approach saves resourses of the big calculations which
## saves time of program ops.

## Matrix inversion and cashing function. Setup of 4 functions for two pairs of variables with 
## and without calcs.
makeCacheMatrix <- function(matrix_in = matrix()) {
        matrix_m <- NULL
        set_mx <- function(matrix_set) {
                matrix_in <<- matrix_set
                matrix_m <<- NULL
        }
        get_mx <- function() matrix_in
        set_mx_inv <- function(mxsolve) matrix_m <<- mxsolve
        get_mx_inv <- function() matrix_m
        list(setmx = set_mx,
             getmx = get_mx,
             setmxinv = set_mx_inv,
             getmxinv = get_mx_inv)
}

## Check of matrix inversion done previously, use it as soon as exist. On other case makes inversion calcualtion.
cacheSolve <- function(matrix_in, ...) {
        matrix_m <- matrix_in$getmxinv()
        #print(matrix_m)
        if(!is.null(matrix_m)) {
                message("Getting cached data")
                #print(matrix_m)
                return(matrix_m)
        }
        data <- matrix_in$getmx()
        matrix_m <- solve(data, ...)
        matrix_in$setmxinv(matrix_m)
        matrix_m
}

## a <- makeCacheMatrix()       # call function makeCacheMatrix()
## p                            # test matrix
## [,1] [,2]
## [1,]    1    2
## [2,]    3    4
## a$setmx(p)                   # assign matrix
## b <- cacheSolve(a)           # call fuction cacheSolve(a) - nothing reported due to no inverted data were cashed yet, cashed now
## b <- cacheSolve(a)           # call fuction cacheSolve(a) - now there is a cashed inversion and it is used (printed)
## Getting cached data
## [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
