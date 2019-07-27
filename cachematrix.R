## ProgrammingAssignment2, R Programming, Week 3, Data Science
## Data Science course

## Matrix inversion and cashing function
makeCacheMatrix <- function(matrix_in = matrix()) {
        matrix_m <- NULL
        set_mx <- function(matrix_set) {
                matrix_in <<- matrix_set
                matrix_m <<- NULL
        }
        get_mx <- function() matrix_in
        set_mx_inv <- function(mxsolve) {
                matrix_m <<- solve(mxsolve)
        }
        get_mx_inv <- function() matrix_m
        list(setmx = set_mx,
             getmx = get_mx,
             setmxinv = set_mx_inv,
             getmxinv = get_mx_inv)
}
## Matrix inversion with cashe use condition function
cacheSolve <- function(matrix_in, ...) {
        matrix_m <- matrix_in$getmxinv()
        #print(matrix_m)
        if(!is.null(matrix_m)) {
                message("Getting cached data")
                return(matrix_m)
        }
        data <- matrix_in$getmx()
        matrix_m <- solve(data, ...)
        matrix_in$setmxinv(matrix_m)
        matrix_m
}
