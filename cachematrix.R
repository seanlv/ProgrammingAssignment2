## Put comments here that give an overall description of what your
## functions do
# 缓存逆矩阵
# 包括两个函数：makeCacheMatrix() & cacheSolve()
# makeCacheMatrix() - 用于创建可缓存逆矩阵的特殊“矩阵”对象。
# cacheSolve() - 用于计算上述makeCacheMatrix()返回的特殊“矩阵”的逆矩阵。
#                如果已经计算逆矩阵（且尚未更改矩阵），那么cachesolve()将检索缓存中的逆矩阵。

## Write a short comment describing this function
# makeCacheMatrix() 包含具有以下用途的函数的列表:
# 1. 设置矩阵值
# 2. 获取矩阵值
# 3. 设置逆矩阵值
# 4. 获取逆矩阵值
makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	
	get <- function() x
	
	setsolve <- function(solve) s <<- solve
	
	getsolve <- function() s
	
	list(set = set, get = get,
	     setsolve = setsolve,
	     getsolve = getsolve)
}


## Write a short comment describing this function
# cacheSolve() 返回逆矩阵。
#              首先查看是否已经存在逆矩阵。如果是，直接返回已经缓存的逆矩阵。
#              否则，重新计算并缓存。
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	s <- x$getsolve()
	if (!is.null(s)) {
		message("geting cached data")
		return(s)
	}
	
	data <- x$get()
	s <- solve(data, ...)
	x$setsolve(s)
	s
}
