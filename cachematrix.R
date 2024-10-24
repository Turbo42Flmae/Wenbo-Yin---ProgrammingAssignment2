# 创建一个特殊的矩阵对象，可以缓存其逆矩阵
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # 初始化 inv 为 NULL

    # 设置矩阵
    set <- function(y) {
        x <<- y
        inv <<- NULL  # 设置新矩阵后，重置 inv
    }
    
    # 获取矩阵
    get <- function() x
    
    # 设置逆矩阵
    setinverse <- function(inverse) inv <<- inverse
    
    # 获取逆矩阵
    getinverse <- function() inv

    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# 计算矩阵的逆，使用缓存的逆矩阵（如果已存在）
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()  # 尝试获取已缓存的逆矩阵
    
    # 如果缓存中有逆矩阵，直接返回
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # 如果缓存中没有，计算逆矩阵
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)  # 把计算得到的逆矩阵缓存起来
    
    inv
}
