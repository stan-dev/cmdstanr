
`%||%` <- function(x, y) if (!is.null(x)) x else y

os_is_windows <- function() .Platform$OS.type == "windows"
