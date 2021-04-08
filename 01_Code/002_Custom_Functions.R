
# Check for error ---------------------------------------------------------

f_Error_Check <- function(expr){
  any(class(tryCatch(expr, error = function(e) e)) == "error")
}
