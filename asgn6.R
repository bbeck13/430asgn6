library('RUnit')

pg1 = list("+", list("+", 2, 1), 1)
num = 1
num = simpleError("Expected Number", call = NULL)
evaluate <- function(prog) {
  if (typeof(prog) == "list"){
    if (typeof(prog[[1]]) == "character") {
      op <- prog[[1]]
      if (op == "+")
        return((evaluate(prog[[2]]) + evaluate(prog[[3]])))
    } else {
      return("Not yet")
    }
  } else if (typeof(prog) == "double") {
    return (prog)
  }
  else
    return("error input must be list")
}
checkEquals(evaluate(pg1), 4)
checkEquals(evaluate(num), 1)