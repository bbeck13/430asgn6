library('RUnit')

pg1 = list("+", list("+", 2, 1), 1)
badpg = list("+", list("+", 2, 1), 1)
num = 1
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
  } else if (typeof(prog) == "logical") {
    return (prog) 
  } else
    simpleError("Input must be list, number, or boolean")
}

checkEquals(evaluate(pg1), 4)
checkEquals(evaluate(num), 1)
