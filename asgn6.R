library('RUnit')

pg1 = list("+", list("+", 2, 1), 1)
badpg = list("+", list("+", 2, 1), 1)
num = 1
evaluate <- function(prog) {
  if (typeof(prog) == "list"){
    if (typeof(prog[[1]]) == "character") {
      op <- prog[[1]]
        if (length(pg1) != 3) {
          signalCondition(simpleError("Binop expects 2 args", call = NULL))
        } else if (op == "+") {
          return(evaluate(prog[[2]]) + evaluate(prog[[3]]))
        } else {
          signalCondition(simpleError("Bad Binop", call = NULL))
        }
      }
  } else if (typeof(prog) == "double") {
    return (prog)
  } else if (typeof(prog) == "logical") {
    return (prog) 
  } else
    signalCondition(simpleError("Input must be list, number, or boolean", call = NULL))
}

checkEquals(evaluate(pg1), 4)
checkEquals(evaluate(num), 1)
checkException(evaluate("bad"))