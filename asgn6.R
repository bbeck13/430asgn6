library('RUnit')

pg1 = list("+", list("+", 2, 1), 1)
pg2 = list("+", list("*", list("-", 5, 8), list("/", 12, 3)), 8)
eq1 = list("eq?", 2, 1)
eq2 = list("eq?", 2, 2)
eq3 = list("eq?", FALSE, 2)
eq4 = list("eq?", FALSE, FALSE)
badpg = list("+", list("+", 2, 1), 1)
num = 1
evaluate <- function(prog) {
   if (typeof(prog) == "list"){
      if (typeof(prog[[1]]) == "character") {
         op <- prog[[1]]
         if (op == "+") {
            return(evaluate(prog[[2]]) + evaluate(prog[[3]]))
         } else if (op == "-" && length(pg1) == 3) {
            return(evaluate(prog[[2]]) - evaluate(prog[[3]]))
         } else if (op == "*" && length(pg1) == 3) {
            return(evaluate(prog[[2]]) * evaluate(prog[[3]]))
         } else if (op == "/" && length(pg1) == 3) {
            if (evaluate(prog[[3]]) == 0) {
               signalCondition(simpleError("Divide by zero", call = NULL))
            } else
               return(evaluate(prog[[2]]) / evaluate(prog[[3]]))
         } else if (op == "eq?" && length(pg1) == 3) {
            return(evaluate(prog[[2]]) == evaluate(prog[[3]]))
         } else if (op == "<=" && length(pg1) == 3) {
            return(evaluate(prog[[2]]) <= evaluate(prog[[3]]))
         } else {
            signalCondition(simpleError("Bad Syntax", call = NULL))
         }
      }
   }
   else if (typeof(prog) == "double") {
      return (prog)
   } else if (typeof(prog) == "logical") {
      return (prog)
   } else
      signalCondition(simpleError("Input must be list, number, or boolean", call = NULL))
}

checkEquals(evaluate(pg1), 4)
checkEquals(evaluate(num), 1)
checkEquals(evaluate(eq1), FALSE)
checkEquals(evaluate(eq2), TRUE)
checkEquals(evaluate(eq3), FALSE)
checkEquals(evaluate(eq4), TRUE)
checkException(evaluate("bad"))
