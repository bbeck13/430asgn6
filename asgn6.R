library('RUnit')

pg1 = list("+", list("+", 2, 1), 1)
pg2 = list("+", list("*", list("-", 5, 8), list("/", 12, 3)), 8)
eq1 = list("eq?", 2, 1)
eq2 = list("eq?", 2, 2)
eq3 = list("eq?", FALSE, 2)
eq4 = list("eq?", FALSE, FALSE)
minus = list("-", 3, 2)
multiply = list("*", 2, 3)
divide = list("/", 6, 3)
compare1 = list("<=", 2, 4)
compare2 = list("<=", 5, 3)
compare3 = list("<=", 2, 2)
if1 = list("if", TRUE, 1, 0)
if2 = list("if", FALSE, 1, 0)
badpg = list("+", list("+", 2, 1), 1)
num = 1
evaluate <- function(prog) {
   if (typeof(prog) == "list"){
      if (typeof(prog[[1]]) == "character") {
         op <- prog[[1]]
         if (op == "+" && length(prog) == 3) {
            return(evaluate(prog[[2]]) + evaluate(prog[[3]]))
         } else if (op == "-" && length(prog) == 3) {
            return(evaluate(prog[[2]]) - evaluate(prog[[3]]))
         } else if (op == "*" && length(prog) == 3) {
            return(evaluate(prog[[2]]) * evaluate(prog[[3]]))
         } else if (op == "/" && length(prog) == 3) {
            if (evaluate(prog[[3]]) == 0) {
               signalCondition(simpleError("Divide by zero", call = NULL))
            } else
               return(evaluate(prog[[2]]) / evaluate(prog[[3]]))
         } else if (op == "eq?" && length(prog) == 3) {
            return(evaluate(prog[[2]]) == evaluate(prog[[3]]))
         } else if (op == "<=" && length(prog) == 3) {
            return(evaluate(prog[[2]]) <= evaluate(prog[[3]]))
         } else if (op == "if" && length(prog) == 4) {
         	if (evaluate(prog[[2]]) == TRUE) {
         		return(evaluate(prog[[3]]))
         	}
         	return(evaluate(prog[[4]]))
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

checkEquals(evaluate(num), 1)
checkEquals(evaluate(pg1), 4)
checkEquals(evaluate(minus), 1)
checkEquals(evaluate(multiply), 6)
checkEquals(evaluate(divide), 2)
checkEquals(evaluate(TRUE), TRUE)
checkEquals(evaluate(eq1), FALSE)
checkEquals(evaluate(eq2), TRUE)
checkEquals(evaluate(eq3), FALSE)
checkEquals(evaluate(eq4), TRUE)
checkEquals(evaluate(compare1), TRUE)
checkEquals(evaluate(compare2), FALSE)
checkEquals(evaluate(compare3), TRUE)
checkEquals(evaluate(if1), 1)
checkEquals(evaluate(if2), 0)
checkException(evaluate("bad"))
