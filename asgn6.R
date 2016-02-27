library('RUnit')

pg1 = list("+", list("+", 2, 1), 1)
pg2 = list("+", list("*", list("-", 5, 8), list("/", 12, 3)), 8)
eq1 = list("eq?", 2, 1)
eq2 = list("eq?", 2, 2)
eq3 = list("eq?", FALSE, 2)
eq4 = list("eq?", FALSE, FALSE)
badpg = list("+", list("+", 2, 1), 1)
num = 1

myRep <- function(rep, inHere, with) {
  i2 = 1
  for (ch in inHere) {
    if (ch == rep)
      inHere[[i2]] <- with
    i2 <- i2 + 1
  }
  inHere
}

evaluate <- function(prog) {

   if (typeof(prog) == "list") {
      if (typeof(prog[[1]]) == "character") {
         op <- prog[[1]]
         if (op == "func" && length(prog) == 3) {
            return (list("func", prog[[2]], prog[[3]]))
         } else if (op == "+") {
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
         } else if (op == "eq?" && length(pg1) == 3) {
            return(evaluate(prog[[2]]) == evaluate(prog[[3]]))
         } else if (op == "<=" && length(pg1) == 3) {
            return(evaluate(prog[[2]]) <= evaluate(prog[[3]]))
         } else {
            signalCondition(simpleError("Bad Syntax", call = NULL))
         }
      } else {
         if (evaluate(prog[[1]])[1] == "func") {
            fnc <- evaluate(prog[[1]])
            if (length(fnc[[2]]) != length(prog[[2]])) {
               signalCondition(simpleError("Rad BArity", call = length(prog[[2]])))
            } else {
               i = 1
               for (ch in fnc[[2]]) {
                  fnc[[3]] = myRep(ch, fnc[[3]], prog[[2]][[i]])
                  i = i + 1
               }
               rm(i)
               return(evaluate(fnc[[3]]))
            }
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
checkEquals(myRep("a", list("a"), 1), list(1))
#(evaluate(list(list("func", list(), list("+", 1, 2)), list())))
checkEquals(evaluate(list(list("func", list(), list("+", 1, 2)), list())), 3)
checkEquals(evaluate(list(list("func", list("a"), list("+", "a", "a")), list(1))), 2)

