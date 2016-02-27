library('RUnit')
rm(x)
pg1 = list("+", list("+", 2, 1), 1)
pg2 = list("+", list("*", list("-", 5, 8), list("/", 12, 3)), 8)
pg3 = list("with", list("x", "=", 8), "x")
pg4 = list("with", list("x", "=", 8), list("y", "=", 2), list("+", "x", "y"))
pg5 = list("with", list("x", "=", list("with", list("y", "=", 3), "y")), "x")
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
badpg = list("+", list("+", 2, 1), 1)
num = 1

getFromEnv <- function(var, environment) {
  if (length(environment) == 0) {
    signalCondition(simpleError("Could not find variable", call = NULL))
  }
  else if (environment[[1]] == var) {
	return(environment[[2]])
  }
  else if (length(environment) <= 2) {
    signalCondition(simpleError("Could not find variable", call = NULL))
  }
  else {
    getFromEnv(var, environment[3:length(environment)])
  }
}

bindLocals <- function(env, locals) {
  newenv = env
  for (bind in locals) {
	newenv = c(newenv, bind[[1]], bind[[3]])
  }
  return(newenv)
}

evaluate <- function(prog, env) {
   if (typeof(prog) == "list"){
      if (typeof(prog[[1]]) == "character") {
         op <- prog[[1]]
         if (op == "+") {
            return(evaluate(prog[[2]], env) + evaluate(prog[[3]], env))
         } else if (op == "-" && length(prog) == 3) {
            return(evaluate(prog[[2]], env) - evaluate(prog[[3]], env))
         } else if (op == "*" && length(prog) == 3) {
            return(evaluate(prog[[2]], env) * evaluate(prog[[3]], env))
         } else if (op == "/" && length(prog) == 3) {
            if (evaluate(prog[[3]], env) == 0) {
               signalCondition(simpleError("Divide by zero", call = NULL))
            } else
               return(evaluate(prog[[2]], env) / evaluate(prog[[3]], env))
         } else if (op == "eq?" && length(prog) == 3) {
            return(evaluate(prog[[2]], env) == evaluate(prog[[3]], env))
         } else if (op == "<=" && length(prog) == 3) {
            return(evaluate(prog[[2]], env) <= evaluate(prog[[3]], env))
         } else if (op == "with" && length(prog) >= 3) {
		newenv = bindLocals(env, prog[2: (length(prog) - 1)])
		temp = evaluate(prog[[length(prog)]], newenv)
		return(temp)
	   } else {
            signalCondition(simpleError("Bad Syntax", call = NULL))
         }
      }
   }
   else if (typeof(prog) == "double") {
      return (prog)
   } else if (typeof(prog) == "logical") {
      return (prog)
   } else if (typeof(prog) == "character") {
	return(getFromEnv(prog, env))
   } else {
      signalCondition(simpleError("Input must be list, number, or boolean", call = NULL))
   }
}

topeval <- function(prog) {
  evaluate(prog, list())
}

checkEquals(topeval(pg1), 4)
checkEquals(topeval(minus), 1)
checkEquals(topeval(multiply), 6)
checkEquals(topeval(divide), 2)
checkEquals(topeval(num), 1)
checkEquals(topeval(eq1), FALSE)
checkEquals(topeval(eq2), TRUE)
checkEquals(topeval(eq3), FALSE)
checkEquals(topeval(eq4), TRUE)
checkEquals(topeval(compare1), TRUE)
checkEquals(topeval(compare2), FALSE)
checkEquals(topeval(compare3), TRUE)
checkException(topeval("bad"))
checkEquals(topeval(pg3), 8)
checkEquals(topeval(pg4), 10)