pg1 = list("+", 2, 1)
#list("+", 2, 1)
num = 1
num = simpleError("Expected Number", call = NULL)
evaulate <- function(prog) {
  if (typeof(prog) == "list"){
    if (typeof(prog[[1]]) == "character") {
      op <- prog[[1]]
      if (op == "+")
        return((parse(prog[[2]]) + parse(prog[[3]])))
    } else {
      return("Not yet")
    }
  } else if (typeof(prog) == "double") {
    return (prog)
  }
  else
    return("error input must be list")
}
parse(pg1)
parse(num)