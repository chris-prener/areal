test <- function(...){

  args <- rlang::list2(...)

  out <- args[[1]][2]

  return(out)

}


test(c("ham", "intensive"), c("cars", "extensive"))
