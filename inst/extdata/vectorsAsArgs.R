test <- function(...){

  args <- rlang::list2(...)

  out <- c(args$extensive, args$intensive)

  return(out)

}


test(extensive = c("ham", "bacon"), intensive = c("eggs"))
