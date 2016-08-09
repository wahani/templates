warnings <- modules::module({

  hasLengthOne <- function(x) {
    if (length(x) != 1)
      warning(
        "Inputs should be of length = 1; ",
        "the first element will be used."
      ) 
  }
  
})
