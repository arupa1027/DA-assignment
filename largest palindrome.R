is_palindrome <- function(n, d) {
  if (d %% 2 != 0) {
    stop("The number of digits must be even")
  }
  if (d == 2) {
    return(n %% 11 == 0)
  }
  digits <- array(0, d / 2) 
  digits[1] <- n %% 10
  for (i in 2:length(digits)) {
    digits[i] <- ((n %% (10*i)) - sum(digits * (10(0:(length(digits)-1))))) / 10*(i-1)
  }
  if (sum(digits * (10*((length(digits) - 1):0))) == (n - sum(digits * (10(0:(length(digits) - 1))))) / 10*length(digits)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}