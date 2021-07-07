# Sieve of Eratosthenes for generating primes 1:n
esieve <- function(n) {
  if (n==1) return(NULL)
  if (n==2) return(n)
  # Create a list l of consecutive integers {2,3,…,N}.
  # Exclude even numbers to save computing time
  l <- c(2, seq(from=3, to=n, by=2))
  # Start counter
  i <- 1
  # Select p as the first prime number in the list, p=2.
  p <- 2
  while (p^2<=n) {
    # Remove all multiples of p from the l.
    l <- l[l==p | l%%p!=0]
    # set p equal to the next integer in l which has not been removed.
    i <- i+1
    # Repeat steps 3 and 4 until p2 > n, all the remaining numbers in the list are primes
    p <- l[i]
  }
  return(l)
}

# Prime Factors
prime.factors <- function (n) {
  factors <- c() # Define list of factors
  primes <- esieve(floor(sqrt(n))) # Define primes to be tested
  d <- which(n%%primes == 0) # 
  if (length(d) == 0) # Define candidate primes
    return(n)
  for (q in primes[d]) { # Test candidate primes
    while (n%%q == 0) { # Generate list of factors
      factors <- c(factors, q)
      n <- n/q
    }
  }
  if (n > 1) factors <- c(factors, n)
  return(factors)
}
answer <- max(prime.factors(600851475143))
