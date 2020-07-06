#'HDR Uniform RNG
#'
#'Creates a vector of uniform values ranging between 0 and 1 to be used in
#'conjunction with quantile functions to generate repeatable simulations.
#'Allows for the use of two values, "Var" and "Ent" to create repeatable
#'simulations across platforms.
#'
#'@param n The number of trials to be generated
#'@param Var Unique #ID for the simulation between 1 and 100,000,000
#'@param Ent A secondary optional unique ID between 1 and 100,000,000; defaults to 1000
#'@param Att1 A tertiary optional attribute ID between 1 and 100,000,000; defaults to 0
#'@param Att2 A quaternary optional attribute ID between 1 and 100,000,000; defaults to 0
#'@return A vector n length with psuedorandom uniform values between 0 and 1
#'@export
#'@examples
#'randoms <- HDRUniformGen(100000)
#'qnorm(randoms, 10, 2)
#'qpois(randoms, 10)
#'qexp(randoms, 2)
#'qgamma(randoms, 5)
#'qlnorm(randoms, 2, .5)
HDRUniformGen <- function(n, Var = sample(1:100000000,1), Ent = 1000, Att1 = 0, Att2 = 0){
  RandNums <- rep(0, n)
  for (i in 1:n) {
    RandNums[i] <- (((((((999999999999989%%(((i*2499997+(Var)*1800451+(Ent)*2000371+(Att1)*1796777+(Att2)*2299603)%%7450589)*4658+7450581))*383)%%99991)*7440893+(((999999999999989%%(((i*2246527+(Var)*2399993+(Ent)*2100869+(Att1)*1918303+(Att2)*1624729)%%7450987)*7580+7560584))*17669)%%7440893))*1343)%%4294967296)+0.5)/4294967296
  }
  print(RandNums)
  print("Variable seed value:")
  print(Var)
  print("Entity seed value:")
  print(Ent)
  return(RandNums)
}
