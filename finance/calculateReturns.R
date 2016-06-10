# 6/10/2016

## Time-weighted returns
# Could also implement recursively
geometricReturn <- function(listOfReturns,help="n") { 

if (help == "y") {
cat("Arithmetic Return: ((1+x1)*(1+x2) ... * (1+x3)) ^ (1/n) \
	Economic Assumption: returns are dependent on each other, i.e. compounding, linked / n \n");
}
else
{}

result <- 1;
n <- length(listOfReturns);

 for (i in listOfReturns) {
   result <- (1+i)*result 
 }

result <- (result^(1/n)) - 1;
return(result)
}

arithmeticReturn <- function(listOfReturns,help="n") { 
if (help == "y") {
cat("Arithmetic Return: x1 + x2 ... + x3 / n \
	Economic Assumption: returns are independent, i.e. non-compounding, so that they are effectively non-linked / n\n");
}
else
{}


result <- 0;
n <- length(listOfReturns);

 for (i in listOfReturns) {
   result <- i+result 
 }

result <- (result)/n;
return(result)
}

## Money (Currency) -weighted returns
