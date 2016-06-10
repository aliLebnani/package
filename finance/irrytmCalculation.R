# 6/10/2016

IRRandYTMCalculation <- function(listOfCashFlows,help="n") { 

if (help == "y") {
cat("Discount Rate: For IRR, 0, for Fixed Income, market price/or face value = CF/(1+r) + CF/(1+r)^2 + ... + CF/(1+r)^n solving for r   \
	Economic Assumption: Internal Rate of Return, Yield To Maturity are both the same calculation based on sovling for
	a discount rate based on numerical-analysis methods. This assumes the same cash flows but can be calculated for different cash flows / n \n");
}
else
{}
 
npv <- function(i, cf, t=seq(along=listOfCashFlows)) sum(listOfCashFlows/(1+i)^t) 
irr <- function(cf) { uniroot(npv, c(0,1), cf=listOfCashFlows)$root } 
result <-- irr(listOfCashFlows) 

return(result)
}