#Rolling Window ASE Function
# series is the array of the series
# horizon is how far you want to predict into the future
# training size should be at least max(p,q,s,d)
# horizon is how far you want to predict into the future
# d is the order of the differencing: (1-B^)^d
# s is the order of the seasonality: (1-B^s)
# phis = order of the stationary AR term
# thetas = order of the invertible MA term



Rolling_Window_ASE = function(series, trainingSize, horizon = 1, s = 0, d = 0, phis = 0, thetas = 0)
{
  trainingSize = 70
  horizon = 12
  ASEHolder = numeric()
  s = 10
  d = 0
  phis = phis
  thetas = thetas
  
  for( i in 1:(length(series)-(trainingSize + horizon) + 1))
  {
    
    forecasts = fore.aruma.wge(series[i:(i+(trainingSize-1))],phi = phis, theta = thetas, s = s, d = d,n.ahead = horizon)
    
    ASE = mean((series[(trainingSize+i):(trainingSize+ i + (horizon) - 1)] - forecasts$f)^2)
    
    ASEHolder[i] = ASE
    
  }
  
  ASEHolder
  hist(ASEHolder)
  WindowedASE = mean(ASEHolder)
  
  print("The Summary Statistics for the Rolling Window ASE Are:")
  print(summary(ASEHolder))
  print(paste("The Rolling Window ASE is: ",WindowedASE))
  return(WindowedASE)
}
