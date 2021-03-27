#Rolling Window ASE Function
# series is the array of the series
# horizon is how far you want to predict into the future
# d is the order of the differencing: (1-B^)^d
# s is the order of the seasonality: (1-B^s)
# phis = order of the stationary AR term
# thetas = order of the invertible MA term

# It simply takes the given horizon and the model in the form of s,d,phis and 
# thetas and figures out how many windows it can create in the data (series) and then calculates the ASE for each window.  
#The output is the average off all the ASEs from each individual window.  

roll.win.ase.wge = function(series, horizon = 1, s = 0, d = 0, phis = 0, thetas = 0)
{
  
  trainingSize = length(phis) + length(thetas) + s + d + 1
  numwindows = length(series)-(trainingSize + horizon) + 1
  ASEHolder = numeric(numwindows)

  print(paste("Please Hold For a Moment, TSWGE is processing the Rolling Window ASE with", numwindows, "windows."))
  
  for( i in 1:numwindows)
  {
    
    invisible(capture.output(forecasts <- fore.aruma.wge(series[i:(i+(trainingSize-1))],phi = phis, theta = thetas, s = s, d = d,n.ahead = horizon)))
    
    ASE = mean((series[(trainingSize+i):(trainingSize+ i + (horizon) - 1)] - forecasts$f)^2)
    
    ASEHolder[i] = ASE
    
  }
  
  ASEHolder
  hist(ASEHolder, main = "ASEs for Individual Windows")
  WindowedASE = mean(ASEHolder)
  
  print("The Summary Statistics for the Rolling Window ASE Are:")
  print(summary(ASEHolder))
  print(paste("The Rolling Window ASE is: ",round(WindowedASE,3)))
  return(list(rwASE = WindowedASE, numwindows = numwindows, horizon = horizon, s = s, d = d, phis = phis, thetas = thetas))
}
