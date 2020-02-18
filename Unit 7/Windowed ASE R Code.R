# Rolling Window ASE Example: Amtrack

# This idea is to calculate many ASEs and take there average by using a smaller training set and forecasting the last 'n' many times. 


amtrak = read.csv(file.choose(),header = TRUE)


modelA = aic.wge(amtrak$Ridership, p = 0:20)

trainingSize = 50
horizon = 12
ASEHolder = numeric()

for( i in 1:98)
{
  
  forecasts = fore.arma.wge(amtrak$Ridership[i:(i+(trainingSize-1))],phi = modelA$phi, theta = modelA$theta,n.ahead = horizon)
  
  ASE = mean((amtrak$Ridership[(trainingSize+1):(trainingSize + (horizon))] - forecasts$f)^2)
         
  ASEHolder[i] = ASE

}

ASEHolder
hist(ASEHolder)
WindowedASE = mean(ASEHolder)

summary(ASEHolder)
WindowedASE


