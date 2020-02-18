# Rolling Window ASE Example: Amtrack

# This idea is to calculate many ASEs and take there average by using a smaller training set and forecasting the last 'n' many times. 


amtrak = read.csv(file.choose(),header = TRUE)


        
#Model 1
phis = c(0.5511, 0.1680, -0.0145, 0.0651, 0.1388, -0.2966, 0.1539, 0.1270, -0.1815, 0.0364, 0.1456, 0.6287, -0.3832, -0.0199, -0.1679)
thetas = 0
s  = 0
d  = 0
  
#Model 2  
phis = c(-0.02709541,  0.74213105)
thetas = c(-0.5844596,  0.3836931)
s = 12
d = 0
  
#Model 3
phis = 0.306943
thetas = 0.7431719
s = 12
d = 1
  
        

trainingSize = 70
horizon = 12
ASEHolder = numeric()

for( i in 1:(159-(trainingSize + horizon) + 1))
{
  
  forecasts = fore.aruma.wge(amtrak$Ridership[i:(i+(trainingSize-1))],phi = phis, theta = thetas, s = s, d = d,n.ahead = horizon)
  
  ASE = mean((amtrak$Ridership[(trainingSize+i):(trainingSize+ i + (horizon) - 1)] - forecasts$f)^2)
         
  ASEHolder[i] = ASE

}

ASEHolder
hist(ASEHolder)
WindowedASE = mean(ASEHolder)

summary(ASEHolder)
WindowedASE

# Visualization

i = 78
fs = fore.aruma.wge(amtrak$Ridership[i:(i+(trainingSize+horizon)-1)],phi = phis, theta = thetas, s = s, d = d,n.ahead = 12, lastn = TRUE)
ASE = mean((amtrak$Ridership[(i+trainingSize):(i+(trainingSize+horizon)-1)] - fs$f )^2)
ASE
