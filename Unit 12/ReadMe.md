
Hint on 12.5.5:  

BSVar1 = VAR(cbind(BSales$sales,BSales$ad_tv,BSales$ad_online,BSales$discount), type = "const", lag.max = 2)  
AIC(BSVar1)

