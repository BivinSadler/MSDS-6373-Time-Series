#Walmart Store 8Item 1 Filtering / spectral analysis / AR(3)
# Read in the data
Walmart = read.csv(file.choose(),header = TRUE)
# Load the Data
Stor8Item1 = Walmart %>% filter(item == 1 & store == 8)
#Look at and Visualize the data
head(Stor8Item1)
plotts.wge(Stor8Item1$sales)
#Break into month day and year.
Stor8Item1 = separate(Stor8Item1,col = date,into = c("month","day","year"), sep = "/")
#Change to dataframe
Stor8Item1 = data.frame(Stor8Item1)
#Look at Spectral density... evidence of yearly, monthly and weekly trend?  
#Yearly and Monthly are going to be tough with daily data. 
parzen.wge(Stor8Item1$sales, trunc= 500)
# Change to integers for easier sorting later.... could use other package to handle dates. 
Stor8Item1$month = as.integer(Stor8Item1$month)
Stor8Item1$year = as.integer(Stor8Item1$year)
# Aggregate to get monthly sales
Stor8Item1_grouped = Stor8Item1 %>% group_by(year,month) %>% summarise(mean_sales = mean(sales))
#Note data is out of order and that is a big deal.
head(Stor8Item1_grouped)
#Order by year and month
Stor8Item1_grouped = Stor8Item1_grouped[order(Stor8Item1_grouped$year,Stor8Item1_grouped$month),]
# Evidence of yearly trend?  Montly trend is still tough since there are differnt number of days in a month.
parzen.wge(Stor8Item1_grouped$mean_sales)
# to more clearly see the annual trend
parzen.wge(Stor8Item1_grouped$mean_sales,trunc = 30) 
# Shows combo of pseudo cyclic and wandering behavior.
acf(Stor8Item1_grouped$mean_sales,lag = 30) 