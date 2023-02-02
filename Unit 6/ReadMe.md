
# Required Reading

 ## 7 ARIMA, Seasonal, and ARCH/GARCH Models
* 7.1 ARIMA(p,d,q) Models  
      * 7.1.1 Properties of ARIMA(p,d,q) Models  
      * 7.1.2 Model Identification and Parameter Estimation of ARIMA(p,d,q) Models   
      
* 7.2 Seasonal Models  
      * 7.2.1 Properties of Seasonal Models  
      * 7.2.2 Fitting Seasonal Models to Data  

## Homework
Chapter 7: Problems 1 and 2    
        
  
## Optional Reading for Unit 6 From Woodward, Gray and Elliot 

Chapter 5: 

5.1 (all)

5.2 (all but Theorem 5.1 and Example 5.7 ... although these are suggested)

5.3 (all)

# Erata

1. 6.4 card 4: none of the answer choices match the output from R.  The answer is ARMA(2,1).
2. There is a 'B' missing in the model specification in the MA part of 6.6 Card 2 and 3
3. In 6.6 Card 2: The there is a B missing, please do not take the second difference (mistake on my part) and  
then choose the most favored by the aic from the choices that are given. 
4. FOR LIVE SESSION: In question 3 of the For Live Session Assignment, the models are missing.  Just skip that question and I will ask a similar question in Live Session.  
5. 6.6 Card 6 the model should have a -B^4. Also .. change the answer choices to be s = 4 rather than "Quarterly Seasonal Model."

6. 6.6 Card 7 ... there is an issue with the coefficients ... here are list of good coefficients you should use instead of the model on the card:  
7. 6.6 Card 3: "What do we know about the true stationary correlation structure of the model with the (1-B^12) removed?"  Take out the part about generating a realization. c(-0.0361, 0.0140, -0.1459, -0.0964, -0.1118, -0.0850, -0.0717, -0.0971, -0.1300, -0.1024, -0.0869, 0.8938, -0.0658, -0.0906, 0.0868)
8. 6.5 Card 2: Assume a graduate program has 3 semesters a year and thus each semeester is 4 months long.  Assume that the number of emails a professor recieves is farily constant for most of the semester but tend to peak around the time of finals. If the number of emails the professor receives is recorded monthly, "Which model would be most appropriate for modeling the number of emails received each month?" Answer: s = 4
