# Model Building!!!

## Suggested Reading

## Erata

11.3.1
There is no code.  Here is the code:

data(airlog) # load from tswge package
airlog1 = artrans.wge(airlog,phi.tr=1)
airlog1.12 = artrans.wge(airlog1,phi.tr = c(rep(0,11),1))
ww = est.ar.wge(airlog1.12,p = 12)

11.3.2

To run the Llung-Box test, use K = 24
You should get a pvalue of .01645 # this is the correct answer but the computer has a different answer.  

The computer will want .4969

11.3.3

The computer will want "YES" but the answer is actually "NO" since the pvalue in the last questions changed from above .05 to below .05.  (FTR Ho to Reject Ho).  


11.6.2
The model specification in the video is listed as .33 instead of 1.33 (missing the 1.)

