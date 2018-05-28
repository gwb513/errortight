# CacheExample

This example takes advantage of R's lexical scoping and function closures for downloading and storing stock data. 
The main function returns a list of callable functions, each having access to the original environment (closure) in which they were created.
The closure in this case keeps the global environment tidy by keeping the stock data within the function closure.
The code also prevents re-downloading of existing data to save on API calls.

A similar result could be obtained with an objected-oriented approach, which looks similar to the lexical scoping approach. 
Having some familiarity with scoping rules and environments could be beneficial for future tasks.

https://www.r-bloggers.com/closures-in-r-a-useful-abstraction/

https://stackoverflow.com/questions/33738820/r-programming-cache-the-inverse-of-a-matrix

