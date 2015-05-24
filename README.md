Process
=========

1. We first create file copies in order to manipulate them without losing the original data.
2. Then we have erased double spaces in copied data, to have a uniformly separated data
3. We read and merge train and test data
4. We extract required columns (those ended in "mean()" or "std()")
5. We renamed activities
6. We renamed variables
7. We store the means of variables per subject and activities in a dataframe
8. we store it in a file called "result"