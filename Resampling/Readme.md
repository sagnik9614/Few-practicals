!!!!Instructions!!!!

1. Copy both the files (init.R and source.R) into a folder.
2. Open init.R .
3. Uncomment the first line by removing the hash.
4. inside the inverted commas, write the folder path where the two scripts are copied.
5. Run (for your life) the script.


!!!Description!!!

A) Description of init.R

Sources the source.R script, and calls the main function.
Try with different parameter values.




B) Description of source.R

Loads the required packages, if not installed, installs it, then loads.

1) generating(n ,B, mu, sigma, seed)

Parameters -----

n <- the size of the initial sample (in our question it is 5)
B <- the number of Bootstrap and large samples
mu <- mean parameter for generating normal samples
sigma <- sd parameter for generating normal samples
seed <- seed for random number generation (reproducibility of results)

Values -----

Returns a list of 4 objects, viz.

[[1]] The initial sample of size n
[[2]] A matrix containing the Jackknife samples, i.e. each row is a sample of size n-1
[[3]] A matrix containing the Bootstrap samples, i.e. each row is a bootstrap sample of size n, there are B rows
[[4]] A matrix containing B rows and n columns, where each row is a independent sample drawn from normal population with mean = mu, sd = sigma
	This is used for the large sample approximations

2) est(samp, n, B, p)

Parameters -----

samp <- a list of samples, that is returned from the generating function
n <- same as before
B <- same as before
p <- can be a vector or a scalar containing the quantile values at which to calculate the estimates


Values -----

Returns a tibble of estimated variances, 
with columns denoting p, Jakckinfe variance estimates, 
Bootstrap variance estimates, Large sample variance estimates.

There are as many rows as there are values of p.

3) main(n, B, mu, sigma, seed)

The main function takes parameters n, B, mu, sigma, seed

saves the following outputs.

The Output.txt file contains the output of the est function.

The Samples.txt file contains the output of the generating function

ComparativePlot.png is the plot for p vs Estimated variance for different mode of estimation
i.e. Jackknife, Bootstrap, or Large sample


Have fun!
