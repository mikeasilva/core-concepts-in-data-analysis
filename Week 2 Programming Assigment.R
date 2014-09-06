# Read in Iris data
d <- read.csv('~/dataset_374_1.txt', header=F, sep ="\t")

# Define the number of bins
nbins <- 10

# Get the step size
dmax <- max(d$V1)
dmin <- min(d$V1)
step <- (dmax-dmin)/nbins

# Get the break points
t <- 0:(nbins -1)
x <- dmin + (t*step)
b <- c(x,dmax)

# Get the Histogram
h <- hist(d$V1, breaks = b)

# Display the frequencies by bin
h$count