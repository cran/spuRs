# program spuRs/resources/scripts/freqcount.r

# calculate a frequency table

# clear the workspace
rm(list=ls())

# inputs
input_file = "data3.txt"
output_file = "data3freq.txt"

# read input_file, assumed to be integers separated by spaces and/or new lines
data <- scan(input_file)

# calculate frequencies
data.min <- min(data)
data.max <- max(data)
# counts[j] is the number of occurrences of the value j + data.min - 1
counts = rep(0, data.max - data.min + 1)
for (i in 1:length(data)) {
    j <- data[i] - data.min + 1
    counts[j] <- counts[j] + 1
    cat("data[i] =", data[i], "\n")
    cat("j       =", j, "\n")
    cat("counts  =", counts, "\n")
}

# write output_file
output <- file(output_file, open = "w")
cat("value : frequency\n", file = output)
for (i in 1:length(counts)) {
    if (counts[i] != 0) {
        cat(i + data.min - 1, ":", counts[i], "\n", file = output)
    }
}
close(output)
