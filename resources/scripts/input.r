# program spuRs/resources/scripts/input.r

# illustrates the readline command

finished = FALSE
while (!finished) {
    input <- readline("Give me a number between 0 and 9 please: ")
    x <- as.numeric(input)
    if ((x < 0) | (x > 9)) {
        cat("Try again!\n")
    } else {
        finished = TRUE
    }
}
cat("You entered", x, "\n")
