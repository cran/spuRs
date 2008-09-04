ufc <- read.csv("../data/ufc.csv")
l.reg <- function(x, y) {
    mu.x <- mean(x)
    b <- sum((x - mu.x)*y)/sum((x - mu.x)^2)
    a <- mean(y) - b*mu.x
    return(c(a, b))
}
postscript("../graphics/lattice_fig.ps", width=6, height=4)
xyplot(height.m ~ dbh.cm | species,
       data = ufc,
       subset = species %in% list("WC", "GF"),
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.abline(coef = l.reg(x, y))
       },
       xlab = "Diameter (cm)",
       ylab = "Height (m)"
)
dev.off()

