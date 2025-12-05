install.packages("DoE.base")
install.packages("ggplot2")
library(DoE.base)
library(ggplot2)
OA <- oa.design(ID = "L9.3.4")   # Standard L9 Orthogonal Array
OA$A <- factor(c(10,10,10,15,15,15,20,20,20))
OA$B <- factor(c(10,15,20,10,15,20,10,15,20))

strength_7  <- c(4.3, 7.0, 5.5, 7.9, 6.7, 8.8, 8.2, 9.6, 4.3)
strength_14 <- c(6.3, 8.4, 9.2, 12.8, 10.6, 11.9, 13.2, 15.6, 16.3)
strength_28 <- c(11.8, 17.5, 21.9, 28.5, 27.9, 30.9, 31.7, 34.0, 36.1)

SN_LTB <- function(y) { -10 * log10(mean(1 / (y^2))) }

data <- data.frame(
  A = OA$A,
  B = OA$B,
  S7  = strength_7,
  S14 = strength_14,
  S28 = strength_28,
  SN7  = -10 * log10(1 / (strength_7^2)),
  SN14 = -10 * log10(1 / (strength_14^2)),
  SN28 = -10 * log10(1 / (strength_28^2))
)

print(data)

ME7 <- MEPlot(data, "SN7", c("A","B"))

ME14 <- MEPlot(data, "SN14", c("A","B"))

ME28 <- MEPlot(data, "SN28", c("A","B"))
