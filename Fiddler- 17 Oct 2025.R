# https://thefiddler.substack.com/p/can-you-reach-the-edge-of-the-square

n <- 10^7

AB <- 1/2
angles <- runif(n, min = 0, max = 45)
radians <- angles * (pi / 180)

AC <- AB / cos(radians)

soln <- sum(AC) / n
sprintf("%.4f", soln)

# 0.5611


a <- max(AC)
b <- sqrt(AB^2 + AB^2)
sprintf("%.4f", a)
sprintf("%.4f", b)

# https://thefiddler.substack.com/p/will-the-odds-be-ever-in-your-favor