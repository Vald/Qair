`froid` <-
function(n=100, inverted=FALSE){hsv(seq(0.5, 0.66, length=100), seq(0.1, 1, length=100), 1)[round(seq(ifelse(!inverted, 1, 100), ifelse(!inverted, 100, 1), length=n))]}

