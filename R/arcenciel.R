`arcenciel` <-
function(n=101, inverted=FALSE){c(hsv(0.5, s=15:40/40, v=50:25/50), hsv(0.34, s=40:16/40 ,v=26:50/50), hsv(0.17, s=16:40/40, v=50:26/50), hsv(0, s=40:16/40, v=26:50/50))[round(seq(ifelse(!inverted, 1, 101), ifelse(!inverted, 101, 1), length=n))]}

