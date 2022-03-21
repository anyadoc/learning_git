##Aniruddha Belsare; github.com/anyadoc
library(deSolve)
library(ggplot2)
library(plotly)

## Create an SIR function

sir <- function(time, y, parameters) {   #time, state , parameters
  
  with(as.list(c(y, parameters)), {
    
    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <-                 gamma * I
    dN <- dS + dI + dR
    
    return(list(c(dS, dI, dR, dN)))
  })
}

## Set parameters
## Proportion in each compartment
init <- c(S = 0.99, I = 0.01, R = 0, N = 1)
## beta: infection parameter; gamma: recovery parameter
#b=.0952381 g = 0.04761905
parameters <- c(beta = 0.0952381, gamma = 0.04761905)
## Time frame
times      <- seq(0, 300, by = 1)

## Solve using ode (General Solver for Ordinary Differential Equations)
out <- ode(y = init, times = times, func = sir, parms = parameters)
## change to data frame
out <- as.data.frame(out)
## Delete time variable
#out$time <- NULL
## Show data
head(out, 10)
write.csv(x=out, file="out")

plot <- ggplot () +
  geom_line(data = out, aes(x = time, y = S), color = "blue") +
  geom_line(data = out, aes(x = time, y = I), color = "red") +
  geom_line(data = out, aes(x = time, y = R), color = "brown") +
  geom_line(data = out, aes(x = time, y = N), color = "black") + 
  xlab("time") +
  ylab("Proportion of hosts") +
  annotate(geom="text", x=50, y=0.75, label="S",color="black") +
  annotate(geom="text", x=50, y=0.95, label="N",color="black") +
  annotate(geom="text", x=80, y=0.18, label="I",color="black") +
  annotate(geom="text", x=80, y=0.28, label="R",color="black") +
  theme_bw()

ggplotly(plot)
