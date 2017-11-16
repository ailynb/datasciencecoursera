
# --------------------------------------------------------------------------------------------------
# CLEAN OBJECTS FROM WORKSPACE
# --------------------------------------------------------------------------------------------------
rm(list=ls())

# --------------------------------------------------------------------------------------------------
# WORKING DIRECTORY
# --------------------------------------------------------------------------------------------------
setwd("C:/Users/ABC/Cursos/Data Scientists - Johns Hopkins University/WorkingDirectory_Curso/datasciencecoursera_repository/Course 6")


# --------------------------------------------------------------------------------------------------
# EXAMPLE PROBABILITY DENSITY FUNCTION
# f(x)= 2x for 0<x<1
# f(x)= 0  for otherwise
# --------------------------------------------------------------------------------------------------
x <- c(-0.5,0,1,1,1.5)
y <- c(0,0,2,0,0)
plot(x,y, lwd=3, frame=FALSE, type="l")
# - Clacule its area( right triangle): area = 1 por tanto es una funcion valida de densidad
area <- 1 * 2 /2
# - What is the probability that 75% or fewer of calls get addressed?
abline(v=0.75, lwd=3, type="l", col="green")
area <- 1.5 * 0.75 / 2
# -- Other form to calculate
pbeta(.75, 2, 1)
# - What are the survival function and CDF from the density considered before? For 1>= x >= 0
# -- CDF: F(x)=P(X<=x)=1/2 base x height = 1/2 x * 2 x = x^2
# -- Survival function: S(x)= 1- F(x) = 1- x^2
pbeta(c(.4,.5,.6),2,1)
# - Calcule mean
# -- x=.5 F(x)=x^2
sqrt(.5) 
# --- This means is that on about 50% of the days, 70% of the phone calls, or fewer get answered.
# -- Other form to calculate
qbeta(.5, 2, 1)



# --------------------------------------------------------------------------------------------------
# EXPECTED VALUES
# --------------------------------------------------------------------------------------------------
library(manipulate)
library(ggplot2)

myHist <- function(mu){
    mse <- round(mean((galton$child - mu)^2),3)    
    
    g <- ggplot(galton, aes(x=child)) +
        geom_histogram(fill="salmon", binwidth = 1, aes(y= ..density..), colour="black") +
        geom_density(size=2)+
        geom_vline(xintercept = mu, size=2) +
        labs(title=paste("mu = ",mu, " MSE =", mse))
    
    return(g)   
}
manipulate(myHist(mu), mu=slider(62,74,step=.5))














