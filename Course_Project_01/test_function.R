
library(ggplot2)

set.seed(1986)
exponentials <- 40
lambda <- 0.2
sims <- 1000    # number of simulations to run

# create a blank data frame for the simulated data
simulated_Data <- data.frame(ncol=2, nrow=sims)
names(simulated_Data) = c("simulation number", "simulated mean")

for(i in 1:sims) {
  simulated_Data[i,1] <- i
  simulated_Data[i,2] <- mean(rexp(exponentials, lambda))
  #simulated_Data[i,3] <- mean(simulated_Data[i,2])
}

sim_Mean <- mean(simulated_Data[,2])
test_Mean <- 1/lambda
diff_Mean <- test_Mean - sim_Mean

sim_Variance <- var(simulated_Data[2])
test_Variance <- ((1/lambda)^2)/exponentials
diff_Variance <- test_Variance - sim_Variance

sim_plot <- ggplot(data = simulated_Data, aes(simulated_Data$`simulated mean`)) +
            geom_histogram(aes(y = ..density..),bins = 50, col = "black", fill = "grey", alpha = 0.5) +
            labs(title = "Histogram of Simulated Mean Values", x = "Simulated Mean Value", y = "Density") +
            stat_function(fun = dnorm, args = list(mean = test_Mean, sd = sqrt(test_Variance)), size = 1, col = "red") +
            stat_function(fun = dnorm, args = list(mean = sim_Mean, sd = sqrt(sim_Variance)), size = 1, col = "blue")
plot(sim_plot)
