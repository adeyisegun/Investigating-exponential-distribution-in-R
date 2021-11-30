sim <- 1000
n <- 40
lambda <- 0.2
sim_data <- matrix(rexp(n*sim,lambda), sim, n)


sample_mean <- apply(sim_data, 1, mean)
sim_mean <- mean(sample_mean) 
theoretical_mean <- 1/lambda

  result1 <-data.frame("Mean"=c(sim_mean,theoretical_mean), 
                       row.names = c("Sample Mean","Theoretical mean"))
  result1

library(ggplot2)
    g = ggplot(data.frame(sample_mean=sample_mean), aes(x = sample_mean))
    g = g + geom_histogram(color = "black", fill = "grey", binwidth = 0.3)
    g = g + geom_vline(aes(xintercept = theoretical_mean,
                           colour="theoretical_center"))
    g = g + geom_vline(aes(xintercept = sim_mean,colour="sim_center"))
    g = g +  scale_color_manual(name = "Distribution", 
                                values = c(theoretical_center = "blue", 
                                           sim_center = "red"))
    g = g +ggtitle ("Fig 1. Showing histogram of the sample means ")+xlab("Sample mean")+ylab("Density")
    g


sample_variance <- (apply(sim_data, 1, sd))^2
sim_variance <- mean(sample_variance)
theoretical_variance <- (1/lambda)^2

  result2 <-data.frame("Variance"=c(sim_variance,theoretical_variance), 
                       row.names = c("Sample Variance","Theoretical Variance"))
  result2

    g = ggplot(data.frame(sample_variance=sample_variance), aes(x = sample_variance))
    g = g + geom_histogram(color = "black", fill = "grey", binwidth = 2)
    g = g + geom_vline(aes(xintercept = theoretical_variance,colour="theoretical_variance"))
    g = g + geom_vline(aes(xintercept = sim_variance,colour="simulated_variance"))
    g = g +  scale_color_manual(name = "Variance", 
                                values = c(theoretical_variance = "blue", 
                                           simulated_variance = "red"))
    g = g +ggtitle ("Fig 2. Showing histogram of the sample variance")+xlab("Sample variance")+ylab("Density")
    g


qqnorm(sample_mean, main ="Normal probability plot")
qqline(sample_mean,col = "3")

h <- hist(sample_mean, breaks = 20, density = 10,
          col = "lightgray", xlab = "Sample Mean", main = "Normal distribution plot") 
xfit <- seq(min(sample_mean), max(sample_mean), length = 40) 
yfit <- dnorm(xfit, mean = mean(sample_mean), sd = sd(sample_mean)) 
yfit <- yfit * diff(h$mids[1:2]) * length(sample_mean) 

lines(xfit, yfit, col = "black", lwd = 2)


##Both histogram and the normal probability plot show that distribution of averages is approximately normal.




##Load the ToothGrowth data
library(datasets)
data(ToothGrowth)

##perform some basic exploratory data analyses
require(graphics)
coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,
        xlab = "ToothGrowth data: length vs dose, given type of supplement")

##Provide a basic summary of the data
head(ToothGrowth)
summary(ToothGrowth)


library(dplyr)
data_tooth <- ToothGrowth %>% group_by(supp, dose) %>% summarise_all(list(mean)) %>% arrange(len)


# data for dose
data_tooth_0.5 = ToothGrowth %>% filter(dose==0.5)
data_tooth_1.0 = ToothGrowth %>% filter(dose==1.0)
data_tooth_2.0 = ToothGrowth %>% filter(dose==2.0)


#There are two hypotheses to be tested:
#- There is no relation between the supplement and the length of the tooth
#- There is no relation between the dose and the length of the tooth


#Relation between the supplement and the length of the tooth:
x1 <- t.test(len ~ supp, data = ToothGrowth)


x2 <- t.test(len ~ dose, data = rbind(data_tooth_0.5,data_tooth_1.0))
x3 <- t.test(len ~ dose, data = rbind(data_tooth_1.0,data_tooth_2.0))
x4 <- t.test(len ~ dose, data = rbind(data_tooth_0.5,data_tooth_2.0))


data.frame(
  "p-value" = c(x1$p.value, x2$p.value, x3$p.value, x4$p.value),
  "Conf.Int_Low" = c(x1$conf.int[1],x2$conf.int[1],x3$conf.int[1],x4$conf.int[1]),
  "Conf.Int_High" = c(x1$conf.int[2],x2$conf.int[2],x3$conf.int[2],x4$conf.int[2]),
  row.names = c("len ~ supp","len ~ dose(0.5&1)","len ~ dose(1.0&2.0)","len ~ dose(0.5&2.0)")
)

