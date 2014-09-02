
setwd("~/Workspace/MyMachineLearningforHackers/02-Exploration")

data.file <- file.path('data', '01_heights_weights_genders.csv')
heights.weights <- read.csv(data.file, header = TRUE, sep = ',')

heights <- with(heights.weights, Height)
summary(heights)
mean(heights)
var(heights)
  
library(ggplot2)
ggplot(heights.weights, aes(x = Height)) + geom_histogram(binwidth = 1)
ggplot(heights.weights, aes(x = Height)) + geom_histogram(binwidth = 5)
ggplot(heights.weights, aes(x = Height)) + geom_histogram(binwidth = 0.01)
ggplot(heights.weights, aes(x = Height)) + geom_density()

ggplot(heights.weights, aes(x = Height, fill = Gender)) + geom_density()
ggplot(heights.weights, aes(x = Weight, fill = Gender)) + geom_density()
ggplot(heights.weights, aes(x = Weight, fill = Gender)) + geom_density() + facet_grid(Gender ~ .)

m <- 0
s <- 1
ggplot(data.frame(X = rnorm(100000, m, s)), aes(x = X)) + geom_density()

m <- 1
s <- 3
ggplot(data.frame(X = rnorm(100000, m, s)), aes(x = X)) + geom_density()

m <- 0
s <- 5
ggplot(data.frame(X = rnorm(100000, m, s)), aes(x = X)) + geom_density()

set.seed(1)
normal.values <- rnorm(250, 0, 1)
cauchy.values <- rcauchy(250, 0, 1)
range(normal.values)
range(cauchy.values)
ggplot(data.frame(X = normal.values), aes(x = X)) + geom_density()
ggplot(data.frame(X = cauchy.values), aes(x = X)) + geom_density()

gamma.values <- rgamma(100000, 1, 0.001)
ggplot(data.frame(X = gamma.values), aes(x = X)) + geom_density()

exp.values <- rexp(100000, 1)
ggplot(data.frame(X = exp.values), aes(x = X)) + geom_density()

ggplot(heights.weights, aes(x = Height, y = Weight)) + geom_point()
ggplot(heights.weights, aes(x = Height, y = Weight)) + geom_point() + geom_smooth()
ggplot(heights.weights[1:20, ], aes(x = Height, y = Weight)) + geom_point() + geom_smooth()
ggplot(heights.weights[1:200, ], aes(x = Height, y = Weight)) + geom_point() + geom_smooth()
ggplot(heights.weights[1:2000, ], aes(x = Height, y = Weight)) + geom_point() + geom_smooth()

ggplot(heights.weights, aes(x = Height, y = Weight, color = Gender)) + geom_point()

# How to draw the line of classification hyperplane?
heights.weights <- transform(heights.weights, Male = ifelse(Gender == 'Male', 1, 0))
logit.model <- glm(Male ~ Height + Weight, data = heights.weights, family = binomial(link = 'logit'))
ggplot(heights.weights, aes(x = Height, y = Weight, color = Gender)) + geom_point() + 
  stat_abline(intercept = -coef(logit.model)[1] / coef(logit.model)[2],
              slope = - coef(logit.model)[3] / coef(logit.model)[2],
              geom = 'abline', color = 'blue')
