library(np)
set.seed(123)
x <- rnorm(100)
u <- rnorm(100, mean=0, sd=0.4)
fx <- x + 2*exp(-16*x^2)
y <- fx+u
df <- data.frame(x,y)
index <- sample(1:100 ,80)
train.data <- df[index,]
colnames(train.data) <- c('train.x', 'y')
test.data <- df[-index, ]
colnames(test.data) <- c('train.x', 'y')
train.x <- train.data$train.x
train.y <- train.data$y
test.y <- test.data$y
func <- function(x) x + 2*exp(-16*x^2)

model.lm <- lm(train.y~train.x)
model.np <- npreg(train.y~train.x, regtype='lc', gradients=T)

newx <- data.frame(seq(-3, 3, length=100))
colnames(newx) <- c('train.x')
predict.lm <- predict(model.lm, newdata=newx)
predict.np <- predict(model.np, newdata=newx)

plot(x, y)
lines(x=newx$train.x, y=predict.np, col='red')
abline(model.lm, col='blue')
curve(func, add=T, from=-3, to=3)
legend('topleft', 
       legend=c('data', 'True f', 'Linear Regression', 'Nonparametric Regression'),
       bty='n', pch=c(1,NA,NA,NA), lty=c(NA,1,1,1), col=c('black', 'black', 'blue', 'red'))

mse.lm.train <- mean((train.y-predict(model.lm, newdata=train.data))^2)
mse.lm.test <- mean((test.data$y-predict(model.lm, newdata=test.data))^2)
mse.np.train <- mean((train.y-predict(model.np, newdata=train.data))^2)
mse.np.test <- mean((test.data$y-predict(model.np, newdata=test.data))^2)

