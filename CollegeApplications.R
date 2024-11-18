# Predicting number of received college applications from ISLR2 package
library(ISLR2)
library(dplyr)
library(leaps)

summary(College)
str(College)
dim(College)

################################################################################

# least squares
lm <- lm(Apps ~ ., data = College)
summary(lm)

# write predict function
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[ , xvars] %*% coefi
}

# 10-fold CV for variable selection
set.seed(10)
k <- 10
n <- nrow(College)
folds <- sample(rep(1:k, length = n))
# I'm choosing d = 16 to exclude the intercept I think?
cv.errors <- matrix(NA, k, 17, dimnames = list(NULL, paste(1:17)))

# CV loop
# should get a 10 x 16 matrix of which the (j,i) element corresponds to the
#test MSE for the jth cv fold for the best i-variable model
for (j in 1:k) {
  best.fit <- regsubsets(Apps ~ ., data = College[folds != j, ], nvmax = 18)
  for (i in 1:17) {
    pred <- predict(best.fit, College[folds == j, ], id = i)
    cv.errors[j, i] <- mean((College$Apps[folds == j] - pred)^2)
  }
}

# Average the columns in the matrix and plot # of variable vs. cv.errors
(meanCV.errors <- apply(cv.errors, 2, mean))
par(mfrow = c(1, 1))
plot(meanCV.errors, type = 'b') # CV selects the 11 variable model(MSE = 1178893)
which.min(meanCV.errors) 
# Perform best subset on the FULL data set
bestLinReg <- regsubsets(Apps ~ ., data = College, nvmax = 18)
coef(bestLinReg, 11) # these are the coefficients for the best linear model

# Using best subset selection our best linear model for predicting Applications
LinearModel <- lm(Apps ~ Private + Enroll + Accept + Top10perc +
                    Top25perc + F.Undergrad + Outstate + Room.Board +
                    PhD + Grad.Rate + Expend, data = College)
summary(LinearModel)

# Accept = number of applicants accepted
# Top10perc = percent of students from top 10% of HS class
# Outstate = out of state tuition
# Expend = instructional expenditure per student

# To make prediction with your new linear model
# Here's my fake 2024 Salisbury dataset (no way tuition is this cheap anymore)
Salisbury2024 <- data.frame(Accept = 3000, Top10perc = 20,
                            Outstate = 10000, Expend = 6000, ) # etc.plug in all values

predict(LinearModel, Salisbury2024) # predicts ~4100 students will apply in 2024



################################################################################

#Ridge Regression 
library(glmnet)

# split data
set.seed(10)
x <- model.matrix(Apps ~., data = College)
y <- College$Apps
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

ridge.mod <- glmnet(x[train, ], y[train], alpha = 0, lambda = seq(350,500))
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
(bestLam <- cv.out$lambda.min) # = 411.4
ridgePred <- predict(ridge.mod, s = bestLam, newx = x[test, ])
mean((ridgePred - y.test)^2) 
# MSE of 985467.9 is lower than least squares model
out <- glmnet(x, y, alpha = 0)
(ridge.coef <- predict(out, type = "coefficients", s = bestLam)[1:18,])

################################################################################

# Lasso
library(glmnet)

# split data
set.seed(10)
x <- model.matrix(Apps ~., data = College)
y <- College$Apps
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = seq(0, 50))
plot(lasso.mod)

cv.out <- cv.glmnet(x[train, ], y[train], alpha =1)
plot(cv.out)

(bestLamL <- cv.out$lambda.min) # = 32.6
lasso.pred <- predict(lasso.mod, s = bestLamL, newx = x[test, ])
mean((lasso.pred - y.test)^2) # 1004415 MSE not lower than ridge regression

# The MSE is comparable to ridge regression, but the lasso has an advantage in
#that the resulting coefficients estimates are sparse --> smaller simpler model
out <- glmnet(x, y, alpha = 1, lambda = seq(0,50))
(lasso.coef <- predict(out, type = "coefficients", s = bestLamL)[1:18,])

################################################################################

# Principal components regression
library(pls)

# split data
set.seed(10)
n <- length(Apps)
Z <- sample(n, n/2)

# Fit PCR; scale = TRUE to standardize, 10-fold CV
pcr.mod <- pcr(Apps ~ ., data = College, scale = TRUE, validation = "CV")
summary(pcr.mod) # these are the square-rooted MSE
validationplot(pcr.mod, val.type = "MSEP") # optimal M seems to be at 17 comps
# which is equivalent to the full least squares model
# compute test MSE
pcr.mod <- pcr(Apps ~ ., data = College[Z,], ncomp = 17, scale = TRUE)
Yhat <- predict(pcr.mod, data.frame(College[-Z, ]))
mean((College$Apps[-Z] - Yhat)^2)
# highest MSE so far!
################################################################################


# Ridge regression provided the best model (lowest test MSE)!











