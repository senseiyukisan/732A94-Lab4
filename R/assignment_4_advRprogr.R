linreg = function(formula, data) {
  X <- model.matrix(formula, data) #What is this doing exactly?
  y <- as.matrix(data[all.vars(formula)[1]]) #this gives the right answer but I don't know why
  # y <- as.matrix(data[all.vars(formula)])
  # print(y)
  # print(class <- class(y))
  # print(attributes <- attributes(y))
  # print(type_of <- typeof(y))
  # 
  #Regression coefficients
  Beta <- solve(t(X) %*% X) %*% t(X) %*% y

  #Fitted values
  fit_val <- X %*% Beta

  #Residuals
  e <- y - fit_val

  #Degrees of freedom
  n <- nrow(X)
  p <- ncol(X)
  df <- n-p

  #Residual variance
  sigma2 <- (t(e)%*%e)/df
  
  # #variance of regression coefficients
  var_beta <- solve(t(X) %*% X) * sigma2[1]
  # print(Beta)
  # print(class <- class(Beta))
  # print(attributes <- attributes(Beta))
  # print(type_of <- typeof(Beta))

  #t-values for each coefficient
  t <- Beta %*% 1/sqrt(abs(var_beta)) #wrong size of table
 
  # #Return
  # return(linreg1(formula <- formula,
  #               data <- data,
  #               regression_coefficients <- Beta,
  #               fitted_values <- fit_val,
  #               residuals <- e,
  #               degrees_freedom <- df,
  #               residual_variance <- sigma2,
  #               variance_regression_coefficicents <- var_beta,
  #               t_values <- t))
}


# Create object with fields and methods
linreg1 <- setRefClass("linreg1",
                       fields = list(formula = "formula",
                                     data = "data.frame",
                                     regression_coefficients = "vector",
                                     fitted_values = "matrix",
                                     residuals = "matrix",
                                     degrees_freedom = "numeric",
                                     residual_variance = "numeric",
                                     variance_regression_coefficients = "matrix",
                                     t_values = "vector"),
                       methods = list(
                         
                         # print = function(){
                         #   cat(paste0("Formula: ", .self$formula, "data: ", .self$data, "\n"))
                         #   },
                         resid <- function(){
                           return(.self$residuals)
                         }
                         },
                         pred <- function(){
                           return(.self$fitted_values)
                         },
                         coef <- function(){
                           Beta_named <- .self$regression_coefficients
                           for(i in 1:length(Beta_named)){
                             names(Beta_named)[i] <- cat("Coefficient ", as.character(i))
                           }
                           return(Beta_named)
                         }
                       )
)

