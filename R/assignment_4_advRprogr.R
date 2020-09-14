# Create object with fields and methods
linreg <- setRefClass("linreg",
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
                         init = function(formula, data) {
                           X <- model.matrix(formula, data)
                           y <- all.vars(formula[1])
                           linreg$formula <- formula
                           linreg$data.frame <- data

                           #Regression coefficients
                           Beta <- solve(t(X) %*% X) %*% t(X) %*% y
                           linreg$regression_coefficients <- Beta
                           
                           #Fitted values
                           fit_val <- X %*% Beta
                           linreg$fitted_values <- fit_val
                           
                           #Residuals
                           e <- y - fit_val
                           linreg$residuals <- e
                           
                           #Degrees of freedom
                           n <- nrow(X)
                           p <- ncol(X)
                           df <- n-p
                           linreg$degrees_freedom <- df
                           
                           #Residual variance
                           sigma2 <- (t(e)%*%e)/df
                           linreg$residual_variance <- sigma2
                           
                           #variance of regression coefficients
                           var_beta <- sigma2 * solve(t(X) %*% X)
                           linreg$variance_regression_coefficicents <- var_beta
                           
                           #t-values for each coefficient
                           t <- Beta/sqrt(var_beta)
                           linreg$t_values <- t
                           
                           #p-values
                           p_values <- pt(Beta)
                         },
                         # },
                         # print = function(){
                         #   cat(paste0("Formula: ", .self$formula, "data: ", .self$data, "\n"))
                         #   },
                         resid <- function(){
                           return(e)
                         }
                         # },
                         # pred <- function(){
                         #   return(fit_val)
                         # },
                         # coef <- function(){
                         #   Beta_named <- Beta
                         #   for(i in 1:length(Beta_named)){
                         #     names(Beta_named)[i] <- cat("Coefficient ", as.character(i))
                         #   }
                         #   return(Beta_named)
                         # }
                       )
)