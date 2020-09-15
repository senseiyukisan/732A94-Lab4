#' @title Linear Regression
#' @param formula formula
#' @param data data
#' @description 
#' Implementation of a linear regression model to calculate the relationship between a dependent variable
#' and N independent variables.
#' @return Instance of linreg1 class
#' @references \url{https://en.wikipedia.org/wiki/Linear_regression}
#' @export
linreg = function(formula, data) {
  X <- model.matrix(formula, data) 
  y <- as.matrix(data[all.vars(formula)[1]]) 

  #Regression coefficients
  Beta <- solve(t(X) %*% X) %*% t(X) %*% y
  print(Beta)
  
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
  # print(sigma2)
  # print(class <- class(Beta))
  # print(attributes <- attributes(Beta))
  # print(type_of <- typeof(Beta))
  
  #t-values for each coefficient
  # t <- Beta %*% (1/sqrt(abs(var_beta))) #wrong size of table, brute force with a for loop
  t <- rep(0, length(Beta))
  for (i in 1:length(Beta)){
    for (j in 1:length(Beta)){
      t[i] <- t[i] + Beta[i]/sqrt(abs(var_beta[i,j]))
      j <- j+1
    }
    i <- i + 1
  }
    
  #Return #I think the Class needs to be created before the function is
  return(linreg(formula <- formula,
                data <- data,
                regression_coefficients <- Beta,
                fitted_values <- fit_val,
                residuals <- e,
                degrees_freedom <- df,
                residual_variance <- sigma2,
                variance_regression_coefficicents <- var_beta,
                t_values <- t))
}


# Create object with fields and methods
linreg <- setRefClass("linreg",
                       fields = list(formula = "formula",
                                     data = "data.frame",
                                     regression_coefficients = "matrix",
                                     fitted_values = "matrix",
                                     residuals = "matrix",
                                     degrees_freedom = "numeric",
                                     residual_variance = "numeric",
                                     variance_regression_coefficients = "matrix",
                                     t_values = "vector"),
                       methods = list(
                         
                         print = function(){
                           cat(paste0("Formula: ", .self$formula, "data: ", .self$data, "\n"))
                           cat("Coefficients: \n")
                           print(.self$regression_coefficients)
                         },
                         plot <- function(){
                           
                          standardized_residuals <- sqrt(abs(.self$residuals - mean(.self$residuals))) 
                           
                           library(ggplot2)
                           p1 <- ggplot() + 
                                geom_point(mapping=aes(x = .self$fitted_values, y = .self$residuals)) +
                                geom_path(aes(x = .self$fitted_values, y = .self$residuals), color = red)
                                xlab(cat(paste0("Fitted Values \n", as.character(.self$formula))))
                                ylab("Residuals")
                                ggtitle("Residuals vs Fitted")
                                
                           p2 <- ggplot()+
                                geom_point(mapping=aes(x = .self$fitted_values, y = standardized_residuals))
                                geom_path(aes(x = .self$fitted_values, y = standardized_residuals), color = red)
                                xlab(cat(paste0("Fitted Values \n", as.character(.self$formula)))
                                ylab(expression(sqrt("Standardized Residuals")))
                                ggtitle("Scale-Location")
                                
                                
                                grid.arrange(p1, p2, nrow = 2) #Not sure about this one. It requires the gridExtra package.Uncertain how dependencies work
                         },
                         resid <- function(){
                           return(.self$residuals)
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
                         },
                         summary <- function(){
                           cat("Call: \n")
                           cat(paste0("linreg(formula =", as.character(.self$formula), ", data = ", as.character(.self$data),"\n"))
                           cat("Coefficients: \n")
                           # code a table here that is similar to summary(lm(Petal.Length~Species, data = iris))
                           # only coefficients (Beta), sqrt(variance_regression_coefficients), t value and p value
                           #rownames(table) <- rownames(var_beta)
                           #colnames(table) <- c("Estimate", "Std. Error", "t value", "p value")
                           
                           cat(paste0("Residual standard error: ", as.character(sqrt(.self$residual_variance)), " on", .self$degrees_freedom, " degrees of freedom"))
                         }
                       )
)
