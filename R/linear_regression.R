#' @title LinearRegression class
#' @description LinearRegression class containing information about relevant results from the linear regression method.
#' @field formula A formula
#' @field data A data.frame
#' @field regressions_coefficients A vector
#' @field fitted_values A matrix
#' @field residuals A matrix
#' @field degrees_freedom A numeric
#' @field residual_variance A matrix
#' @field variance_regression_coefficients A matrix
#' @field t_values A vector
#' @export
linreg = setRefClass("linreg",
  fields = list(
    dependent_var = "character",
    formula = "formula",
    data = "character",
    regression_coefficients = "matrix",
    fitted_values = "matrix",
    residuals = "matrix",
    degrees_freedom = "numeric",
    residual_variance = "matrix",
    variance_regression_coefficients = "matrix",
    t_values = "matrix"
  ),
  methods = list(
    initialize=function(formula, data) {
      X <- model.matrix(formula, data) 
      y <- as.matrix(data[all.vars(formula)[1]])
      
      .self$dependent_var = all.vars(formula)[1]
      
      #Assign formula and data
      .self$formula <- formula
      .self$data <- deparse(substitute(data))
      
      #Regression coefficients
      .self$regression_coefficients <- solve(t(X) %*% X) %*% t(X) %*% y

      #Fitted values
      .self$fitted_values <- X %*% .self$regression_coefficients
      
      #Residuals
      .self$residuals <- y - .self$fitted_values
      
      #Degrees of freedom
      n <- nrow(X)
      p <- ncol(X)
      .self$degrees_freedom <- n-p
      
      #Residual variance
      .self$residual_variance <- (t(.self$residuals)%*%.self$residuals)/.self$degrees_freedom
      
      #Variance of regression coefficients
      .self$variance_regression_coefficients <- solve(t(X) %*% X) * .self$residual_variance[1]
      .self$t_values <- .self$regression_coefficients/sqrt(diag(abs(.self$variance_regression_coefficients)))
    },
    print = function(){
      "Prints input parameters and regression coefficients."
      cat("Call:\nlinreg(formula = ", format(.self$formula), ", data = ", .self$data, ")\n\n", sep = "")
      
      v_reg_coef.values = as.vector(.self$regression_coefficients)
      names(v_reg_coef.values) = rownames(.self$regression_coefficients)
      cat("Coefficients:\n")
      print_linreg(v_reg_coef.values)
    },
    plot = function(){
      "Plots the residuals ~ fitted values and scale-location."
      library(ggplot2)
      standardized_residuals = .self$residuals/sd(.self$residuals)
      
      p1 = 
        ggplot(
          data.frame(.self$residuals, .self$fitted_values),
          aes(x=.self$fitted_values, y=.self$residuals)
        ) + 
        geom_point() +
        geom_smooth(method="lm", col="red", se=FALSE) +
        xlab(paste("Fitted Values \n", "linreg(", format(.self$formula), ")")) +
        ylab("Residuals") +
        ggtitle("Residuals vs Fitted")

      print_linreg(p1)
      p2 = 
        ggplot(
          data.frame(standardized_residuals, .self$fitted_values),
          aes(x=.self$fitted_values, y=standardized_residuals)
        ) +
        geom_point() +
        geom_smooth(method="lm", col="red", se=FALSE) +
        xlab(paste("Fitted Values \n", format(.self$formula))) +
        ylab(expression(sqrt("Standardized Residuals"))) +
        ggtitle("Scale-Location")
      print_linreg(p2)
    },
    resid = function(){
      "Returns the residuals."
      return(.self$residuals)
    },
    #predicted values method
    pred = function(){
      "Returns the fitted values."
      return(.self$fitted_values)
    },
    #regression coefficients method
    coef = function(){
      "Returns the regression coefficients as a named vector."
      v_reg_coef.values = as.vector(.self$regression_coefficients)
      names(v_reg_coef.values) = rownames(.self$regression_coefficients)
      return(v_reg_coef.values)
    },
    #summary method
    summary = function(){
      "Summarizes values of regression."
      cat("Call:\nlinreg(formula = ", format(.self$formula), ", data = ", .self$data, ")\n\n", sep = "")
      cat("Coefficients:\n")
      
      p_values <- 2*pt(-abs(.self$t_values), df=.self$degrees_freedom)
      formatted_p_values <- format.pval(2*pt(-abs(.self$t_values), df=.self$degrees_freedom), digits=1)
      std_error_coef <- sqrt(diag(abs(.self$variance_regression_coefficients)))

      round_reg_coef = round(.self$regression_coefficients, 6)
      round_std_error_coef = round(std_error_coef, 6)
      round_t_values = round(.self$t_values, 4)
      stars = c(0, length=length(p_values))
      for (val in 1:length(p_values)) {
        if (p_values[val] < 0.001) {
          stars[val] = "***"
        }
        else if (p_values[val] < 0.01) {
          stars[val] = "**"
        }
        else if (p_values[val] < 0.05) {
          stars[val] = "*"
        }
        else if (p_values[val] < 0.1) {
          stars[val] = "."
        }
        else {
          stars[val] = " "
        }
      }
      table = data.frame(round_reg_coef, round_std_error_coef, round_t_values, formatted_p_values, stars)
      colnames(table) = c("Estimate", "Std. Error", "t value", "p value", " ")
      print.data.frame(table)

      cat("\nResidual standard error: ", as.character(sqrt(.self$residual_variance)), " on ", .self$degrees_freedom, " degrees of freedom", sep="")
    }
  )
)

#' @title print_linreg
#' @description our own print function for use inside linreg class
#' @param object_to_print object
#' @return None
#' @export
print_linreg = function(object_to_print) {
  print(object_to_print)
}

