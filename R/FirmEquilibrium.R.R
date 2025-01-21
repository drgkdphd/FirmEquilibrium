
#' Analyze Firm Equilibrium
#'
#' This function calculates the equilibrium quantity, revenue, cost, and other metrics for a firm based on given total revenue (TR) and total cost (TC) functions.
#'
#' @param TR An expression for the total revenue function as a function of quantity (Q).
#' @param TC An expression for the total cost function as a function of quantity (Q).
#' @return A list containing equilibrium quantity, revenue, cost, and other related metrics.
#' @examples
#' library(Deriv)
#' library(ggplot2)
#' library(ggthemes)
#' TR <- expression(100 * Q - 8 * Q^2)
#' TC <- expression(50 + 20 * Q)
#' results <- FirmEquilibrium(TR, TC)
#' print(results)
#' @export
FirmEquilibrium <- function(TR, TC) {
# Calculate the first order derivatives
TR_q <- Deriv(TR, "Q")
TC_q <- Deriv(TC, "Q")

# Calculate the second order derivatives
TR_qq <- Deriv(TR_q, "Q")
TC_qq <- Deriv(TC_q, "Q")

  # Derive the marginal revenue function
  MR_expr <- Deriv(TR, "Q")
    # Derive the marginal cost function
  MC_expr <- Deriv(TC, "Q")
    # Define the MR and MC functions
  MR <- function(Q){eval(MR_expr, envir = list(Q = Q))}
  MC <- function(Q){eval(MC_expr, envir = list(Q = Q))}
   
 # Calculate Equilibrium Quantity
  Q_1 <- uniroot(function(Q) MR(Q) - MC(Q), c(0, 100))$root
  # Calculate at Equilibrium Quantity

# Evaluate the second-order derivatives at Q_1
TR_qqv <- eval(TR_qq, envir = list(Q = Q_1))
TC_qqv <- eval(TC_qq, envir = list(Q = Q_1))

# Check the second-order condition for maximization
if (TR_qqv < 0) {
  print("The second-order condition for maximization is satisfied.")
} else {
  print("The second-order condition for maximization is not satisfied.")
}
  TR_1 <- eval(TR, envir = list(Q = Q_1))
  TC_1 <- eval(TC, envir = list(Q = Q_1))
  MR_1 <- MR(Q_1)
  MC_1 <- MC(Q_1)
  AR_1 <- TR_1/Q_1
  AC_1 <- TC_1/Q_1
  profit_1 <- TR_1 - TC_1
 
 # Print the results
  print(paste("The quantity that maximizes profit is:", round(Q_1,2)))
  print(paste("The TR at the quantity that maximizes profit is:", round(TR_1,2)))
  print(paste("The TC at the quantity that maximizes profit is:", round(TC_1,2)))
  print(paste("The MR at the quantity that maximizes profit is:", round(MR_1,2)))
  print(paste("The MC at the quantity that maximizes profit is:", round(MC_1,2)))
  print(paste("The AR/Price at the quantity that maximizes profit is:", round(AR_1,2)))
  print(paste("The AC at the quantity that maximizes profit is:", round(AC_1,2)))
  print(paste("The maximum profit is:", round(profit_1,2)))
    
# Create a sequence of quantities
  Q <- seq(0,Q_1*1.5, by = 1)
    # Calculate the values for each curve
  TR_values <- sapply(Q, function(x) eval(TR, envir = list(Q = x)))
  TC_values <- sapply(Q, function(x) eval(TC, envir = list(Q = x)))
  MR_values <- sapply(Q, function(x) MR(x))
  MC_values <- sapply(Q, function(x) MC(x))
  AR_values <- TR_values/Q
  AC_values <- TC_values/Q
  profit_values <- TR_values - TC_values
  
  # Create a data frame
  df <- data.frame(Q, TR = TR_values, TC = TC_values, AR = AR_values, AC = AC_values, Profit = profit_values, MR = MR_values, MC = MC_values)
  
  #1. Plot the curves

print(
  ggplot(df, aes(x = Q)) + 
    geom_line(aes(y = TR, color = "TR")) + 
    geom_line(aes(y = TC, color = "TC")) + 
    geom_line(aes(y = MR, color = "MR")) + 
    geom_line(aes(y = MC, color = "MC")) + 
    geom_line(aes(y = AR, color = "AR")) + 
    geom_line(aes(y = AC, color = "AC")) + 
    geom_line(aes(y = Profit, color = "Profit")) + 
    geom_vline(xintercept = Q_1, color = "red", linetype = "dashed") +
    labs(title = "TR, TC, AR, AC, MR, MC, and Profit Curves", x = "Quantity", y = "Value") +
    scale_color_manual(values = c("TR" = "blue", "TC" = "red", "MR" = "green", "MC" = "purple", "AR" = "orange", "AC" = "brown", "Profit" = "darkgreen")) +
    theme_economist()
)

# 2 Plot the curves
print(
ggplot(df, aes(x = Q)) +
 geom_line(aes(y = TR, color = "TR")) +
 geom_line(aes(y = TC, color = "TC")) +
 geom_line(aes(y = Profit, color = "Profit")) +
 geom_vline(xintercept = Q_1, color = "red", linetype = "dashed") +
 labs(title = "TR, TC, and Profit Curves", x = "Quantity", y = "Value") +
 scale_color_manual(values = c("TR" = "blue", "TC" = "red",  "Profit" = "darkgreen")) + theme_economist()
)

# 3 Plot the curves
print(
ggplot(df, aes(x = Q)) +
 geom_line(aes(y = MR, color = "MR")) +
 geom_line(aes(y = MC, color = "MC")) +
 geom_line(aes(y = Profit, color = "Profit")) +
 geom_vline(xintercept = Q_1, color = "red", linetype = "dashed") +
 labs(title = "MR, MC, and Profit Curves", x = "Quantity", y = "Value") +
 scale_color_manual(values = c("MR" = "blue", "MC" = "red",  "Profit" = "darkgreen")) + theme_economist()
)

# Create matrices to store the results
equilibrium_results <- matrix(c(
round(Q_1, 2), 
round(TR_1, 2),
round(TC_1, 2), 
round(MR_1, 2),
round(MC_1, 2),
round(AR_1, 2),
round(AC_1, 2),
round(profit_1,2)), 
nrow = 8, ncol = 1, 
dimnames = list(c(
"Equilibrium Quantity_1=", 
"Total Revenue at Equilibrium Quantity  =",
"Total Cost at Equilibrium Quantity  =",
"Marginal Revenue at Equilibrium Quantity  =",
"Marginal Cost at Equilibrium Quantity  =",
"Average Revenue(or)Price at Equilibrium Quantity  =",
"Average Cost at Equilibrium Quantity  =",
"Maximum Profit at Equilibrium Quantity  =" ), c("Value")))
# Print the results matrices
     print(equilibrium_results)

# Return results
  return(list(
Equilibrium_Quantity= round(Q_1, 2),
Total_Revenue_at_Equilibrium_Quantity = round(TR_1, 2),
Total_Cost_at_Equilibrium_Quantity = round(TC_1, 2),
Marginal_Revenue_at_Equilibrium_Quantity = round(MR_1, 2),
Marginal_Cost_at_Equilibrium_Quantity = round(MC_1, 2),
Average_Revenue_Price_at_Equilibrium_Quantity = round(AR_1, 2),
Average_Cost_at_Equilibrium_Quantity = round(AC_1, 2),
Maximum_Profit_at_Equilibrium_Quantity = round(profit_1, 2)
))
}


