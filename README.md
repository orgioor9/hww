# hww
setwd("C:/Users/Lenovo/Downloads")
read.csv("ac.csv")

calculate_stats <- function(x, y) {
  model <- lm(y ~ x)
  return(paste0("y = ", round(model$coefficients[1], 2), 
                " + ", round(model$coefficients[2], 2), "x"))
}

par(mar=c(4, 4, 2, 2), cex.lab=1.2, cex.axis=1.2)
par(mfrow=c(2,2))
plot(data$FOOD, data$LIMPET_A, xlab="FOOD", ylab="LIMPET_A", 
     main="LIMPET_A vs. FOOD")
model <- lm(data$LIMPET_A ~ data$FOOD)
abline(model, col="red")

# Plot LIMPET_A vs. OTH_LIMPETS
plot(data$OTH_LIMPETS, data$LIMPET_A, xlab="OTH_LIMPETS", ylab="LIMPET_A", 
     main="LIMPET_A vs. OTH_LIMPETS")
model <- lm(data$LIMPET_A ~ data$OTH_LIMPETS)
abline(model, col="red")

# Plot LIMPET_A vs. TIDE_HT
plot(data$TIDE_HT, data$LIMPET_A, xlab="TIDE_HT", ylab="LIMPET_A", 
     main="LIMPET_A vs. TIDE_HT")
model <- lm(data$LIMPET_A ~ data$TIDE_HT)
abline(model, col="red")

# Plot LIMPET_A vs. PREDS
plot(data$PREDS, data$LIMPET_A, xlab="PREDS", ylab="LIMPET_A", 
     main="LIMPET_A vs. PREDS")
model <- lm(data$LIMPET_A ~ data$PREDS)
abline(model, col="red")


# Calculate the slope and R^2 values for each reg
data <- read.csv("ac.csv")

calculate_stats <- function(x, y) {
  model <- lm(y ~ x)
  r_squared <- summary(model)$r.squared
  slope <- coef(model)[2]
  return(list(slope=slope, r_squared=r_squared))
}

food_stats <- calculate_stats(data$FOOD, data$LIMPET_A)
oth_limpets_stats <- calculate_stats(data$OTH_LIMPETS, data$LIMPET_A)
tide_ht_stats <- calculate_stats(data$TIDE_HT, data$LIMPET_A)
preds_stats <- calculate_stats(data$PREDS, data$LIMPET_A)

# Print the results
cat("FOOD vs. LIMPET_A\n")
cat("Slope: ", round(food_stats$slope, 3), "\n")
cat("R^2: ", round(food_stats$r_squared, 3), "\n\n")

cat("OTH_LIMPETS vs. LIMPET_A\n")
cat("Slope: ", round(oth_limpets_stats$slope, 3), "\n")
cat("R^2: ", round(oth_limpets_stats$r_squared, 3), "\n\n")

cat("TIDE_HT vs. LIMPET_A\n")
cat("Slope: ", round(tide_ht_stats$slope, 3), "\n")
cat("R^2: ", round(tide_ht_stats$r_squared, 3), "\n\n")

cat("PREDS vs. LIMPET_A\n")
cat("Slope: ", round(preds_stats$slope, 3), "\n")
cat("R^2: ", round(preds_stats$r_squared, 3), "\n")

