#### Library ####

library(ggplot2)
library(corrplot)
library(car)
library(caret)
library(glmnet)
library(grpreg)
library(grplasso)
library(gglasso)
library(dplyr)
library(gridExtra)
library(RColorBrewer)
library(plotly)

#### Data Preparation ####

par(mfrow=c(1,1))

df <- read.csv("C:/Users/pc/Desktop/Statistical Method for High Dimensional Data/Project/Transfermarket/Transfermarket/transfermarkt_fbref_201819.csv", header = TRUE, sep = ';')

df <- df[!is.na(df$value), , drop = FALSE]
df[df$player == "Adrián Diéguez"  ,"foot"] = 1
df[df$player == "Hervé Lybohy"  ,"foot"] = 1
df[df$player == "Hervé Lybohy"  ,"height"] = 187
df[df$player == "Juan Soriano"  ,"foot"] = 0
any(is.na(df))

gk_vars <- names(df)[grepl("gk", names(df), ignore.case = TRUE)]
players_vars <- setdiff(names(df), gk_vars)
players_vars <- names(df)[names(df) %in% players_vars]
players_df <- df[df$position != 'GK', players_vars]

gk_df <- df[df[, "position"] == "GK", ]

cols_to_drop <- c("Column1","MP","Season","Attendance","birth_year", "nationality", "position2", "league", "squad")
gk_df <- gk_df[, -c(which(names(gk_df) %in% cols_to_drop))]
players_df <- players_df[, -c(which(names(players_df) %in% cols_to_drop))]
any(is.na(gk_df))
any(is.na(players_df))

filtered_columns <- character(0)
for (col in colnames(players_df)) {
  sum_zero <- sum(players_df[[col]] == 0, na.rm = TRUE)
  if (sum_zero / nrow(players_df) > 0.95) {
    filtered_columns <- c(filtered_columns, col)
  }
}

filtered_columns <- head(filtered_columns, -2)

a <- ggplot(gk_df, aes(y = log(value))) +
  geom_boxplot(fill = "#009490", color = "#000000") + 
  ggtitle("Boxplot of log(value)") +
  ylab("log(value)") +
  theme_minimal()

b <- ggplot(gk_df, aes(y = value)) +
  geom_boxplot(fill = "#009490", color = "#000000") + 
  ggtitle("Boxplot of value") +
  ylab("value") +
  theme_minimal()

grid.arrange(b, a, ncol = 2)

par(mfrow = c(1, 1))

boxplot_stats <- boxplot(log(gk_df$value))$stats
inf <- boxplot_stats[1] - 1.5 * IQR(gk_df$value)
sup <- boxplot_stats[5] + 1.5 * IQR(gk_df$value)
gk_df <- gk_df[gk_df$value >= inf & gk_df$value <= sup, ]

filtered_columns <- gk_df[, filtered_columns]

columns <- gk_df[, c("player", "foot", "position")]
Columns_2 <- gk_df[, c("age", "height", "value")]

gk_df <- gk_df[, grepl("gk", names(gk_df))]

gk_df <- data.frame(Columns_2, gk_df)
gk_df <- data.frame(gk_df, filtered_columns)

# Identify and remove variables with zero standard deviation
zero_sd_vars <- sapply(gk_df, sd) == 0
gk_df <- gk_df[, !zero_sd_vars]

# Impute missing values (you can choose an appropriate imputation method)
gk_df <- na.omit(gk_df)

corr_mat <- cor(gk_df)
corr_threshold <- 0.8
columns_to_remove <- findCorrelation(corr_mat, cutoff = corr_threshold)
data_without_high_correlation <- gk_df[, -columns_to_remove]
length(data_without_high_correlation)

par(mfrow=c(1,1))

my_palette <- colorRampPalette(c("#009490", "white", "#6ce5e8"))(n = 100)

corrplot(cor(data_without_high_correlation), method = "color",
         tl.col = "#000000",
         addCoef.col = "#000000",
         tl.srt = 40,
         type = "upper",
         diag = FALSE,  
         order = "hclust", 
         tl.cex = 0.5,
         cl.cex = 0.7,
         number.cex = 0.4,
         col = my_palette
)

my_palette <- colorRampPalette(c("#009490", "white", "#6ce5e8"))(n = 100)

corrplot(cor(data_without_high_correlation[, 1:20]), method = "color",
         tl.col = "#000000",
         addCoef.col = "#000000",
         tl.srt = 40,
         type = "upper",
         diag = FALSE,  
         order = "hclust", 
         tl.cex = 0.7,
         cl.cex = 0.7,
         number.cex = 0.6,
         col = my_palette
)

gk_df <- data.frame(columns, data_without_high_correlation)

y <- log(as.numeric(gk_df$value))
gk_df <- gk_df[, -c(which(names(gk_df) %in% c("foot", "player", "position", "value")))]
length(gk_df)

colnames(gk_df)

gk_df <- as.matrix(gk_df)
gk_df_grp <- as.matrix(gk_df)
gk_df_lr <- as.matrix(gk_df)
gk_df_lasso <- as.matrix(gk_df)
gk_df_ridge <- as.matrix(gk_df)
gk_df_final <- as.matrix(gk_df)

y_grp <- y
y_lr <- y
y_lasso <- y
y_ridge <- y
y_final <- y

group_vector <- c(
  1,  # age                           
  1,  # height
  2,  # wins_gk                       
  2,  # draws_gk
  2,  # pens_missed_gk
  2,  # free_kick_goals_against_gk
  2,  # corner_kick_goals_against_gk
  2,  # own_goals_against_gk
  2,  # psxg_net_gk
  2,  # passes_pct_launched_gk
  2,  # pct_passes_launched_gk
  2,  # def_actions_outside_pen_area_gk
  2,  # def_actions_outside_pen_area_per90_gk
  2,  # avg_distance_def_actions_gk
  3,  # games_starts_gkm              
  3,  # goals_against_per90_gkm
  3,  # wins_gkm
  3,  # draws_gkm
  3,  # losses_gkm
  3,  # pens_att_gkm
  3,  # pens_missed_gkm
  3,  # minutes_90s_gkm
  3,  # free_kick_goals_against_gkm
  3,  # corner_kick_goals_against_gkm
  3,  # own_goals_against_gkm
  3,  # psxg_gkm
  3,  # psxg_net_gkm
  3,  # passes_launched_gkm
  3,  # passes_gkm
  3,  # passes_throws_gkm
  3,  # crosses_gkm
  3,  # crosses_stopped_gkm
  3,  # crosses_stopped_pct_gkm
  3,  # def_actions_outside_pen_area_per90_gkm
  3,  # avg_distance_def_actions_gkm
  3,  # save_pctm
  3,  # clean_sheetsm
  3,  # clean_sheets_pctm
  3,  # pens_savedm
  3,  # psnpxg_per_shot_on_target_againstm
  3,  # goal_kicksm
  3,  # pct_goal_kicks_launchedm
  4,  # save_pct                       
  4,  # clean_sheets_pct
  4,  # pens_allowed
  4,  # pens_saved
  4,  # psnpxg_per_shot_on_target_against
  4   # pct_goal_kicks_launched
)


#### Goalkeeper Grouped Lasso ####

set.seed(123)

size <- nrow(gk_df)
idx <- sample(1:size, size * 0.8)
gk_train <- as.matrix(gk_df[idx, ])
gk_test <- as.matrix(gk_df[-idx, ])
y_train <- y[idx]
y_test <- y[-idx]

grp_lasso <- gglasso(gk_train, y_train, group = group_vector, loss = "ls")
coef_mat <- grp_lasso$beta

plot(grp_lasso)

g1=max(which(coef_mat[1,]==0))
g1

g2=max(which(coef_mat[3,]==0))
g2

g3=max(which(coef_mat[15,]==0))
g3

g4=max(which(coef_mat[43,]==0))
g4

predictions <- predict(grp_lasso, newx = gk_test)
grp_predictions <- predict(grp_lasso, newx = gk_test)

par(mfrow=c(1, 1))

plot(grp_lasso$b0,main="Coefficient vs Step",
     ylab="Intercept",xlab="Step (decreasing Lambda =>)",
     xlim=c(-1,100),
     ylim=c(7, 16),
     type="l",lwd=2)
grid()
x=c(g1,g2,g3,g4)
y=c(grp_lasso$b0[g1],grp_lasso$b0[g2],grp_lasso$b0[g3],grp_lasso$b0[g4])
points(x=x, y=y, pch=13, lwd=2, cex=2)
lmda=round(grp_lasso$lambda[c(g1,g2,g3,g4)],2)
text(x=x+0.5, y=y+0.1, labels=c("Group1", "Group2", "Group3", "Group4"), pos=3, cex=0.6)
text(x=x-0.7, y=y-0.1, labels=paste("Lambda\n=",lmda), pos=1, cex=0.6)
legend(x=80, y=16, legend = c(paste("Group1 =", lmda[1]), paste("Group2 =", lmda[2]), paste("Group3 =", lmda[3]), paste("Group4 =", lmda[4])), bty = "n", cex = 0.7)


#### Goalkeeper Cross-Validation Grouped Lasso ####

set.seed(123)

par(mfrow=c(1, 1))

size <- nrow(gk_df_grp)
idx <- sample(1:size, size * 0.8)
gk_train <- as.matrix(gk_df_grp[idx, ])
gk_test <- as.matrix(gk_df_grp[-idx, ])
y_train <- y_grp[idx]
y_test <- y_grp[-idx]

grp_lasso_cv <- cv.gglasso(gk_train, y_train, group=group_vector, nfolds=10)
plot(grp_lasso_cv)

grp_lasso <- gglasso(gk_train, y_train, group=group_vector, loss='ls')

lmbda <- grp_lasso_cv$lambda.1se
lmbda1 <- grp_lasso_cv$lambda.min

plot(grp_lasso)
abline(v=log(lmbda), lty=2, col=2)
abline(v=log(lmbda1), lty=2, col=2)

coefs_1se <- coef(object=grp_lasso, s=lmbda)
coefs_min <- coef(object=grp_lasso, s=lmbda1)
cbind(coefs_1se, coefs_min)

plt <- cbind(y_test, predict(object=grp_lasso, newx=gk_test, s=lmbda), predict(object=grp_lasso, newx=gk_test, s=lmbda1), type='link')
matplot(plt, main="Predicted vs Actual", type='l', lwd=2, 
        ylab="Value",
        xlab="Players",
        ylim=c(10, 18),
        col = c("#000000", "#009490", "#BDC2C5"))
grid()
legend(x=25, y=18, legend=c("Actual","Fitted-1se","Fitted-min"), fill=c("#000000", "#009490", "#BDC2C5"), bty="n", cex=0.7)

predictions_1se <- predict(object=grp_lasso, newx=gk_test, s=lmbda)
predictions_min <- predict(object=grp_lasso, newx=gk_test, s=lmbda1)

mse_1se <- mean((predictions_1se - y_test)^2)
mae_1se <- mean(abs(predictions_1se - y_test))
rmse_1se <- sqrt(mse_1se)
cat("Mean Squared Error (MSE):", mse_1se, "\n")
cat("Mean Absolute Error (MAE):", mae_1se, "\n")
cat("Root Mean Squared Error (RMSE):", rmse_1se, "\n")

mse_min <- mean((predictions_min - y_test)^2)
mae_min <- mean(abs(predictions_min - y_test))
rmse_min <- sqrt(mse_min)
cat("Mean Squared Error (MSE):", mse_min, "\n")
cat("Mean Absolute Error (MAE):", mae_min, "\n")
cat("Root Mean Squared Error (RMSE):", rmse_min, "\n")

#### Coefficients Display ####

coefs_1se
coefs_min

feature_importance <- coefs_1se
feature_importance <- feature_importance[-1]
normalized_importance <- feature_importance / sum(abs(feature_importance))
plot_data <- data.frame(
  feature = colnames(gk_train),
  importance = normalized_importance
)

plot_data <- subset(plot_data, importance > 0.01 | importance < -0.01)
plot_data <- plot_data[order(-plot_data$importance), ]

bar_grp_1se <- ggplot(plot_data, aes(x = reorder(feature, -importance), y = importance, fill = importance)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_gradient(low = "#009490", high = "#009490", guide = FALSE) +
  labs(title = "Normalized Grouped Lasso 1 SE Feature Importance", x = "Features", y = "Importance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 12, face = "bold"))

feature_importance <- coefs_min
feature_importance <- feature_importance[-1]
normalized_importance <- feature_importance / sum(abs(feature_importance))
plot_data <- data.frame(
  feature = colnames(gk_train),
  importance = normalized_importance
)

plot_data <- subset(plot_data, importance > 0.01 | importance < -0.01)
plot_data <- plot_data[order(-plot_data$importance), ]

bar_grp_min <- ggplot(plot_data, aes(x = reorder(feature, -importance), y = importance, fill = importance)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_gradient(low = "#009490", high = "#009490", guide = FALSE) +
  labs(title = "Normalized Grouped Lasso MIN Feature Importance", x = "Features", y = "Importance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 12, face = "bold"))

#### Goalkeeper Linear Regression ####

set.seed(123)

size <- nrow(gk_df_lr)
idx <- sample(1:size, size * 0.8)
gk_train <- as.data.frame(gk_df_lr[idx, ])
gk_test <- as.data.frame(gk_df_lr[-idx, ])
y_train <- y_lr[idx]
y_test <- y_lr[-idx]

lm_model <- lm(y_train~. , data = gk_train)

par(mfrow=c(2,2))

plot(lm_model)
summary(lm_model)

predictions <- predict(lm_model, newdata = gk_test)
lm_predictions <- predict(lm_model, newdata = gk_test)

mse <- mean((predictions - y_test)^2)
mae <- mean(abs(predictions - y_test))
rmse <- sqrt(mse)
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

par(mfrow = c(2, 3))

plot(lm_model$fitted.values, residuals(lm_model),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values", col = "#009490")
abline(h = 0, col = "#000000", lty = 1)

qqnorm(residuals(lm_model), col = "#009490")
qqline(residuals(lm_model), col="#000000")

plot(gk_train$save_pct, residuals(lm_model),
     xlab = "save_pct", ylab = "Residuals",
     main = "Residuals vs. save_pct", col = "#009490")
abline(h = 0, col = "#000000", lty = 1)

plot(gk_train$psxg_net_gk, residuals(lm_model),
     xlab = "psxg_net_gk", ylab = "Residuals",
     main = "Residuals vs. psxg_net_gk", col = "#009490")
abline(h = 0, col = "#000000", lty = 1)

res <- residuals(lm_model)
hist(res, freq=FALSE, breaks="FD", col="#009490", main = "Histogram of Residuals", xlab = "Residuals", ylab = "Frequency", ylim=c(0, 0.7))
x <- seq(min(res), max(res), length.out=100) 
y <- dnorm(x, mean=mean(res), sd=sd(res)) 
lines(x, y, col="#000000", lwd=2)

coeff <- coef(lm_model)
std_dev <- summary(lm_model)$coef[, "Std. Error"]
coeff_normalized <- coeff / std_dev

coeff_normalized <- coeff_normalized[-1]
coeff_normalized <- coeff_normalized[coeff_normalized > 0.4 | coeff_normalized < -0.4]
barplot(coeff_normalized, names.arg=names(coeff_normalized), col="#009490", main="Normalized Feature Importance")

feature_importance <- coef(lm_model)
std_dev <- std_dev
normalized_importance <- feature_importance / std_dev
normalized_importance <- normalized_importance[-1]


plot_data <- data.frame(features = colnames(gk_train), importance = normalized_importance)
plot_data <- subset(plot_data, importance > 0.4 | importance < -0.4)
plot_data <- plot_data[order(-plot_data$importance), ]

bar_lr <- ggplot(plot_data, aes(x = reorder(features, -importance), y = importance, fill = importance)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_gradient(low = "#009490", high = "#009490", guide = FALSE) +
  labs(title = "Normalized Linear Feature Importance", x = "Features", y = "Importance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 12, face = "bold"))

par(mfrow=c(1, 1))

plt <- cbind(y_test, predictions, type='link')
matplot(plt, main="Predicted vs Actual", type='l', lwd=2, 
        ylab="Value",
        xlab="Players",
        col = c("#000000", "#009490"))
grid()
legend(x=25, y=18, legend=c("Actual","Fitted"), fill=c("#000000", "#009490"), bty="n", cex=0.7)

#### Goalkeeper Lasso Regression + Cross-Validation ####

set.seed(123)

par(mfrow= c (1, 1))

size <- nrow(gk_df_lasso)
idx <- sample(1:size, size * 0.8)
gk_train <- as.data.frame(gk_df_lasso[idx, ])
gk_test <- as.data.frame(gk_df_lasso[-idx, ])
y_train <- y_lasso[idx]
y_test <- y_lasso[-idx]

lambda_seq <- seq(0, 5, length = 500)
cv.lasso <- cv.glmnet(as.matrix(gk_train), y_train, alpha = 1, lambda = lambda_seq)
plot(cv.lasso)

best_lambda <- cv.lasso$lambda.min
cat("Best Lambda:", best_lambda, "\n")

lasso_model <- glmnet(as.matrix(gk_train), y_train, alpha = 1, lambda = best_lambda)
plot(coef(lasso_model), type = "o", main = "Lasso Coefficient Magnitudes")
predictions <- predict(lasso_model, newx = as.matrix(gk_test), s = best_lambda)
lasso_predictions <- predict(lasso_model, newx = as.matrix(gk_test), s = best_lambda)

mse <- mean((predictions - y_test)^2)
mae <- mean(abs(predictions - y_test))
rmse <- sqrt(mse)
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

feature_importance <- coef(lasso_model)
feature_importance <- feature_importance[-1]
normalized_importance <- feature_importance / sum(abs(feature_importance))

plot_data <- data.frame(
  feature = colnames(gk_train),
  importance = normalized_importance
)

plot_data <- subset(plot_data, importance > 0.01 | importance < -0.01)
plot_data <- plot_data[order(-plot_data$importance), ]

bar_lasso <- ggplot(plot_data, aes(x = reorder(feature, -importance), y = importance, fill = importance)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_gradient(low = "#009490", high = "#009490", guide = FALSE) +
  labs(title = "Normalized Lasso Feature Importance", x = "Features", y = "Importance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 12, face = "bold"))

par(mfrow=c(1, 1))

plt <- cbind(y_test, predictions, type='link')
matplot(plt, main="Predicted vs Actual", type='l', lwd=2, 
        ylab="Value",
        xlab="Players",
        col = c("#000000", "#009490"))
grid()
legend(x=25, y=16, legend=c("Actual","Fitted"), fill=c("#000000", "#009490"), bty="n", cex=0.7)

#### Goalkeeper Ridge Regression + Cross-Validation ####

set.seed(123)

par(mfrow= c (1, 1))

size <- nrow(gk_df_ridge)
idx <- sample(1:size, size * 0.8)
gk_train <- as.data.frame(gk_df_ridge[idx, ])
gk_test <- as.data.frame(gk_df_ridge[-idx, ])
y_train <- y_ridge[idx]
y_test <- y_ridge[-idx]

lambda_seq <- seq(0, 500, length = 500)
cv.ridge <- cv.glmnet(as.matrix(gk_train), y_train, alpha = 0, lambda = lambda_seq)
plot(cv.ridge)

best_lambda <- cv.ridge$lambda.min
cat("Best Lambda:", best_lambda, "\n")

ridge_model <- glmnet(as.matrix(gk_train), y_train, alpha = 0, lambda = best_lambda)
plot(coef(ridge_model), type = "o", main = "Ridge Coefficient Magnitudes")

predictions <- predict(ridge_model, newx = as.matrix(gk_test), s = best_lambda)
ridge_predictions <- predict(ridge_model, newx = as.matrix(gk_test), s = best_lambda)

mse <- mean((predictions - y_test)^2)
mae <- mean(abs(predictions - y_test))
rmse <- sqrt(mse)
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

feature_importance <- coef(ridge_model)
feature_importance <- feature_importance[-1]
normalized_importance <- feature_importance / sum(abs(feature_importance))

plot_data <- data.frame(
  feature = colnames(gk_train),
  importance = normalized_importance
)

plot_data <- subset(plot_data, importance > 0.01 | importance < -0.01)
plot_data <- plot_data[order(-plot_data$importance), ]

bar_ridge <- ggplot(plot_data, aes(x = reorder(feature, -importance), y = importance, fill = importance)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_gradient(low = "#009490", high = "#009490", guide = FALSE) +
  labs(title = "Normalized Ridge Feature Importance", x = "Features", y = "Importance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 12, face = "bold"))

par(mfrow=c(1, 1))

plt <- cbind(y_test, predictions, type='link')
matplot(plt, main="Predicted vs Actual", type='l', lwd=2, 
        ylab="Value",
        xlab="Players",
        col = c("#000000", "#009490"))
grid()
  legend(x=25, y=16, legend=c("Actual","Fitted"), fill=c("#000000", "#009490"), bty="n", cex=0.7)
  
grid.arrange(bar_lasso, bar_ridge, ncol=2)

#### Goalkeeper Comparison ####

set.seed(123)

size <- nrow(gk_df_final)
idx <- sample(1:size, size * 0.8)
gk_train <- as.data.frame(gk_df_final[idx, ])
gk_test <- as.data.frame(gk_df_final[-idx, ])
y_train <- y_final[idx]
y_test <- y_final[-idx]

mae_lm <- mean(abs(lm_predictions - y_test))
mae_lasso <- mean(abs(lasso_predictions - y_test))
mae_ridge <- mean(abs(ridge_predictions - y_test))
mae_1se <- mean(abs(predictions_1se - y_test))
mae_min <- mean(abs(predictions_min - y_test))

mse_lm <- mean((lm_predictions - y_test)^2)
mse_lasso <- mean((lasso_predictions - y_test)^2)
mse_ridge <- mean((ridge_predictions - y_test)^2)
mse_1se <- mean((predictions_1se - y_test)^2)
mse_min <- mean((predictions_min - y_test)^2)

rmse_lm <- sqrt(mse_lm)
rmse_lasso <- sqrt(mse_lasso)
rmse_ridge <- sqrt(mse_ridge)
rmse_1se <- sqrt(mse_1se)
rmse_min <- sqrt(mse_min)

cat("MAE - LR:", mae_lm, 4, "\n")
cat("MAE - Lasso:", mae_lasso, 4, "\n")
cat("MAE - Ridge:", mae_ridge, "\n")
cat("MAE - Grouped Lasso 1SE:", mae_1se, "\n")
cat("MAE - Grouped Lasso MIN:", mae_min, "\n")

cat("MSE - LR:", mse_lm, "\n")
cat("MSE - Lasso:", mse_lasso, "\n")
cat("MSE - Ridge:", mse_ridge, "\n")
cat("MSE - Grouped Lasso 1SE:", mse_1se, "\n")
cat("MSE - Grouped Lasso MIN:", mse_min, "\n")

cat("RMSE - LR:", rmse_lm, "\n")
cat("RMSE - Lasso:", rmse_lasso, "\n")
cat("RMSE - Ridge:", rmse_ridge, "\n")
cat("RMSE - Grouped Lasso 1SE:", rmse_1se, "\n")
cat("RMSE - Grouped Lasso MIN:", rmse_min, "\n")

results_df <- data.frame(
  Model = c("LR", "Lasso", "Ridge", "GR Lasso 1SE", "GR Lasso MIM"),
  MAE = c(mae_lm, mae_lasso, mae_ridge, mae_1se, mae_min),
  MSE = c(mse_lm, mse_lasso, mse_ridge, mse_1se, mse_min),
  RMSE = c(rmse_lm, rmse_lasso, rmse_ridge, rmse_1se, rmse_min)
)

print(results_df)

par(mfrow = c(2, 3))

plot(y_test, lm_predictions, main = "Scatter Plot - LR ",
     xlab = "Actual Values", ylab = "Predicted Values", pch = 16, col = "#009490")
abline(h = mean(y_test), col = "#000000", lwd = 1, lty = 2)


plot(y_test, ridge_predictions, main = "Scatter Plot - Ridge",
     xlab = "Actual Values", ylab = "Predicted Values", pch = 16, col = "#009490",ylim = c(13, 16))
abline(h = mean(y_test), col = "#000000", lwd = 1, lty = 2)


plot(y_test, lasso_predictions, main = "Scatter Plot - Lasso",
     xlab = "Actual Values", ylab = "Predicted Values", pch = 16, col = "#009490")
abline(h = mean(y_test), col = "#000000", lwd = 1, lty = 2)


plot(y_test, predictions_1se, main = "Scatter Plot - GL LAMBDA 1SE ",
     xlab = "Actual Values", ylab = "Predicted Values", pch= 16, col = "#009490")
abline(h = mean(y_test), col = "#000000", lwd = 1, lty = 2)


plot(y_test, predictions_min, main = "Scatter Plot - GL LAMBDA MIN ",
     xlab = "Actual Values", ylab = "Predicted Values", pch = 16, col = "#009490")
abline(h = mean(y_test), col = "#000000", lwd = 1, lty = 2)


grid.arrange(bar_lr, bar_lasso, bar_ridge, bar_grp_1se, bar_grp_min, ncol=3, nrow=2)

par(mfrow = c(2, 3))

plt <- cbind(y_test, lm_predictions, type='link')
matplot(plt, main="Linera Predicted vs Actual", type='l', lwd=2, 
        ylab="Value",
        xlab="Players",
        col = c("#000000", "#009490"))
grid()
legend(x=25, y=18, legend=c("Actual","Fitted"), fill=c("#000000", "#009490"), bty="n", cex=0.7)

plt <- cbind(y_test, ridge_predictions, type='link')
matplot(plt, main="Ridge vs Actual", type='l', lwd=2, 
        ylab="Value",
        xlab="Players",
        col = c("#000000", "#009490"))
grid()
legend(x=25, y=16, legend=c("Actual","Fitted"), fill=c("#000000", "#009490"), bty="n", cex=0.7)

plt <- cbind(y_test, lasso_predictions, type='link')
matplot(plt, main="Lasso Predicted vs Actual", type='l', lwd=2, 
        ylab="Value",
        xlab="Players",
        col = c("#000000", "#009490"))
grid()
legend(x=25, y=16, legend=c("Actual","Fitted"), fill=c("#000000", "#009490"), bty="n", cex=0.7)

plt <- cbind(y_test, predictions_1se, type='link')
matplot(plt, main="Grouped Lasso 1SE Predicted vs Actual", type='l', lwd=2, 
        ylab="Value",
        xlab="Players",
        col = c("#000000", "#009490"))
grid()
legend(x=25, y=16, legend=c("Actual","Fitted"), fill=c("#000000", "#009490"), bty="n", cex=0.7)

plt <- cbind(y_test, predictions_min, type='link')
matplot(plt, main="Grouped Lasso MIN Predicted vs Actual", type='l', lwd=2, 
        ylab="Value",
        xlab="Players",
        col = c("#000000", "#009490"))
grid()
legend(x=25, y=16, legend=c("Actual","Fitted"), fill=c("#000000", "#009490"), bty="n", cex=0.7)


