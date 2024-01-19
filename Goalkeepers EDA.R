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

library(dplyr)
library(lmtest)
library(fastDummies)
library(glmnet)
library(corrplot)
library(car)
library(sparsepca)
library(ggplot2)
library(FactoMineR)
library(cowplot)



#### Data Preparation ####

par(mfrow=c(1,1))

df <- read.csv("transfermarkt_fbref_201819.csv", header = TRUE, sep = ';')

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
)

corrplot(cor(data_without_high_correlation[, 1:20]), method = "color",
         tl.col = "#000000",
         addCoef.col = "#000000",
         tl.srt = 40,
         type = "upper",
         diag = FALSE,  
         order = "hclust", 
         tl.cex = 0.6,
         cl.cex = 0.7,
         number.cex = 0.6,
)

gk_df <- data.frame(columns, data_without_high_correlation)

col_names <- colnames(gk_df)
col_names



########################################################
#           Exploratory Data Analysis (EDA)
########################################################

# Value histogram
gk_h_v <- ggplot(gk_df, aes(x = gk_df$value)) +
  geom_histogram(aes(y =..density..), fill="#009490", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(gk_df$value)), color="#32435b",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(gk_df$value)), color="#ff3131",linetype="dashed", size=1) +
  labs(x ="log(value)", y="Density")+
  ggtitle("Histogram of value for GK") +
  theme_light() +
  theme(panel.grid = element_blank())

# Value boxplot
gk_b_v <- ggplot(gk_df, aes(x = gk_df$value))+
  geom_boxplot(fill = "#009490")+
  geom_point(aes(x= mean(gk_df$value), y=0), color="#32435b")+
  labs(x ="log(value)", y = " ")+
  ggtitle("Boxplot of value for GK")+
  theme_light() +
  theme(panel.grid = element_blank())

plot_grid(gk_h_v, gk_b_v, nrow = 1)



# Log(Value) histogram
gk_h_lv <- ggplot(gk_df, aes(x = log(gk_df$value))) +
  geom_histogram(aes(y =..density..), fill="#009490", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(log(gk_df$value))), color="#32435b",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(log(gk_df$value))), color="#ff3131",linetype="dashed", size=1) +
  labs(x ="log(value)", y="Density")+
  ggtitle("Histogram of log(value) for GK") +
  theme_light() +
  theme(panel.grid = element_blank())

# Log(Value) boxplot
gk_b_lv <- ggplot(gk_df, aes(x = log(gk_df$value)))+
  geom_boxplot(fill = "#009490")+
  geom_point(aes(x= mean(log(gk_df$value)), y=0), color="#32435b")+
  labs(x ="log(value)", y = " ")+
  ggtitle("Boxplot of log(value) for GK")+
  theme_light() +
  theme(panel.grid = element_blank())

plot_grid(gk_h_lv, gk_b_lv, nrow = 1)
# plot_grid(h_lv, gk_h_lv, nrow = 1)

plot_grid(gk_h_v, h_v, gk_h_lv, h_lv, nrow = 2)

#Height

gk_h_h <- ggplot(gk_df, aes(x = gk_df$height)) +
  geom_histogram(aes(y =..density..), fill="#009490", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(gk_df$height)), color="#32435b",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(gk_df$height)), color="#ff3131",linetype="dashed", size=1) +
  labs(x ="log(value)", y="Density")+
  ggtitle("Histogram of log(value) for Goalkeeper") +
  theme_light() +
  theme(panel.grid = element_blank())

print(gk_h_h)

height_vs_value <- ggplot(data = gk_df, aes(x = height, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between height and value for Goalkeeper")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

print(height_vs_value)


#Age

# Age histogram
gk_h_age <- ggplot(gk_df, aes(x = gk_df$age)) +
  geom_histogram(aes(y =..density..), fill="#009490", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(gk_df$age)), color="#32435b",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(gk_df$age)), color="#ff3131",linetype="dashed", size=1) +
  labs(x ="age", y="Density")+
  ggtitle("Histogram of age")+
  theme_light() +
  theme(panel.grid = element_blank())

# Age boxplot
gk_b_age <- ggplot(gk_df, aes(x = gk_df$age))+
  geom_boxplot(fill = "#009490")+
  geom_point(aes(x= mean(gk_df$age), y=0), color="#32435b")+
  labs(x ="age", y = " ")+
  ggtitle("Boxplot of age")+
  theme_light() +
  theme(panel.grid = element_blank())

plot_grid(gk_h_age, gk_b_age, nrow = 1)

age_vs_value <- ggplot(data = gk_df, aes(x = age, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between age and value for Goalkeeper")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

print(age_vs_value)
# the older a goalkeeper the lower the value


# goals_against_per90_gkm

# goals_against_per90_gkm histogram
gk_h_goals_against_per90_gkm <- ggplot(gk_df, aes(x = gk_df$goals_against_per90_gkm)) +
  geom_histogram(aes(y =..density..), fill="#009490", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(gk_df$goals_against_per90_gkm)), color="#32435b",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(gk_df$goals_against_per90_gkm)), color="#ff3131",linetype="dashed", size=1) +
  labs(x ="goals_against_per90_gkm", y="Density")+
  ggtitle("Histogram of goals_against_per90_gkm")+
  theme_light() +
  theme(panel.grid = element_blank())

# goals_against_per90_gkm boxplot
gk_b_goals_against_per90_gkm <- ggplot(gk_df, aes(x = gk_df$goals_against_per90_gkm))+
  geom_boxplot(fill = "#009490")+
  geom_point(aes(x= mean(gk_df$goals_against_per90_gkm), y=0), color="#32435b")+
  labs(x ="goals_against_per90_gkm", y = " ")+
  ggtitle("Boxplot of goals_against_per90_gkm")+
  theme_light() +
  theme(panel.grid = element_blank())

plot_grid(gk_h_goals_against_per90_gkm, gk_b_goals_against_per90_gkm, nrow = 1)

goals_against_per90_gkm_vs_value <- ggplot(data = gk_df, aes(x = goals_against_per90_gkm, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between goals_against_per90_gkm and value for Goalkeeper")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

print(goals_against_per90_gkm_vs_value)



# free_kick_goals_against_gkm

# free_kick_goals_against_gkm histogram
gk_h_free_kick_goals_against_gkm <- ggplot(gk_df, aes(x = gk_df$free_kick_goals_against_gkm)) +
  geom_histogram(aes(y =..density..), fill="#009490", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(gk_df$free_kick_goals_against_gkm)), color="#32435b",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(gk_df$free_kick_goals_against_gkm)), color="#ff3131",linetype="dashed", size=1) +
  labs(x ="free_kick_goals_against_gkm", y="Density")+
  ggtitle("Histogram of free_kick_goals_against_gkm")+
  theme_light() +
  theme(panel.grid = element_blank())

# free_kick_goals_against_gkm boxplot
gk_b_free_kick_goals_against_gkm <- ggplot(gk_df, aes(x = gk_df$free_kick_goals_against_gkm))+
  geom_boxplot(fill = "#009490")+
  geom_point(aes(x= mean(gk_df$free_kick_goals_against_gkm), y=0), color="#32435b")+
  labs(x ="free_kick_goals_against_gkm", y = " ")+
  ggtitle("Boxplot of free_kick_goals_against_gkm")+
  theme_light() +
  theme(panel.grid = element_blank())

plot_grid(gk_h_free_kick_goals_against_gkm, gk_b_free_kick_goals_against_gkm, nrow = 1)

free_kick_goals_against_gkm_vs_value <- ggplot(data = gk_df, aes(x = free_kick_goals_against_gkm, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between free_kick_goals_against_gkm and value for Goalkeeper")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

print(free_kick_goals_against_gkm_vs_value)


# corner_kick_goals_against_gkm

corner_kick_goals_against_gkm_vs_value <- ggplot(data = gk_df, aes(x = corner_kick_goals_against_gkm, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between corner_kick_goals_against_gkm and value for Goalkeeper")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

print(corner_kick_goals_against_gkm_vs_value)


# clean_sheets_pct

# clean_sheets_pct histogram
gk_h_clean_sheets_pct <- ggplot(gk_df, aes(x = gk_df$clean_sheets_pct)) +
  geom_histogram(aes(y =..density..), fill="#009490", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(gk_df$clean_sheets_pct)), color="#32435b",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(gk_df$clean_sheets_pct)), color="#ff3131",linetype="dashed", size=1) +
  labs(x ="clean_sheets_pct", y="Density")+
  ggtitle("Histogram of clean_sheets_pct")+
  theme_light() +
  theme(panel.grid = element_blank())

# clean_sheets_pct boxplot
gk_b_clean_sheets_pct <- ggplot(gk_df, aes(x = gk_df$clean_sheets_pct))+
  geom_boxplot(fill = "#009490")+
  geom_point(aes(x= mean(gk_df$clean_sheets_pct), y=0), color="#32435b")+
  labs(x ="clean_sheets_pct", y = " ")+
  ggtitle("Boxplot of clean_sheets_pct")+
  theme_light() +
  theme(panel.grid = element_blank())

plot_grid(gk_h_clean_sheets_pct, gk_b_clean_sheets_pct, nrow = 1)

clean_sheets_pct_vs_value <- ggplot(data = gk_df, aes(x = clean_sheets_pct, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between clean_sheets_pct and value for Goalkeeper")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

print(clean_sheets_pct_vs_value)


# pens_saved

# pens_saved histogram
gk_h_pens_saved <- ggplot(gk_df, aes(x = gk_df$pens_saved)) +
  geom_histogram(aes(y =..density..), fill="#009490", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(gk_df$pens_saved)), color="#32435b",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(gk_df$pens_saved)), color="#ff3131",linetype="dashed", size=1) +
  labs(x ="pens_saved", y="Density")+
  ggtitle("Histogram of pens_saved")+
  theme_light() +
  theme(panel.grid = element_blank())

# pens_saved boxplot
gk_b_pens_saved <- ggplot(gk_df, aes(x = gk_df$pens_saved))+
  geom_boxplot(fill = "#009490")+
  geom_point(aes(x= mean(gk_df$pens_saved), y=0), color="#32435b")+
  labs(x ="pens_saved", y = " ")+
  ggtitle("Boxplot of pens_saved")+
  theme_light() +
  theme(panel.grid = element_blank())

plot_grid(gk_h_pens_saved, gk_b_pens_saved, nrow = 1)

pens_saved_vs_value <- ggplot(data = gk_df, aes(x = pens_saved, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between pens_saved and value for Goalkeeper")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

print(pens_saved_vs_value)

plot_grid(age_vs_value, goals_against_per90_gkm_vs_value, clean_sheets_pct_vs_value, pens_saved_vs_value, nrow = 2)



# save_pct

# save_pct histogram
gk_h_save_pct <- ggplot(gk_df, aes(x = gk_df$save_pct)) +
  geom_histogram(aes(y =..density..), fill="#009490", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(gk_df$save_pct)), color="#32435b",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(gk_df$save_pct)), color="#ff3131",linetype="dashed", size=1) +
  labs(x ="save_pct", y="Density")+
  ggtitle("Histogram of save_pct")+
  theme_light() +
  theme(panel.grid = element_blank())

# save_pct boxplot
gk_b_save_pct <- ggplot(gk_df, aes(x = gk_df$save_pct))+
  geom_boxplot(fill = "#009490")+
  geom_point(aes(x= mean(gk_df$save_pct), y=0), color="#32435b")+
  labs(x ="save_pct", y = " ")+
  ggtitle("Boxplot of save_pct")+
  theme_light() +
  theme(panel.grid = element_blank())

plot_grid(gk_h_save_pct, gk_b_save_pct, nrow = 1)

save_pct_vs_value <- ggplot(data = gk_df, aes(x = save_pct, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between save_pct and value for Goalkeeper")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

print(save_pct_vs_value)

# Analysis of the distributions of some variables
plot_grid(gk_h_age, gk_h_goals_against_per90_gkm, gk_h_clean_sheets_pct, gk_h_save_pct, nrow = 2)

# Analysis of the relationships between some variables and log(value)
plot_grid(age_vs_value, goals_against_per90_gkm_vs_value, clean_sheets_pct_vs_value, save_pct_vs_value, nrow = 2)
