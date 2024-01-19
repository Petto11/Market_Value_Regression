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


# Import the dataset
df <- read.csv("cleaned_dataset.csv")
dim(df)

# Observe the different values of the column "nationality"
nationality <- unique(df$nationality)
nationality
n_nat <- length(nationality)
n_nat

# Add the "continent" column, containing the continent of origin
country_to_cont <- read.csv("Country_to_cont.csv", sep = ";")
continent <- vector("character", length = nrow(df))
continent[] <- NA
temp <- substr(df$nationality, start = nchar(df$nationality)-2, nchar(df$nationality))
matching_indices <- match(temp, country_to_cont$Code)
continent[!is.na(matching_indices)] <- country_to_cont$Continent[na.omit(matching_indices)]
# df$continent <- continent
df <- cbind(df[, 1:2], continent, df[, 3:ncol(df)])
# missing <- unique(df[is.na(df$continent),"nationality"]) 
# length(missing)

col_names <- colnames(df)
col_names


########################################################
#           Exploratory Data Analysis (EDA)
########################################################


# Value histogram
h_v <- ggplot(df, aes(x = df$value)) +
  geom_histogram(aes(y =..density..), fill="#009490", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(df$value)), color="#32435b",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(df$value)), color="#ff3131",linetype="dashed", size=1) +
  labs(x ="value", y="Density")+
  ggtitle("Histogram of value for Outfield Players") +
  theme_light() +
  theme(panel.grid = element_blank())

# Value boxplot
b_v <- ggplot(df, aes(x = df$value))+
  geom_boxplot(fill = "#009490")+
  geom_point(aes(x= mean(df$value), y=0), color="#32435b")+
  labs(x ="value", y = " ")+
  ggtitle("Boxplot of value for Outfield Players")+
  theme_light() +
  theme(panel.grid = element_blank())

plot_grid(h_v, b_v, nrow = 1)
# We notice as the distribution of the variable value is clearly unbalanced towards left,
# and is characterized from a great presence of outliers in the right tail

# Log(Value) histogram
h_lv <- ggplot(df, aes(x = log(df$value))) +
  geom_histogram(aes(y =..density..), fill="#009490", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(log(df$value))), color="#32435b",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(log(df$value))), color="#ff3131",linetype="dashed", size=1) +
  labs(x ="log(value)", y="Density")+
  ggtitle("Histogram of log(value) for Outfield Players") +
  theme_light() +
  theme(panel.grid = element_blank())

# Log(Value) boxplot
b_lv <- ggplot(df, aes(x = log(df$value)))+
  geom_boxplot(fill = "#009490")+
  geom_point(aes(x= mean(log(df$value)), y=0), color="#32435b")+
  labs(x ="log(value)", y = " ")+
  ggtitle("Boxplot of log(value) for Outfield Players")+
  theme_light() +
  theme(panel.grid = element_blank())

plot_grid(h_lv, b_lv, nrow = 1)
# We can observe that after this transformation, the value variable assumes 
# an almost Gaussian distribution with mean and median almost coinciding


# Age histogram
h_age <- ggplot(df, aes(x = df$age)) +
  geom_histogram(aes(y =..density..), fill="#009490", color="black", alpha=0.6, bins = 15) +
  geom_density(color= "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(df$age)), color="#32435b",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(df$age)), color="#ff3131",linetype="dashed", size=1) +
  labs(x ="age", y="Density")+
  ggtitle("Histogram of age")+
  theme_light() +
  theme(panel.grid = element_blank())

# Age boxplot
b_age <- ggplot(df, aes(x = df$age))+
  geom_boxplot(fill = "#009490")+
  geom_point(aes(x= mean(df$age), y=0), color="#32435b")+
  labs(x ="age", y = " ")+
  ggtitle("Boxplot of age")+
  theme_light() +
  theme(panel.grid = element_blank())

plot_grid(h_age, b_age, nrow = 1)


# Continent
c_p <- ggplot(df, aes(x = df$continent)) +
  geom_bar(color="black", fill="#009490")+
  labs(x ="continent", y="Number of players")+
  ggtitle("Continents")+
  theme_light() +
  theme(panel.grid = element_blank())
# We notice that most of the players are European

# Leagues
l_p <- ggplot(df, aes(x = df$league)) +
  geom_bar(color="black", fill="#009490")+
  labs(x ="league", y="Number of players")+
  ggtitle("Leagues")+
  theme_light() +
  theme(panel.grid = element_blank())
# We note that the number of players in the Bundesliga is lower than that of the other leagues,
# this is due to the fact that it is an 18-team championship and not to 20 like the others

# Foot
f_p <- ggplot(df, aes(x = df$foot)) +
  geom_bar(color="black", fill="#009490")+
  labs(x ="foot", y="Number of players")+
  ggtitle("Foot")+
  theme_light() +
  theme(panel.grid = element_blank())+
  scale_x_discrete(labels = c("Left-footed", "Righ-footed", "Two-footed"))
# We notice that most of the players are right-footed

# Position
p_p <- ggplot(df, aes(x = df$position)) +
  geom_bar(color="black", fill="#009490")+
  labs(x ="position", y="Number of players")+
  ggtitle("Positions")+
  theme_light() +
  theme(panel.grid = element_blank())
# We notice how the defenders are more than the midfielders who in turn are more than the attackers

plot_grid(c_p, l_p, f_p, p_p, nrow = 2)



## Leagues

# Premier League

# Log(Value) histogram
h_p <- ggplot(data = subset(df, league == "Premier League"), aes(x = log(value))) +
  geom_histogram(aes(y =..density..), fill = "#009490", color = "black", alpha = 0.6, bins = 15) +
  geom_density(color= "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(log(value))), color = "#32435b", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(log(value))), color = "#ff3131", linetype = "dashed", size = 1) +
  labs(x = "log(value)", y = "Density") +
  ggtitle("Density Plot of log(value) in Premier League")+
  theme_light() +
  theme(panel.grid = element_blank())


# Serie A

# Log(Value) histogram
h_a <- ggplot(data = subset(df, league == "Serie A"), aes(x = log(value))) +
  geom_histogram(aes(y =..density..), fill = "#009490", color = "black", alpha = 0.6, bins = 15) +
  geom_density(color= "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(log(value))), color = "#32435b", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(log(value))), color = "#ff3131", linetype = "dashed", size = 1) +
  labs(x = "log(value)", y = "Density") +
  ggtitle("Density Plot of log(value) in Serie A")+
  theme_light() +
  theme(panel.grid = element_blank())


# La Liga

# Log(Value) histogram
h_l <- ggplot(data = subset(df, league == "La Liga"), aes(x = log(value))) +
  geom_histogram(aes(y =..density..), fill = "#009490", color = "black", alpha = 0.6, bins = 15) +
  geom_density(color= "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(log(value))), color = "#32435b", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(log(value))), color = "#ff3131", linetype = "dashed", size = 1) +
  labs(x = "log(value)", y = "Density") +
  ggtitle("Density Plot of log(value) in La Liga")+
  theme_light() +
  theme(panel.grid = element_blank())


# Ligue 1

# Log(Value) histogram
h_1 <- ggplot(data = subset(df, league == "Ligue 1"), aes(x = log(value))) +
  geom_histogram(aes(y =..density..), fill = "#009490", color = "black", alpha = 0.6, bins = 15) +
  geom_density(color= "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(log(value))), color = "#32435b", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(log(value))), color = "#ff3131", linetype = "dashed", size = 1) +
  labs(x = "log(value)", y = "Density") +
  ggtitle("Density Plot of log(value) in Ligue 1")+
  theme_light() +
  theme(panel.grid = element_blank())


# Bundesliga

# Log(Value) histogram
h_b <- ggplot(data = subset(df, league == "Bundesliga"), aes(x = log(value))) +
  geom_histogram(aes(y =..density..), fill = "#009490", color = "black", alpha = 0.6, bins = 15) +
  geom_density(color= "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(log(value))), color = "#32435b", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(log(value))), color = "#ff3131", linetype = "dashed", size = 1) +
  labs(x = "log(value)", y = "Density") +
  ggtitle("Density Plot of log(value) in Bundesliga")+
  theme_light() +
  theme(panel.grid = element_blank())

# Overlapping density graph for all leagues
leagues_density <- ggplot(df, aes(x = log(value), color = league)) +
  geom_density(size = 1.5, alpha = 0.7) +
  labs(x = "log(value)", y = "Density") +
  ggtitle("Density Plot of log(value) across Leagues") +
  theme_light() +
  theme(panel.grid = element_blank()) +
  scale_color_manual(values = c("#009490", "#161b1b", "#32435b", "#ff3131", "#bdc2c5"))

plot_grid(h_p, h_a, h_l, h_1, h_b, leagues_density, nrow = 2)
print(leagues_density)

combined_boxplot_leagues <- ggplot(df, aes(x = log(value))) +
  geom_boxplot(fill = "#009490") +
  geom_point(aes(x = mean(log(value)), y = 0), color = "#32435b", size = 3, shape = 18) +
  labs(x = "log(value)", y = "") +
  ggtitle("Density Plot of log(value) across Leagues") +
  theme_light() +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ league, scales = "free")

print(combined_boxplot_leagues)



## Continent

combined_continent_hist <- ggplot(df, aes(x = log(value))) +
  geom_histogram(aes(y = ..density..), fill = "#009490", color = "black", alpha = 0.6, bins = 15) +
  geom_density(color = "#161b1b", size = 0.9) +
  geom_vline(aes(xintercept = mean(log(value))), color = "#32435b", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(log(value))), color = "#ff3131", linetype = "dashed", size = 1) +
  labs(x = "log(value)", y = "Density") +
  ggtitle("Density Plot of log(value) across Continents") +
  theme_light() +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ continent, scales = "free")

combined_continent_boxplot <- ggplot(df, aes(x = log(value))) +
  geom_boxplot(fill = "#009490") +
  geom_point(aes(x = mean(log(value)), y = 0), color = "#32435b", size = 3, shape = 18) +
  labs(x = "log(value)", y = "") +
  ggtitle("Density Plot of log(value) across Continents") +
  theme_light() +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ continent, scales = "free")

print(combined_continent_hist)

print(combined_continent_boxplot)

# Overlapping densities plot for all continents
continent_density <- ggplot(df, aes(x = log(value), color = continent)) +
  geom_density(size = 1.5, alpha = 0.7) +
  labs(x = "log(value)", y = "Density") +
  ggtitle("Density Plot of log(value) across Continents") +
  theme_light() +
  theme(panel.grid = element_blank()) +
  scale_color_manual(values = c("#009490", "#161b1b", "#32435b", "#ff3131", "#bdc2c5", "#6ce5e8"))

print(continent_density)

plot_grid(combined_continent_hist, continent_density)


## Foot

combined_foot_hist <- ggplot(df, aes(x = log(value))) +
  geom_histogram(aes(y = ..density..), fill = "#009490", color = "black", alpha = 0.6, bins = 15) +
  geom_density(color = "#161b1b", size = 0.9) +
  geom_vline(aes(xintercept = mean(log(value))), color = "#32435b", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(log(value))), color = "#ff3131", linetype = "dashed", size = 1) +
  labs(x = "log(value)", y = "Density") +
  ggtitle("Density Plot of log(value) across Foot") +
  theme_light() +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ foot, scales = "free", labeller = labeller(foot = c("0" = "Left", "1" = "Right", "both" = "Both")))

combined_foot_boxplot <- ggplot(df, aes(x = log(value))) +
  geom_boxplot(fill = "#009490") +
  geom_point(aes(x = mean(log(value)), y = 0), color = "#32435b", size = 3, shape = 18) +
  labs(x = "log(value)", y = "") +
  ggtitle("Density Plot of log(value) across Foot") +
  theme_light() +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ foot, scales = "free", labeller = labeller(foot = c("0" = "Left", "1" = "Right", "both" = "Both")))

print(combined_foot_hist)

print(combined_foot_boxplot)

# Overlapping density graph for all Foot
foot_density <- ggplot(df, aes(x = log(value), color = foot)) +
  geom_density(size = 1.5, alpha = 0.7) +
  labs(x = "log(value)", y = "Density") +
  ggtitle("Density Plot of log(value) across Foot") +
  theme_light() +
  theme(panel.grid = element_blank()) +
  scale_color_manual(values = c("#009490", "#32435b", "#ff3131"), labels = c("0" = "Left", "1" = "Right", "both" = "Both"))

print(foot_density)

plot_grid(combined_foot_hist, foot_density)



#########################################################################
#    Exploratory Data Analysis (EDA) after the division by positions
#########################################################################

df_df <- df[df$position=="DF",]
dim(df_df)

mf_df <- df[df$position=="MF",]
dim(mf_df)

fw_df <- df[df$position=="FW",]
dim(fw_df)

df_df <- df_df[,-4]
rownames(df_df) <- NULL

mf_df <- mf_df[,-4]
rownames(mf_df) <- NULL

fw_df <- fw_df[,-4]
rownames(fw_df) <- NULL

summary(df_df)
summary(mf_df)
summary(fw_df)


## Defenders

# Log(Value) histogram
h_df <- ggplot(df_df, aes(x = log(df_df$value))) +
  geom_histogram(aes(y =..density..), fill="#009490", color="black", alpha=0.6, bins = 15) +
  geom_density(color = "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(log(df_df$value))), color="#32435b",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(log(df_df$value))), color="#ff3131",linetype="dashed", size=1) +
  labs(x ="log(value)", y="Density")+
  ggtitle("Histogram of value (Defenders)")+
  theme_light() +
  theme(panel.grid = element_blank())

# Log(Value) boxplot
b_df <- ggplot(df_df, aes(x = log(df_df$value)))+
  geom_boxplot(fill = "#009490")+
  geom_point(aes(x= mean(log(df_df$value)), y=0), color="#32435b")+
  labs(x ="log(value)", y = " ")+
  ggtitle("Boxplot of value (Defenders)")+
  theme_light() +
  theme(panel.grid = element_blank())

# Age histogram
h_df_age <- ggplot(df_df, aes(x = df_df$age)) +
  geom_histogram(aes(y =..density..), fill="#009490", color="black", alpha=0.6, bins = 15) +
  geom_density(color = "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(df_df$age)), color="#32435b",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(df_df$age)), color="#ff3131",linetype="dashed", size=1) +
  labs(x ="age", y="Density")+
  ggtitle("Histogram of age (Defenders)")+
  theme_light() +
  theme(panel.grid = element_blank())

# Age boxplot
b_df_age <- ggplot(df_df, aes(x = df_df$age))+
  geom_boxplot(fill = "#009490")+
  geom_point(aes(x= mean(df_df$age), y=0), color="#32435b")+
  labs(x ="age", y = " ")+
  ggtitle("Boxplot of age (Defenders)")+
  theme_light() +
  theme(panel.grid = element_blank())

plot_grid(h_df_age, b_df_age)

# Continent
c_df <- ggplot(df_df, aes(x = df_df$continent)) +
  geom_bar(color="black", fill="#009490")+
  labs(x ="continent", y="Number of players")+
  ggtitle("Continents (Defenders)")+
  theme_light() +
  theme(panel.grid = element_blank())

# Leagues
l_df <- ggplot(df_df, aes(x = df_df$league)) +
  geom_bar(color="black", fill="#009490")+
  labs(x ="league", y="Number of players")+
  ggtitle("Leagues (Defenders)")+
  theme_light() +
  theme(panel.grid = element_blank())

# Foot
f_df <- ggplot(df_df, aes(x = df_df$foot)) +
  geom_bar(color="black", fill="#009490")+
  labs(x ="foot", y="Number of players")+
  ggtitle("Foot (Defenders)")+
  theme_light() +
  theme(panel.grid = element_blank())+
  scale_x_discrete(labels = c("Left-footed", "Righ-footed", "Two-footed"))

plot_grid(l_df, c_df, f_df, nrow = 1)


## Midfielders

# Log(Value) histogram
h_mf <- ggplot(mf_df, aes(x = log(mf_df$value))) +
  geom_histogram(aes(y =..density..), fill="#009490", color="black", alpha=0.6, bins = 15) +
  geom_density(color = "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(log(mf_df$value))), color= "#32435b", linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(log(mf_df$value))), color="#ff3131", linetype="dashed", size=1) +
  labs(x ="log(value)", y="Density") +
  ggtitle("Histogram of value (Midfielders)") +
  theme_light() +
  theme(panel.grid = element_blank())

# Log(Value) boxplot
b_mf <- ggplot(mf_df, aes(x = log(mf_df$value)))+
  geom_boxplot(fill = "#009490")+
  geom_point(aes(x= mean(log(mf_df$value)), y=0), color="#32435b")+
  labs(x ="log(value)", y = " ")+
  ggtitle("Boxplot of value (Midfielders)")+
  theme_light() +
  theme(panel.grid = element_blank())

# Age histogram
h_mf_age <- ggplot(mf_df, aes(x = mf_df$age)) +
  geom_histogram(aes(y =..density..), fill="#009490", color="black", alpha=0.6, bins = 15) +
  geom_density(color = "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(mf_df$age)), color="#32435b",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(mf_df$age)), color="#ff3131",linetype="dashed", size=1) +
  labs(x ="age", y="Density")+
  ggtitle("Histogram of age (Midfielders)")+
  theme_light() +
  theme(panel.grid = element_blank())

# Age boxplot
b_mf_age <- ggplot(mf_df, aes(x = mf_df$age))+
  geom_boxplot(fill = "#009490")+
  geom_point(aes(x= mean(mf_df$age), y=0), color="#32435b")+
  labs(x ="age", y = " ")+
  ggtitle("Boxplot of age (Midfielders)")+
  theme_light() +
  theme(panel.grid = element_blank())

plot_grid(h_mf_age, b_mf_age)

# Continent
c_mf <- ggplot(mf_df, aes(x = mf_df$continent)) +
  geom_bar(color="black", fill="#009490")+
  labs(x ="continent", y="Number of players")+
  ggtitle("Continents (Midfielders)")+
  theme_light() +
  theme(panel.grid = element_blank())

# Leagues
l_mf <- ggplot(mf_df, aes(x = mf_df$league)) +
  geom_bar(color="black", fill="#009490")+
  labs(x ="league", y="Number of players")+
  ggtitle("Leagues (Midfielders)")+
  theme_light() +
  theme(panel.grid = element_blank())

# Foot
f_mf <- ggplot(mf_df, aes(x = mf_df$foot)) +
  geom_bar(color="black", fill="#009490")+
  labs(x ="foot", y="Number of players")+
  ggtitle("Foot (Midfielders)")+
  theme_light() +
  theme(panel.grid = element_blank())+
  scale_x_discrete(labels = c("Left-footed", "Righ-footed", "Two-footed"))

plot_grid(l_mf, c_mf, f_mf, nrow = 1)


## Forwards

# Log(Value) histogram
h_fw <- ggplot(fw_df, aes(x = log(fw_df$value))) +
  geom_histogram(aes(y =..density..), fill="#009490", color="black", alpha=0.6, bins = 15) +
  geom_density(color = "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(log(fw_df$value))), color="#32435b",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(log(fw_df$value))), color="#ff3131",linetype="dashed", size=1) +
  labs(x ="log(value)", y="Density")+
  ggtitle("Histogram of value (Forwards)")+
  theme_light() +
  theme(panel.grid = element_blank())

# Log(Value) boxplot
b_fw <- ggplot(fw_df, aes(x = log(fw_df$value)))+
  geom_boxplot(fill = "#009490")+
  geom_point(aes(x= mean(log(fw_df$value)), y=0), color="#32435b")+
  labs(x ="log(value)", y = " ")+
  ggtitle("Boxplot of value (Forwards)")+
  theme_light() +
  theme(panel.grid = element_blank())

# Age histogram
h_fw_age <- ggplot(fw_df, aes(x = fw_df$age)) +
  geom_histogram(aes(y =..density..), fill="#009490", color="black", alpha=0.6, bins = 15) +
  geom_density(color = "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(fw_df$age)), color="#32435b",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(fw_df$age)), color="#ff3131",linetype="dashed", size=1) +
  labs(x ="age", y="Density")+
  ggtitle("Histogram of age (Forwards)")+
  theme_light() +
  theme(panel.grid = element_blank())

# Age boxplot
b_fw_age <- ggplot(fw_df, aes(x = fw_df$age))+
  geom_boxplot(fill = "#009490")+
  geom_point(aes(x= mean(fw_df$age), y=0), color="#32435b")+
  labs(x ="age", y = " ")+
  ggtitle("Boxplot of age (Forwards)")+
  theme_light() +
  theme(panel.grid = element_blank())

plot_grid(h_fw_age, b_fw_age)

# Continent
c_fw <- ggplot(fw_df, aes(x = fw_df$continent)) +
  geom_bar(color="black", fill="#009490")+
  labs(x ="continent", y="Number of players")+
  ggtitle("Continents (Forwards)")+
  theme_light() +
  theme(panel.grid = element_blank())

# Leagues
l_fw <- ggplot(fw_df, aes(x = fw_df$league)) +
  geom_bar(color="black", fill="#009490")+
  labs(x ="league", y="Number of players")+
  ggtitle("Leagues (Forwards)")+
  theme_light() +
  theme(panel.grid = element_blank())

# Foot
f_fw <- ggplot(fw_df, aes(x = fw_df$foot)) +
  geom_bar(color="black", fill="#009490")+
  labs(x ="foot", y="Number of players")+
  ggtitle("Foot (Forwards)")+
  theme_light() +
  theme(panel.grid = element_blank())+
  scale_x_discrete(labels = c("Left-footed", "Righ-footed", "Two-footed"))

plot_grid(l_fw, c_fw, f_fw, nrow = 1)




# Log(Value) histogram
h_df <- ggplot(df_df, aes(x = log(df_df$value))) +
  geom_histogram(aes(y =..density..), fill="#009490", color="black", alpha=0.6, bins = 15) +
  geom_density(color = "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(log(df_df$value))), color="#32435b",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(log(df_df$value))), color="#ff3131",linetype="dashed", size=1) +
  labs(x ="log(value)", y="Density")+
  ggtitle("Defenders")+
  theme_light() +
  theme(panel.grid = element_blank())

h_mf <- ggplot(mf_df, aes(x = log(mf_df$value))) +
  geom_histogram(aes(y =..density..), fill="#009490", color="black", alpha=0.6, bins = 15) +
  geom_density(color = "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(log(mf_df$value))), color= "#32435b", linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(log(mf_df$value))), color="#ff3131", linetype="dashed", size=1) +
  labs(x ="log(value)", y="Density") +
  ggtitle("Midfielders") +
  theme_light() +
  theme(panel.grid = element_blank())

h_fw <- ggplot(fw_df, aes(x = log(fw_df$value))) +
  geom_histogram(aes(y =..density..), fill="#009490", color="black", alpha=0.6, bins = 15) +
  geom_density(color = "#161b1b", size=0.9)+
  geom_vline(aes(xintercept = mean(log(fw_df$value))), color="#32435b",linetype="dashed", size=1) +
  geom_vline(aes(xintercept = median(log(fw_df$value))), color="#ff3131",linetype="dashed", size=1) +
  labs(x ="log(value)", y="Density")+
  ggtitle("Forwards")+
  theme_light() +
  theme(panel.grid = element_blank())


# Comparison of the variable value in the various positions
plot_grid(h_df, h_mf, h_fw, nrow = 1)
plot_grid(h_df, h_mf, h_fw, b_df, b_mf, b_fw, nrow = 2)

# Comparison of the variable age in the various positions
plot_grid(h_df_age, b_df_age, h_mf_age, b_mf_age, h_fw_age, b_fw_age, nrow = 3)

# Comparison of the variables league, continent and foot in the various positions
plot_grid(l_df, c_df, f_df,l_mf, c_mf, f_mf,l_fw, c_fw, f_fw, nrow = 3)



# Relation between goals and log(value) for each position

g_vs_fw <- ggplot(data = subset(df, position == "FW"), aes(x = goals, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between goals and value for Forwards")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

g_vs_mf <- ggplot(data = subset(df, position == "MF"), aes(x = goals, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between goals and value for Midfielders")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

g_vs_df <- ggplot(data = subset(df, position == "DF"), aes(x = goals, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between goals and value for Defenders")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

plot_grid(g_vs_df, g_vs_mf, g_vs_fw, nrow = 1)



# Relation between assists and value for each position

a_vs_fw <- ggplot(data = subset(df, position == "FW"), aes(x = assists, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between assists and value for Forwards")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

a_vs_mf <- ggplot(data = subset(df, position == "MF"), aes(x = assists, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between assists and value for Midfielders")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

a_vs_df <- ggplot(data = subset(df, position == "DF"), aes(x = assists, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between assists and value for Defenders")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

plot_grid(a_vs_df, a_vs_mf, a_vs_fw, nrow = 1)


# Relation between age and value for each position

age_vs_fw <- ggplot(data = subset(df, position == "FW"), aes(x = age, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between age and value for Forwards")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

age_vs_mf <- ggplot(data = subset(df, position == "MF"), aes(x = age, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between age and value for Midfielders")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

age_vs_df <- ggplot(data = subset(df, position == "DF"), aes(x = age, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between age and value for Defenders")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

plot_grid(age_vs_df, age_vs_mf, age_vs_fw, nrow = 1)


# Relation between xg_per90 and value for each position

xg_per90_vs_fw <- ggplot(data = subset(df, position == "FW"), aes(x = xg_per90, y = log(value))) + 
  geom_jitter() +
  labs(title = "Forwards")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

xg_per90_vs_mf <- ggplot(data = subset(df, position == "MF"), aes(x = xg_per90, y = log(value))) + 
  geom_jitter() +
  labs(title = "Midfielders")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

xg_per90_vs_df <- ggplot(data = subset(df, position == "DF"), aes(x = xg_per90, y = log(value))) + 
  geom_jitter() +
  labs(title = "Defenders")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

plot_grid(xg_per90_vs_df, xg_per90_vs_mf, xg_per90_vs_fw, nrow = 1)


# Relation between minutes and value for each position

min_vs_fw <- ggplot(data = subset(df, position == "FW"), aes(x = minutes, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between minutes and value for Forwards")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

min_vs_mf <- ggplot(data = subset(df, position == "MF"), aes(x = minutes, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between minutes and value for Midfielders")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

min_vs_df <- ggplot(data = subset(df, position == "DF"), aes(x = minutes, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between minutes and value for Defenders")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

plot_grid(min_vs_df, min_vs_mf, min_vs_fw, nrow = 1)


# Relation between interceptions and value for each position

interceptions_vs_fw <- ggplot(data = subset(df, position == "FW"), aes(x = interceptions, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between interceptions and value for Forwards")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

interceptions_vs_mf <- ggplot(data = subset(df, position == "MF"), aes(x = interceptions, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between interceptions and value for Midfielders")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

interceptions_vs_df <- ggplot(data = subset(df, position == "DF"), aes(x = interceptions, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between interceptions and value for Defenders")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

plot_grid(interceptions_vs_df, interceptions_vs_mf, interceptions_vs_fw, nrow = 1)


# Relation between carry_distance and value for each position

carry_distance_vs_fw <- ggplot(data = subset(df, position == "FW"), aes(x = carry_distance, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between carry_distance and value for Forwards")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

carry_distance_vs_mf <- ggplot(data = subset(df, position == "MF"), aes(x = carry_distance, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between carry_distance and value for Midfielders")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

carry_distance_vs_df <- ggplot(data = subset(df, position == "DF"), aes(x = carry_distance, y = log(value))) + 
  geom_jitter() +
  labs(title = "Relation between carry_distance and value for Defenders")+
  geom_smooth(method = lm, formula = y~x)+
  theme_light() +
  theme(panel.grid = element_blank())

plot_grid(carry_distance_vs_df, carry_distance_vs_mf, carry_distance_vs_fw, nrow = 1)
