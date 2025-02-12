library('ggplot2')
library('googlesheets4')
library('tidyverse')

#Load Wingspan Sheet 
wingspan <- read_sheet("https://docs.google.com/spreadsheets/d/1h_5Zr_wTf3Gvvsu8-Od-Sks-CUM2QllxdJ0aX-8pC18/edit?gid=0#gid=0")

wingspan1 <- wingspan %>% 
  transmute(Game, Winner, Day, Ben, Kandace)
# Remove unwanted Rows from spreadsheet
wingspan1 <- wingspan1[-c(1, 2,3, 4, 5), ]

#Fun Statistics
median(wingspan1$Kandace)
median(wingspan1$Ben)
count(wingspan1, Kandace > Ben & Ben > 120)
max(wingspan1$Ben)
sd(wingspan1$Ben)

biggest_win <- wingspan1 %>% 
  mutate(difference = ifelse(Ben > Kandace, Ben - Kandace, Kandace - Ben)) %>% 
  arrange(desc(difference))

tight_games <- biggest_win %>% 
  filter(difference < 5) %>% 
  arrange(Winner)

Kandace_close <- tight_games %>% 
  filter(Winner == "Kandace")
Ben_close <- tight_games %>% 
  filter(Winner == "Ben")
  
together <- wingspan1 %>% 
  mutate(together = Ben + Kandace) %>% 
  arrange(desc(together))

#Oceanea___________________________________-

# Convert the comparison date to POSIXct
comparison_date <- as.POSIXct("2024-09-29")

# Subset the dataset
Oceanea <- wingspan1 %>%
  filter(Game > 121)

ggplot(data = Oceanea) + 
  aes(x = Game, y = Ben) + 
  geom_bar(stat = "identity", fill = "skyblue")+
  theme_minimal() + 
  labs(title = "Ben's Oceanea Scores Over Time", x = "Game", y = "Score")

ggplot(data = Oceanea) + 
  aes(x = Game) + 
  geom_col(aes(y = Ben, fill = "Ben"), width = 0.4) + 
  geom_col(aes(y = Kandace, fill = "Kandace"), width = 0.4) + 
  scale_fill_manual(values = c("skyblue", "purple")) + 
  theme_minimal() + labs(title = "Ben's and Kandace's Oceanea Scores Over Time", x = "Game", y = "Score", fill = "Player")

#Reshape the data for easier plotting 
Oceanea_long <- Oceanea %>% 
  gather(key = "Player", value = "Score", Ben, Kandace) 
# Create the bar plot with side-by-side bars 
ggplot(data = Oceanea_long,  aes(x = Game, y = Score, fill = Player)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme_minimal() + 
  labs(title = "Ben's and Kandace's Oceanea Scores Over Time", x = "Game", y = "Score", fill = "Player") + scale_fill_manual(values = c("skyblue", "purple"))

#_______________________________________

# Define score bins for Kandace 
KLG_range <- wingspan1 %>% 
  mutate(Kandace_Score_Range = cut(Kandace, breaks = seq(50, 140, by = 5), include.lowest = TRUE))

proportion_data <- KLG_range %>% 
  group_by(Kandace_Score_Range) %>% 
  summarise(Ben_Wins = sum(Winner == "Ben"), Total_Games = n(), Proportion_Ben_Wins = Ben_Wins / Total_Games)

ggplot(proportion_data, aes(x = Kandace_Score_Range, y = Proportion_Ben_Wins)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() + 
  labs(title = "Proportion of Games Won by Ben Based on Kandace's Scores", x = "Kandace's Score Range", y = "Proportion of Ben's Wins")

BCS_range <- wingspan1 %>% 
  mutate(Ben_Score_Range = cut(Ben, breaks = seq(50, 140, by = 5), include.lowest = TRUE))

prop_data2 <- BCS_range %>% 
  group_by(Ben_Score_Range) %>% 
  summarise(Kandace_Wins = sum(Winner == "Kandace"), Total_Games = n(), Proportion_Kandace_Wins = Kandace_Wins/ Total_Games)

ggplot(prop_data2, aes(x = Ben_Score_Range, y = Proportion_Kandace_Wins)) +
  geom_bar(stat = "identity", fill = "purple")+
  theme_minimal()+
  labs(title = "Proportion of Games won by Kandace Based on Ben's Scores", x = "Ben's Score Range", y = "Proportion of Kandace's Wins")

ggplot(proportion_data, aes(x = Kandace_Score_Range, y = Proportion_Ben_Wins, group = 1)) + 
  geom_line(color = "blue", size = 1) + geom_point(color = "blue", size = 3) + 
  theme_minimal() + labs(title = "Proportion of Games Won by Ben Based on Kandace's Scores", x = "Kandace's Score Range", y = "Proportion of Ben's Wins")

ggplot(proportion_data, aes(x = Kandace_Score_Range, y = Proportion_Ben_Wins, group = 1)) + 
  geom_line(color = "green", linewidth = 1) + 
  scale_y_log10() + 
  theme_minimal() + labs(title = "Proportion of Games Won by Ben Based on Kandace's Scores", x = "Kandace's Score Range", y = "Proportion of Ben's Wins (Log Scale)")

combined_plot <- ggarrange(plot_ben, plot_kandace, ncol = 2, nrow = 1, labels = c("A", "B"))
view(combined_plot)

#____________________________________

# Assuming prop_data2 and proportion_data are your data frames
# Add a column to identify the type of data
prop_data2 <- prop_data2 %>%
  mutate(Type = "Kandace Wins Based on Ben's Scores")

proportion_data <- proportion_data %>%
  mutate(Type = "Ben Wins Based on Kandace's Scores")

# Combine the data frames
combined_data <- bind_rows(
  prop_data2 %>% rename(Score_Range = Ben_Score_Range, Proportion_Wins = Proportion_Kandace_Wins),
  proportion_data %>% rename(Score_Range = Kandace_Score_Range, Proportion_Wins = Proportion_Ben_Wins)
)

ggplot(combined_data, aes(x = Score_Range, y = Proportion_Wins, fill = Type)) + 
  geom_bar(stat = "identity") + facet_wrap(~ Type, scales = "free") + 
  theme_minimal() + 
  scale_fill_manual(values = c("purple", "skyblue")) + labs(title = "Proportion of Games Won by Ben and Kandace", x = "Score Range", y = "Proportion of Wins")

#Log Odds_______________________________________________________________

wingspan2 <- wingspan1%>% 
  mutate(Ben_Wins = ifelse(Winner == "Ben", 1, 0))

# Fit the logistic regression model
model <- glm(Ben_Wins ~ Kandace, data = wingspan2, family = binomial)

# Summary of the model
summary(model)

# Extract the coefficients
coefficients <- coef(model)

# Assuming Kandace's score is 80
kandace_score <- 90

# Calculate the log odds
log_odds <- coefficients[1] + coefficients[2] * kandace_score

# Print the log odds
print(log_odds)

# Calculate the probability
probability <- exp(log_odds) / (1 + exp(log_odds))

# Print the probability
print(probability)

#__________________________________

# Generate a sequence of Kandace's scores
kandace_scores <- seq(min(wingspan2$Kandace), max(wingspan2$Kandace), by = 1)

# Predict probabilities for the sequence of Kandace's scores
predicted_probabilities <- predict(model, newdata = data.frame(Kandace = kandace_scores), type = "response")

# Combine the observed data with the predicted probabilities
plot_data <- data.frame(Kandace = kandace_scores, Probability = predicted_probabilities)

ggplot(wingspan2, aes(x = Kandace, y = Ben_Wins)) +
  geom_point(aes(color = as.factor(Ben_Wins)), size = 3) +
  geom_line(data = plot_data, aes(x = Kandace, y = Probability), color = "blue", size = 1) +
  scale_color_manual(values = c("red", "green"), labels = c("Kandace Wins", "Ben Wins")) +
  theme_minimal() +
  labs(title = "Probability of Ben Winning Based on Kandace's Score",
       x = "Kandace's Score",
       y = "Probability of Ben Winning",
       color = "Outcome")
#______________________________________________--

wingspan3 <- wingspan1%>% 
  mutate(Kandace_Wins = ifelse(Winner == "Kandace", 1, 0))

# Fit the logistic regression model
model <- glm(Kandace_Wins ~ Ben, data = wingspan3, family = binomial)

# Summary of the model
summary(model)

# Extract the coefficients
coefficients <- coef(model)

# Assuming Kandace's score is 80
Ben_Score <- 90

# Calculate the log odds
log_odds <- coefficients[1] + coefficients[2] * Ben_Score

# Print the log odds
print(log_odds)

# Calculate the probability
probability <- exp(log_odds) / (1 + exp(log_odds))

# Print the probability
print(probability)


# Generate a sequence of Kandace's scores
Ben_Scores <- seq(min(wingspan3$Ben), max(wingspan3$Ben), by = 1)

# Predict probabilities for the sequence of Kandace's scores
predicted_probabilities <- predict(model, newdata = data.frame(Ben = Ben_Scores), type = "response")

ggplot(wingspan3, aes(x = Ben, y = Kandace_Wins)) +
  geom_point(aes(color = as.factor(Kandace_Wins)), size = 3) +
  geom_line(data = plot_data, aes(x = Ben, y = Probability), color = "blue", size = 1) +
  geom_ribbon(data = plot_data, aes(x = Ben, ymin = Lower, ymax = Upper), alpha = 0.2, fill = "blue") +
  scale_color_manual(values = c("red", "green"), labels = c("Kandace Wins", "Ben Wins")) +
  theme_minimal() +
  labs(title = "Probability of Kandace Winning Based on Ben's Score", x = "Ben's Score", y = "Probability of Kandace Winning", color = "Outcome")

