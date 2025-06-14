# Endpoint-focus exercise vs. Present-focus exercise
# Behavioral data analysis & plotting
# This script requires one file: "rawdata.xlsx"
# Programmed by Feng XIAO (updated on 2025.6.14)
############################################################################################################

### Preparation
## Load required packages for analysis
package_list <- c('car','tidyr','dplyr','readxl','effsize','e1071','lme4','lmerTest',
                  'emmeans','ggplot2','patchwork')
lapply(package_list, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Data input
rd <- read_excel('rawdata.xlsx', sheet = 'rawdata', na = "---")
rd_endpoint <- filter(rd, Group == 'endpoint')
rd_present <- filter(rd, Group == 'present')

### Demographic data analysis
## Overall
## Age
mean(rd$Age) #26.61
sd(rd$Age) #8.49
mean((filter(rd, Gender == 1))$Age) #male: 28.96
sd((filter(rd, Gender == 1))$Age) #male: 8.11
mean((filter(rd, Gender == 2))$Age) #female: 25.38
sd((filter(rd, Gender == 2))$Age) #female: 8.47
t.test((filter(rd, Gender == 1))$Age, (filter(rd, Gender == 2))$Age,
       paired =FALSE, alternative = c("two.sided"), var.equal=FALSE,
       conf.level=0.95) #Age: Male>female, p=.014
## Gender
dim(filter(rd, Gender == 1))[1] #51 males
dim(filter(rd, Gender == 2))[1] #97 females

## Endpoint-focus group
## Age
mean(rd_endpoint$Age) #26.72
sd(rd_endpoint$Age) #8.69
mean((filter(rd_endpoint, Gender == 1))$Age) #male: 29.70
sd((filter(rd_endpoint, Gender == 1))$Age) #male: 9.16
mean((filter(rd_endpoint, Gender == 2))$Age) #female: 24.68
sd((filter(rd_endpoint, Gender == 2))$Age) #female: 7.82
t.test((filter(rd_endpoint, Gender == 1))$Age, (filter(rd_endpoint, Gender == 2))$Age,
       paired =FALSE, alternative = c("two.sided"), var.equal=FALSE,
       conf.level=0.95) #Age: Male>female, p=.017
## Gender
dim(filter(rd_endpoint, Gender == 1))[1] #30 males
dim(filter(rd_endpoint, Gender == 2))[1] #44 females

## Present-focus group
## Age
mean(rd_present$Age) #26.51
sd(rd_present$Age) #8.35
mean((filter(rd_present, Gender == 1))$Age) #male: 27.90
sd((filter(rd_present, Gender == 1))$Age) #male: 6.39
mean((filter(rd_present, Gender == 2))$Age) #female: 25.96
sd((filter(rd_present, Gender == 2))$Age) #female: 9.01
t.test((filter(rd_present, Gender == 1))$Age, (filter(rd_present, Gender == 2))$Age,
       paired =FALSE, alternative = c("two.sided"), var.equal=FALSE,
       conf.level=0.95) #Age: Male=female, p=.303
## Gender
dim(filter(rd_present, Gender == 1))[1] #21 males
dim(filter(rd_present, Gender == 2))[1] #53 females

### Behavioral data analysis
## Exercise involvement
mean(rd$MediInvol) #overall: 7.69
sd(rd$MediInvol) #overall: 1.24
mean(rd_endpoint$MediInvol) #Endpoint: 7.50
sd(rd_endpoint$MediInvol) #Endpoint: 1.16
mean(rd_present$MediInvol) #Present: 7.88
sd(rd_present$MediInvol) #Present: 1.30
t.test(rd_endpoint$MediInvol, rd_present$MediInvol, paired =FALSE,
       alternative = c("two.sided"), var.equal=FALSE,
       conf.level=0.95) #involvement did not differ between groups

## Delay discounting
# Analysis
rd$logk_pre <- log(rd$k_pre)
rd$logk_post1 <- log(rd$k_post1)
rd$logk_post2 <- log(rd$k_post2)
rd$logk_post3 <- log(rd$k_post3)
rd_long <- rd %>%
  pivot_longer(cols = starts_with("logk"), 
               names_to = "time", 
               values_to = "logk", 
               names_prefix = "logk_") 
rd_long$time <- factor(rd_long$time, levels = c("pre", "post1", "post2", "post3"))
rd_long$Group <- factor(rd_long$Group, levels = c("endpoint","present"))
rd_long$Group <- relevel(rd_long$Group, ref = "present")
model_logk <- lmer(logk ~ Group * time + Gender * Age + (1|SubjNum), data = rd_long)
summary(model_logk) #linear mixed effects model
emm1 <- emmeans(model_logk, ~ Group | time) #Between-Group pairwise
contrast1 <- contrast(emm1, method = "pairwise")
emm2 <- emmeans(model_logk, ~ time | Group) #Within-Group pairwise
contrast2 <- contrast(emm2, method = "pairwise")

subject_sd <- sqrt(VarCorr(model_logk)$`SubjNum`[1])
residual_sd <- attr(VarCorr(model_logk), "sc")  # Residual standard deviation
pooled_sd <- sqrt(subject_sd^2 + residual_sd^2) # Total pooled SD

# Function to calculate Cohen's d
calculate_d <- function(contrast) {
  contrast_summary <- as.data.frame(summary(contrast))
  contrast_summary$d <- contrast_summary$estimate / pooled_sd
  return(contrast_summary)
}

d_between <- calculate_d(contrast1)
d_within <- calculate_d(contrast2)
# Plotting
emmeans_logk_df <- as.data.frame(emmeans(model_logk, ~ Group * time))
emmeans_logk_df$time <- factor(emmeans_logk_df$time, levels = c('pre','post1','post2','post3'))
p_logk<-ggplot(emmeans_logk_df, aes(x = time, y = emmean, color = Group, group = Group)) +
  geom_line(position = position_dodge(width = 0.3)) + 
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.4,
                position = position_dodge(width = 0.3)) +  
  labs(x = NULL, y = "Marginal means of log k-value") +
  ggtitle(NULL) +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 7, color = "black"),
    axis.text = element_text(size = 7, color = "black"),
    panel.background = element_rect(fill = "transparent"),
    legend.position = 'none'
  ) +
  scale_color_manual(values = c("endpoint" = "#B22222", "present" = "#4169E1")) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(-8, -4),
    breaks = seq(-8, -4, by = 1)
  ) +
  plot_layout(nrow = 1) +
  plot_annotation(title = '(A) Delay discounting',
                  theme = theme(plot.title = element_text(size = 7, color = 'black',
                                                          face = 'bold')))
ggsave("pic_logk.pdf", plot = p_logk, width = 2, height = 2)

## Time perception
# Analysis
model_time <- lm(TenPercp ~ Group + Gender * Age, data = rd_long)
summary(model_time) #endpoint<present, faster, p<.001
# Plotting
emmeans_time_df <- as.data.frame(emmeans(model_time, ~ Group))
emmeans_time_df$Group <- factor(emmeans_time_df$Group, 
                                     levels = c("endpoint","present"))
p_time <- ggplot(emmeans_time_df, aes(x = Group, y = emmean, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +  
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, position = position_dodge(width = 0.6)) + 
  labs(x = NULL, y = "Marginal means of rating") +  
  ggtitle(NULL) +  
  theme(
    axis.line = element_line(colour = "black", size = 0.4),  
    axis.title = element_text(size = 7, color = "black"),
    axis.text = element_text(size = 7, color = "black"),
    panel.background = element_rect(fill = "transparent"), 
    legend.position = 'none'  
  ) +
  scale_fill_manual(values = c("present" = "#4169E1", "endpoint" = "#B22222")) +
  scale_y_continuous(expand = c(0, 0), limits = c(1, 9), oob = scales::squish,
                     breaks = c(1, seq(1, 9, by = 2))) +
  plot_layout(nrow = 1) +
  plot_annotation(title = '(B) Time perception',
                  theme = theme(plot.title = element_text(size = 7, color = 'black',
                                                          face = 'bold'))) 
ggsave("pic_time.pdf", plot = p_time, width = 2, height = 2)

## Temporal preference
# Analysis
rd_long2 <- rd %>%
  pivot_longer(cols = starts_with("Account"), 
               names_to = "temporal_preference", 
               values_to = "Account", 
               names_prefix = "Account_") 
rd_long2$Group <- factor(rd_long2$Group, levels = c("endpoint","present"))
rd_long2$Group <- relevel(rd_long2$Group, ref = "present")
# Within Group
model_tp_end <- lm(Account ~ temporal_preference + Gender * Age, data = rd_long2[rd_long2$Group == "endpoint", ])
summary(model_tp_end) 
emm <- emmeans(model_tp_end, ~ temporal_preference)
contrast(emm, method = "pairwise") #Endpoint: ShortTerm<LongTerm, p<.001; ShortTerm<MidTerm
model_tp_mindfulness <- lm(Account ~ temporal_preference + Gender * Age, data = rd_long2[rd_long2$Group == "present", ])
summary(model_tp_mindfulness) 
emm <- emmeans(model_tp_mindfulness, ~ temporal_preference)
contrast(emm, method = "pairwise") #Present: ShortTerm<LongTerm, p<.001; MidTerm<LongTerm, p=.003
# Between Groups
model_tp <- lm(Account ~ Group * temporal_preference + Gender * Age, data = rd_long2)
summary(model_tp) #MidTerm: endpoint>present, p=.044
# Between Groups (combining MidTerm and LongTerm)
rd_long2$tp2 <- factor(ifelse(rd_long2$temporal_preference == "ShortTerm", 
                              "ShortTerm", 
                              "LongMid"),
                       levels = c("LongMid", "ShortTerm"))
model_tp <- lm(Account ~ Group * tp2 + Gender * Age, data = rd_long2)
summary(model_tp)
emm <- emmeans(model_tp, ~ Group | tp2)
contrast(emm, method = "pairwise")
model_tp_orig <- lm(Account ~ Group * temporal_preference + Gender * Age,
                    data = rd_long2)
summary(model_tp_orig)
# Plotting1
emmeans_tp_df <- as.data.frame(emmeans(model_tp_orig, ~ Group * temporal_preference))
emmeans_tp_df$temporal_preference <- factor(emmeans_tp_df$temporal_preference, 
                                            levels = c("ShortTerm", "MidTerm", "LongTerm"))
p_tp <- ggplot(emmeans_tp_df, aes(x = temporal_preference, y = emmean, color = Group, group = Group)) +
  geom_line(position = position_dodge(width = 0.3)) + 
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.4,
                position = position_dodge(width = 0.3)) + 
  labs(x = NULL, y = "Marginal means of allocation (%)") +
  ggtitle(NULL) +  
  theme(
    axis.line = element_line(colour = "black", size = 0.4), 
    axis.title = element_text(size = 7, color = "black"),
    axis.text = element_text(size = 7, color = "black"),
    panel.background = element_rect(fill = "transparent"), 
    legend.position = 'none'
  ) +
  scale_color_manual(values = c("endpoint" = "#B22222", "present" = "#4169E1")) +  
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1),
    oob = scales::squish,
    breaks = seq(0, 1, by = 0.25),
    labels = scales::label_percent(scale = 100) 
  ) +
  plot_layout(nrow = 1) +
  plot_annotation(title = '(C) Temporal preference',
                  theme = theme(plot.title = element_text(size = 7, color = 'black',
                                                          face = 'bold'))) 
ggsave("pic_tp.pdf", plot = p_tp, width = 2, height = 2)
# Plotting2
rd_long2$temporal_preference <- factor(rd_long2$temporal_preference,
                                       levels = c('ShortTerm','MidTerm','LongTerm'))
p_int <- ggplot(rd_long2, aes(x = temporal_preference, y = Account, color = Group)) +
  geom_point(aes(shape = Group), size = 0.8, alpha = 0.8) +  
  geom_smooth(method = "lm", aes(group = Group, fill = Group), se = TRUE, 
              linewidth = 1, linetype = "solid") + 
  labs(x = "Temporal preference", y = "Account allocation (%)") +
  ggtitle(NULL) +  
  theme(
    axis.line = element_line(colour = "black", size = 0.4),  
    axis.title = element_text(size = 7, color = "black"),
    axis.text = element_text(size = 7, color = "black"),
    panel.background = element_rect(fill = "transparent"), 
    legend.position = 'none'  
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 1),
    oob = scales::squish,
    breaks = seq(0, 1, by = 0.25),
    labels = scales::label_percent(scale = 100) 
  ) +
  scale_color_manual(values = c("endpoint" = "#B22222", "present" = "#4169E1")) +
  scale_fill_manual(values = c("endpoint" = "#B22222", "present" = "#4169E1")) +  
  plot_layout(nrow = 1) +
  plot_annotation(title = '(D) Interaction effect of Group on temporal preference',
                  theme = theme(plot.title = element_text(size = 7, color = 'black',
                                                          face = 'bold'))) 
ggsave("pic_int.pdf", plot = p_int, width = 2, height = 2)

## Emotional response
# Endpoint-focus
# Analysis
rd_endpoint_long1 <- rd_endpoint %>%
  pivot_longer(cols = starts_with("Emotion"), 
               names_to = "emo_type", 
               values_to = "Emotion", 
               names_prefix = "Emotion") 
rd_endpoint_long2 <- rd_endpoint %>%
  pivot_longer(cols = starts_with("Rating"), 
               names_to = "emo_rating", 
               values_to = "Rating", 
               names_prefix = "Rating") 
rd_endpoint_long1 <- rd_endpoint_long1 %>%
  select(SubjNum, Emotion)
rd_endpoint_long2 <- rd_endpoint_long2 %>%
  select(SubjNum, Rating)
rd_endpoint_long <- na.omit(data.frame(SubjNum = rd_endpoint_long1$SubjNum,
                               Emotion_type = rd_endpoint_long1$Emotion,
                               Emotion_rating = rd_endpoint_long2$Rating))
emotion_percentage_endpoint <- rd_endpoint_long %>%
  group_by(Emotion_type) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(desc(percentage)) #top3: peace, relaxation, sadness
emotion_rating_mean <- rd_endpoint_long %>%
  group_by(Emotion_type) %>%
  summarize(mean_rating = mean(Emotion_rating, na.rm = TRUE)) 
emotion_percentage_endpoint <- merge(emotion_percentage_endpoint, emotion_rating_mean, by = "Emotion_type")
# Plotting
p_emo_end <- ggplot(emotion_percentage_endpoint, aes(x = reorder(Emotion_type, -percentage), y = percentage, fill = Emotion_type)) +
  geom_bar(stat = "identity", fill = "#B22222", show.legend = FALSE) +
  geom_line(aes(y = mean_rating * 10, group = 1), color = "#B2222250", size = 0.4) + 
  geom_point(aes(y = mean_rating * 10), color = "#B22222", size = 0.8, shape = 16) +
  labs(x = "Self-reported emotion", y = "Percentage (%)") +
  ggtitle(NULL) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = seq(0, 100, by = 25),
                     sec.axis = sec_axis(
                       trans = ~ . / 10,  
                       name = "Rating", 
                       breaks = seq(1, 9, by = 2)  
                     )) +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),  
    axis.title = element_text(size = 7, color = "black"),
    axis.text = element_text(size = 7, color = "black"),
    panel.background = element_rect(fill = "transparent"), 
    legend.position = 'none'  
  ) +
  plot_layout(nrow = 1) +
  plot_annotation(title = '(A) Emotional experiences (Endpoint-focus)',
                  theme = theme(plot.title = element_text(size = 7, color = 'black',
                                                          face = 'bold')))
ggsave("pic_emo_endpoint.pdf", plot = p_emo_end, width = 4, height = 2)
#Present-focus
# Analysis
rd_present_long1 <- rd_present %>%
  pivot_longer(cols = starts_with("Emotion"), 
               names_to = "emo_type", 
               values_to = "Emotion", 
               names_prefix = "Emotion") 
rd_present_long2 <- rd_present %>%
  pivot_longer(cols = starts_with("Rating"), 
               names_to = "emo_rating", 
               values_to = "Rating", 
               names_prefix = "Rating") 
rd_present_long1 <- rd_present_long1 %>%
  select(SubjNum, Emotion)
rd_present_long2 <- rd_present_long2 %>%
  select(SubjNum, Rating)
rd_present_long <- na.omit(data.frame(SubjNum = rd_present_long1$SubjNum,
                                      Emotion_type = rd_present_long1$Emotion,
                                      Emotion_rating = rd_present_long2$Rating))
emotion_percentage_present <- rd_present_long %>%
  group_by(Emotion_type) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(desc(percentage)) #top3: peace, relaxation, sadness
emotion_rating_mean <- rd_present_long %>%
  group_by(Emotion_type) %>%
  summarize(mean_rating = mean(Emotion_rating, na.rm = TRUE)) 
emotion_percentage_present <- merge(emotion_percentage_present, emotion_rating_mean, by = "Emotion_type")
# Plotting
p_emo_present <- ggplot(emotion_percentage_present, aes(x = reorder(Emotion_type, -percentage), y = percentage, fill = Emotion_type)) +
  geom_bar(stat = "identity", fill = "#4169E1", show.legend = FALSE) +
  geom_line(aes(y = mean_rating * 10, group = 1), color = "#4169E150", size = 0.4) + 
  geom_point(aes(y = mean_rating * 10), color = "#4169E1", size = 0.8, shape = 16) +
  labs(x = "Self-reported emotion", y = "Percentage (%)") +
  ggtitle(NULL) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100), breaks = seq(0, 100, by = 25),
                     sec.axis = sec_axis(
                       trans = ~ . / 10,  
                       name = "Rating", 
                       breaks = seq(1, 9, by = 2)  
                     )) +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),  
    axis.title = element_text(size = 7, color = "black"),
    axis.text = element_text(size = 7, color = "black"),
    panel.background = element_rect(fill = "transparent"), 
    legend.position = 'none'  
  ) +
  plot_layout(nrow = 1) +
  plot_annotation(title = '(B) Emotional experiences (Present-focus)',
                  theme = theme(plot.title = element_text(size = 7, color = 'black',
                                                          face = 'bold')))
ggsave("pic_emo_present.pdf", plot = p_emo_present, width = 3, height = 2)
# Comparisons between Groups
rd_endpoint_long_filtered <- rd_endpoint_long %>%
  filter(Emotion_type %in% c("peace", "relaxation", "sadness")) %>%
  select(SubjNum, Emotion_type, Emotion_rating)
rd_present_long_filtered <- rd_present_long %>%
  filter(Emotion_type %in% c("peace", "relaxation", "sadness")) %>%
  select(SubjNum, Emotion_type, Emotion_rating)
rd_endpoint_long_filtered$Group <- "Endpoint-focus"
rd_present_long_filtered$Group <- "Present"
combined_emo_data <- bind_rows(rd_endpoint_long_filtered, rd_present_long_filtered)
comparison_emo_results <- combined_emo_data %>%
  group_by(Emotion_type) %>%
  summarize(
    p_value = t.test(Emotion_rating ~ Group, paired = FALSE)$p.value, 
    mean_diff = mean(Emotion_rating[Group == "Endpoint-focus"]) - mean(Emotion_rating[Group == "Present"]) 
  ) #NS

## Chi-square analysis for emotions (peace, relaxation, and sadness)
endpoint_counts <- emotion_percentage_endpoint %>%
  filter(Emotion_type %in% c("peace", "relaxation", "sadness")) %>%
  arrange(Emotion_type) %>%
  pull(count)
present_counts <- emotion_percentage_present %>%
  filter(Emotion_type %in% c("peace", "relaxation", "sadness")) %>%
  arrange(Emotion_type) %>%
  pull(count)
emotions <- c("peace", "relaxation", "sadness")
for (emo in emotions) {
  a <- emotion_percentage_endpoint %>% filter(Emotion_type == emo) %>% pull(count)
  b <- emotion_percentage_present  %>% filter(Emotion_type == emo) %>% pull(count)

  total_endpoint <- sum(emotion_percentage_endpoint$count)
  total_present  <- sum(emotion_percentage_present$count)
  c <- total_endpoint - a
  d <- total_present  - b
  
  tbl <- matrix(c(a, c, b, d), nrow = 2, byrow = TRUE,
                dimnames = list(Group = c("endpoint", "present"),
                                Emotion = c(emo, paste0("not_", emo))))
  cat("\n====", emo, "====\n")
  print(tbl)
  
  if(any(tbl < 5)) {
    test <- fisher.test(tbl)
    cat("Fisher¡¯s exact test p-value =", test$p.value, "\n")
  } else {
    test <- chisq.test(tbl, correct = FALSE)
    cat("Chi-squared test p-value =", test$p.value, "\n")
  }
}

## Regression for delay discounting change on emotional rating
# Analysis
rd_logk_change <- rd %>%
  select(SubjNum, logk_change)
combined_emo_data_with_logk <- combined_emo_data %>%
  left_join(rd_logk_change, by = "SubjNum")
model_peace <- lm(Emotion_rating ~ logk_change * Group, data = combined_emo_data_with_logk %>%
                         filter(Emotion_type == "peace"))
summary(model_peace) #NS
model_relax <- lm(Emotion_rating ~ logk_change * Group, data = combined_emo_data_with_logk %>%
                    filter(Emotion_type == "relaxation"))
summary(model_relax) #NS
model_sad <- lm(Emotion_rating ~ logk_change * Group, data = combined_emo_data_with_logk %>%
                    filter(Emotion_type == "sadness"))
summary(model_sad) #NS