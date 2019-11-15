library(tidyverse)
library(gganimate)
library(gifski)

#import and tidy data with "tidy and merge data.R" to produce df_merged dataframe


#plot overall
df_merged_long_collapsed <- df_merged_long %>%
  group_by(Month, source) %>%
  #  filter(source !="Adzuna_count") %>%
  summarise(count = sum(count))

ggplot(df_merged_long_collapsed, aes(x = Month, y = count, colour = source)) +
  geom_line() +
  labs(title = "Comparison of Adzuna and IVI counts (all occupations) per GCCSA",
       x = "Date") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cor(df_merged[, c(4,5,7)])

#plot data for GCCSAs
df_merged_long_GCCSA <- df_merged_long %>%
  group_by(Month, GCCSA, source) %>%
  filter(source !="Adzuna_count") %>%
  summarise(count = sum(count))

ggplot(df_merged_long_GCCSA, aes(x = Month, y = count, colour = source)) +
  geom_point(shape = 1, alpha = 0.7) +
  geom_smooth(method = 'loess') +
  labs(title = "Comparison of Adzuna and IVI counts (all occupations) per GCCSA",
       x = "Date") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~GCCSA, scales = "free_y")

#plot data for occupations
df_merged_long_occupation <- df_merged_long %>%
  group_by(Month, ANZSCO_TITLE, source) %>%
  summarise(count = sum(count))

ggplot(df_merged_long_occupation, aes(x = Month, y = count, colour = source)) +
  geom_line() +
  labs(title = "Comparison of Adzuna and IVI counts (all occupations) per GCCSA",
       x = "Date") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ANZSCO_TITLE, scales = "free_y")


df_merged_GCCSA <- df_merged %>%
  group_by(Month, GCCSA) %>%
  summarise(Adzuna_count = sum(Adzuna_count),
            Adzuna_mav = sum(Adzuna_mav),
            IVI_count = sum(IVI_count))

df_merged_occupation <- df_merged %>%
  group_by(Month, ANZSCO_TITLE) %>%
  summarise(Adzuna_count = sum(Adzuna_count),
            Adzuna_mav = sum(Adzuna_mav),
            IVI_count = sum(IVI_count))

#####
#Correlations
correlations_GCCSA <- data.frame(state=character(),
                           metro=numeric(), 
                           rural=numeric(),
                           stringsAsFactors=FALSE)
correlations_GCCSA[1,1] <- "NSW"
correlations_GCCSA[1,2] <- with(df_merged_GCCSA %>% filter(GCCSA == "Greater Sydney"), round(cor(Adzuna_mav, IVI_count), 2))
correlations_GCCSA[1,3] <- with(df_merged_GCCSA %>% filter(GCCSA == "Rest of NSW"), round(cor(Adzuna_mav, IVI_count), 2))
correlations_GCCSA[2,1] <- "VIC"
correlations_GCCSA[2,2] <- with(df_merged_GCCSA %>% filter(GCCSA == "Greater Melbourne"), round(cor(Adzuna_mav, IVI_count), 2))
correlations_GCCSA[2,3] <- with(df_merged_GCCSA %>% filter(GCCSA == "Rest of Vic."), round(cor(Adzuna_mav, IVI_count), 2))
correlations_GCCSA[3,1] <- "QLD"
correlations_GCCSA[3,2] <- with(df_merged_GCCSA %>% filter(GCCSA == "Greater Brisbane"), round(cor(Adzuna_mav, IVI_count), 2))
correlations_GCCSA[3,3] <- with(df_merged_GCCSA %>% filter(GCCSA == "Rest of Qld"), round(cor(Adzuna_mav, IVI_count), 2))
correlations_GCCSA[4,1] <- "SA"
correlations_GCCSA[4,2] <- with(df_merged_GCCSA %>% filter(GCCSA == "Greater Adelaide"), round(cor(Adzuna_mav, IVI_count), 2))
correlations_GCCSA[4,3] <- with(df_merged_GCCSA %>% filter(GCCSA == "Rest of SA"), round(cor(Adzuna_mav, IVI_count), 2))
correlations_GCCSA[5,1] <- "WA"
correlations_GCCSA[5,2] <- with(df_merged_GCCSA %>% filter(GCCSA == "Greater Perth"), round(cor(Adzuna_mav, IVI_count), 2))
correlations_GCCSA[5,3] <- with(df_merged_GCCSA %>% filter(GCCSA == "Rest of WA"), round(cor(Adzuna_mav, IVI_count), 2))
correlations_GCCSA[6,1] <- "TAS"
correlations_GCCSA[6,2] <- with(df_merged_GCCSA %>% filter(GCCSA == "Greater Hobart"), round(cor(Adzuna_mav, IVI_count), 2))
correlations_GCCSA[6,3] <- with(df_merged_GCCSA %>% filter(GCCSA == "Rest of Tas."), round(cor(Adzuna_mav, IVI_count), 2))
correlations_GCCSA[7,1] <- "NT"
correlations_GCCSA[7,2] <- with(df_merged_GCCSA %>% filter(GCCSA == "Greater Darwin"), round(cor(Adzuna_mav, IVI_count), 2))
correlations_GCCSA[7,3] <- with(df_merged_GCCSA %>% filter(GCCSA == "Rest of NT"), round(cor(Adzuna_mav, IVI_count), 2))
correlations_GCCSA[8,1] <- "ATC"
correlations_GCCSA[8,2] <- with(df_merged_GCCSA %>% filter(GCCSA == "Australian Capital Territory"), round(cor(Adzuna_mav, IVI_count), 2))
print(correlations_GCCSA)


cor(df_merged_GCCSA[, 3:5])
cor(df_merged_occupation[, 3:5])



###########
#animation
ggplot(df_merged_long_GCCSA, aes(x = GCCSA, y = count, colour = source)) +
   geom_point() +
   labs(title = "Comparison of Adzuna and IVI counts (all occupations) per GCCSA",
        x = "Date") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
 # facet_wrap(~Month)
  transition_time(Month)

df_merged_long_collapsed <- df_merged_long %>%
  group_by(Month, source) %>%
  summarise(count = sum(count))
  
p <-ggplot(df_merged_long_collapsed, aes(x = Month, y = count, colour = source)) +
    geom_line() +
    geom_point() +
    labs(title = "Comparison of Adzuna and IVI counts (all occupations) per GCCSA",
       x = "Date") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size=7)) +
  transition_reveal(Month)
animate(p, renderer = gifski_renderer(loop = F))


########
#correlations for all GCCSAs and Occupations
correlations <- df_merged %>%
  group_by(GCCSA, ANZSCO_TITLE) %>%
  summarise(correlation = cor(Adzuna_mav, IVI_count)) %>%
  mutate(correlation = round(correlation, 2))

ggplot(correlations, aes(x = ANZSCO_TITLE, y = correlation)) +
  geom_bar(stat="identity", alpha = 0.5) + 
  geom_text(aes(label = correlation), nudge_y = 0.2, colour = "blue", size = 2) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  facet_wrap(~GCCSA)

ggplot(correlations, aes(x = GCCSA, y = correlation)) +
  geom_bar(stat="identity", alpha = 0.5) + 
  geom_text(aes(label = correlation), nudge_y = 0.2, colour = "blue", size = 2) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  facet_wrap(~ANZSCO_TITLE)

  df_merged_long %>%
    filter(source != "Adzuna_count")%>%
    group_by(GCCSA, ANZSCO_TITLE, source) %>%
    summarise(count = sum(count)) %>%
  ggplot(aes(x=GCCSA, y = ANZSCO_TITLE, colour = source))+
  geom_jitter(aes(size = count), alpha = 0.5, width = 0.1, height = 0.1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size=7))


