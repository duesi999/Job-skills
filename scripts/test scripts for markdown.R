library(tidyverse)

#import and tidy data with tidy... script - will produce df_merged dataframe

df_merged_long_GCCSA <- df_merged_long %>%
  group_by(Month, GCCSA, source) %>%
  summarise(count = sum(count))

ggplot(df_merged_long_GCCSA, aes(x = Month, y = count, colour = source)) +
  geom_line(alpha = 0.5) +
  labs(title = "Comparison of Adzuna and IVI counts (all occupations) per GCCSA",
       x = "Date") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~GCCSA, scales = "free_y")

cor(df_merged[, c(4,5,7)])
