library(tidyverse)
library(dplyr)

df <- read.csv("project.csv")

sum(is.na(df$records))
colnames(df)
df$method %>% unique()

df <- df %>% slice(-1) %>% select(-alternative.name, -interesting.story, -X,
                                  -displayed.records, -X2nd.source.link, -story,
                                  -ID, -X1st.source.link) %>%
  arrange(year, desc(date)) %>% 
  mutate(method = na_if(method, "oops!")) %>%
  mutate(method = replace_na(method, "unknown"))

df$method %>% unique()
df %>% group_by(organisation) %>% summarize(n = n()) %>%
                  arrange(desc(n))


# 1. Typical vs record-breaking breach size (histogram, log scale):

# 2. Breaches and total records by year (line charts):

# 3. Records lost by sector (boxplot, log scale):

# 4. Most common breach methods (horizontal bars):

# 5. Top 10 largest breaches by organization (horizontal bars):

# 6. Breach method by sector (heatmap):




