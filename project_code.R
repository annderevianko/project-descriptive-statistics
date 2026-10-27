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
  mutate(method = replace_na(method, "unknown")) %>%
  mutate(across(where(is.character), str_squish)) %>%
  separate_rows(sector, sep = ",\\s*") %>%
  mutate(sector = str_to_lower(sector) %>%
           str_replace("^financial$", "finance")) %>%
  mutate(records.lost = as.numeric(str_remove_all(records.lost, ",")))
  


df$sector %>%df$sector %>%df$sector %>% unique()
df$method %>% unique()
df %>% group_by(organisation) %>% summarize(n = n()) %>%
                  arrange(desc(n))


# 1. Typical vs record-breaking breach size (histogram, log scale):

# 2. Breaches and total records by year (line charts):

# 3. Records lost by sector (boxplot, log scale):

# 4. Most common breach methods (horizontal bars):

# 5.Top 10 organizations by total records lost (horizontal bars):



# 6. Breach method by sector (heatmap):
  
df %>%
  filter(method != "unknown") %>%
  mutate(sector = fct_rev(fct_infreq(sector)),   
               method = fct_infreq(method)) %>%  
  count(sector, method) %>%
  complete(sector, method, fill = list(n = 0)) %>%
  ggplot(aes(x = method, y = sector, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = c("#eae6f0", "#bdb0ca", "#9295b7", "#5f77a1", "#395176"),
                       transform = "sqrt") +
  theme_minimal() + 
  geom_text(aes(label = n),
            size = 3.2,
            family = "Times New Roman") +
  labs(title = "How each sector loses data",
       x = "Method",
       y = "Sector",
       fill = "Num. of cases") +
  theme(
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    legend.title = element_text(size = 10),
    plot.title = element_text(size = 17, margin = margin(b = 10), hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.title.x = element_text(margin = margin(t = 15))
  ) +
  theme(plot.margin = margin(t = 15, r = 15, b = 15, l = 15))


