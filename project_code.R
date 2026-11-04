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
  

class(df$records.lost)
df$sector %>% unique()
df$method %>% unique()
df %>% group_by(organisation) %>% summarize(n = n()) %>%
                  arrange(desc(n))

# 1. Typical vs record-breaking breach size (histogram, log scale):
graph_1 <- df %>%
  filter(records.lost > 0) %>%
  mutate(log_records = log10(records.lost)) %>%
  ggplot(aes(x=log_records)) +
  geom_histogram(binwidth = 0.25, colour = 'white', fill = "#90b1cf") + 
  labs(
    title = 'Typical vs record-breaking breach size',
    subtitle = 'Distribution of lost records (log scale)',
    x = 'Number of lost records (log10)',
    y = 'Number of incidents (frequency)'
  ) +
  theme_minimal() 

graph_1

#used 

# 2. Breaches and total records by year (line charts):

# 3. Records lost by sector (boxplot, log scale):

# 4. Most common breach methods (horizontal bars):

# 5.Top 10 organizations by total records lost (horizontal bars):

avg_sum <- df %>%
  distinct(organisation, year, records.lost, .keep_all = TRUE) %>% 
  group_by(organisation) %>% 
  summarize(rec.l_sum = sum(records.lost)) %>%
  summarize(avg = mean(rec.l_sum)) %>%
  pull(avg)


graph_5 <- df %>%
  distinct(organisation, year, records.lost, .keep_all = TRUE) %>%
  group_by(organisation) %>% 
  summarize(rec.l_sum = sum(records.lost), n = n()) %>% 
  arrange(desc(rec.l_sum)) %>% 
  head(10) %>%
  ggplot(aes(x = rec.l_sum, y = fct_reorder(organisation, rec.l_sum))) +
  geom_col(fill = "#90b1cf") +
  geom_text(aes(label = paste0(round(rec.l_sum / 1000000), "M")), hjust = -0.1, size = 3) +
  geom_vline(xintercept = avg_sum, linetype = "dashed", color = "#395176", linewidth = 0.5) +
  scale_x_continuous(labels = function(x) paste0(x / 1000000, "M"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Top 10 companies by total records lost",
       x = "Total records lost",
       y = NULL,
      subtitle = paste0("Dashed line: average of ", round(avg_sum / 1000000, 1),
                        "M records per company")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0, size = 17, margin = margin(b = 4)),
        plot.subtitle = element_text(hjust = 0, size = 11, margin = margin(b = 6)),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.title.position = "panel")

graph_5

ggsave("graph_5.png", plot = graph_5, width = 10, height = 6, dpi = 300) 

# For this graph AI was used to help explain how to adjust and move text farther
# from the graph for it to look nicer. Also AI was used to help label each row in the
# graph with 700M, 412M and so on. The same approach of labeling where used to normaly
# label x axis, so i would look nicer.

# 6. Breach method by sector (heatmap):
  
graph_6 <- df %>%
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
  )

graph_6

ggsave("graph_6.png", plot = graph_6, width = 10, height = 6, dpi = 300) 

# For this graph AI was used to fill empty sells of the heat map with 0. Also
# AI recommended this function -> transform = "sqrt", for the heat map to look
# more readable in case of colors.

# Final graph: