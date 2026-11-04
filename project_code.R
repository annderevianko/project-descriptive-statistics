library(tidyverse)
library(dplyr)

df <- read.csv("project.csv")

sum(is.na(df$records.lost))
colnames(df)
df$method %>% unique()

df <- df %>% slice(-1) %>% select(-alternative.name, -interesting.story, -X,
                                  -displayed.records, -X2nd.source.link, -story,
                                  -X1st.source.link) %>%
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
task1 <-df %>%
  distinct(organisation, year, records.lost) %>%
  filter(records.lost > 0) 

meadian <- median(task1$records.lost)

graph_1 <-
  ggplot(task1, aes(x=records.lost)) +
  geom_histogram(binwidth = 0.25, colour = 'white', fill = "#90b1cf") +
  geom_vline(xintercept = meadian, linetype = 'dashed', color = '#395176' ) +
  scale_x_log10(
    breaks = c(1e3, 1e4,1e5,1e6,1e7,1e8,1e9),
    labels = c('1K', '10K', '100K', '1M', '10M', '100M', '1B')
  ) +
  labs(
    title = 'Typical vs record-breaking breach size',
    subtitle = paste('Distribution of lost records (log scale). Median is dashed line and = ', round(meadian, 1)),
    x = 'Number of lost records (log scale)',
    y = 'Number of incidents'
  ) +
  theme_minimal() 

graph_1
ggsave('graph_1.png', plot = graph_1, width = 10, height = 6, dpi = 300) 

#AI suggested axis labels, helped fix errors, proposed adding the median 
#and recommended using scale_x_log10() with quality labels

# 2. Breaches and total records by year (line charts):

year_summary <- df %>%
  distinct(ID, .keep_all = TRUE) %>%
  mutate(year = as.integer(year)) %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarise(
    breaches = n(),
    total_records = sum(records.lost, na.rm = TRUE)
  ) %>%
  arrange(year)

View(year_summary)


graph_2a <- year_summary %>%
  ggplot(aes(x = year, y = breaches)) +
  geom_line(color = "#90b1cf", linewidth = 1.1) +
  geom_point(color = "#395176", size = 2.5) +
  labs(
    title = "Number of Data Breaches by Year",
    x = "Year",
    y = "Number of breaches"
  ) +
  theme_minimal()

graph_2a

ggsave("graph_2a.png", plot = graph_2a,
       width = 10, height = 6, dpi = 300)


graph_2b <- year_summary %>%
  mutate(total_records_billion = total_records / 1000000000) %>%
  ggplot(aes(x = year, y = total_records_billion)) +
  geom_line(color = "#9295b7", linewidth = 1.1) +
  geom_point(color = "#395176", size = 2.5) +
  labs(
    title = "Total Records Lost by Year",
    x = "Year",
    y = "Records lost (billions)"
  ) +
  theme_minimal()

graph_2b

ggsave("graph_2b.png", plot = graph_2b,
       width = 10, height = 6, dpi = 300)


# 3. Records lost by sector (boxplot, log scale):

df %>%
  group_by(sector) %>%
  summarize(median = median(records.lost, na.rm = TRUE), n = n()) %>%
  arrange(desc(median))

graph_3 <- df %>%
  filter(!is.na(records.lost), records.lost > 0) %>%   # log scale cannot show 0
  ggplot(aes(x = records.lost, y = fct_reorder(sector, records.lost, .fun = median))) +
  geom_boxplot(fill = "#bdb0ca", color = "#9295b7", outlier.color = "#395176") +
  scale_x_log10(labels = function(x) ifelse(x >= 1000000,
                                            paste0(x / 1000000, "M"),
                                            paste0(x / 1000, "K"))) +
  labs(title = "Records lost per breach by sector",
       subtitle = "Sectors ordered by median breach size, log scale",
       x = "Records lost (log scale)",
       y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0, size = 17, margin = margin(b = 4)),
        plot.subtitle = element_text(hjust = 0, size = 11, margin = margin(b = 6)),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.title.position = "panel")

graph_3

ggsave("graph_3.png", plot = graph_3, width = 10, height = 6, dpi = 300)

# For this graph, AI was used to assist in the construction of the graph, namely in explaining
# the structure, correcting errors, and making the graph look good.

# 4. Most common breach methods (horizontal bars):

graph_4 <- df %>%
  filter(method != "unknown") %>%
  distinct(organisation, year, records.lost, .keep_all = TRUE) %>%  # one row per breach
  count(method) %>%
  ggplot(aes(x = n, y = fct_reorder(method, n), fill = method)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.2, size = 4) +
  scale_fill_manual(values = c("hacked" = "#395176",
                               "poor security" = "#9295b7",
                               "lost device" = "#90b1cf",
                               "inside job" = "#bdb0ca")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Most common breach methods",
       x = "Number of breaches",
       y = NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0, size = 17, margin = margin(b = 8)),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.title.position = "panel")

graph_4

ggsave("graph_4.png", plot = graph_4, width = 10, height = 6, dpi = 300)

# For this graph, AI was used to structure and explain the code diagram,
# fix errors, and display the graph bars in different colors to make it
# look nicer and clearer.

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
task_finale <- df %>%
  distinct(organisation, year, records.lost, method) %>%
  filter(records.lost > 0, method != 'unknown')

graph_finale <- 
  ggplot(task_finale, aes(x=year, y= records.lost, colour = method)) +
  geom_point(size=2, alpha = 0.7) +
  facet_wrap(~method, axes = 'all_x') +
  scale_y_log10(
    breaks = c(1e3, 1e4,1e5,1e6,1e7,1e8,1e9),
    labels = c('1K', '10K', '100K', '1M', '10M', '100M', '1B')
  ) +
  scale_colour_manual(values= c('#bdb0ca', '#90b1cf', '#9295b7', '#395176')) +
  labs(
    title = 'Breach size over time by hacking methods',
    subtitle = 'One point is one incident',
    x='Year',
    y= 'Number of lost records(log scale)'
  )+
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9),
    legend.title = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = 'bold'),
    strip.text = element_text(face = 'bold', size = 12))


graph_finale
ggsave('graph_finale.png', plot = graph_finale, width = 10, height = 6, dpi = 300) 

#AI helped fix errors, explained facet_wrap(~method, axes = 'all_x') for 
#splitting the plot into separate panels for each method, assisted with theme() 
#and with color palette (scale_colour_manual()) and suggested axis labels
