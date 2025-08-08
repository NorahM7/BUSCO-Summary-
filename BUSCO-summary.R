# === Load Required Libraries ===
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

# === Define Custom Colors ===
custom_colors <- c(
  "Complete_Single"     = "#2b8c4e",
  "Complete_Duplicated" = "#a1d99b",
  "Fragmented"          = "#8DA0CB",
  "Missing"             = "#FC8D62"
)

# === Extract BUSCO Metrics ===
extract_busco <- function(file_path, sample_label) {
  lines <- readLines(file_path)
  line <- lines[grepl("C:[0-9.]+%", lines)][1]
  
  line <- trimws(line)
  
  #summary <- strsplit(line, "\t")[[1]][2]
  
  S <- as.numeric(sub(".*S:([0-9.]+)%.*", "\\1", line))
  D <- as.numeric(sub(".*D:([0-9.]+)%.*", "\\1", line))
  F <- as.numeric(sub(".*F:([0-9.]+)%.*", "\\1", line))
  M <- as.numeric(sub(".*M:([0-9.]+)%.*", "\\1", line))
  
  
  data.frame(
    Condition = sample_label,
    Category = factor(c("Complete_Single", "Complete_Duplicated", "Fragmented", "Missing")),
    Percentage = c(S, D, F, M)
  )
}

#the last section is for sample name 
# === Load BUSCO Summary Files ===
SAMPLE_NAME     <- extract_busco("path/toshor/summaruy/short_summary.txt", "title_of_the_sample")
SAMPLE_NAME     <- extract_busco("path/toshor/summaruy/short_summary.txt", "title_of_the_sample")
SAMPLE_NAME     <- extract_busco("path/toshor/summaruy/short_summary.txt", "title_of_the_sample")
SAMPLE_NAME     <- extract_busco("path/toshor/summaruy/short_summary.txt", "title_of_the_sample")




#change this to match the section befor <- extract_busco(...
busco_all <- bind_rows(
  SAMPLE_NAME,
  SAMPLE_NAME,
  SAMPLE_NAME,
  SAMPLE_NAME
)

bracket_df <- busco_all %>%
  # Arrange by stacking order in your plot
  mutate(Category = factor(Category, levels = c("Missing", "Fragmented", "Complete_Duplicated", "Complete_Single"))) %>%
  group_by(Condition) %>%
  arrange(Condition, Category) %>%
  mutate(
    x_min = lag(cumsum(Percentage), default = 0),
    x_max = cumsum(Percentage)
  ) %>%
  filter(Category %in% c("Complete_Single", "Complete_Duplicated")) %>%
  summarise(
    x_start = min(x_min),
    x_end   = max(x_max),
    label = paste0("Complete: ", round(sum(Percentage), 1), "%"),
    .groups = "drop"
  ) %>%
  mutate(y_pos = as.numeric(factor(Condition)))

# === Plot Stacked Bar Chart with Labels ===
# Plot
final_plot <- ggplot(busco_all, aes(y = Condition, x = Percentage, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = round(Percentage, 1)),
            position = position_stack(vjust = 0.5), size = 3, color = "black") +
  
  # Bracket line
  geom_segment(data = bracket_df,
               aes(x = x_start, xend = x_end,
                   y = y_pos + 0.2, yend = y_pos + 0.2),
               inherit.aes = FALSE, color = "black") +
  
  # Bracket arms
  geom_segment(data = bracket_df,
               aes(x = x_start, xend = x_start,
                   y = y_pos + 0.15, yend = y_pos + 0.25),
               inherit.aes = FALSE, color = "black") +
  geom_segment(data = bracket_df,
               aes(x = x_end, xend = x_end,
                   y = y_pos + 0.15, yend = y_pos + 0.25),
               inherit.aes = FALSE, color = "black") +
  
  # Bracket label
  geom_text(data = bracket_df,
            aes(x = (x_start + x_end)/2,
                y = y_pos + 0.35,
                label = label),
            inherit.aes = FALSE, size = 3.5, fontface = "bold") +
  
  labs(title = "TITLE OF THE PLOT",
       x = "BUSCO(%)", y = "Assembly") +  # optional axis labels +title of the plot
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave("Name_of_your_plot.png", plot = final_plot, width = 10, height = 6, dpi = 300)


