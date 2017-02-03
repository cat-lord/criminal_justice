## Interim proven reoffending rates ##

library(httr) ; library(readxl) ; library(dplyr) ; library(tibble); library(ggplot2)

# Interim proven reoffending for PbR cohorts (Oct-Dec 2015)
url <- "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/585910/crc-nps-interim-results-tables-jan17.xlsx"
GET(url, write_disk(df1 <- tempfile(fileext = ".xlsx")))
df1 <- read_excel(df1, sheet = 2, skip = 2, col_names = TRUE) %>% 
  na.omit() %>% 
  mutate(period = "Oct-Dec 2015") %>% 
  select(period, everything())
# Interim proven reoffending for PbR cohorts (Jan-Mar 2016)
url <- "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/585910/crc-nps-interim-results-tables-jan17.xlsx"
GET(url, write_disk(df2 <- tempfile(fileext = ".xlsx")))
df2 <- read_excel(df2, sheet = 4, skip = 2, col_names = TRUE) %>% 
  na.omit() %>% 
  mutate(period = "Jan-Mar 2016") %>% 
  select(period, everything())

df <- merge(df1, df2, all = TRUE) %>% 
  rename(crc = `CRC\r\nname`,
         cohort = `Number of\r\noffenders in\r\nthe eligible\r\ncohort`,
         pbr_cohort = `Number of\r\noffenders in\r\nthe measurable cohort`,
         reoffenders = `Number of\r\nreoffenders`,
         reoffences = `Number of reoffences`,
         binary = `Proportion of\r\noffenders who\r\nreoffend (%)`,
         frequency = `Average number of reoffences per reoffender`) %>% 
  mutate(type = ifelse(grepl('Cheshire & Greater Manchester|Hampshire & Isle of Wight|Humberside, Lincolnshire & North Yorkshire|Merseyside|West Yorkshire', crc), "Interserve CRCs", "Other CRCs"))

# Proportion of offenders who reoffend (%)
# Interim rates for proportion of offenders who reoffend for payment by results cohorts, by CRC

temp <- filter(df, period == "Jan-Mar 2016" & type == "Interserve CRCs") %>% 
  select(crc, binary, type) %>% 
  add_row(crc = "Interserve CRC average", binary = mean(.$binary), type = "")

filter(df, period == "Jan-Mar 2016") %>% 
  select(crc, binary, type) %>% 
  add_row(crc = "CRC average", binary = mean(.$binary), type = "") %>% 
  merge(temp, all = TRUE) %>% 
  mutate(type = factor(type, levels = c("Interserve CRCs", "Other CRCs", ""))) %>% 
  ggplot(aes(x = reorder(crc, -binary), y = binary, colour = type)) + 
  geom_point(size = 3) + 
  scale_y_continuous(limits = c(20, 50), breaks = seq(20, 50, 5)) +
  labs(title = "Interim proven reoffending rates for PbR cohorts by CRC",
       subtitle = "January - March 2016", 
       x = NULL, y = "\nProportion of offenders who reoffend (%)",
       caption = "Source: Ministry of Justice") +
  coord_flip() + 
  facet_grid(type ~ ., scales = "free", space = "free") +
  theme_minimal() + 
  theme(plot.background = element_rect(fill = "gray97", color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = 3, size = 0.3, color = "gray50"),
        panel.border = element_blank(),
        axis.ticks = element_blank()) +
  theme(legend.position="none")
ggsave("binary_jan_mar_2016.png", scale=1.3, dpi=300)
