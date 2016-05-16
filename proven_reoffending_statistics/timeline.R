library(dplyr) ; library(highcharter)

df <- read.csv("adult_proven_reoffending_new_measure.csv", header = T) %>% 
  mutate(quarter = as.Date(quarter, format = "%Y-%m-%d")) %>% 
  filter(quarter >= "2002-01-01")

dates <- as.Date(c("2013-05-01","2014-06-01","2015-02-01","2015-05-01", "2017-10-01"), format = "%Y-%m-%d")

highchart(type = "stock") %>% 
  hc_title(text = "Proven re-offending by adults, 3 monthly cohorts") %>% 
  hc_add_series_times_values(df$quarter, df$binary,
                             name = "Proportion of offenders who re-offend (%)") %>% 
  hc_add_series_times_values(df$quarter, df$adjusted,
                             name = "OGRS4/G adjusted") %>% 
  hc_yAxis(labels = list(format = "{value}%")) %>% 
  hc_add_series_flags(dates,
                      title = c("2013-05", "2014-06", "2015-02", "2015-05", "2017-10"), 
                      text = c("Transforming Rehabilitation strategy published",
                               "Probation services split into NPS and 21 CRCs",
                               "CRCs transferred to the public sector", 
                               "'Through the Gate' begins",
                               "One year proven reoffending statistics for PbR"),
                      id = "events") %>% 
  hc_tooltip(valueDecimals = 1) %>% 
  hc_credits(enabled = TRUE, text = "Data: Ministry of Justice",
             href = "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/519686/proven-reoffending-consultation-supporting-tables.xlsx",
             style = list(fontSize = "10px")) %>% 
  hc_exporting(enabled = TRUE)  %>% 
  hc_add_theme(hc_theme_google())
  
  