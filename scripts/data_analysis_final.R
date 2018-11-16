library(tidyverse)
library(ggrepel)

#> Set options ----

# disable scientific notation
options(scipen = 999)
# Read in the data 
# df_analysis <- haven::read_dta('data/analgesics/allregdata.dta')
# saveRDS(df_analysis, "data/analgesics.RDS")
df <- readRDS('data/analgesics.RDS')

# Brand description
brand_map <- unique(df$descrip)
active_substance <- c('Aspirin', 'Paracetamol','Paracetamol','Ibuprofen','Paracetamol','Paracetamol', 'Paracetamol','Paracetamol', 'Ibuprofen','Ibuprofen','Aspirin', 'Aspirin','Aspirin')

# Firm Names
firm_map <- c(1,2,3,4,5)
firms_lab <- c('Dominicks', 'Tylenol', 'Advil', 'Anacin', 'Bayer')


# Data 
df_analysis <- df %>% 
  select(store, upc, ok, descrip, size, 
         nsize, brand_id, firm_id, segment_id, code, 
         quant_weekly_help, quant_weekly, pricezone, 
         pricecluster, datestring, city, latitude, 
         longitude, price_weekly, retailmargin_weekly, 
         msize_custcount_weekly, urban, saledum_s_mon, 
         whprice_weekly, week, educ, age9, ethnic, hsizeavg, hvalmean, nocar, income) %>% 
  filter(price_weekly != 0,
         quant_weekly != 0,
         quant_weekly_help != 0,
         ok ==1,
         firm_id != 6) %>% 
  mutate(active_substance = plyr::mapvalues(descrip, from = brand_map, to = active_substance),
         diff = price_weekly - whprice_weekly,
         datestring = as.Date(datestring, '%y%m%d'),
         firms = parse_factor(firm_id, NULL),
         firms_new = plyr::mapvalues(firms, from = firm_map, to = firms_lab)) 


df_analysis %>% 
  group_by(firms_new) %>% 
  dplyr::summarise(mean_s = mean(quant_weekly), 
                   med_p = median(price_weekly)) %>% 
  ggplot(aes(y = med_p, x = mean_s)) + 
  geom_point(size = 10,  aes(colour=firms_new)) + 
  scale_y_continuous("Median Price") +
  scale_x_continuous("Mean Sales") + 
  geom_label_repel(aes(label = firms_new), label.padding = 1, box.padding = 1,
                  col = "black", label.size = 1, label.r = 0.3, size = 7) +
  geom_abline(slope = 1, intercept = 0, col = "lightgrey") + 
  theme(legend.position="none") + theme(text = element_text(size=20))
  #ggtitle("Median Price vs Average Sales by Firm")  

#ggsave("fig-4.png", plot = last_plot(),
  #     scale = 1, dpi = 300, limitsize = TRUE)
  
# Price 
df_analysis %>% 
  mutate(descrip_factor = parse_factor(descrip, NULL)) %>% 
  group_by(descrip_factor, week, active_substance) %>% 
  summarise(price_total = mean(price_weekly)) %>% 
  ggplot(aes(reorder(x = descrip_factor, price_total), 
             y = price_total, 
             fill = active_substance)) + 
  geom_boxplot() + coord_flip() +
  labs(title = "Box Plots", 
       caption = "Source: Own Compilation",
       y = "Weekly Prices", x = "Product Description") + 
  scale_fill_discrete(name = "Active Substance Group")
# theme(text = element_text(size=8), legend.position="none")

# Sales UPC
df_analysis %>% 
  mutate(descrip_factor = parse_factor(descrip, NULL)) %>% 
  group_by(descrip_factor, week, active_substance) %>% 
  summarise(quant_total = sum(quant_weekly)) %>% 
  ggplot(aes(reorder(x = descrip_factor, quant_total), 
             y = quant_total, 
             fill = active_substance)) + 
  geom_boxplot() + coord_flip() +
  labs(title = "Box Plots", 
       caption = "Source: Own Compilation",
       y = "Weekly Sales", x = "Product Description") + 
  scale_fill_discrete(name = "Active Substance Group")
# theme(text = element_text(size=8), legend.position="none")

# Sales UPC

df_analysis %>% 
  mutate(descrip_factor = parse_factor(descrip, NULL)) %>% 
  group_by(descrip_factor, week, active_substance) %>% 
  summarise(diff_total = mean(diff)) %>% 
  ggplot(aes(reorder(x = descrip_factor, diff_total), 
             y = diff_total, 
             fill = active_substance)) + 
  geom_boxplot() + coord_flip() +
  labs(title = "Box Plots", 
       caption = "Source: Own Compilation",
       y = "Difference", x = "Product Description") + 
  scale_fill_discrete(name = "Active Substance Group")

# Average Sales vs Average Price by Brands
df_analysis %>% 
  group_by(week, active_substance) %>% 
  summarise(mean_sales = sum(quant_weekly),
            mean_price = median(price_weekly)) %>% 
  ggplot(aes(x = mean_sales, y = mean_price, color = active_substance)) + geom_point(aes(color = active_substance)) +
  geom_line(stat="smooth",method = "lm", formula = y ~ x,
            size = 1,
            linetype ="dashed",
            alpha = 0.8) +
  facet_wrap( ~ active_substance) +
  labs(title = "Average Sales vs Median Prices by Active Ingredient", caption = "Source: Own Compilation", y = "Median Price", x = "Mean Sales") +
  theme(legend.title=element_blank()) 

library(zoo)
weekly <- df_analysis %>% select(quant_weekly,datestring) %>%
  group_by(datestring) %>%
  dplyr::summarise(weekly_total = sum(quant_weekly))

data_grouped <-df_analysis %>% select(firm_id,active_substance, quant_weekly, descrip,datestring)%>%
  group_by(active_substance, datestring)%>%
  dplyr::summarise(total = sum(quant_weekly))

aspirin <- data_grouped %>% filter(active_substance == 'Aspirin')
aspirin$mkt_share <- aspirin$total / weekly$weekly_total

ibuprofen <-data_grouped %>% filter(active_substance == 'Ibuprofen')
ibuprofen$mkt_share <- ibuprofen$total / weekly$weekly_total

paracetamol <- data_grouped %>% filter(active_substance == 'Paracetamol')
paracetamol$mkt_share <- paracetamol$total / weekly$weekly_total

# Merge
merged <- rbind(aspirin,ibuprofen, paracetamol)

merged %>%  mutate(mkt_share = rollmean(mkt_share,k = 3, na.pad = TRUE)) %>%
  ggplot( aes( x= datestring, y = mkt_share, fill = active_substance)) + geom_area(alpha=0.6 , size=1, colour="black")  +
  labs(title = 'Market Shares by Active Substance (% of Qty Sold)',  caption = "Source: Own Compilation", y = 'Market Share', x = 'Date', colour ='Substance') + scale_fill_discrete(name = "Active Substance Group")
#  theme(text = element_text(size =8))

