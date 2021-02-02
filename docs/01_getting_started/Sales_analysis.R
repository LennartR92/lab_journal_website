library("tidyverse")
library("tibble")
library("readxl")
bikes <- read_xlsx(
  "01_getting_started/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
bikeshops <- read_excel(
  "01_getting_started/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")
orderlines <- read_excel(
  "01_getting_started/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
orderlines_w_bikes <- left_join(orderlines,bikes, by = c("product.id" = "bike.id"))
bike_orderlines_joined_tbl <- left_join(orderlines_w_bikes,bikeshops, by = c("customer.id" = "bikeshop.id"))
bike_orderlines_joined_tbl <- bike_orderlines_joined_tbl %>% separate("category", 
                                        c("category.1","category.2","category.3"),
                                        sep = " - ")

bike_orderlines_joined_tbl <- bike_orderlines_joined_tbl %>% mutate("total_sales" = price * quantity) %>%
  rename(bikeshop = name) %>% set_names(names(.) %>% str_replace_all("\\.", "_"))
library("lubridate")
sales_per_year <- bike_orderlines_joined_tbl %>% 
  select(total_sales,order_date) %>%
  mutate("year" = year(order_date)) %>%
  group_by(year) %>%
  summarize(sales = sum(total_sales)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))
sales_per_year %>% ggplot(aes(x = year, y = sales)) + geom_col(fill = "#2DC6D6") + 
  geom_label(aes(label = sales_text)) +
  geom_smooth(method = "lm", se = FALSE)

sales_per_year_cat <- bike_orderlines_joined_tbl %>%
  select(total_sales,order_date,category_1) %>%
  mutate("year" = year(order_date)) %>%
  group_by(year, category_1) %>%
  summarize(sales = sum(total_sales)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark =",",
                                     prefix="",
                                     suffix = " €"))
  
sales_per_year_cat %>% ggplot(aes(x = year, y = sales, fill = category_1)) + 
  geom_col() +
  facet_wrap(~category_1) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Revenue per year",
       subtitle = "grouped by Category 1",
       x = "Year", y = "Revenue")

bike_orderlines_joined_tbl <- bike_orderlines_joined_tbl %>% 
  separate("location", c("city","state"),sep = ", ")

sales_per_year_state <- bike_orderlines_joined_tbl %>%
  select(total_sales,order_date,state) %>%
  mutate("year" = year(order_date)) %>%
  group_by(year, state) %>%
  summarize(sales = sum(total_sales)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark =",",
                                     prefix="",
                                     suffix = " €"))

sales_per_year_state %>% ggplot(aes(x = year, y = sales, fill = state)) + 
  geom_col() +
  facet_wrap(~state) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Revenue per year",
       subtitle = "grouped by State",
       x = "Year", y = "Revenue")

