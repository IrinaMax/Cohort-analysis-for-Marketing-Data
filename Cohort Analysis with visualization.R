
> # Cohort Analysis
> # 
> library(dplyr)
> library(ggplot2)
> 
> #I will make synthetic sample cohort data 
> cohort_data <- data.frame(
+   customer_id = 1:1000,
+   acquisition_date = sample(seq(as.Date('2020-01-01'), as.Date('2021-01-01'), by="day"), 1000, replace=TRUE),
+   transaction_date = sample(seq(as.Date('2020-01-01'), as.Date('2021-01-01'), by="day"), 1000, replace=TRUE),
+   revenue = runif(1000, min=0, max=100)
+ )
> cohort_data %>% head
  customer_id acquisition_date transaction_date     revenue
1           1       2020-01-04       2020-05-09  0.05020492
2           2       2020-01-10       2020-05-26 90.50272387
3           3       2020-12-21       2020-07-20 73.96877517
4           4       2020-05-23       2020-04-22 81.64673408
5           5       2020-07-17       2020-04-28 58.43730404
6           6       2020-07-16       2020-05-18 35.51850880

> # Calculate cohort metrics (retention rate)
> cohort_metrics <- cohort_data %>%
+   mutate(acquisition_month = format(acquisition_date, "%Y-%m")) %>%
+   group_by(acquisition_month) %>%
+   summarize(
+     total_customers = n_distinct(customer_id),
+     total_revenue = sum(revenue),
+     retention_rate = sum(transaction_date >= acquisition_date + 30) / total_customers  # Example: 30-day retention
+   )
> cohort_metrics
# A tibble: 12 × 4
   acquisition_month total_customers total_revenue retention_rate
   <chr>                       <int>         <dbl>          <dbl>
 1 2020-01                        92         4658.         0.804 
 2 2020-02                        79         4222.         0.759 
 3 2020-03                        79         4060.         0.785 
 4 2020-04                        83         3915.         0.554 
 5 2020-05                        79         3995.         0.620 
 6 2020-06                        93         5053.         0.441 
 7 2020-07                        97         4697.         0.454 
 8 2020-08                        94         4650.         0.383 
 9 2020-09                        78         3759.         0.205 
10 2020-10                        81         3724.         0.136 
11 2020-11                        75         3638.         0.0533
12 2020-12                        70         3526.         0     
> # Visualize cohort metrics (e.g., retention rate)
> ggplot(cohort_metrics, aes(x = acquisition_month, y = retention_rate, group = 1)) +
+   geom_line(color = "blue") +
+   geom_point(color = "blue") +
+   labs(title = "Cohort Retention Rate Over Time",
+        x = "Acquisition Month",
+        y = "Retention Rate") +
+   theme_minimal()


#------------------
# 
library(dplyr)
library(ggplot2)

# Sample cohort data (replace with your own data)
cohort_data <- data.frame(
  customer_id = 1:1000,
  country = sample(c("USA", "UK", "Canada", "Australia"), 1000, replace = TRUE),
  acquisition_date = sample(seq(as.Date('2020-01-01'), as.Date('2021-01-01'), by="day"), 1000, replace=TRUE),
  transaction_date = sample(seq(as.Date('2020-01-01'), as.Date('2021-01-01'), by="day"), 1000, replace=TRUE),
  revenue = runif(1000, min=0, max=100)
)

# Calculate cohort metrics (e.g., retention rate) based on country
cohort_metrics <- cohort_data %>%
  mutate(acquisition_month = format(acquisition_date, "%Y-%m")) %>%
  group_by(country, acquisition_month) %>%
  summarize(
    total_customers = n_distinct(customer_id),
    total_revenue = sum(revenue),
    retention_rate = sum(transaction_date >= acquisition_date + 30) / total_customers  # Example: 30-day retention
  )

# Visualize cohort metrics (e.g., retention rate) by country
ggplot(cohort_metrics, aes(x = acquisition_month, y = retention_rate, color = country)) +
  geom_line() +
  geom_point() +
  labs(title = "Cohort Retention Rate by Country Over Time",
       x = "Acquisition Month",
       y = "Retention Rate",
       color = "Country") +
  theme_minimal()

#--------------------- Product--------
# If you want to perform cohort analysis based on both product and state, you can create a plot where each state is represented by a separate line, and the color of the line represents the product category.
library(dplyr)
library(ggplot2)

# Sample cohort data (replace with your own data)
cohort_data <- data.frame(
  customer_id = 1:1000,
  product_category = sample(c("Electronics", "Clothing", "Books", "Home & Kitchen"), 1000, replace = TRUE),
  acquisition_date = sample(seq(as.Date('2020-01-01'), as.Date('2021-01-01'), by="day"), 1000, replace=TRUE),
  transaction_date = sample(seq(as.Date('2020-01-01'), as.Date('2021-01-01'), by="day"), 1000, replace=TRUE),
  revenue = runif(1000, min=0, max=100)
)

# Calculate cohort metrics (e.g., retention rate) based on product category
cohort_metrics <- cohort_data %>%
  mutate(acquisition_month = format(acquisition_date, "%Y-%m")) %>%
  group_by(product_category, acquisition_month) %>%
  summarize(
    total_customers = n_distinct(customer_id),
    total_revenue = sum(revenue),
    retention_rate = sum(transaction_date >= acquisition_date + 30) / total_customers  # Example: 30-day retention
  )

# Visualize cohort metrics (e.g., retention rate) by product category
ggplot(cohort_metrics, aes(x = acquisition_month, y = retention_rate, color = product_category)) +
  geom_line() +
  geom_point() +
  labs(title = "Cohort Retention Rate by Product Category Over Time",
       x = "Acquisition Month",
       y = "Retention Rate",
       color = "Product Category") +
  theme_minimal()
--------
# I also show how to visualize the retention rate of each cohort by state and product category over time using a line chart with different colors for each product category and different line types for each state.
library(dplyr)
library(ggplot2)

# Sample cohort data (replace with your own data)
cohort_data <- data.frame(
  customer_id = 1:1000,
  product_category = sample(c("Electronics", "Clothing", "Books", "Home & Kitchen"), 1000, replace = TRUE),
  state = sample(c("CA", "NY", "TX", "FL"), 1000, replace = TRUE),
  acquisition_date = sample(seq(as.Date('2020-01-01'), as.Date('2021-01-01'), by="day"), 1000, replace=TRUE),
  transaction_date = sample(seq(as.Date('2020-01-01'), as.Date('2021-01-01'), by="day"), 1000, replace=TRUE),
  revenue = runif(1000, min=0, max=100)
)

# Calculate cohort metrics (retention rate) based on product category and state
cohort_metrics <- cohort_data %>%
  mutate(acquisition_month = format(acquisition_date, "%Y-%m")) %>%
  group_by(product_category, state, acquisition_month) %>%
  summarize(
    total_customers = n_distinct(customer_id),
    total_revenue = sum(revenue),
    retention_rate = sum(transaction_date >= acquisition_date + 30) / total_customers  # Example: 30-day retention
  )

# With facet_grid(. ~ state, scales = "free_y"), we create separate panels for each state, allowing us to visualize the cohort retention rates by state and product category effectively. 
ggplot(cohort_metrics, aes(x = acquisition_month, y = retention_rate, color = product_category)) +
  geom_line() +
  geom_point() +
  facet_grid(.~ state, scales = "free_y") +  # Facet by state
  labs(title = "Cohort Retention Rate by State and Product Category Over Time",
       x = "Acquisition Month",
       y = "Retention Rate",
       color = "Product Category") +
  theme_minimal()
