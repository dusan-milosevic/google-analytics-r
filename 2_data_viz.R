## Install libraries
# install.packages("googleAuthR")
# install.packages("googleAnalyticsR")
# install.packages("tidyverse")

## Load libraries
library("googleAuthR")
library("googleAnalyticsR")
library("tidyverse")

## Authorization with GA GA servers
ga_auth()

## Google Analytics Account List
account_list <- google_analytics_account_list()

## Google Analytics View ID 
GA_id <- XXXXXXXX

## Query  
df <- google_analytics(GA_id, 
                                date_range = c("2018-01-01","2018-12-31"),  
                                metrics = c("sessions"), 
                                dimensions = c("date"),
                                anti_sample = TRUE)
                                                              
## Line chart 
## Using ggplot2 and pipe operator from dplyr library
line_plot <- df %>% 
                    ggplot(aes(x = date, y = sessions)) +
                    geom_line() +
                    geom_smooth(method="lm", se = TRUE, color = "blue") +
                    labs(title = "Sessions Overview",
                         caption = "Source: Google Analytics", 
                         x = "Date", y = "Sessions") +
                    theme_minimal()
line_plot

## Histogram plot
histogram_plot <- df %>%
                  ggplot(aes(x = sessions)) +
                  geom_histogram(bins = 80, fill = "steelblue") + 
                  geom_vline(aes(xintercept=mean(sessions, na.rm=T)),   # Ignore NA values for mean
                              color="red", linetype="dashed", size=1) +
                  labs(title = "Sessions Distribution",
                       caption = "Source: Google Analytics") +
                  theme_minimal()

histogram_plot

## Query 
df2 <- google_analytics(GA_id, 
                        date_range = c("2018-01-01", "2018-07-27"), 
                        metrics = c("sessions", "transactions", "pageviews", "bounceRate"), 
                        dimensions = c("date", "yearMonth"), 
                        anti_sample = TRUE)

## Scatter plot
scatter_plot <- df2  %>%
  ggplot(aes(x = sessions, y = transactions)) +
  geom_jitter() +
  geom_smooth(method=lm) +  # Add linear regression line (by default includes 95% confidence region)
  labs(title = "Relationship between two variables (Sessions and Transactions)",
       caption = "Source: Google Analytics") +
  theme_minimal()

scatter_plot

## Bar plot
bar_plot  <- df2 %>% 
                   ggplot(aes(x = yearMonth, y = sessions)) +
                   geom_bar(stat = "identity", fill = "steelblue") +
                   labs(title = "Sessions Overview",
                        caption = "Source: Google Analytics", 
                        x = "yearMonth", y = "Sessions") +
                   geom_text(aes(label = paste(round(sessions,1), "", sep = ""), 
                   family = "Arial", fontface = "bold", size = 14, position = "center"), 
                   vjust = 1.6, color = "white", size = 4) +
                   theme_minimal()
bar_plot

## Query 
df3 <- google_analytics(GA_id, 
                        date_range = c("2016-01-01","2016-09-30"), 
                        metrics = c("transactions"), 
                        dimensions = c("dayOfWeekName", "hour", "deviceCategory"),
                        anti_sample = TRUE)

## Box plot (desktop)
box_plot_desktop <- df3 %>%
                    filter(deviceCategory == "desktop") %>%
                    ggplot(aes(x = dayOfWeekName, y = transactions)) + 
                    geom_boxplot(fill = "steelblue") + 
                    scale_x_discrete(limits = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')) + 
                    labs(title = "Desktop transactions distribution Overview",
                         subtitle = "Distribution of data based on the five number summary: minimum, first quartile, median, third quartile, and maximum.",
                         caption = "Source: Google Analytics", 
                         x = "yearMonth", y = "Transactions") +
                    theme_minimal()
box_plot_desktop

## Query
df4 <- google_analytics(GA_id, 
                                date_range, 
                                metrics = c("sessions"), 
                                dimensions = c("deviceCategory", "dayOfWeekName", "hour"), 
                                anti_sample = TRUE)

## Data frame manipulation using dplyr library from tidyverse
df4_weekly_avg_sessions <- df4 %>%
              filter(deviceCategory == "desktop") %>%
              group_by(deviceCategory, dayOfWeekName, hour) %>%
              summarise(avg_sessions_per_day = mean(sessions)) 

## Facet plots by weekday using ggplot2
facet_weekly_plot <- df4_weekly_avg_sessions %>%
        ggplot(aes(hour, avg_sessions_per_day, fill = deviceCategory, group = deviceCategory)) + 
        geom_area(position = "stack", fill = "steelblue") + 
        labs(title = "Desktop changes over the week",
        subtitle = "Average number of visits per hour and day filtered by only desktop sessions",
        caption = "Source: Google Analytics", 
        x = "hour", y = "avg_sessions_per_day") +
        theme_minimal() + 
        facet_wrap(~dayOfWeekName, ncol = 3) 

facet_weekly_plot

## Query
df5 <- google_analytics(GA_id, 
                        date_range, 
                        metrics = c("sessions", "transactions", "transactionsRevenue"), 
                        dimensions = c("channelGrouping"),
                        anti_sample = TRUE)

## Data frame cleaning and creating calculated metrics using dplyr library from tidyverse
df5_clean <- df5 %>%
                 mutate(session_share = sessions / sum(sessions),
                 sales_share = transactions / sum(transactions),
                 revenue_share = transactionRevenue / sum(transactionRevenue)) %>%
             arrange(-session_share) %>%
             transmute(
             channel = channelGrouping,
             sessions,
             users,
             sales = transactions,
             revenue = transactionRevenue,
             session_share,
             session_addup = cumsum(session_share),
             sales_share,
             sales_addup = cumsum(sales_share),
             revenue_share,
             revenue_addup = cumsum(revenue_share),
             cr_sessions = transactions / sessions,
             cr_users = transactions / users,
             rps = revenue / sessions,
             rpu = revenue / users)

head(df5_clean)

## Plotting
scatter_plot_share_channels <- df5_clean %>%         
  ggplot(aes(x = session_share, y = sales_share, color = channel)) +
  geom_abline(slope = 1, alpha = 1/10) +
  geom_point(alpha = 5/7, size = 8) + # specifying the type of the plot we want +
  scale_x_continuous(name = "Share of sessions", limits = c(0, NA), labels = percent) +
  scale_y_continuous(name = "Share of sales", limits = c(0, NA), labels = percent) +
  scale_color_brewer(palette="Spectral") +
  labs(title = "Relationship between sessions and sales by channels, 2018",
                caption = "Source: Google Analytics") +
  theme_minimal()
  
scatter_plot_share_channels

## Query
df6 <- google_analytics(GA_id, 
                          date_range,
                          metrics = c("pageviews"),
                          dimensions = c("dayOfWeekName","hour"),
                          anti_sample = TRUE)

## Heatmap with ggplot and viridis colour palette
plot_heatmap_day_of_week <- df6 %>% 
           ggplot(aes(x = hour, y = dayOfWeekName, fill = pageviews)) + 
           geom_tile(color='White', size=0.1) +
           scale_fill_viridis(option = "B") +
           scale_y_discrete(limits = c('Sunday', 'Saturday','Friday', 'Thursday', 
                                       'Wednesday', 'Tuesday', 'Monday')) +
           labs(x = "Hour", 
                y = "Day of Week",
                title = "Heat Map by Hour and Day of Week ") +
           coord_equal()
           
plot_heatmap_day_of_week

## Query
df7 <- google_analytics(GA_id, 
                        date_range, 
                        metrics = c( "sessions"), 
                        dimensions = c("hour", "userGender"),
                        anti_sample = TRUE)

## Facet plot by gender 
line_plot_gender <- df7 %>% 
          ggplot(aes(x = hour, y = sessions, group = userGender)) + 
          geom_line(aes(colour = userGender)) +
          facet_grid(userGender ~ ., scales = "free") +
          theme_minimal()

line_plot_gender

## Query
df_affinity <- google_analytics(GA_id, 
                        date_range, 
                        metrics = c( "sessions"), 
                        dimensions = c("interestAffinityCategory"),
                        anti_sample = TRUE)

## Data manipulation with dplyr
df_affinity_cleaned <- df_affinity %>%
                           separate("interestAffinityCategory", "interestAffinityCategory2", sep = "/" ) 
                           ddply(c("interestAffinityCategory2"), summarize, sessions = sum(sessions)) %>% 
                           arrange(-sessions) %>% 
                           mutate(interestAffinityCategory2 = factor(interestAffinityCategory2, 
                                  levels = rev(interestAffinityCategory2)))

affinity_plot <- df_affinity_cleaned  %>% 
  ggplot(aes(x = interestAffinityCategory2, y = sessions)) +
  geom_bar(stat = "identity", fill = "#cd0000") +
  coord_flip() +
  labs(title = "Affinity Category Overview, 2018",
       subtitle = "Lifestyles similar to TV audiences, for example: Technophiles, Sports Fans, and Cooking Enthusiasts.",
       caption = "Source: Google Analytics", 
       x = "Affinity Category", y = "Session") +
  theme_minimal() 

affinity_plot



