library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(zoo)
library(plotly)

# Import financial data ----
person_1 <- read.csv("../data/example_statement_1.csv", stringsAsFactors=FALSE, header = TRUE, na.strings=c("","NA"))

person_2 <- read.csv("../data/example_statement_2.csv", stringsAsFactors=FALSE, header = TRUE, na.strings=c("","NA"))

person_1$label <- "person_1"
person_2$label <- "person_2"

df <- merge(person_1, person_2, all = TRUE)

df$Date <- as.Date(df$Booking.date, "%d.%m.%Y")
df$Year <- format(as.Date(df$Date), "%Y")
df$Month <- format(as.Date(df$Date), "%b")
df$month <- format(as.Date(df$Date), "%m")
df$Day <- format(as.Date(df$Date), "%d")
df$DayOfYear <- as.numeric(format(as.Date(df$Date), "%j"))
df$Year_month <- paste(df$Year, df$month, sep="-")

# Label change in income/outcome  ----
# E.g. Split colors based on moving apartment which now costs more
A <- df %>% subset(Date < as.numeric(as.Date("2023-09-27")) )
A$term <- "A"

B <- df %>% subset(Date >= as.numeric(as.Date("2023-09-27")) )
B$term <- "B"

df <- rbind(A, B)
rm(A, B)

# line colors
cols1 <- c("#ff8080", "#ff4d4d", "#3399ff")

# equity plot ----
p1 <- 
  df %>% 
  ggplot(aes(x = Date, y = Balance, 
             group = label))+
  geom_point(alpha=0.3, size = 1 )+
  geom_smooth(aes(color = cols1[1])) +
  ylim(0, 80000)+
  scale_x_date(expand = c(0, 0),
               limits = as.Date(c("2022-06-16","2024-10-15")),
               date_breaks = "1 months"
  )+
  #theme( axis.title.x=element_blank(), 
   #      axis.text.x=element_blank())+
  labs(y="Equity CHF") +
  geom_vline(xintercept = as.numeric(as.Date("2023-09-15")), linetype=2)+
  stat_smooth(data=subset(df, term== "A"), method = "lm", aes(color = cols1[2]), linetype=2)+
  stat_smooth(data=subset(df, term== "B"), method = "lm", aes(color = cols1[3]), linetype=2) + 
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(
    angle = 45, hjust = 1),
    legend.position="none")

p1
ggplotly(p1)

# monthly change calculate ----
df_debit <- 
  df %>% 
  ungroup() %>%
  select(Year_month, Debit) %>%
  na.omit() %>%
  group_by(Year_month) %>% 
  summarize(sum = (-1 * sum(Debit)),
            min = min(Debit), 
            max = max(Debit),
            SD=sd(Debit),
            n = n()
            )

df_credit <- 
  df %>% 
  ungroup() %>%
  select(Year_month, Credit) %>%
  na.omit() %>%
  group_by(Year_month) %>% 
  summarize(sum = sum(Credit),
            min = min(Credit), 
            max = max(Credit),
            SD=sd(Credit),
            n = n())

df_credit$Type <- "Credit" 
df_debit$Type <- "Dedit" 
df_monthly <- merge(df_debit, df_credit, all = TRUE)

# monthly change plot ----
p2 <- 
  df_monthly %>% 
  ungroup() %>% 
  ggplot(aes(x = Year_month, y = sum, group = 1 , fill=Type)) +
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=sum-SD, ymax=sum+SD),
                colour="black",
                alpha = 0.3,
                width=.5) +
  theme( axis.title.x=element_blank(), 
         axis.text.x=element_blank(),
         legend.position="none")+
#  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="bottom")+
  labs(y="Monthly change")
p2
ggplotly(p2)

# combined plot  ----
require(gridExtra)

grid.arrange(p1, p2, ncol=1, 
                   heights=c(2,2),
                   bottom = "Top - total balance over time. Bottom - month income/expense.
Dotted line shows a change in living costs.") # equity_combined.pdf 6x8

# estimate spending per month (incl rent) ----
# Note the estimate can be set based on a specific period such that the living expense change is best represented

# combined equity plot ----
# combined mean per month and per day
df_mothly_combined <- df %>%
  select(label, Year_month, Balance, Date) %>%
  na.omit() %>%
  group_by(Year_month, label) %>%
  summarize(month_mean = (mean(Balance)),
            month_min = min(Balance), 
            month_max = max(Balance),
            month_SD=sd(Balance),
            month_n = n()) %>%
  group_by(Year_month) %>%
  summarize(month_sum = (sum(month_mean)),
            month_min = min(month_mean), 
            month_max = max(month_mean),
            month_SD=sd(month_mean),
            month_n = n()) %>%
  ungroup() 

df_mothly_combined$yrmo <- 
  as.yearmon(df_mothly_combined$Year_month, "%Y-%m")

df_mothly_combined$Date <- 
as.Date(df_mothly_combined$yrmo)

# equity with monthly changes
p_comb_month <-
  df_mothly_combined %>% 
  ggplot(aes(x = Date, y = month_sum))+
  geom_point(alpha=0.3, size = 1 )+
    geom_errorbar(aes(ymin=month_sum-month_SD, 
                      ymax=month_sum+month_SD),
                  width=10) +
  geom_smooth(aes(color = cols1[1])) +
  scale_x_date(expand = c(0, 0),
               limits = as.Date(c("2022-06-16","2024-10-15")),
               date_breaks = "1 months"
  )+
   scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  theme( axis.title.x=element_blank(), 
         axis.text.x=element_blank())+
  labs(y="Equity CHF") +
  theme(legend.position = "none")
# p_comb_month # df_mothly_mean.pdf 4x8

df_daily_combined <- df %>%
  select(label, Year_month, Balance, Date) %>%
  na.omit() %>%
  group_by(label, Date) %>%
  summarize(day_mean = (mean(Balance)),
            day_min = min(Balance), 
            day_max = max(Balance),
            day_SD=sd(Balance),
            day_n = n()) %>%  
  group_by(Date) %>%
  summarize(day_sum = (sum(day_mean)),
            day_min = min(day_mean), 
            day_max = max(day_mean),
            day_SD=sd(day_mean),
            day_n = n()) %>%
  ungroup() 

# equity with daily changes
p_comb_day <-
  df_daily_combined %>% 
  ggplot(aes(x = Date, y = day_sum))+
  geom_point(alpha=0.3, size = 1 )+
  geom_smooth(aes(color = cols1[1])) +
  scale_x_date(expand = c(0, 0),
               limits = as.Date(c("2022-06-16","2024-10-15")),
               date_breaks = "1 months"
  )+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  coord_cartesian(ylim = c(0, 150000))+
  theme(
    axis.title.x=element_blank(), 
    axis.text.x=element_blank(),
    #axis.text.x = element_text( angle = 45, hjust = 1), 
    legend.position="none")+
  labs(y="Equity CHF")+
  theme(legend.position = "none")
p_comb_day
# p_comb_day # df_mean_day.pdf 5x8

# Plot together
require(gridExtra)
grid.arrange(p_comb_month, p_comb_day, ncol=1, 
             heights=c(2,2.5),
             bottom = "Equity change over time, monthly (top) and daily (bottom).") # combined_equity.pdf 8x8

# Linear model to predict monthly savings ----
# modify the df: Don't include recent month that had an unusual large debit
df_mod <- df_mothly_combined %>% head(33)

str(df_mod)
df_mod$Date <- as.factor(df_mod$Date)
df_mod$Date <- as.numeric(df_mod$Date)
lm <- glm(month_sum ~ Date,data=df_mod,family=gaussian(link = "identity"))
summary(lm) # display results
coef(lm)
confint(lm) # 95% CI for the coefficients
exp(cbind(coef(lm), confint(lm))) # exponentiated coefficients and 95% CI for exponentiated coefficients
predict <- predict(lm, type="response") %>%
  as.data.frame() # predicted values 
residuals(lm, type="deviance") # residuals
predict$Year_month <- df_mod$Year_month
predict$total <- predict$.

predict%>% 
  ggplot(aes(x = Year_month, y = total ))+
  geom_point(alpha=0.3, size = 1)+
  geom_smooth(aes(color = cols1[1])) + 
  theme( axis.text.x = element_text(angle = 45, hjust = 1) )
# prediction of ~4'300 per month

# After buying ----
# Once you buy a home, you will likely spend most of your cash and have close to zero again. 
# Calculate how much income versus spending
# Determine how long you can last based on your guaranteed income

# monthly spend ----
# Estimate spending per month (incl rent/mortgage)
month_spend <- df %>%
  select(Year_month, Debit, Date) %>%
  na.omit() %>%
  filter(Date >= ("2024-03-01")) %>%
  filter(Date < ("2024-11-01")) %>%
  group_by(Year_month) %>%
  filter(Debit < 3000) %>% # exclude a large once off debit
  summarize(total = (sum(Debit)),
            min = min(Debit), 
            max = max(Debit),
            SD=sd(Debit),
            n = n()) %>%
  ungroup() %>%
  summarize(mean = (mean(total)),
            min = min(total), 
            max = max(total),
            SD=sd(total),
            n = n())
month_spend
#saving/income - costs

# guaranteed equity ----
# Make an expected future debit df
# Get the most recent date
current_date <- (tail(df_mothly_combined$Date, 1))

# Print the dates for future months, e.g. 12 months
# set values for the known income period
# Define known incomes for person 1 and person 2

total_period = 48 # months
Date <-c( seq(as.Date(current_date),length=total_period,by="months") )
Expense  <- c(rep(month_spend$mean,length=total_period))

# person 1
total_income_1 = 5000
known_period_1 = 12
unknown_period_1 = (total_period - known_period_1)
Income_1  <- c( rep( total_income_1, length=known_period_1), rep(0, length=unknown_period_1))

# person 2
total_income_2 = 4999
known_period_2 = 24
unknown_period_2 = (total_period - known_period_2)
Income_2  <- c( rep( total_income_2, length=known_period_2), rep(0, length=unknown_period_2))

Income <- (Income_1 + Income_2)
future <- data.frame(Date, Expense, Income)

future$saving <- future$Income - future$Expense
future$equity <- cumsum(future$saving) 

# guaranteed equity plot ----
future %>% 
  ggplot(aes(x = Date, y = equity))+
  geom_point(alpha=0.3, size = 1 )+
  geom_smooth(aes(color = cols1[1])) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
 # coord_cartesian(ylim = c(0, 30000))+
  geom_hline(linetype="dotted", yintercept= 0) +
  theme(axis.text.x = element_text(
    angle = 45, hjust = 1),
    legend.position="none")+
  labs(y="Equity CHF")+
  theme(legend.position = "none")
# df_future.pdf 5x8

# guaranteed equity insurance ----
# Account for unemployed insurance (70% of income)
# Chose the person, e.g. person 2
total_income_insr = ((total_income_2) * 0.7)
Income_3 <- c( rep( total_income_2, length=known_period_2), rep(total_income_insr, length=unknown_period_2))

Income_insr <- (Income_1 + Income_3)
future_insr <- data.frame(Date, Expense, Income_insr)

future_insr$saving_insr <- future_insr$Income_insr - future_insr$Expense
future_insr$equity_insr <- cumsum(future_insr$saving_insr) 

future_merged <- merge(future_insr, future)
future_merged <- gather(future_merged, key = "period", value = "equity", equity_insr, equity)

# Get the end data for known periods (contract end)
Date_known_period_1 <- tail(c( seq(as.Date(current_date),length=known_period_1,by="months") ), 1)
Date_known_period_2 <- tail(c( seq(as.Date(current_date),length=known_period_2,by="months") ), 1)

# guaranteed equity insurance plot ----
p3 <- future_merged %>% 
  ggplot(aes(x = Date, y = equity ))+
  geom_point(alpha=0.9, size = 1, aes( color=period))+
  geom_smooth(aes(color = period)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  # coord_cartesian(ylim = c(0, 30000))+
  geom_hline(linetype="dotted", yintercept= 0) +
  theme(axis.text.x = element_text(
    angle = 45, hjust = 1) )+
  labs(y="Equity CHF") +
  geom_vline(linetype="dotted", xintercept= ( Date_known_period_1) ) +
  geom_vline(linetype="dotted", xintercept= ( Date_known_period_2) ) +
  geom_text(aes(x=Date_known_period_1, y=0, label="person 1\ncontract end"), colour="darkgrey", angle=45)+
  geom_text(aes(x=Date_known_period_2, y=0, label="person 2\ncontract end"), colour="darkgrey", angle=45)

p3


# ten year example ----
total_period = 120 # months
Date_10 <-c( seq(as.Date(current_date),length=total_period,by="months") )
Expense_10  <- c(rep(month_spend$mean,length=total_period))

known_period_10 = 120
Income_10_1  <- c( rep( total_income_1, length=known_period_10))
Income_10_2  <- c( rep( total_income_2, length=known_period_10))

Income_10 <- (Income_10_1 + Income_10_2)
future_10 <- data.frame(Date_10, Expense_10, Income_10)

future_10$saving <- future_10$Income - future_10$Expense_10
future_10$equity <- cumsum(future_10$saving) 

# guaranteed equity plot ----
p4 <- future_10 %>% 
  ggplot(aes(x = Date_10, y = equity))+
  geom_point(alpha=0.3, size = 1 )+
  geom_smooth(aes(color = cols1[1])) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  # coord_cartesian(ylim = c(0, 30000))+
  geom_hline(linetype="dotted", yintercept= 0) +
  theme(axis.text.x = element_text(
    angle = 45, hjust = 1),
    legend.position="none")+
  labs(y="Equity CHF")+
  theme(legend.position = "none")
# df_future.pdf 5x8

# Summary plots ----
# repeat of plots 
grid.arrange(p1, p2, p_comb_month, p_comb_day, p3, p4, ncol=1,
             heights=c(3,2,2,2,2,2),
             bottom = "A: current total balance over time (dotted line - change in living costs).
             B: current monthly income and expenses.
             C. Equity change over time, monthly.
             D. Equity change over time, daily.
             E: forcast guaranteed equity with insurance (dotted line - switch to insured income).
             F: forcast 10 year with fulltime, modest contract."
             ) # ../img/equity_combined.pdf 24x8

