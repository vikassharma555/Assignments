library(dplyr)
library(lubridate)

# Import data as data frame
walmart_data <- read.csv("Dataset/Walmart_Store_sales.csv")

# new dataframe to store analyzed results
analyzed_data <- cbind.data.frame(Store = unique(walmart_data$Store))


############# 1. Which store has maximum sales ####################

total_sales_data = walmart_data %>%                                     # data sent to next line
                  select("Store", "Weekly_Sales") %>%                 # select variables (columns) of interest
                    group_by(Store) %>%                               # group data by unique store ID
                      summarise(total_sales = sum(Weekly_Sales))        # create new variables(columns)
                  
analyzed_data = transform(analyzed_data, total_sales = total_sales_data$total_sales)
print(paste("Store " , analyzed_data$Store[which.max(analyzed_data$total_sales)], 
      " has maximum sales :", max(analyzed_data$total_sales)))
#rm(total_sales_data)

############# 2. Which store has maximum standard deviation #################
#' i.e., the sales vary a lot. Also, find out the coefficient of mean to standard deviation
temp_sd_data = walmart_data %>%                                     # data sent to next line
                  select("Store", "Weekly_Sales") %>%                 # select variables (columns) of interest
                    group_by(Store) %>%                               # group data by unique store ID
                      summarise(sd = sd(Weekly_Sales), avg_sales = mean(Weekly_Sales)) %>% # sd eval
                        mutate(coeff_m_sd = sd/avg_sales)             #coeff eval

#populate our analyzed_data
analyzed_data = transform(analyzed_data, sd = temp_sd_data$sd, 
                          avg_sales = temp_sd_data$avg_sales, 
                          coeff_m_sd = temp_sd_data$coeff_m_sd)

#analyze the coefficient against Ideal_ratio = 10 % (0.1)
tmp_sd_analysis = analyzed_data %>%
  select(c("Store", "coeff_m_sd")) %>%  # prepare data
  mutate(ratio_percentage = round((coeff_m_sd * 100) , digits = 0), # in percentage
         dev_from_IdelRatio = ifelse (between(ratio_percentage, 5, 15), "Good",
                                      ifelse (between(ratio_percentage, 0, 5) | between(ratio_percentage, 15, 20),
                                      "Acceptable", "Beyond"))
           )             # deviations from Ideal ratio (10%)


analysis = tmp_sd_analysis %>% count(dev_from_IdelRatio)


print(analysis)
print(paste("Store", temp_sd_data[which.max(temp_sd_data$sd),1], "has max standard deviation of:", 
            max(temp_sd_data$sd)))
print(paste("Store/s", round(analysis[1,2]), "are in", analysis[1,1] ,"range of Ideal Ratio"))
print(paste("Store/s", round(analysis[2,2]), "are in", analysis[2,1] , "range of Ideal Ratio"))
print(paste("Store/s", round(analysis[3,2]), "are in", analysis[3,1], "range of Ideal Ratio"))



#rm(tmp_sd_analysis)
#rm(temp_sd_data)
#rm(analysis)


############# 3. Which store/s has good quarterly growth rate in Q3’2012 ############

#' Check whether date is in proper format... if not update in format
if (!is.Date(mode(walmart_data$Date))) walmart_data$Date = lubridate::dmy(walmart_data[,"Date"])

#' add variables(columns) year, month, quarter in walmart_data
walmart_data = walmart_data %>%
                mutate(quarter = quarter(Date), 
                          month = month(Date, label = TRUE, abbr = TRUE),
                          semester = semester(Date),
                           year = year(Date),
                           quarter = factor(quarter, levels = c(1:4), labels= c("Q1", "Q2", "Q3", "Q4")),
                           semester = factor(semester, levels = c(1:2), labels= c("S1", "S2")))

# compute (2 quarter data (Q2 2012,Q3 2012) for Weekly_Sales) from walmart_data
tmp_quarter_data = walmart_data %>%
                subset((quarter == "Q2" |  quarter == "Q3") & year == 2012, select = c("Store",  "quarter", "Weekly_Sales")) %>%
                  group_by(Store, quarter) %>%
                    summarise(sales = sum(Weekly_Sales))

# append Q2,Q3 to analyzed_data
analyzed_data = analyzed_data %>%
                  mutate(Q2_2012 = pull(subset(tmp_quarter_data, tmp_quarter_data$quarter == "Q2", select = "sales")),  #from tibble created by subset, pull vector
                         Q3_2012 = pull(subset(tmp_quarter_data, tmp_quarter_data$quarter == "Q3", select = "sales")),  #from tibble created by subset, pull vector
                         Q3_2012_growth = (( (Q3_2012 - Q2_2012) / Q2_2012) * 100) )

#analyze  the positive growths
tmp_stores_good_growth = analyzed_data %>%
                      subset(Q3_2012_growth > 0, select = c("Store", "Q3_2012_growth")) %>%
                        arrange(desc(Q3_2012_growth)) %>%
                          slice(1:4) %>%
                              mutate(Q3_2012_grate = paste0(round(Q3_2012_growth, digits = 0), "%"))
 
print(tmp_stores_good_growth)

#rm(tmp_quarter_data)
#rm(tmp_stores_good_growth)

############# 4. Some holidays have a negative impact on sales. #####################
#' Find out holidays which have higher sales than the mean sales in non-holiday season for all stores togethe

#' # list of holidays
temp_holiday = c("12-Feb-10", "11-Feb-11", "10-Feb-12", "8-Feb-13", "10-Sep-10", "9-Sep-11", 
                 "7-Sep-12", "6-Sep-13", "26-Nov-10", "26-Nov-10", "23-Nov-12", "29-Nov-13",
                 "31-Dec-10", "30-Dec-11", "28-Dec-12", "27-Dec-13")

# holidays in Date format
temp_holiday_date = lubridate::dmy(temp_holiday)

# avg of all sales non-holiday
temp_s_nh = walmart_data %>%
                      subset(Holiday_Flag == 0, select = c("Weekly_Sales")) %>%
                        summarise(sales = mean(Weekly_Sales)) 

# analyze avg of all sales on holiday_date
temp_s_h = walmart_data %>%
                    subset((Holiday_Flag == 1 & any(Date %in% temp_holiday_date)), 
                           select = c("Date", "Weekly_Sales")) %>%
                      group_by(Date) %>%
                        summarise(sales = mean(Weekly_Sales)) %>%
                          mutate(sales_high = sales > temp_s_nh$sale)

print(temp_s_h)
paste("Following holiday dates have higher sales than non-holiday: ")
print(temp_s_h %>% subset(sales_high == TRUE, select= c("Date", "sales_high") ))

# rm(temp_holiday)
# rm(temp_holiday_date)
# rm(temp_s_nh)
# rm(temp_s_h)



############# 5. Provide a monthly and semester view of sales in units and give insights #############  
temp_sales_sem = walmart_data %>%
                  group_by(year, semester) %>%
                    summarise(sales_sem = sum(Weekly_Sales))
temp_sales_sem$Sem_Year = interaction(temp_sales_sem$semester, temp_sales_sem$year, sep=" ")


temp_sales_month = walmart_data %>%
              group_by(year, month) %>%
                summarise(sales_month = sum(Weekly_Sales))
temp_sales_month$month_year = interaction(temp_sales_month$month, temp_sales_month$year, sep=" ")

# plot month
options(scipen=999999)
len = length(temp_sales_month$month_year)
x1 = 1:len
y1 = temp_sales_month$sales_month
plot(y1 ~ x1, frame.plot= TRUE, xaxt='n', xlab = "Months of 2011, 2012, 2013", ylab = "Sales")
axis(1, 1:len, temp_sales_month$month)
curve(splinefun(x1, y1, method = "monoH.FC")(x), add = TRUE, lty  = 1, lwd = 1.5, col = "blue", n = 1001)
grid()

# plot semester
options(scipen=999999)
len = length(temp_sales_sem$Sem_Year)
x1 = 1:len
y1 = temp_sales_sem$sales_sem
plot.default(y1 ~ x1, frame.plot= TRUE, xaxt='n', xlab = "Semester Year", ylab="Sales",
     panel.first = grid(length(len), length(len)))
axis(1, 1:len, temp_sales_sem$Sem_Year)
curve(splinefun(x1, y1, method = "monoH.FC")(x), add = TRUE, lty  = 1, lwd = 2, bg = "red", col = "blue", n = 1001)
grid()

print(temp_sales_sem)
print(temp_sales_month)

# rm(temp_sales_sem)
# rm(temp_sales_month)

#' ################### Linear Regression ########################
#' Statistical Model
#' For Store 1 – Build  prediction models to forecast demand
#'        Linear Regression – Utilize variables like date and restructure dates as 1 for 5 Feb 2010 (starting from the earliest date in order). Hypothesize if CPI, unemployment, and fuel price have any impact on sales.
#'        Change dates into days by creating new variable.
#' Select the model which gives best accuracy.

############# functions and variables utilized for Regression analysis ##############################
# significance computations, p-value of model
significance_range = list("***" = c(left = 0, right = 0.001), "**" = c(left = 0.001, right = 0.01), "*" = c(left = 0.01, right = 0.05), 
                          "." = c(left = 0.05, right = 0.1), " " = c(left = 0.1, right = 1))

#return significance stars shown in summary
getStars = function(significance){
  range_result = sapply(significance_range, FUN = function(range) {
    return (between(significance, range["left"], range["right"]))
  })
  ifelse(range_result == TRUE, # if we found the significance range for p-value of variable
         return(names(significance_range)[which(range_result)][1]), # return the stars that we see in summary..return first match in cases of multiple matches
         return(" ")) # return default nothing
}

#shamelessly copied from stackoverflow
p_value <- function (summary) {
  if (class(summary) != "summary.lm") return ("")
  f <- summary$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

bind_coeff = function(df, summary){
  temp_df = data.frame(summary$coefficients[,])
  temp_df = temp_df[-1,] # remove row for intercept
  if (nrow(df) == 0) return(temp_df)
  return (rbind(df, temp_df))
  
}


############# Regression Analysis Data preparation ##############################

#. create data
store_ID = "1"
col_vars = c("Date", "CPI", "Unemployment", "Fuel_Price", "Weekly_Sales")

store1_data = walmart_data %>%
  subset(Store == store_ID, select = col_vars) %>% # subset walmart_data
  arrange(Date) #%>%  #sort Date ascending order
#mutate(Date = seq(1:nrow(.))) #replace date with sequence of numbers
store1_data = mutate(store1_data, Date = seq(1:nrow(store1_data))) #replace date with sequence of numbers

############# Simple Linear Regression analysis  ##############################

#require(utils) # for attach
attach(store1_data) # attach a data frame as database so we can directly call variable names (instead of store1_data$Date, simply Date will do)

# plot graphs
op <- par(mfrow = c(2,2), mar = .1+ c(2,2,3,1))
plot(Weekly_Sales ~ Date, main = "Weekly_Sales(y), Date(x)") 
mtext(paste("corr: ", cor(Weekly_Sales, Date)), col = "red", cex = 0.8)

plot(Weekly_Sales ~ CPI, main = "Weekly_Saless(y), CPI(x)")
mtext(paste("corr: ", cor(Weekly_Sales, CPI)), col = "red", cex = 0.8)
plot(Weekly_Sales ~ Unemployment, main = "Weekly_Sales(y), Unemployment(x)")
mtext(paste("corr: ", cor(Weekly_Sales, Unemployment)), col = "red", cex = 0.8)

plot(Weekly_Sales ~ Fuel_Price, main = "Weekly_Sales(y), Fuel_Price(x)")
mtext(paste("corr: ", cor(Weekly_Sales, Fuel_Price)), col = "red", cex = 0.8)
par(op)


# run model
lm1 = lm(Weekly_Sales ~Date)
lm2 = lm(Weekly_Sales ~CPI)
lm3 = lm(Weekly_Sales ~Unemployment)
lm4 = lm(Weekly_Sales ~Fuel_Price)

# summary
s_lm1 = summary(lm1)  # Date significance = "*"
s_lm2 = summary(lm2)  # CPI significance  = "**"
s_lm3 = summary(lm3)  # Unemployment significance = NULL
s_lm4 = summary(lm4)  # Fuel_Price significance = NULL

#bind coeficients
summary_data = data.frame() %>%
                bind_coeff(s_lm1) %>%
                  bind_coeff(s_lm2) %>% 
                    bind_coeff(s_lm3) %>% 
                      bind_coeff(s_lm4)

#bind significance, adjusted r-squared, 
significances = pull(summary_data %>% select(starts_with("Pr")))
summary_data = summary_data %>%
                mutate(significance = sapply(significances, getStars),
                       Adjusted_r_squared = c(s_lm1$adj.r.squared, s_lm2$adj.r.squared, s_lm3$adj.r.squared, 
                                              s_lm4$adj.r.squared),
                       p_value_Model = c(p_value(s_lm1), p_value(s_lm2), p_value(s_lm3), p_value(s_lm4)))             

############# Multiple Linear Regression #####################

# run linear model
lm_multi = lm(Weekly_Sales ~Date+CPI+Unemployment+Fuel_Price)

# summary
s_lm_multi = summary(lm_multi)

#bind coeficients
summary_data_multi = data.frame() %>%
                      bind_coeff(s_lm_multi)

#bind significance
significances = pull(summary_data_multi %>% select(starts_with("Pr")))
summary_data_multi = summary_data_multi %>%
                mutate(significance = sapply(significances, getStars))

print(summary_data_multi)
print(paste("p-value: ", p_value(s_lm_multi)))
print(paste("Adjusted-r: ", s_lm_multi$adj.r.squared))


# run linear model dropping Date (with CPI, Unemployment, Fuel_Price)
lm_multi2 = lm(Weekly_Sales ~CPI+Unemployment+Fuel_Price)

# summary
s_lm_multi2 = summary(lm_multi2)

# choosing the model with (Weekly_Sales ~CPI+Unemployment+Fuel_Price)
predicted_sales = predict(lm_multi2)
store1_data = mutate(store1_data, predicted_sale = predicted_sales)

store1_data = transform(store1_data, Err_pct=(abs(store1_data$Weekly_Sales -store1_data$predicted_sale)/store1_data$Weekly_Sales))
#View(store1_data)

# Model accuracy
mean(store1_data$Err_pct)
1- mean(store1_data$Err_pct)

#bind coeficients
summary_data_multi2 = data.frame() %>%
  bind_coeff(s_lm_multi2)

#bind significance
significances = pull(summary_data_multi2 %>% select(starts_with("Pr")))
summary_data_multi2 = summary_data_multi2 %>%
  mutate(significance = sapply(significances, getStars))

print(summary_data_multi2)
print(paste("p-value: ", p_value(s_lm_multi2)))
print(paste("Adjusted-r: ", s_lm_multi2$adj.r.squared))


# an overshot since we already know the answer to our model
# why Unemployment is significant in multiple regression whereas non-significant in Simple regression

# check correlations with other variables
op <- par(mfrow = c(2,2), mar = .1+ c(2,2,3,1))
plot(CPI ~ Unemployment, main = "CPI(y), Unemployment(x)") 
mtext(paste("corr: ", cor(CPI, Unemployment)), col = "red", cex = 0.8)

plot(Fuel_Price ~ Unemployment, main = "Fuel_Price(y), Unemployment(x)")
mtext(paste("corr: ", cor(Fuel_Price, Unemployment)), col = "red", cex = 0.8)

par(op)

detach(store1_data)

