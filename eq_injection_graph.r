library(zoo) # Convert Year_Month to Date format
# Given location, radius, minimum magnitude, and time window, plot earthquake rate, injection rate on interval, and earthquake magnitude
Interval = "Deep"
Radius = 20
start_date = as.Date("2010-01-01")
end_date = as.Date("2023-06-01")
min_mag = 3

# plot map of earthquakes and injection wells

# Load earthquakes from file
earthquakes <- read.csv("C:/MOVE/Documents/CISR/Data/20240517_all_events.csv", header=TRUE, sep=',', stringsAsFactors = FALSE)
earthquakes$value = rep(1, nrow(earthquakes))
earthquakes$Origin.Date<- as.Date(earthquakes$Origin.Date)
earthquakes <- subset(earthquakes, Origin.Date >= start_date & Origin.Date <= end_date)

mag_subset <-subset(earthquakes, Local.Magnitude >=  min_mag)
#mag_subset$Origin.Date <- as.Date(mag_subset$Origin.Date)
eq_month_count <- mag_subset %>% group_by(Year_Month = cut(Origin.Date, breaks = "1 month")) %>% summarise(total = sum(value))

earthquakes$Year_Month <-  as.Date(cut(earthquakes$Origin.Date, breaks = "1 month"))
eq_month_count$Year_Month <- as.Date(eq_month_count$Year_Month)

# Calculate the 12-month moving average
eq_month_count$moving_avg <- rollmean(eq_month_count$total, k = 12, align = "right", fill = NA)

# Load injection data from file and foramt dates
library(readxl)
inj_head <- read_excel("C:/MOVE/Documents/CISR/Data/Delaware_Basin_CMEZ_for_Bissett.xlsx")
inj <- read_excel("C:/MOVE/Documents/CISR/Data/Delaware_Basin_CMEZ_for_Bissett.xlsx", sheet = 2)
names(inj)[str_detect(names(inj), "\\d{5}")] <- format(as.Date(as.numeric(names(inj)[str_detect(names(inj), "\\d{5}")]), origin = "1899-12-30"), "%Y-%m-%d")
total_injection <- cbind(inj_head, inj[, c(3:4, 8:490)])
total_injection <- total_injection[which(total_injection$`Relative Depth`== Interval),]

# Calculate the total bbl for a given interval
total.bbl <- round(as.numeric((colSums(total_injection[,-c(1:16)], na.rm=TRUE))/10000000), 2)
month_yr <- format(as.Date(names(total_injection)[-c(1:16)]))
injection <- data.frame(cbind(format(as.Date(month_yr)), as.numeric(total.bbl)))
names(injection) = c("Year_Month", "BBL")
injection <- subset(injection, Year_Month >= start_date & Year_Month <= end_date)
injection$Year_Month <- as.Date(injection$Year_Month)
injection$sum = cumsum(injection$BBL)/100

merge_table <- merge(injection, eq_month_count, by = "Year_Month", all.x = T, all.y = T)
# Ensure BBL and sum columns are numeric
merge_table$BBL <- as.numeric(as.character(merge_table$BBL))
merge_table$sum <- as.numeric(as.character(merge_table$sum))

labels <-  c(expression(paste('Rate of Deep Wastewater Injection per Month(10'^7, ' barrels/mo)', sep = "")), 
expression(paste('12 Month Moving Average of M'[L], ' > 3.0+ Earthquakes per Month', sep = "")), 
expression(paste('Total Deep Wastewater Injection(10'^9, ' barrels)', sep = "")), 
expression(paste('M'[L], ' > 3.0+ Earthquakes per Month', sep = "")))

## Plotting
coeff <-  5 # Coefficient of multipication for secondary y axis

#png("C:/MOVE/Documents/CISR/Results/EQ_Injection_Graph/EQ_Injection_Graph_20180601_20230401.png", height=3, width=6, units="in", res=300)
ggplot(merge_table, aes(x = Year_Month)) +
  geom_line(aes(y = as.numeric(BBL), color = "BBL"), linetype = "solid") +
  geom_line(aes(y = as.numeric(sum), color = "sum"), linetype = "dashed") +
  scale_color_manual(values = c("BBL" = "purple", "sum" = "purple", "total" = "black", "moving_avg" = "black"), labels = labels) +
  labs(x = "Year", y = paste("Earthquake Rate,", "Wastewater Injection Rate and Total", sep = "\n"), size = 5) +
  theme(panel.grid.major.y  = element_line(size = 0.1, color = "gray"), 
    panel.grid.major.x = element_blank(), 
    panel.background = element_blank(), 
    legend.position = c(0.24, 0.81), 
    legend.title = element_blank(), 
    legend.key = element_blank(), 
    #legend.background = element_blank(), 
    legend.text = element_text(size=5),
    legend.key.height=unit(0.6,"line"),
    legend.key.width=unit(1,"line"),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    panel.border = element_rect(color = "black", fill = NA)) +
  scale_y_continuous(sec.axis = sec_axis(~ ./coeff, name = expression(paste('Earthquake Magnitude (M '[L], ')', sep = "")),
    breaks = seq(0, max(earthquakes$Local.Magnitude, na.rm = TRUE), by = 1),  labels = scales::comma),
    expand = c(0, 0)) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  geom_point(data = earthquakes, aes(x = Origin.Date, y = Local.Magnitude*coeff),color = "gray", shape = 1) +
  #geom_point(data = earthquakes, aes(x = Origin.Date, y = Magnitude*coeff, color = "Earthquake Magnitude"), shape = 1) +
  #scale_color_manual(values = c("Earthquake Magnitude" = "gray"), labels = "Earthquake Magnitude") +  # Adding point to legend
  theme(axis.title.y.right = element_text(color = "black")) +
  geom_line(aes(y = total, color = "total"), linetype = "solid") +
  geom_line(aes(y = moving_avg, color = "moving_avg"), linetype = "dashed") 
#dev.off()

  
