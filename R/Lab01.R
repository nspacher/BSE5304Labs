if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,dplyr,patchwork,rnoaa,tidyverse,lubridate,cowplot)
theme_set(theme_classic())
print("hello world version 3")

system("git config --global user.email 'nspacher@vt.edu' ") 
system("git config --global user.name 'Nick Bentelspacher' ")

source("https://goo.gl/Cb8zGn") #function for getting usgs data
myflowgage_id="01668000"
myflowgage=get_usgs_gage(myflowgage_id,
                         begin_date="2017-02-01",end_date="2023-02-01")
#class(myflowgage)
View(myflowgage$flowdata)
#str(myflowgage)

plot(myflowgage$flowdata$mdate,myflowgage$flowdata$flow,
     main=myflowgage$gagename,xlab = "Date",
     ylab="Flow m^3/day",type="l")

#gathering weather data using rnoaa function
stns=meteo_distance(
  station_data=ghcnd_stations(),
  lat=myflowgage$declat,
  long=myflowgage$declon,
  units = "deg",
  radius = 20,
  limit = NULL
)

weather_station_ID <- "USC00443204"

WXData=meteo_pull_monitors(
  monitors=weather_station_ID,    
  keep_flags = FALSE,
  date_min = "2016-01-01",
  date_max = NULL,
  var = c("TMAX","TMIN","PRCP") 
)
#?meteo_pull_monitors has units for the data in the WXData dataframe

WXData <- WXData %>% mutate(tmin=tmin/10,
                            tmax=tmax/10,
                            prcp=prcp/10)

#HW1 weather plot
precipColor <- "#009E73"
start <- ymd("2022/11/01")
end <- ymd("2023/01/01")

weather_plot <- WXData %>% filter(date>start, date<end) %>% ggplot(aes(x=date))+
  geom_col(aes(y=prcp, fill="Precipitation"), alpha = 0.6)+
  geom_line(aes(y=tmin, color="Minimum Temperature"))+
  geom_line(aes(y=tmax, color="Maximum Temperature"))+
  scale_y_continuous(
    # Features of the first axis
    name = "Temperature (C)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~., name="Precipitation (mm)")
  ) +
  scale_color_manual(name=element_blank(), values= c("Minimum Temperature" = "#0072B2", "Maximum Temperature" = "#D55E00"))+
  scale_fill_manual(name=element_blank(), values=c("Precipitation" = "#009E73"))+
  xlab(element_blank())+
  ggtitle("Weather Data for Staion USC00443204")

#HW2 plot
streamflow_plot <- myflowgage$flowdata %>% filter(mdate>start, mdate<end) %>% 
  ggplot(aes(mdate, flow))+
  geom_line(aes(color="Flow"))+
  ylab(Flow~(m^3/d))

HW2_w <- WXData %>% filter(date>start, date<end) %>% ggplot(aes(x=date))+
  geom_col(aes(y=prcp, fill="Precipitation"), alpha = 0.6)+
  geom_line(aes(y=tmin, color="Minimum Temperature"))+
  geom_line(aes(y=tmax, color="Maximum Temperature"))+
  geom_line(data=myflowgage$flowdata %>% filter(mdate>start, mdate<end),
            aes(mdate, flow/1000000, color="Flow"))+
  scale_y_continuous(
    # Features of the first axis
    name = "Temperature (C)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~., name="Precipitation (mm)")
  ) +
  scale_color_manual(element_blank(), values= c("Minimum Temperature" = "#0072B2", "Maximum Temperature" = "#D55E00","Flow"= "#000000"))+
  scale_fill_manual(element_blank(), values=c("Precipitation" = "#009E73"))+
  xlab(element_blank())+
  ggtitle("Weather and Streamflow for the Rappahannock River near Fredericksburg, VA")

HW2_plot <- wrap_elements(get_plot_component(streamflow_plot, "ylab-l"))+
  wrap_elements(get_y_axis(streamflow_plot))+
  HW2_w+
  plot_layout(widths=c(3,1,40))


HW2_plot

