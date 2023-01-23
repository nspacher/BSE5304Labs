if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,dplyr,patchwork,rnoaa,tidyverse,lubridate,cowplot)
#theme_set(theme_classic())
print("hello world version 3")

system("git config --global user.email 'nspacher@vt.edu' ") 
system("git config --global user.name 'Nick Bentelspacher' ")

data <- data.frame(
  day = as.Date("2019-01-01") + 0:99,
  tmin = runif(100) + seq(1,100)^2.5 / 10000,
  price = runif(100) + seq(100,1)^1.5 / 10
)

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

#weather plot
precipColor <- "#009E73"
coeff <- 1
start <- ymd("2023/01/01")
end <- ymd("2023/01/14")

weather_plot <- WXData %>% filter(date>start, date<end) %>% ggplot(aes(x=date))+
  geom_line(aes(y=tmin, color="Minimum Temperature"))+
  geom_line(aes(y=tmax, color="Maximum Temperature"))+
  geom_line(aes(y=prcp, color="Precipitation"))+
  geom_line(data=myflowgage$flowdata %>% filter(mdate>start, mdate<end),
            aes(mdate, flow/100000, color="Flow"))+
  scale_y_continuous(
    # Features of the first axis
    name = "Temperature (C)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Precipitation (mm)")
  ) +
  scale_color_manual(name=element_blank(), values= c("Minimum Temperature" = "#0072B2", "Maximum Temperature" = "#D55E00","Precipitation" = "#009E73", "Flow"= "#000000"))+
  xlab(element_blank())+
  ggtitle("Weather Data for Staion USC00443204")

#HW2 plot
streamflow_plot <- myflowgage$flowdata %>% filter(mdate>start, mdate<end) %>% 
  ggplot(aes(mdate, flow))+
  geom_line(aes(color="Flow"))+
  ylab(Flow~(m^3/d))

HW2_w <- WXData %>% filter(date>start, date<end) %>% ggplot(aes(x=date))+
  geom_col(aes(y=prcp, fill="Precipitation"))+
  geom_line(aes(y=tmin, color="Minimum Temperature"))+
  geom_line(aes(y=tmax, color="Maximum Temperature"))+
  geom_line(data=myflowgage$flowdata %>% filter(mdate>start, mdate<end),
            aes(mdate, flow/100000, color="Flow"))+
  scale_y_continuous(
    # Features of the first axis
    name = "Temperature (C)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Precipitation (mm)")
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


# data$tmax = runif(100)*10  + data$tmin
# coeff <- 6
# # A few constants
# tminColor <- "#0000ff"
# tmaxColor <- "#ff0000"
# priceColor <- rgb(0.2, 0.6, 0.9, 1)
# dir.create("pdfs")
# basestr=format(Sys.time(),"./pdfs/%Y%m%d%H%M")
# 
# p1= ggplot(data, aes(x=day)) +
#   geom_line( aes(y=tmin), linewidth=2, color=tminColor) + 
#   geom_line( aes(y=tmax), linewidth=2, color=tmaxColor) + 
#   geom_line( aes(y=price / coeff), linewidth=2, color=priceColor) +
#     scale_y_continuous(
#       # Features of the first axis
#       name = "Temp(C)",
#       
#       # Add a second axis and specify its features
#       sec.axis = sec_axis(~.*coeff, name="Price")
#     ) + 
#     # theme_ipsum() +
#     theme(
#       axis.title.y = element_text(color = "black", size=13),
#       axis.title.y.right = element_text(color = priceColor, size=13)
#     ) +
#     ggtitle("Temperature down, price up")
# 
# filename=paste0(basestr,"graph01.pdf")
# pdf(filename) 
# plot(p1)
# dev.off()
# print("file size")
# print(file.size(filename))
# print("I finished!")
