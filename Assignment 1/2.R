data$weekday <- paste('2012','05',S$day, sep = '-')  
> data$weekday <- paste('2012','05',data$day, sep = '-')
data$weekday <- paste('2012','05',data$Day, sep = '-') 
View(data)
> data$weekday <- weekdays.Date(as.Date(data$weekday))
> d.order <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
> dt.imp <- aggregate(data$Impressions, by=list(weekday=data$weekday),sum)
> dt.clk <- aggregate(data$Clicks, by=list(weekday=data$weekday),sum)
> dt.sgn <- aggregate(data$Signed_In, by=list(weekday=data$weekday),sum)
mt.clk <- aggregate(data$Clicks, by=list(weekday=data$Day),sum) 
> mt.clk <- aggregate(data$Clicks, by=list(weekday=data$Day),sum) 
> mt.sgn <- aggregate(data$Signed_In, by=list(weekday=data$Day),sum)
> dt.imp$weekday <- factor(as.character(dt.imp$weekday), levels = d.order)
> dt.imp <- dt.imp[order(dt.imp$weekday),]
> dt.clk$weekday <- factor(as.character(dt.clk$weekday), levels = d.order)
> dt.clk <- dt.clk[order(dt.clk$weekday),]
> dt.sgn$weekday <- factor(as.character(dt.sgn$weekday), levels = d.order)
> dt.sgn <- dt.sgn[order(dt.sgn$weekday),]
> vizr <- function(data, ylab, title){
  +     require(ggplot2)
  +     p<-ggplot(data, aes(weekday, x, group=1)) + geom_line() + 
    +         labs(x="Days", y= ylab ,title = title) + geom_smooth(method=lm)
  +     return(p)
  + }
> vizr(dt.imp, "# of Impressions", "Impressions by day of week")
> vizr(dt.clk, "# of Clicks", "Clicks by day of week")
> vizr(dt.sgn, "# of Sign-ins", "Sign-ins by day of week")
mt.imp <- aggregate(data$Impressions, by=list(weekday=data$Day),sum) 
> vizr(mt.imp, "# of Impressions", "Impressions by day of May")
> vizr(mt.clk, "# of Clicks", "Clicks by day of May")
> vizr(mt.sgn, "# of Sign-ins", "Sign-ins by day of May")