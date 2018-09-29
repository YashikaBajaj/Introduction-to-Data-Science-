setwd("C:/Users/Yashika Bajaj/Downloads/all_nyt")
> X=lapply(dir(),read.csv)
> data=do.call("rbind",X)
> attach(data)
> data$Age_Group<- ifelse(Age<18, "<18", ifelse((Age>=18) & (Age<25), "18-24", ifelse((Age>24) & (Age<35), "25-34", ifelse((Age>34) & (Age<45), "35-44", ifelse((Age>44) & (Age<55), "45-54", ifelse((Age>54) & (Age<65), "55-64", "65+"))))))
> view(data)
x1<- read.csv(file.choose())
> attach(x1)
x1$CTR<- (x1$Clicks/x1$Impressions)
> ggplot(x1, aes(x1$CTR, colour=x1$Age_Group)) +
  +     geom_density(fill = "blue", alpha = 0.1)
ggplot(x1, aes(x=x1$Impressions, fill=x1$Age_Group)) + geom_histogram(binwidth=.5)
ggplot(x1, aes(x=x1$CTR, fill=x1$Age_Group)) + geom_histogram(binwidth=.2)
x1$Category<- ifelse(Clicks==0, "Not clicked", "Clicked")
x1[,8] <- ifelse(x1[,8] == "Clicked", "1", "0")
x1$Click_behaviour<- ifelse(Clicks==0, 0, 1)
ggplot(x1, aes(x=x1$Click_behaviour, fill=x1$Age_Group)) + geom_histogram(binwidth=.1)
ggplot(x1, aes(x1$Age_Group=="<18", x1$Gender)) +
  +     geom_bar(stat = "identity")
ggplot(x1, aes(x1$Signed_In)) +
  +     geom_density(fill = "blue", alpha = 0.1)
> ggplot(x1, aes(x1$Age_Group)) +
  +     geom_line(stat = "density")
ggplot(x1, aes(x1$Age_Group, x1$Impressions)) +
  +     geom_line()
ggplot(x1, aes(x1$Age_Group, x1$Clicks)) +
  +     geom_line()
ggplot(x1, aes(x1$Age_Group, x1$Gender, color=x1$Gender)) +
  +     geom_bar(stat = "identity")
ggplot(x1, aes(x1$Age_Group, x1$Gender)) +
  +     geom_bar(stat = "identity")
x1 %>%select(x1$Age_Group=="<18",Gender)%>%group_by(Age_Group,Gender)%>%summarise(c=n()) %>%ggplot(aes(x=Age_Group=="<18",y=c,fill=Gender))+geom_bar(stat="identity",position="dodge")+labs(title="Age_Group vs Gender",y="count")
ggplot(x1,aes(Age,Clicks,color=Gender))+ geom_point(size=3)