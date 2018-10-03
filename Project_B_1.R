install.packages('tidyquant')
library('tidyquant')

setwd('~/15.458/HW/HW2')
input='Query2_date.txt'
mydata=read.delim(input, sep=',')
#mydata=setNames(mydata, c('d', 'id', 'r', 'ticker'))
mydata$date=as.Date(mydata$nm_tradedate, format='%m/%d/%Y')
mydata$project_flag=NULL
mydata$nm_tradedate=NULL

# some of useful funciton taught on class
mss = function(x) round(c(mean(x,na.rm=T)*252, sqrt(252)*sd(x, na.rm=T), sqrt(252)*mean(x,na.rm=T)/sd(x,na.rm=T)),4)
ht = function(x) x[c(1:4, nrow(x)-1, nrow(x)),]
mss(mydata$r)
ht(mydata)

# select relevant columns
mydata.select = mydata %>%
  select(date, id, r) %>%
  arrange(date, id)

# look at the unique date and id
D = sort(unique(mydata$date))
U = sort(unique(mydata$id))
ND = length(D)
NU = length(U)

# get a matrix of returns
returns_raw = spread(mydata.select, id, r)
returns = as.matrix(returns_raw[,c(-1)])

# get excess Returns
Returns = exp(returns)-1
Returns.mean = apply(Returns, 1, mean)
Returns.excess = Returns - Returns.mean

# calculate weights
weight_calculator = function(input_matrix){
  dup_matrix=input_matrix
  dup_matrix[input_matrix<0]=0
  Weights.normalizer = apply(dup_matrix, 1, sum)
  weights = -input_matrix/Weights.normalizer
  return(weights)
}
weights = weight_calculator(Returns.excess)

# constrarian portfolio
contrarian_portfolio = function(Return, weight, k){
  nr = nrow(weight)
  weight[(k+1):nr,]=weight[-((nr-k+1):nr),]
  weight[1:k,]=NA
  return(apply(Return*weight, 1, sum))
}
# try k_lag=1:10
portfolio.return = contrarian_portfolio(Returns, weights, 1) #k_lag=1
# this is for part-3
portfolio.k = matrix(ncol=10, nrow=ND)
for(i in 1:10){
  portfolio.k[,i] =contrarian_portfolio(Returns, weights, i)
}
portfolio.k=data.frame(portfolio.k)
return.k = matrix(ncol=3, nrow=10)
for(i in 1:10){
  return.k[i,] = mss(portfolio.k[,i])
}
return.k=data.frame(return.k)
names(return.k)=c("mean","volatility","sharpe ratio")
return.k$k_lag=seq(1:10)

# create a dataframe which shows the mean of Return, portfolio Return by date 
Returns_by_d = mydata %>%
  group_by(date) %>%
  arrange(date, id) %>%
  mutate(R=exp(r)-1) %>%
  select(date,R) %>%
  summarise(R.mean = mean(R))
Returns_by_d$portfolio.return.flag1 = portfolio.return

# Let's plot the market return vs. portfolio return
# point plot as function of date
ggplot(Returns_by_d, aes(x=date))+
  geom_point(aes(y=portfolio.return.flag1, colour = "portforlio return"))+
  geom_point(aes(y=R.mean, colour="market return"))+
  labs(title = 'Market and portfolio daily return from 1/1/1996 to 12/31/2001', x="Date", y='Daily return')+
  scale_color_manual(name = "strategies", 
                     values = c("portforlio return" = "red", "market return" = "black"))

# Look at return by year
# density plot (with different year)
ggplot(Returns_by_d, aes(portfolio.return.flag1))+facet_wrap(~format(date,'%Y'))+
  geom_density(aes(colour = "portforlio return"))+
  geom_density(aes(R.mean, colour="market return"))+
  labs(title = 'Comparison of return by year', x='Daily return', y="Kernel density")+
  scale_color_manual(name = "strategies", 
                     values = c("portforlio return" = "red", "market return" = "black"))+
  theme(axis.text=element_text(size=8),axis.text.x = element_text(angle=90, hjust=1))
# mss function by year
# weight with different lag: function
weights_lag = function(weight, k){
  nr = nrow(weight)
  weight[(k+1):nr,]=weight[-((nr-k+1):nr),]
  weight[1:k,]=NA
  return(weight)
}
new_weight = as.vector(t(weights_lag(weights, 1)))
mss_by_year=mydata %>%
  mutate(w=new_weight, k=rep(1,NU*ND), R=exp(r)-1) %>%
  select(date, id, k, r, R, w) %>%
  group_by(date) %>%
  summarise(mean.R = mean(R), portfolio.R = sum(R*w)) %>%
  group_by(format(date,'%Y')) %>%
  summarise(mss(mean.R)[1], mss(mean.R)[2], mss(mean.R)[3], mss(portfolio.R)[1], mss(portfolio.R)[2], mss(portfolio.R)[3])
write.csv(mss_by_year,"mms_by_year.csv")

# annualized mean return, volatility, and Sharpe ratio of the strategy and of the market average
r = mss(Returns_by_d$portfolio.return.flag1)
r2= mss(Returns_by_d$R.mean)

# outliers
qqnorm(Returns_by_d$R.mean, main = "Normal Q-Q plot of daily Return of market");grid()
qqline(Returns_by_d$R.mean)

# get the outlier date
outliers_vals = boxplot.stats(Returns_by_d$R.mean)$out
outliers = Returns_by_d[which(Returns_by_d$R.mean %in% outliers_vals),]
unusual = outliers %>%
  arrange(desc(abs(R.mean)))
date.selected=unusual$date

# get the dates and stocks that gives highest weighted returns

# top unusual stocks
Returns_1=mydata %>%
  mutate(w=new_weight, k=rep(1,NU*ND), R=exp(r)-1) %>%
  select(date, id, k, r, R, w)

unusual_stock = Returns_1 %>%
  ungroup() %>%
  group_by(id) %>%
  summarise(volatility = mss(R)[2]) %>%
  arrange(desc(volatility)) %>%
  top_n(17,volatility)
ggplot(unusual_stock, aes(unusual_stock$volatility))+
  geom_histogram()+
  xlab("Stock volatility")
id.selected = unique(unusual_stock$id)

# if get rid of unusual dates
Returns_by_d_outlier_event = Returns_by_d %>%
  filter(!(date %in% date.selected))
r_no_outlier= mss(Returns_by_d_outlier_event$portfolio.return.flag1)

# if get rid of unusual stocks
Returns_by_d_outlier_stock = mydata %>%
  filter(!(id %in% id.selected))
Returns_by_d_outlier_stock = TEAMID_contrarianfunction(Returns_by_d_outlier_stock,1)
r_no_outlier2= mss(Returns_by_d_outlier_stock$portfolio.return.flag1)

xname=c("With outliers","no outlier events","no outlier stocks")
boxplot(Returns_by_d$portfolio.return.flag1,Returns_by_d_outlier_event$portfolio.return.flag1,Returns_by_d_outlier_stock$portfolio.return.flag1,
          main="Boxplot", ylab="portfolio return",names=xname)

# correlation between portfolio return and market return
relation = lm(Returns_by_d$portfolio.return.flag1~Returns_by_d$R.mean)
summary(relation)
plot(Returns_by_d$R.mean, Returns_by_d$portfolio.return.flag1,
     main="Relation between strategy return and market return",
     xlab="Market return", ylab="Portfolio return")
abline(relation)

# long/short portfolio
contrarian_portfolio_long = function(Return, weight, k){
  nr = nrow(weight)
  weight[(k+1):nr,]=weight[-((nr-k+1):nr),]
  weight[1:k,]=NA
  long=weight
  short=weight
  long[weight<0]=0
  short[weight>0]=0
  return(apply(Return*long, 1, sum))
}
contrarian_portfolio_short = function(Return, weight, k){
  nr = nrow(weight)
  weight[(k+1):nr,]=weight[-((nr-k+1):nr),]
  weight[1:k,]=NA
  long=weight
  short=weight
  long[weight<0]=0
  short[weight>0]=0
  return(apply(Return*short, 1, sum))
}
portfolio.long = contrarian_portfolio_long(Returns, weights, 1)
portfolio.short = contrarian_portfolio_short(Returns, weights, 1)
relation = lm(portfolio.long~portfolio.short)
summary(relation)

par(mfrow=c(3,1))
plot(portfolio.long, portfolio.short,
     main="Relation between long/short sub-portfolio",
     xlab="Long return", ylab="Short return")
relation = lm(portfolio.long~portfolio.short)
abline(relation)
plot(Returns_by_d$R.mean, portfolio.short,
     main="Relation between short sub-portfolio and market return",
     xlab="market return", ylab="Short return")
relation2 = lm(portfolio.short~Returns_by_d$R.mean)
abline(relation2)
plot(Returns_by_d$R.mean, portfolio.long,
     main="Relation between long sub-portfolio and market return",
     xlab="market return", ylab="Long return")
relation3 = lm(portfolio.long~Returns_by_d$R.mean)
abline(relation3)


