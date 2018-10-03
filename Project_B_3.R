install.packages('tidyquant')
library('tidyquant')

# Install development version of data.table
install.packages("data.table")  
# Load package
library(data.table)


setwd('~/15.458/HW/HW2')
input='project_b_3.txt'
mydata=read.delim(input, sep=',')
mydata$date=as.Date(mydata$nm_tradedate, format='%m/%d/%Y')
mydata$project_flag=NULL
mydata$nm_tradedate=NULL

mypid=926999629
# some of useful funciton taught on class
mss = function(x) round(c(mean(x,na.rm=T)*252, sqrt(252)*sd(x, na.rm=T), sqrt(252)*mean(x,na.rm=T)/sd(x,na.rm=T)),4)
ht = function(x) x[c(1:10, nrow(x)-20, nrow(x)),]
mss(mydata$r)

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
returns = spread(mydata.select, id, r)
returns = as.matrix(returns[,c(-1)])

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

# weight with different lag: function
weights_lag = function(weight, k){
  nr = nrow(weight)
  weight[(k+1):nr,]=weight[-((nr-k+1):nr),]
  weight[1:k,]=NA
  # prepend NA and remove rows from end
  return(weight)
}
# generate the flat file part-3 requires
for(ii in 1:10){
  new_weight = as.vector(t(weights_lag(weights, ii)))
  data_k=mydata %>%
    mutate(pid=rep(mypid,ND*NU),d=date, w=new_weight, k=rep(ii,ND*NU), vid=rep(0,ND*NU)) %>%
    filter(date>as.Date("1995-12-31")) %>%
    select(pid, d, id, k, w, vid)
   if(ii==1) my_portfolio=data_k else my_portfolio=rbind(my_portfolio, data_k)
}
write.csv(my_portfolio, "portfolio_flat_file.csv")
fwrite(my_portfolio, "portfolio_flat_file.csv")  # write csv file

# calculate return for different k lag (this is for double check of the csv file generated above makes sense)
for(ii in 1:10){
  new_weight = as.vector(t(weights_lag(weights, ii)))
  data_k=mydata %>%
    mutate(d=date, w=new_weight, k=rep(ii,ND*NU), R=exp(r)-1) %>%
    filter(d>as.Date("1995-12-31")) %>%
    select(d, id, k, w, R)
  if(ii==1) portfolio.klag=data_k else portfolio.klag=rbind(portfolio.klag, data_k)
}
# look at different k
portfolio = portfolio.klag %>%
  filter(k==2) %>%
  group_by(d) %>%
  summarise(p=sum(R*w), m=mean(R))
mss(portfolio$p)
mss(portfolio$m)

# Now we write a better script that can show different k at one time.
# Let's see the portfolio with different k and its behavior
contrarian_portfolio = function(Return, weight, k){
  nr = nrow(weight)
  weight[(k+1):nr,]=weight[-((nr-k+1):nr),]
  weight[1:k,]=NA
  return(apply(Return*weight, 1, sum))
}
# try portfolio with k_lag=1:10
portfolio.k = matrix(ncol=10, nrow=ND)
for(i in 1:10){
  portfolio.k[,i] =contrarian_portfolio(Returns, weights, i)
}
portfolio.k=data.frame(portfolio.k)
portfolio.k = portfolio.k %>%
  mutate(date=D) %>%
  filter(date>as.Date("1995-12-31"))
# compute return of portfolio with different k
return.k = matrix(ncol=3, nrow=10)
for(i in 1:10){
  return.k[i,] = mss(portfolio.k[,i])
}
return.k=data.frame(return.k)
names(return.k)=c("mean","volatility","sharpe ratio")
return.k$k_lag=seq(1,10)
write.csv(return.k, "returns_for_different_k.csv")

ggplot(return.k, aes(x=k_lag, y=mean))+
  geom_bar(stat="identity")+
  labs(x="k lag", y='Mean Return')+
  scale_x_discrete(limits=as.character(seq(1:10)))

ggplot(return.k, aes(x=k_lag, y=volatility))+
  geom_bar(stat="identity")+
  labs(x="k lag", y='Volatility')+
  scale_x_discrete(limits=as.character(seq(1:10)))

ggplot(return.k, aes(x=k_lag, y=return.k[,3]))+
  geom_bar(stat="identity")+
  labs(x="k lag", y='Sharpe Ratio')+
  scale_x_discrete(limits=as.character(seq(1:10)))

  