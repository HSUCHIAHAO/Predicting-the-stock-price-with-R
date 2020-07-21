data=read.table("/Users/garyhsu/Library/Mobile Documents/com~apple~CloudDocs/Documents/file/Macquarie 2020/6180 Applied Statistics/Assignment/Assignment datasets-20200430/companies.dat",header=TRUE)
#(a)
pairs(data[,2:8],panel = panel.smooth)
#(b)
my_data <- data[,2:8]

corr=cor(my_data)
corr
#(c)
#(1)The dividend and yield have high correlation, and we also can prove it in scatter plot.
#(2)The dividend with earning and sales low correaltion, but they still have quite relation, and some of the data in the plot show that there are some out of trend.
#(3)The dividend with return_equity and stock_prices have negative correlation. Be more specific, The dividend and return_equity have quite negative relation,and some of the data in the plot show that there are some out of trend.
#(4)The yield with earning and sales have low correlation in the matrix, and the yield and return_sales have quite relation, and we can see the trend in plot.
#(5)The yield with return_equity and stock_pricess have negative correlation in the matrix, but stock_prices have high correlation with the yield. We can see that there are significant trend in the plot.
#(6)The earning and stock_prices have high correlation in matrix, but the earning with return_sales and return_equity have low correaltion.We can see that there is a significant trend in the plot.
#(7) The return_sales and the retrun_equity have quite correlation in matrix.We can see that there is a significant trend in the plot.
#(8) The return_equity and stock_prices have quite correlation in the matrix,We can see that there is a significant trend in the plot.

#(d-i)
model = lm(stock_prices ~ dividend + yield + earnings+sales+return_sales+return_equity, data = data)
summary(model)
#(d-ii)

par(mfrow=c(1,2))
plot(model, which = 1:2)

#(d-iii)
confint(model, 'earnings', level=0.95)
#The estiamte of 'earning' 95% of probability between 1.524989 and 3.915683.

#(d-iv)
anova(model)
#HO: B1=B2=B3=B4=B5=B6, H1:not all Bi=0, i=1,2,...,6
#Compute Sum square
FullRegS.S.=(62.26+1182.77+488.63+10.25+88.31+3.43)
#State null distribution
n=31
df1=6
df2=n-df1-1
#Compute Mean_square_Residual
Sum_square_Residual=543.15
M.S_Residual=(Sum_square_Residual)/df2
#Compute F-value
RegS.S=(FullRegS.S./df1)
Fobs=(RegS.S/M.S_Residual)
#Compute P-value
p_vlaue=1-pf(Fobs,df1,df2)
#Statistic conclusion: p_value=1.1386e-06 <0.05, reject H0.
#Contextual conclusion: Reject at ther 5% level.There is a significant linear relationship between stock_prices and at least one of the six predictor variables.

#(e)
model.aic.backward <- step(model, direction = "backward", trace = 1)
summary(model.aic.backward)

#2
data2=read.table("/Users/garyhsu/Library/Mobile Documents/com~apple~CloudDocs/Documents/file/Macquarie 2020/6180 Applied Statistics/Assignment/Assignment datasets-20200430/prof_2020.dat",header=TRUE)
#(a)
#linear
linear<-lm(mathprof ~ variety,data=data2)
summary(linear)
#quadratic
quadratic=lm(mathprof ~ variety+I(variety^2),data=data2)
summary(quadratic)
#cubic
cubic=lm(mathprof ~ variety+I(variety^2)+I(variety^3),data=data2)
summary(cubic)
#(b)
par(mfrow=c(1,1))
plot(mathprof ~ variety,data=data2, pch=16, ylab = "mathprof ",xlab="variety")

#linear_line
abline(linear, col = "blue")

#set a new dataframe
x <- seq(from=min(data2$variety),to=max(data2$variety))
mydata=data.frame(variety=x)

#quadratic_line
yhat=predict(quadratic, newdata=mydata)
lines(x,yhat, col = "red",lty=3)

#cubic_line
yhat2=predict(cubic, newdata=mydata)
lines(x,yhat2, col = "darkgreen",lty=2)

#set a legend
legend(70, 280, legend=c("Linear line", "Quadratic line","Cubic line"),
       col=c("blue", "red","darkgreen"), lty=1:3, cex=1,title="Line types", text.font=4, bg='lightgray')

#(c)
#Based on these three model, all the p-value on F-test are less than 0.05. The best model I think that is 'quadratic'. The R square in cubic model is the highest, but the predictors in here all larger than 0.05.
#Finally, I think it is good for us to select the 'quadratic' model in this situation.