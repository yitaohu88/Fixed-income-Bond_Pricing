#import the data
bond_data=read_excel("Desktop/UCLAMFE/Fixed Income/week2/Homework 2 Data.xlsx")
bond_data['discountFactors']=bond_data$Price/100

#Question 1 
#compute the spot rates
bond_data['SpotRates']=2*((1/bond_data$discountFactors)^(1/(2*bond_data$Maturity))-1)
#plot the spot rates
plot(x = bond_data$Maturity,y = bond_data$SpotRates,type = 'l',
     main='SpotCurves')
#compute the 3 month discount factor
three_month_discountFactor=bond_data$discountFactors[3]/bond_data$discountFactors[2]
bond_data['3_month_forward_rate']=0#initialization
#compute 3-month forward rates
bond_data$`3_month_forward_rate`[-nrow(bond_data)]=
  ((three_month_discountFactor/bond_data$discountFactors[-1])^(1/(2*(bond_data$Maturity[-1]-0.25)))-1)*2
plot(x = bond_data$Maturity[-nrow(bond_data)],y = bond_data$`3_month_forward_rate`[-nrow(bond_data)],type = 'l',
     main='3MonthForwardCurves')

#Question 2 
#build the regression model 
bond_data['lnDiscountFactor']=log(bond_data$discountFactors)
DiscountFactorModel=lm(formula = bond_data$lnDiscountFactor~0+bond_data$Maturity+
                         I(bond_data$Maturity^2)+
                         I(bond_data$Maturity^3)+
                         I(bond_data$Maturity^4)+
                         I(bond_data$Maturity^5))
summary(DiscountFactorModel)
DiscountFactorModel$coefficients


#Question 3
#create an empty dataframe
Est_Rates=data.frame(matrix(0, nrow = 50, ncol = 2))
colnames(Est_Rates)=c('Maturity','EST_DiscountFactor')

Est_Rates$Maturity=seq(from=0.5,to = 25.0,by=0.5)
Est_Rates$EST_DiscountFactor=exp(cbind(Est_Rates$Maturity,Est_Rates$Maturity^2,Est_Rates$Maturity^3,Est_Rates$Maturity^4,Est_Rates$Maturity^5)%*%DiscountFactorModel$coefficients)
#compute the spot rates
Est_Rates['EST_SpotRates']=2*((1/Est_Rates$EST_DiscountFactor)^(1/(2*Est_Rates$Maturity))-1)
#plot the spot rates
plot(x = Est_Rates$Maturity,y = Est_Rates$EST_SpotRates,type = 'l',
     main='EST.SpotCurves')

#Question 4
#compute par rates
Est_Rates['Est_ParRates']=(2*(100-100*Est_Rates$EST_DiscountFactor))/cumsum(Est_Rates$EST_DiscountFactor)/100
plot(x = Est_Rates$Maturity,y = Est_Rates$Est_ParRates,type = 'l',
     main='EST.ParRates')

#Question 5 
six_m_discountFactor=Est_Rates$EST_DiscountFactor[1]
Est_Rates['six_month_forward_rate']=0#initialization
#compute 3-month forward rates
Est_Rates$six_month_forward_rate[-nrow(Est_Rates)]=
  ((six_m_discountFactor/Est_Rates$EST_DiscountFactor[-1])^(1/(2*(Est_Rates$Maturity[-1]-0.50)))-1)*2
plot(x = Est_Rates$Maturity[-nrow(Est_Rates)],y = Est_Rates$six_month_forward_rate[-nrow(Est_Rates)],type = 'l',
     main='6MonthForwardCurves')

#Question 6
#import the data 
T_notes <- read_excel("Desktop/UCLAMFE/Fixed Income/week2/T-notes.xlsx")
T_notes$Yield=T_notes$Yield/100
Yield_Model=lm(T_notes$Yield~T_notes$Maturity+I(T_notes$Maturity^2)+
                 I(T_notes$Maturity^3)+I(T_notes$Maturity^4)+I(T_notes$Maturity^5))
Yield_Est_Rates=data.frame(matrix(0, nrow = 50, ncol = 2))
colnames(Yield_Est_Rates)=c('Maturity','EST_yield')

Yield_Est_Rates$Maturity=seq(from=0.5,to = 25.0,by=0.5)
Yield_Est_Rates$EST_yield=cbind(1,Yield_Est_Rates$Maturity,Yield_Est_Rates$Maturity^2,Yield_Est_Rates$Maturity^3,Yield_Est_Rates$Maturity^4,Yield_Est_Rates$Maturity^5)%*%Yield_Model$coefficients
#plot the par curves
plot(x = Yield_Est_Rates$Maturity,y = Yield_Est_Rates$EST_yield,type = 'l',
     main='EST_Par Curves')

Yield_Est_Rates['Coupon']=Yield_Est_Rates$EST_yield*100
Yield_Est_Rates['DiscountFactor']=0.0 #initialization
#compute the discount factors recursively 
for (t in 1:50){
  Yield_Est_Rates$DiscountFactor[t]=(100-Yield_Est_Rates$Coupon[t]/2*sum(Yield_Est_Rates$DiscountFactor))/(100+Yield_Est_Rates$Coupon[t]/2)
  
}
Yield_Est_Rates['SpotRates']=2*((1/Yield_Est_Rates$DiscountFactor)^(1/(2*Yield_Est_Rates$Maturity))-1)
plot(x = Yield_Est_Rates$Maturity,y = Yield_Est_Rates$SpotRates,type = 'l',
     main='Yield EST spot rate')

six_m_discountFactor=Yield_Est_Rates$DiscountFactor[1]
Yield_Est_Rates['six_month_forward_rate']=0#initialization
#compute 3-month forward rates
Yield_Est_Rates$six_month_forward_rate[-nrow(Yield_Est_Rates)]=
  ((six_m_discountFactor/Yield_Est_Rates$DiscountFactor[-1])^(1/(2*(Yield_Est_Rates$Maturity[-1]-0.50)))-1)*2
plot(x = Yield_Est_Rates$Maturity[-nrow(Yield_Est_Rates)],y = Yield_Est_Rates$six_month_forward_rate[-nrow(Yield_Est_Rates)],type = 'l',
     main='6MonthForwardCurves')


