# economicdata
library(tidyr)
library(dplyr)
library(ggplot2)

df<-read.csv('C:/Virtual Machines/share/rates.csv')
unique(df$market)


    # 変化率の計算
    tmp<-df%>%filter(market %in% c('quoine','coincheck'))
    tr.buy<-tmp%>%group_by(created_at,market)%>% summarise(buy_rate=max(buy_rate))%>%
        spread(key=market,value=buy_rate,sep='_buy_')%>%data.frame
    tr.sell<-tmp%>%filter(market=='coincheck')%>%group_by(created_at,market)%>% summarise(sell_rate=max(sell_rate))%>%
        spread(key=market,value=sell_rate,sep='_sell_')%>%data.frame
    
    tr<-tr.buy%>%inner_join(tr.sell)
    tr<-tr%>%arrange(created_at)%>%mutate(coincheck_pct=(market_buy_coincheck-lag(market_buy_coincheck))/lag(market_buy_coincheck, k=-1)*100,
                                          quoine_pct=(market_buy_quoine-lag(market_buy_quoine))/lag(market_buy_quoine, k=-1)*100  )
    train<-na.omit(tr)
    train$open<-NA
    train$close<-NA
    position<-NA
    for(d in 2:nrow(train)){
        prev<-train[d-1,]
        if(prev$quoine_pct < 0 & train$quoine_pct[d] >0 & prev$coincheck_pct >=0 &train$coincheck_pct[d] <0 & is.na(position)){
            train$open[d]<-train$market_buy_coincheck[d]
            position<-train$market_buy_coincheck[d]
        }else if (prev$coincheck_pct < 0 & train$coincheck_pct[d] <0  & is.na(position)){
            train$open[d]<-train$market_buy_coincheck[d]
            position<-train$market_buy_coincheck[d]
            
        }else{
            train$open[d]<-position
        }
        if(!is.na(position )){
            if(train$market_sell_coincheck[d]>position){
                train$close[d]<-train$market_sell_coincheck[d]
                position<-NA
            }
        }
            
    }
    head(train,100)    
    train$profit<-train$close-train$open
    train[is.na(train)]<-0
    write.csv(train,'abitra.csv')
    sum(na.omit(train$profit))
    
    train%>%filter(close>0)%>%summarise(cnt=n())

    head(tr)
    tr<-tr%>%mutate(open=ifelse((lag(quoine_pct)<0 & quoine_pct>0 & lag(coincheck_pct) >=0 &coincheck_pct<0)| 
                                    (lag(coincheck_pct)< 0 & coincheck_pct < 0 ),market_buy_coincheck,0))
    
    tr<-tr%>%mutate(open=ifelse((lag(quoine_pct)<0 & quoine_pct>0 & lag(coincheck_pct) >=0 &coincheck_pct<0)| 
                                    (lag(coincheck_pct)< 0 & coincheck_pct < 0 & lag(open)==0 ),market_buy_coincheck,NA))
    
    %>%fill(open)
    
                    
    tr<-tr%>%mutate(close=ifelse(open>0 & market_sell_coincheck > open, market_sell_coincheck,0))

    tr%>%filter(open>0)%>%summarise(cnt=n())
    head(tr,200)
    ds<-tr%>%filter((lag(open)==0 & lead(open)==0 & lead(close)>0)|(lag(close)==0 &  lag(open)>0))%>%filter(open>0 |close>0)
    head(ds,100)
    open.d<-ds%>%filter(open>0)%>%rename(open_date=created_at,
                                         open_coincheck_pct=coincheck_pct,
                                         open_quoine_pct=quoine_pct)%>%select(open_date,open_coincheck_pct,open_quoine_pct,open)
    
    close.d<-ds%>%filter(close>0)%>%rename(close_date=created_at,close_coincheck_pct=coincheck_pct,close_quoine_pct=quoine_pct)%>%
        select(close_date,close_coincheck_pct,close_quoine_pct,close)
    nrow(open.d)
    nrow(close.d)
    result<-cbind(open.d,close.d)
    result<-result%>%mutate(profit=close-open,dff_time=as.POSIXct( close_date)-as.POSIXct( open_date))
    head(result,100)
    
        sum(result$profit)
    result[300:400,]
    qplot(as.Date( close_date),profit,data=result,geom='line')

    }

