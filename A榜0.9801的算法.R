##------------------------------------------------- 初赛数据分析 --------------------------------------------------------------------------------##
##1.导入数据
library(pROC)
library(randomForest)
library(lightgbm)
library(readxl)
library(VIM)
library(dplyr)
library(xgboost)
library(Matrix)
library(catboost)
library(infotheo)    #分箱包
library(reticulate)

options(digits = 9)
train <- read_xlsx("C:\\Users\\13407\\Desktop\\招商银行比赛\\train.xlsx")
test_all <- read_xlsx("C:\\Users\\13407\\Desktop\\招商银行比赛\\test_A榜.xlsx")

train$LABEL = as.factor(train$LABEL)
#for(i in names(train[,c(-1:-2)])){train[[i]] = ifelse((is.na(train[[i]]) | train[[i]] == '?'),NA,train[[i]])}
#train = train[,c(-3,-6:-8,-18,-19,-26,-48)]
#aggr(train)
#summary(train[,c(47:51)])
#par(mfrow = c(2,2))
#for(i in 47:51){train[[i]] = as.factor(ifelse((is.na(train[[i]])), 0,1));plot(train[[i]],train$LABEL)}
#for(i in names(a)){a[[i]] = as.numeric(a[[i]])}

##-------------------------------------------------------------------------------------------------------------------------------------------------
##2.数据初步分析
##2.1字符型数据分析
##CUST_UID、MON_12_CUST_CNT_PTY_ID、AI_STAR_SCO、WTHR_OPN_ONL_ICO、SHH_BCK、LGP_HLD_CARD_LVL、NB_CTC_HLD_IDV_AIO_CARD_SITU
train_char <- train[,c(1,2,18,41,42,45,48,50)]
train_char$LABEL <- as.factor(train$LABEL)
train_char$MON_12_CUST_CNT_PTY_ID <- ifelse((is.na(train_char$MON_12_CUST_CNT_PTY_ID)),0,1);train_char$MON_12_CUST_CNT_PTY_ID<-as.factor(train_char$MON_12_CUST_CNT_PTY_ID)
train_char$AI_STAR_SCO <- ifelse((train_char$AI_STAR_SCO == '?'), 4,train_char$AI_STAR_SCO);train_char$AI_STAR_SCO <- as.factor(train_char$AI_STAR_SCO)
train_char$WTHR_OPN_ONL_ICO <-ifelse((is.na(train_char$WTHR_OPN_ONL_ICO)),0,ifelse((train_char$WTHR_OPN_ONL_ICO == 'B'),1,0));train_char$WTHR_OPN_ONL_ICO<-as.factor(train_char$WTHR_OPN_ONL_ICO)
train_char$SHH_BCK = ifelse((train_char$SHH_BCK == '?'), 22, train_char$SHH_BCK);train_char$SHH_BCK<-as.factor(train_char$SHH_BCK)
train_char$LGP_HLD_CARD_LVL = ifelse((is.na(train_char$LGP_HLD_CARD_LVL) | train_char$LGP_HLD_CARD_LVL == 'A'),1,ifelse((train_char$LGP_HLD_CARD_LVL == '?'),0,train_char$LGP_HLD_CARD_LVL))
train_char$LGP_HLD_CARD_LVL = ifelse((train_char$LGP_HLD_CARD_LVL == 'B'),2,ifelse((train_char$LGP_HLD_CARD_LVL == 'C'),3,train_char$LGP_HLD_CARD_LVL))
train_char$LGP_HLD_CARD_LVL = ifelse((train_char$LGP_HLD_CARD_LVL == 'D'),4,ifelse((train_char$LGP_HLD_CARD_LVL == 'E'),5,train_char$LGP_HLD_CARD_LVL))
train_char$LGP_HLD_CARD_LVL <-ifelse((train_char$LGP_HLD_CARD_LVL == 'F'),6,train_char$LGP_HLD_CARD_LVL);train_char$LGP_HLD_CARD_LVL <- as.factor(train_char$LGP_HLD_CARD_LVL)
train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU = ifelse((is.na(train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU) | train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU == 'F'),6,train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU)
train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU = ifelse((train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU == '?'),0,train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU)
train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU = ifelse((train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU == 'A'),1,ifelse((train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU == 'B'),2,train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU))
train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU = ifelse((train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU == 'C'),3,ifelse((train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU == 'D'),4,train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU))
train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU = ifelse((train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU == 'E'),5,train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU);train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU = as.factor(train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU)
##2.2数值型数据分析
train_num <- train[,c(-2,-18,-41,-42,-45,-48,-50)]
#aggr(train_num)
train_num$factor_AGN_CNT_RCT_12_MON = ifelse((train_num$AGN_CNT_RCT_12_MON == '?'),0, 1);train_num$factor_AGN_CNT_RCT_12_MON = as.factor(train_num$factor_AGN_CNT_RCT_12_MON)

train_num$factor_ICO_CUR_MON_ACM_TRX_TM = as.factor(ifelse((train_num$ICO_CUR_MON_ACM_TRX_TM =='?'),0, 1))
#train_num$ICO_CUR_MON_ACM_TRX_TM = as.numeric(ifelse((train_num$ICO_CUR_MON_ACM_TRX_TM == '?'),0,train_num$ICO_CUR_MON_ACM_TRX_TM))

train_num$factor_NB_RCT_3_MON_LGN_TMS_AGV = as.factor(ifelse((train_num$NB_RCT_3_MON_LGN_TMS_AGV =='?'),0, 1))
#train_num$NB_RCT_3_MON_LGN_TMS_AGV = as.numeric(ifelse((train_num$NB_RCT_3_MON_LGN_TMS_AGV == '?'),0,train_num$NB_RCT_3_MON_LGN_TMS_AGV))

train_num$factor_AGN_CUR_YEAR_AMT = as.factor(ifelse((train_num$AGN_CUR_YEAR_AMT =='?'),0, 1))
#train_num$AGN_CUR_YEAR_AMT = as.numeric(ifelse((train_num$AGN_CUR_YEAR_AMT == '?'),0,train_num$AGN_CUR_YEAR_AMT))

train_num$factor_AGN_CUR_YEAR_WAG_AMT = as.factor(ifelse((train_num$AGN_CUR_YEAR_WAG_AMT =='?'),0, 1))
#train_num$AGN_CUR_YEAR_WAG_AMT = as.numeric(ifelse((train_num$AGN_CUR_YEAR_WAG_AMT == '?'),0,train_num$AGN_CUR_YEAR_WAG_AMT))

train_num$factor_AGN_AGR_LATEST_AGN_AMT = as.factor(ifelse((train_num$AGN_AGR_LATEST_AGN_AMT =='?'),0, 1))
#train_num$AGN_AGR_LATEST_AGN_AMT = as.numeric(ifelse((train_num$AGN_AGR_LATEST_AGN_AMT == '?'),0,train_num$AGN_AGR_LATEST_AGN_AMT))

train_num$factor_ICO_CUR_MON_ACM_TRX_AMT = as.factor(ifelse((train_num$ICO_CUR_MON_ACM_TRX_AMT =='?'),0, 1))
#train_num$ICO_CUR_MON_ACM_TRX_AMT = as.numeric(ifelse((train_num$ICO_CUR_MON_ACM_TRX_AMT == '?'),0,train_num$ICO_CUR_MON_ACM_TRX_AMT))

train_num$factor_COUNTER_CUR_YEAR_CNT_AMT = as.factor(ifelse((train_num$COUNTER_CUR_YEAR_CNT_AMT =='?'),0, 1))
#train_num$COUNTER_CUR_YEAR_CNT_AMT = as.numeric(ifelse((train_num$COUNTER_CUR_YEAR_CNT_AMT == '?'),0,train_num$COUNTER_CUR_YEAR_CNT_AMT))

#train_num$PUB_TO_PRV_TRX_AMT_CUR_YEAR = as.numeric(ifelse((train_num$PUB_TO_PRV_TRX_AMT_CUR_YEAR == '?'),0,train_num$PUB_TO_PRV_TRX_AMT_CUR_YEAR))
#train_num$MON_12_EXT_SAM_TRSF_IN_AMT = as.numeric(ifelse((train_num$MON_12_EXT_SAM_TRSF_IN_AMT == '?'),0,train_num$MON_12_EXT_SAM_TRSF_IN_AMT))
#train_num$MON_12_EXT_SAM_TRSF_OUT_AMT = as.numeric(ifelse((train_num$MON_12_EXT_SAM_TRSF_OUT_AMT == '?'),0,train_num$MON_12_EXT_SAM_TRSF_OUT_AMT))
#train_num$MON_12_EXT_SAM_NM_TRSF_OUT_CNT = as.numeric(ifelse((train_num$MON_12_EXT_SAM_NM_TRSF_OUT_CNT == '?'),0,train_num$MON_12_EXT_SAM_NM_TRSF_OUT_CNT))
#train_num$MON_12_EXT_SAM_AMT = as.numeric(ifelse((train_num$MON_12_EXT_SAM_AMT == '?'),0,train_num$MON_12_EXT_SAM_AMT))
#train_num$CUR_MON_EXT_SAM_CUST_TRSF_IN_AMT = as.numeric(ifelse((train_num$CUR_MON_EXT_SAM_CUST_TRSF_IN_AMT == '?'),0,train_num$CUR_MON_EXT_SAM_CUST_TRSF_IN_AMT))
#train_num$CUR_MON_EXT_SAM_CUST_TRSF_OUT_AMT = as.numeric(ifelse((train_num$CUR_MON_EXT_SAM_CUST_TRSF_OUT_AMT == '?'),0,train_num$CUR_MON_EXT_SAM_CUST_TRSF_OUT_AMT))
#train_num$CUR_YEAR_MON_AGV_TRX_CNT = as.numeric(ifelse((train_num$CUR_YEAR_MON_AGV_TRX_CNT == '?'),0,train_num$CUR_YEAR_MON_AGV_TRX_CNT))
#train_num$MON_12_AGV_TRX_CNT = as.numeric(ifelse((train_num$MON_12_AGV_TRX_CNT == '?'),0,train_num$MON_12_AGV_TRX_CNT))
train_num$factor_MON_12_AGV_TRX_CNT = as.factor(ifelse((train_num$MON_12_AGV_TRX_CNT == '?'),0,1))
#train_num$MON_12_ACM_ENTR_ACT_CNT = as.numeric(ifelse((train_num$MON_12_ACM_ENTR_ACT_CNT == '?'),0,train_num$MON_12_ACM_ENTR_ACT_CNT))
#train_num$MON_12_AGV_ENTR_ACT_CNT = as.numeric(ifelse((train_num$MON_12_AGV_ENTR_ACT_CNT == '?'),0,train_num$MON_12_AGV_ENTR_ACT_CNT))
train_num$factor_MON_12_TRX_AMT_MAX_AMT_PCTT = as.factor(ifelse((train_num$MON_12_TRX_AMT_MAX_AMT_PCTT =='?'),0, 1))
#train_num$MON_12_TRX_AMT_MAX_AMT_PCTT = as.numeric(ifelse((train_num$MON_12_TRX_AMT_MAX_AMT_PCTT == '?'),0,train_num$MON_12_TRX_AMT_MAX_AMT_PCTT))
train_num$factor_CUR_YEAR_PUB_TO_PRV_TRX_PTY_CNT = as.factor(ifelse((train_num$CUR_YEAR_PUB_TO_PRV_TRX_PTY_CNT=='?'),0,1))
train_num$factor_MON_6_50_UP_ENTR_ACT_CNT = as.factor(ifelse((train_num$MON_6_50_UP_ENTR_ACT_CNT=='?'),0,1))
train_num$factor_MON_12_ACT_OUT_50_UP_CNT_PTY_QTY = as.factor(ifelse((train_num$MON_12_ACT_OUT_50_UP_CNT_PTY_QTY=='?'),0,1))
train_num$factor_CUR_YEAR_COUNTER_ENCASH_CNT = as.factor(ifelse((train_num$CUR_YEAR_COUNTER_ENCASH_CNT=='?'),0,1))
train_num$factor_LAST_12_MON_COR_DPS_TM_PNT_BAL_PEAK_VAL = as.factor(ifelse((train_num$LAST_12_MON_COR_DPS_TM_PNT_BAL_PEAK_VAL=='?'),0,1))
train_num$factor_COR_KEY_PROD_HLD_NBR = as.factor(ifelse((train_num$COR_KEY_PROD_HLD_NBR=='?'),0,1))
train_num$factor_REG_CPT = as.factor(ifelse((train_num$REG_CPT=='?'),0,1))
train_num$factor_REG_DT = as.factor(ifelse((train_num$REG_DT=='?'),0,1))
train_num1 = train_num[,c(1,45:62)];train_num2 = train_num[,c(1:44)]
for(i in names(train_num2[,c(-1)])){train_num2[[i]] = as.numeric(ifelse((train_num2[[i]] == "?"),0,train_num2[[i]]))}
#for(i in 1:ncol(train_num)){a = ifelse((train_num[,c(i)] == '?'),0, 1);str(train_num[,c(i)]);summary(as.factor(a));plot(as.factor(a),train_char$LABEL)}
##2.3合并train_num和train_char
train_change = left_join(train_char, train_num1, by = "CUST_UID");train_change = left_join(train_change, train_num2, by = "CUST_UID")
str(train_change)
summary(train_change)
data = train_change
rm(train_char,train_num,train_num1,train_num2,i,train_change)

##2.4 重要特征组合
#str(train[,c(3,6:8,18,19,26,48)])    #19需要斟酌
#data = data[,c(-*"AGN_CNT_RCT_12_MON", -*"AGN_CUR_YEAR_AMT", -*"AGN_CUR_YEAR_WAG_AMT", -*"AGN_AGR_LATEST_AGN_AMT", 
#               -*"MON_12_CUST_CNT_PTY_ID", -*"MON_12_TRX_AMT_MAX_AMT_PCTT", -"CUR_YEAR_PUB_TO_PRV_TRX_PTY_CNT", -*"LGP_HLD_CARD_LVL")]
data = data[,c(-3,-7,-27,-30,-31,-32,-42,-49)]   #去掉缺失多的变量

data$CUR_Mon_COR_DMND_DPS_DAY_AVG_BAL = data$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL/12
data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL = data$CUR_MON_COR_DPS_MON_DAY_AVG_BAL*12
data$CUR_Mon_COR_DPS_YEAR_DAY_AVG_INCR = data$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR/12
data$CUR_Mon_MID_BUS_INC = data$CUR_YEAR_MID_BUS_INC/12
data$ICO_CUR_Year_ACM_TRX_AMT = data$ICO_CUR_MON_ACM_TRX_AMT*12
data$LAST_12_net1 = data$LAST_12_MON_COR_DPS_DAY_AVG_BAL - data$LAST_12_MON_COR_DPS_TM_PNT_BAL_PEAK_VAL
data$LAST_12_net2 = data$LAST_12_MON_DIF_NM_MON_AVG_TRX_AMT_NAV - data$LAST_12_MON_MON_AVG_TRX_AMT_NAV
data$LAST_12_add1 = data$LAST_12_MON_COR_DPS_DAY_AVG_BAL + data$LAST_12_MON_COR_DPS_TM_PNT_BAL_PEAK_VAL
data$LAST_12_add2 = data$LAST_12_MON_DIF_NM_MON_AVG_TRX_AMT_NAV + data$LAST_12_MON_MON_AVG_TRX_AMT_NAV
data$CUR_net1 = data$CUR_Mon_COR_DMND_DPS_DAY_AVG_BAL - data$CUR_MON_COR_DPS_MON_DAY_AVG_BAL
data$CUR_net2 = data$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL - data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL
data$CUR_net3 = data$CUR_Mon_COR_DPS_YEAR_DAY_AVG_INCR - data$CUR_Mon_MID_BUS_INC
data$CUR_net4 = data$CUR_YEAR_MID_BUS_INC -  data$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR
data$CUR_add1 = data$CUR_Mon_COR_DMND_DPS_DAY_AVG_BAL + data$CUR_MON_COR_DPS_MON_DAY_AVG_BAL
data$CUR_add2 = data$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL + data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL
data$CUR_add3 = data$CUR_Mon_COR_DPS_YEAR_DAY_AVG_INCR + data$CUR_Mon_MID_BUS_INC
data$CUR_add4 = data$CUR_YEAR_MID_BUS_INC +  data$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR

data$CUR_MON_EXT_SAM_CUST_TRSF_net_AMT = data$CUR_MON_EXT_SAM_CUST_TRSF_IN_AMT - data$CUR_MON_EXT_SAM_CUST_TRSF_OUT_AMT
data$CUR_MON_EXT_SAM_CUST_TRSF_add_AMT = data$CUR_MON_EXT_SAM_CUST_TRSF_IN_AMT + data$CUR_MON_EXT_SAM_CUST_TRSF_OUT_AMT
data$CUR_Year_EXT_SAM_CUST_TRSF_net_AMT = data$CUR_MON_EXT_SAM_CUST_TRSF_net_AMT*12
data$COUNTER_CUR_YEAR_CNT_AMT_add = data$COUNTER_CUR_YEAR_CNT_AMT + data$CUR_YEAR_COUNTER_ENCASH_CNT
data$COUNTER_CUR_YEAR_CNT_AMT_net = data$COUNTER_CUR_YEAR_CNT_AMT - data$CUR_YEAR_COUNTER_ENCASH_CNT
data$MON_12_ACM_add_ACT_CNT = data$MON_12_ACM_ENTR_ACT_CNT + data$MON_12_ACM_LVE_ACT_CNT
data$MON_12_ACM_net_ACT_CNT = data$MON_12_ACM_ENTR_ACT_CNT - data$MON_12_ACM_LVE_ACT_CNT
data$MON_12_ACT_net_50_UP_CNT_PTY_QTY = data$MON_12_ACT_IN_50_UP_CNT_PTY_QTY - data$MON_12_ACT_OUT_50_UP_CNT_PTY_QTY
data$MON_12_ACT_add_50_UP_CNT_PTY_QTY = data$MON_12_ACT_IN_50_UP_CNT_PTY_QTY + data$MON_12_ACT_OUT_50_UP_CNT_PTY_QTY
data$MON_12_AGV_add_ACT_CNT = data$MON_12_AGV_ENTR_ACT_CNT + data$MON_12_AGV_LVE_ACT_CNT
data$MON_12_AGV_net_ACT_CNT = data$MON_12_AGV_ENTR_ACT_CNT - data$MON_12_AGV_LVE_ACT_CNT
data$MON_12_EXT_SAM_AMT_add = data$MON_12_EXT_SAM_AMT + data$MON_12_EXT_SAM_NM_TRSF_OUT_CNT
data$MON_12_EXT_SAM_AMT_net = data$MON_12_EXT_SAM_AMT - data$MON_12_EXT_SAM_NM_TRSF_OUT_CNT
data$MON_12_EXT_SAM_TRSF_add_AMT = data$MON_12_EXT_SAM_TRSF_IN_AMT + data$MON_12_EXT_SAM_TRSF_OUT_AMT
data$MON_12_EXT_SAM_TRSF_net_AMT = data$MON_12_EXT_SAM_TRSF_IN_AMT - data$MON_12_EXT_SAM_TRSF_OUT_AMT
data$MON_6_50_UP_ENTR_ACT_CNT_net = data$MON_6_50_UP_ENTR_ACT_CNT - data$MON_6_50_UP_LVE_ACT_CNT
data$MON_6_50_UP_ENTR_ACT_CNT_add = data$MON_6_50_UP_ENTR_ACT_CNT + data$MON_6_50_UP_LVE_ACT_CNT
data$REG_add = data$REG_CPT + data$REG_DT
data$REG_net = data$REG_CPT - data$REG_DT

#for(i in names(data[,c(27:86)])){data[[i]] = (data[[i]] - min(data[[i]]))/(max(data[[i]]) - min(data[[i]]))}
#a = data[,c("LABEL","CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL","CUR_MON_COR_DPS_MON_DAY_AVG_BAL","CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR",
#            "CUR_YEAR_MID_BUS_INC","ICO_CUR_MON_ACM_TRX_AMT","LAST_12_MON_COR_DPS_DAY_AVG_BAL","LAST_12_MON_COR_DPS_TM_PNT_BAL_PEAK_VAL",
#            "LAST_12_MON_DIF_NM_MON_AVG_TRX_AMT_NAV","LAST_12_MON_MON_AVG_TRX_AMT_NAV","OPN_TM")];View(a)
#nbins = 10    #分成区域个数设置
#equal_width = discretize(a$CUR_MON_COR_DPS_MON_DAY_AVG_BAL, "equalwidth", nbins)
#table(equal_width)
#kmeans = kmeans(a$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL, nbins)
#table(kmeans$cluster)
data = data[,c(-4,-5,-7:-24,-61)]   #去除无效变量

##-------------------------------------------------------------------------------------------------------------------------------------------------
##3.模型选择和训练
##3.1 RandomForest模型
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

formula <- Formula::as.Formula(train[2:76])
random <- randomForest(formula,data = train, mtry = 15,ntree=500 ,importance = FALSE,na.action = na.omit)
#plot.roc(as.factor(train$LABEL), random$votes[,1],percent=TRUE, lwd=1, print.auc=TRUE, add=TRUE, print.auc.y=40)
varImpPlot(random, n.var = min(50, nrow(random$importance)),
           main = 'Top 50 - variable importance')

pred1 <- predict(random,test)
roccurve<- plot(roc(response = as.numeric(test$LABEL), predictor = as.numeric(pred1)),
                legacy.axes = TRUE, print.auc=TRUE, main = "ROC Curve")
roc1 = roc(response = as.numeric(test$LABEL), predictor = as.numeric(pred1));auc1 = auc(roc1);auc1
#a<-table(pred1,test$LABEL);a;(a[2,2]+a[1,1])/sum(a);2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1])
#score1 <- (a[2,2]+a[1,1])/sum(a);score1

##3.2 LightGBM模型
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

bla1 <- data.matrix(train[3:76])
bla2 <- train$LABEL
dtrain <- lgb.Dataset(data = bla1, label = bla2)
bla3 <- data.matrix(test[3:76])
bla4 <- test$LABEL
dtest <- lgb.Dataset.create.valid(dataset = dtrain, data = bla3, label = bla4)
#params <- list( learning_rate = .04, 
#                num_leaves = 200,
#                max_bin = 600,
#                min_data_in_bin = 45,
#                feature_fraction = .53,
#                min_sum_hessian = 1.1175, 
#                lambda_l1 = .0009,
#                lambda_l2 = .0004,
#                drop_rate = .4, 
#                max_drop = 14)
#lgb <- lightgbm(params = params,data = dtrain,nrounds = 40,early_stopping_rounds = 1,
#                     num_threads = 2,objective = "regression")
lgb <- lightgbm(data = dtrain,nrounds = 80,early_stopping_rounds = 2,num_threads = 2,objective = "regression")
prob1 <- predict(lgb,bla3);prob1 = (prob1 - min(prob1))/(max(prob1) - min(prob1))
#pred2 <- ifelse((prob > 1.3),1,0);summary(factor(pred2))
roccurve<- plot(roc(response = as.numeric(test$LABEL), predictor = as.numeric(prob1)),
                legacy.axes = TRUE, print.auc=TRUE, main = "ROC Curve")
roc2 = roc(response = as.numeric(test$LABEL), predictor = as.numeric(prob1));auc2 = auc(roc2);auc2
#a<-table(pred2,test$LABEL);a;(a[2,2]+a[1,1])/sum(a);2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1])
#score2 <- (a[2,2]+a[1,1])/sum(a)

##3.3 XGBoost模型
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

train_matrix <- data.matrix(train[,c(-1,-2)])
train_label <- as.numeric(train$LABEL)-1

test_matrix <- data.matrix(test[,c(-1,-2)])
test_label <- as.numeric(test$LABEL)-1
params <- list(
  booster= 'gbtree',
  objective= 'binary:logistic',
  eta = 0.29,
  max_depth = 8,
  subsample = 1.0,
  min_child_weight = 3,
  colsample_bytree = 0.2,
  scale_pos_weight = 0.2,
  eval_metric = 'auc',
  gamma = 0.2,           
  lambda = 200
)
xgb <- xgboost(params = params,data = train_matrix,label = train_label,nrounds = 300,early_stopping_rounds = 11,num_threads = 3)
prob2 <- predict(xgb, test_matrix);prob2 = (prob2 - min(prob2))/(max(prob2) - min(prob2))
#pred3 <- ifelse((prob > 0.07),1,0);a <- table(pred3,test$LABEL);a;(a[2,2]+a[1,1])/sum(a)
roccurve<- plot(roc(response = as.numeric(test$LABEL), predictor = as.numeric(prob2)),
                legacy.axes = TRUE, print.auc=TRUE, main = "ROC Curve")
roc3 = roc(response = as.numeric(test$LABEL), predictor = as.numeric(prob2));auc3 = auc(roc3);auc3

##3.4 Catboost模型
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

train_feactures <- train[,-1:-2]
train_label <- as.numeric(train$LABEL)-1
train_pool <- catboost.load_pool(data = train_feactures,label = train_label)
test_pool <- catboost.load_pool(test[,-1])
params = list(loss_function = 'Logloss',
              iterations = 1500,
              metric_period=50,
              border_count = 32,
              depth = 5,
              learning_rate = 0.03,
              use_best_model = TRUE,
              l2_leaf_reg = 3.5
)
cat <- catboost.train(train_pool, NULL,params = params)
prob3 <- catboost.predict(cat,test_pool);prob3 = (prob3 - min(prob3))/(max(prob3) - min(prob3))
#pred4 <- ifelse((prob > -1.1),1,0);a <- table(pred4,test$LABEL);a;(a[2,2]+a[1,1])/sum(a);(2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]))
roccurve<- plot(roc(response = as.numeric(test$LABEL), predictor = as.numeric(prob3)),
                legacy.axes = TRUE, print.auc=TRUE, main = "ROC Curve")
roc4 = roc(response = as.numeric(test$LABEL), predictor = as.numeric(prob3));auc4 = auc(roc4);auc4

##3.5 回归模型：Logistic
data = data[,c(-3:-4,-41:-76)]
data$LABEL = as.numeric(data$LABEL)
for(i in names(data[,c(-1:-2)])){data[[i]] = (data[[i]] - min(data[[i]]))/(max(data[[i]]) - min(data[[i]]))}
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

formula <- Formula::as.Formula(train[2:38])
logmodel = glm(formula, data = train)
summary(logmodel)
#logbestmodel<-step(object = logmodel,trace = 0)    #逐步回归，寻找最优变量
#anova(object = logmodel,test = "Chisq")   #模型的显著性检验
prob4<-predict(object =logmodel,newdata=test);prob4 = (prob4 - min(prob4))/(max(prob4) - min(prob4))
roccurve<- plot(roc(response = as.numeric(test$LABEL), predictor = as.numeric(prob4)),
                legacy.axes = TRUE, print.auc=TRUE, main = "ROC Curve")
roc5 = roc(response = as.numeric(test$LABEL), predictor = as.numeric(prob4));auc5 = auc(roc5);auc5

##3.6 结果组合
pred1 = as.numeric(pred1)-1
prob = (1*pred1+2*prob1+2*prob2+6*prob3)/11
roccurve<- plot(roc(response = as.numeric(test$LABEL), predictor = as.numeric(prob)),
                legacy.axes = TRUE, print.auc=TRUE, main = "ROC Curve")
roc0 = roc(response = as.numeric(test$LABEL), predictor = as.numeric(prob));auc0 = auc(roc0);auc0
#pred = as.factor(ifelse((prob >= 0.5),1,0));summary(pred);a = table(pred, test$LABEL);a;(a[1,1]+a[2,2])/sum(a)

##-------------------------------------------------------------------------------------------------------------------------------------------------
##4.全部数据训练模型
##4.1 RandomForest模型
par(mfrow = c(2,2))
formula <- Formula::as.Formula(data[2:76])
random <- randomForest(formula,data = data, mtry = 15,ntree=400 ,importance = FALSE,na.action = na.omit)

pred1 <- predict(random,data)
roccurve<- plot(roc(response = as.numeric(data$LABEL), predictor = as.numeric(pred1)),
                legacy.axes = TRUE, print.auc=TRUE, main = "ROC Curve")
roc1 = roc(response = as.numeric(data$LABEL), predictor = as.numeric(pred1));auc1 = auc(roc1);auc1

##4.2 LightGBM模型
bla1 <- data.matrix(data[3:76])
bla2 <- data$LABEL
dtrain <- lgb.Dataset(data = bla1, label = bla2)
lgb <- lightgbm(data = dtrain,nrounds = 80,early_stopping_rounds = 2,num_threads = 2,objective = "regression")
prob1 <- predict(lgb,bla1);prob1 = (prob1 - min(prob1))/(max(prob1) - min(prob1))
roccurve<- plot(roc(response = as.numeric(data$LABEL), predictor = as.numeric(prob1)),
                legacy.axes = TRUE, print.auc=TRUE, main = "ROC Curve")
roc2 = roc(response = as.numeric(data$LABEL), predictor = as.numeric(prob1));auc2 = auc(roc2);auc2

##4.3 XGBoost模型
train_matrix <- data.matrix(data[,c(-1:-2)])
train_label <- as.numeric(data$LABEL)-1
params <- list(
  booster= 'gbtree',
  objective= 'binary:logistic',
  eta = 0.29,
  max_depth = 8,
  subsample = 1.0,
  min_child_weight = 3,
  colsample_bytree = 0.2,
  scale_pos_weight = 0.2,
  eval_metric = 'auc',
  gamma = 0.2,           
  lambda = 200
)
xgb <- xgboost(params = params,data = train_matrix,label = train_label,nrounds = 300,early_stopping_rounds = 11,num_threads = 3)
prob2 <- predict(xgb, train_matrix);prob2 = (prob2 - min(prob2))/(max(prob2) - min(prob2))
roccurve<- plot(roc(response = as.numeric(data$LABEL), predictor = as.numeric(prob2)),
                legacy.axes = TRUE, print.auc=TRUE, main = "ROC Curve")
roc3 = roc(response = as.numeric(data$LABEL), predictor = as.numeric(prob2));auc3 = auc(roc3);auc3

##4.4 Catboost模型
train_feactures <- data[,-1:-2]
train_label <- as.numeric(data$LABEL)-1
train_pool <- catboost.load_pool(data = train_feactures,label = train_label)
params = list(loss_function = 'Logloss',
              iterations = 1500,
              metric_period=10,
              border_count = 32,
              depth = 5,
              learning_rate = 0.03,
              use_best_model = TRUE,
              l2_leaf_reg = 3.5)
cat <- catboost.train(train_pool, NULL,params = params)
prob3 <- catboost.predict(cat,train_pool);prob3 = (prob3 - min(prob3))/(max(prob3) - min(prob3))
roccurve<- plot(roc(response = as.numeric(data$LABEL), predictor = as.numeric(prob3)),
                legacy.axes = TRUE, print.auc=TRUE, main = "ROC Curve")
roc4 = roc(response = as.numeric(data$LABEL), predictor = as.numeric(prob3));auc4 = auc(roc4);auc4

##4.5 回归模型：Logistic
data = data[,c(-3:-4,-41:-76)]
data$LABEL = as.numeric(data$LABEL)
for(i in names(data[,c(-1:-2)])){data[[i]] = (data[[i]] - min(data[[i]]))/(max(data[[i]]) - min(data[[i]]))}

formula <- Formula::as.Formula(train[2:38])
logmodel = glm(formula, data = train)
prob4<-predict(object =logmodel,newdata=test);prob4 = (prob4 - min(prob4))/(max(prob4) - min(prob4))
roccurve<- plot(roc(response = as.numeric(test$LABEL), predictor = as.numeric(prob4)),
                legacy.axes = TRUE, print.auc=TRUE, main = "ROC Curve")
roc5 = roc(response = as.numeric(test$LABEL), predictor = as.numeric(prob4));auc5 = auc(roc5);auc5

##4.6 结果组合
pred1 = as.numeric(pred1)-1
prob = (1*pred1+2*prob1+2*prob2+6*prob3 )/11
roccurve<- plot(roc(response = as.numeric(data$LABEL), predictor = as.numeric(prob)),
                legacy.axes = TRUE, print.auc=TRUE, main = "ROC Curve")
roc0 = roc(response = as.numeric(data$LABEL), predictor = as.numeric(prob));auc0 = auc(roc0);auc0

##-------------------------------------------------------------------------------------------------------------------------------------------------
##5.测试数据集预测
##5.1 整理数据
train = test_all
train_char <- train[,c(1,17,40,41,44,47,49)]
train_char$MON_12_CUST_CNT_PTY_ID <- ifelse((is.na(train_char$MON_12_CUST_CNT_PTY_ID)),0,1);train_char$MON_12_CUST_CNT_PTY_ID<-as.factor(train_char$MON_12_CUST_CNT_PTY_ID)
train_char$AI_STAR_SCO <- ifelse((train_char$AI_STAR_SCO == '?'), 4,train_char$AI_STAR_SCO);train_char$AI_STAR_SCO <- as.factor(train_char$AI_STAR_SCO)
train_char$WTHR_OPN_ONL_ICO <-ifelse((is.na(train_char$WTHR_OPN_ONL_ICO)),0,ifelse((train_char$WTHR_OPN_ONL_ICO == 'B'),1,0));train_char$WTHR_OPN_ONL_ICO<-as.factor(train_char$WTHR_OPN_ONL_ICO)
train_char$SHH_BCK = ifelse((train_char$SHH_BCK == '?'), 22, train_char$SHH_BCK);train_char$SHH_BCK<-as.factor(train_char$SHH_BCK)
train_char$LGP_HLD_CARD_LVL = ifelse((is.na(train_char$LGP_HLD_CARD_LVL) | train_char$LGP_HLD_CARD_LVL == 'A'),1,ifelse((train_char$LGP_HLD_CARD_LVL == '?'),0,train_char$LGP_HLD_CARD_LVL))
train_char$LGP_HLD_CARD_LVL = ifelse((train_char$LGP_HLD_CARD_LVL == 'B'),2,ifelse((train_char$LGP_HLD_CARD_LVL == 'C'),3,train_char$LGP_HLD_CARD_LVL))
train_char$LGP_HLD_CARD_LVL = ifelse((train_char$LGP_HLD_CARD_LVL == 'D'),4,ifelse((train_char$LGP_HLD_CARD_LVL == 'E'),5,train_char$LGP_HLD_CARD_LVL))
train_char$LGP_HLD_CARD_LVL <-ifelse((train_char$LGP_HLD_CARD_LVL == 'F'),6,train_char$LGP_HLD_CARD_LVL);train_char$LGP_HLD_CARD_LVL <- as.factor(train_char$LGP_HLD_CARD_LVL)
train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU = ifelse((is.na(train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU) | train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU == 'F'),6,train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU)
train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU = ifelse((train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU == '?'),0,train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU)
train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU = ifelse((train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU == 'A'),1,ifelse((train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU == 'B'),2,train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU))
train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU = ifelse((train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU == 'C'),3,ifelse((train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU == 'D'),4,train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU))
train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU = ifelse((train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU == 'E'),5,train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU);train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU = as.factor(train_char$NB_CTC_HLD_IDV_AIO_CARD_SITU)

train_num <- train[,c(-17,-40,-41,-44,-47,-49)]
train_num$factor_AGN_CNT_RCT_12_MON = ifelse((train_num$AGN_CNT_RCT_12_MON == '?'),0, 1);train_num$factor_AGN_CNT_RCT_12_MON = as.factor(train_num$factor_AGN_CNT_RCT_12_MON)
train_num$factor_ICO_CUR_MON_ACM_TRX_TM = as.factor(ifelse((train_num$ICO_CUR_MON_ACM_TRX_TM =='?'),0, 1))
train_num$factor_NB_RCT_3_MON_LGN_TMS_AGV = as.factor(ifelse((train_num$NB_RCT_3_MON_LGN_TMS_AGV =='?'),0, 1))
train_num$factor_AGN_CUR_YEAR_AMT = as.factor(ifelse((train_num$AGN_CUR_YEAR_AMT =='?'),0, 1))
train_num$factor_AGN_CUR_YEAR_WAG_AMT = as.factor(ifelse((train_num$AGN_CUR_YEAR_WAG_AMT =='?'),0, 1))
train_num$factor_AGN_AGR_LATEST_AGN_AMT = as.factor(ifelse((train_num$AGN_AGR_LATEST_AGN_AMT =='?'),0, 1))
train_num$factor_ICO_CUR_MON_ACM_TRX_AMT = as.factor(ifelse((train_num$ICO_CUR_MON_ACM_TRX_AMT =='?'),0, 1))
train_num$factor_COUNTER_CUR_YEAR_CNT_AMT = as.factor(ifelse((train_num$COUNTER_CUR_YEAR_CNT_AMT =='?'),0, 1))
train_num$factor_MON_12_AGV_TRX_CNT = as.factor(ifelse((train_num$MON_12_AGV_TRX_CNT == '?'),0,1))
train_num$factor_MON_12_TRX_AMT_MAX_AMT_PCTT = as.factor(ifelse((train_num$MON_12_TRX_AMT_MAX_AMT_PCTT =='?'),0, 1))
train_num$factor_CUR_YEAR_PUB_TO_PRV_TRX_PTY_CNT = as.factor(ifelse((train_num$CUR_YEAR_PUB_TO_PRV_TRX_PTY_CNT=='?'),0,1))
train_num$factor_MON_6_50_UP_ENTR_ACT_CNT = as.factor(ifelse((train_num$MON_6_50_UP_ENTR_ACT_CNT=='?'),0,1))
train_num$factor_MON_12_ACT_OUT_50_UP_CNT_PTY_QTY = as.factor(ifelse((train_num$MON_12_ACT_OUT_50_UP_CNT_PTY_QTY=='?'),0,1))
train_num$factor_CUR_YEAR_COUNTER_ENCASH_CNT = as.factor(ifelse((train_num$CUR_YEAR_COUNTER_ENCASH_CNT=='?'),0,1))
train_num$factor_LAST_12_MON_COR_DPS_TM_PNT_BAL_PEAK_VAL = as.factor(ifelse((train_num$LAST_12_MON_COR_DPS_TM_PNT_BAL_PEAK_VAL=='?'),0,1))
train_num$factor_COR_KEY_PROD_HLD_NBR = as.factor(ifelse((train_num$COR_KEY_PROD_HLD_NBR=='?'),0,1))
train_num$factor_REG_CPT = as.factor(ifelse((train_num$REG_CPT=='?'),0,1))
train_num$factor_REG_DT = as.factor(ifelse((train_num$REG_DT=='?'),0,1))
train_num1 = train_num[,c(1,45:62)];train_num2 = train_num[,c(1:44)]
for(i in names(train_num2[,c(-1)])){train_num2[[i]] = as.numeric(ifelse((train_num2[[i]] == "?"),0,train_num2[[i]]))}
#train_change = merge(train_char,train_num1,by = "CUST_UID");train_change = merge(train_change,train_num2,by = "CUST_UID")
train_change = left_join(train_char, train_num1, by = "CUST_UID");train_change = left_join(train_change, train_num2, by = "CUST_UID")
str(train_change)
summary(train_change)
test_all = train_change
rm(train_char,train_num,train_num1,train_num2,i,train_change)

data = test_all
data = data[,c(-2,-6,-26,-29,-30,-31,-41,-48)]

data$CUR_Mon_COR_DMND_DPS_DAY_AVG_BAL = data$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL/12
data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL = data$CUR_MON_COR_DPS_MON_DAY_AVG_BAL*12
data$CUR_Mon_COR_DPS_YEAR_DAY_AVG_INCR = data$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR/12
data$CUR_Mon_MID_BUS_INC = data$CUR_YEAR_MID_BUS_INC/12
data$ICO_CUR_Year_ACM_TRX_AMT = data$ICO_CUR_MON_ACM_TRX_AMT*12
data$LAST_12_net1 = data$LAST_12_MON_COR_DPS_DAY_AVG_BAL - data$LAST_12_MON_COR_DPS_TM_PNT_BAL_PEAK_VAL
data$LAST_12_net2 = data$LAST_12_MON_DIF_NM_MON_AVG_TRX_AMT_NAV - data$LAST_12_MON_MON_AVG_TRX_AMT_NAV
data$LAST_12_add1 = data$LAST_12_MON_COR_DPS_DAY_AVG_BAL + data$LAST_12_MON_COR_DPS_TM_PNT_BAL_PEAK_VAL
data$LAST_12_add2 = data$LAST_12_MON_DIF_NM_MON_AVG_TRX_AMT_NAV + data$LAST_12_MON_MON_AVG_TRX_AMT_NAV
data$CUR_net1 = data$CUR_Mon_COR_DMND_DPS_DAY_AVG_BAL - data$CUR_MON_COR_DPS_MON_DAY_AVG_BAL
data$CUR_net2 = data$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL - data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL
data$CUR_net3 = data$CUR_Mon_COR_DPS_YEAR_DAY_AVG_INCR - data$CUR_Mon_MID_BUS_INC
data$CUR_net4 = data$CUR_YEAR_MID_BUS_INC -  data$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR
data$CUR_add1 = data$CUR_Mon_COR_DMND_DPS_DAY_AVG_BAL + data$CUR_MON_COR_DPS_MON_DAY_AVG_BAL
data$CUR_add2 = data$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL + data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL
data$CUR_add3 = data$CUR_Mon_COR_DPS_YEAR_DAY_AVG_INCR + data$CUR_Mon_MID_BUS_INC
data$CUR_add4 = data$CUR_YEAR_MID_BUS_INC +  data$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR

data$CUR_MON_EXT_SAM_CUST_TRSF_net_AMT = data$CUR_MON_EXT_SAM_CUST_TRSF_IN_AMT - data$CUR_MON_EXT_SAM_CUST_TRSF_OUT_AMT
data$CUR_MON_EXT_SAM_CUST_TRSF_add_AMT = data$CUR_MON_EXT_SAM_CUST_TRSF_IN_AMT + data$CUR_MON_EXT_SAM_CUST_TRSF_OUT_AMT
data$CUR_Year_EXT_SAM_CUST_TRSF_net_AMT = data$CUR_MON_EXT_SAM_CUST_TRSF_net_AMT*12
data$COUNTER_CUR_YEAR_CNT_AMT_add = data$COUNTER_CUR_YEAR_CNT_AMT + data$CUR_YEAR_COUNTER_ENCASH_CNT
data$COUNTER_CUR_YEAR_CNT_AMT_net = data$COUNTER_CUR_YEAR_CNT_AMT - data$CUR_YEAR_COUNTER_ENCASH_CNT
data$MON_12_ACM_add_ACT_CNT = data$MON_12_ACM_ENTR_ACT_CNT + data$MON_12_ACM_LVE_ACT_CNT
data$MON_12_ACM_net_ACT_CNT = data$MON_12_ACM_ENTR_ACT_CNT - data$MON_12_ACM_LVE_ACT_CNT
data$MON_12_ACT_net_50_UP_CNT_PTY_QTY = data$MON_12_ACT_IN_50_UP_CNT_PTY_QTY - data$MON_12_ACT_OUT_50_UP_CNT_PTY_QTY
data$MON_12_ACT_add_50_UP_CNT_PTY_QTY = data$MON_12_ACT_IN_50_UP_CNT_PTY_QTY + data$MON_12_ACT_OUT_50_UP_CNT_PTY_QTY
data$MON_12_AGV_add_ACT_CNT = data$MON_12_AGV_ENTR_ACT_CNT + data$MON_12_AGV_LVE_ACT_CNT
data$MON_12_AGV_net_ACT_CNT = data$MON_12_AGV_ENTR_ACT_CNT - data$MON_12_AGV_LVE_ACT_CNT
data$MON_12_EXT_SAM_AMT_add = data$MON_12_EXT_SAM_AMT + data$MON_12_EXT_SAM_NM_TRSF_OUT_CNT
data$MON_12_EXT_SAM_AMT_net = data$MON_12_EXT_SAM_AMT - data$MON_12_EXT_SAM_NM_TRSF_OUT_CNT
data$MON_12_EXT_SAM_TRSF_add_AMT = data$MON_12_EXT_SAM_TRSF_IN_AMT + data$MON_12_EXT_SAM_TRSF_OUT_AMT
data$MON_12_EXT_SAM_TRSF_net_AMT = data$MON_12_EXT_SAM_TRSF_IN_AMT - data$MON_12_EXT_SAM_TRSF_OUT_AMT
data$MON_6_50_UP_ENTR_ACT_CNT_net = data$MON_6_50_UP_ENTR_ACT_CNT - data$MON_6_50_UP_LVE_ACT_CNT
data$MON_6_50_UP_ENTR_ACT_CNT_add = data$MON_6_50_UP_ENTR_ACT_CNT + data$MON_6_50_UP_LVE_ACT_CNT
data$REG_add = data$REG_CPT + data$REG_DT
data$REG_net = data$REG_CPT - data$REG_DT
data = data[,c(-3,-4,-6:-23,-60)]
test_all = data

##5.2结果预测
pred1 <- predict(random,test_all)
#pred1 = as.factor(ifelse((is.na(pred1)),1,pred1))
#test_all$LABEL = pred1

bla3 <- data.matrix(test_all[2:75])
prob1 <- predict(lgb,bla3);prob1 = (prob1 - min(prob1))/(max(prob1) - min(prob1))

test_pool <- catboost.load_pool(test_all[,-1])
prob3 <- catboost.predict(cat,test_pool);prob3 = (prob3 - min(prob3))/(max(prob3) - min(prob3))

test_matrix <- data.matrix(test_all[,c(-1)])
prob2 <- predict(xgb, test_matrix);prob2 = (prob2 - min(prob2))/(max(prob2) - min(prob2))

pred1 = as.numeric(pred1)-1;
prob = (pred1+2*prob1+2*prob2+6*prob3)/11

test_all$LABEL = prob
#pred = as.factor(ifelse((prob >= 0.5 | is.na(prob)),1,0));summary(pred)
#train <- read_xlsx("C:\\Users\\13407\\Desktop\\招商银行比赛\\train.xlsx")
#test_all <- read_xlsx("C:\\Users\\13407\\Desktop\\招商银行比赛\\test_A榜.xlsx")
#test_all$LABEL = pred
#test_all = test_all[,c(1,51,2:50)]
#all_data = rbind(train,test_all)
#write.csv(all_data,"C:\\Users\\13407\\Desktop\\招商银行比赛\\alldata.csv")
result = test_all[,c(1,76)]
write.csv(result,"C:\\Users\\13407\\Desktop\\招商银行比赛\\result.csv")
repl_python()










