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
test_all <- read_xlsx("C:\\Users\\13407\\Desktop\\招商银行比赛\\test_B榜.xlsx")
test_all <- read_xlsx("C:\\Users\\13407\\Desktop\\招商银行比赛\\test_A榜.xlsx")
train = read.csv("C:\\Users\\13407\\Desktop\\招商银行比赛\\alldata.csv")[,c(-1)]  #A榜的训练集和测试集的集合

train$LABEL = as.factor(train$LABEL)
##数据缺失情况对比分析
data = train_A
for(i in names(data[,c(-1)])){data[[i]] = ifelse((is.na(data[[i]]) | data[[i]] == '?'),NA,1)}
aggr(data)

data1 = test_all
for(i in names(data1[,c(-1)])){data1[[i]] = ifelse((is.na(data1[[i]]) | data1[[i]] == '?'),NA,1)}
aggr(data1)

##各个特征缺失状况进行分析
#summary(data[,c("AGN_AGR_LATEST_AGN_AMT","AGN_CNT_RCT_12_MON","AGN_CUR_YEAR_AMT","AGN_CUR_YEAR_WAG_AMT","AI_STAR_SCO")])
#summary(data[,c("COR_KEY_PROD_HLD_NBR","COUNTER_CUR_YEAR_CNT_AMT","CUR_MON_COR_DPS_MON_DAY_AVG_BAL","CUR_MON_EXT_SAM_CUST_TRSF_IN_AMT")])
#summary(data[,c("CUR_MON_EXT_SAM_CUST_TRSF_OUT_AMT","CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL","CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR","CUR_YEAR_COUNTER_ENCASH_CNT")])
#summary(data[,c("CUR_YEAR_MID_BUS_INC","CUR_YEAR_MON_AGV_TRX_CNT","CUR_YEAR_PUB_TO_PRV_TRX_PTY_CNT","EMP_NBR")])
#summary(data[,c("HLD_DMS_CCY_ACT_NBR","HLD_FGN_CCY_ACT_NBR","ICO_CUR_MON_ACM_TRX_TM","ICO_CUR_MON_ACM_TRX_AMT")])
#summary(data[,c("LAST_12_MON_COR_DPS_DAY_AVG_BAL","LAST_12_MON_COR_DPS_TM_PNT_BAL_PEAK_VAL","LAST_12_MON_DIF_NM_MON_AVG_TRX_AMT_NAV","LAST_12_MON_MON_AVG_TRX_AMT_NAV")])
#summary(data[,c("LGP_HLD_CARD_LVL","MON_12_ACM_ENTR_ACT_CNT","MON_12_ACM_LVE_ACT_CNT","MON_12_ACT_IN_50_UP_CNT_PTY_QTY")])
#summary(data[,c("MON_12_ACT_OUT_50_UP_CNT_PTY_QTY","MON_12_AGV_ENTR_ACT_CNT","MON_12_AGV_LVE_ACT_CNT","MON_12_AGV_TRX_CNT")])
#summary(data[,c("MON_12_CUST_CNT_PTY_ID","MON_12_EXT_SAM_AMT","MON_12_EXT_SAM_NM_TRSF_OUT_CNT","MON_12_EXT_SAM_TRSF_IN_AMT")])
#summary(data[,c("MON_12_EXT_SAM_TRSF_OUT_AMT","MON_12_TRX_AMT_MAX_AMT_PCTT","MON_6_50_UP_ENTR_ACT_CNT","MON_6_50_UP_LVE_ACT_CNT")])
#summary(data[,c("NB_CTC_HLD_IDV_AIO_CARD_SITU","NB_RCT_3_MON_LGN_TMS_AGV","OPN_TM","PUB_TO_PRV_TRX_AMT_CUR_YEAR")])
#summary(data[,c("REG_CPT","REG_DT","SHH_BCK","WTHR_OPN_ONL_ICO")])

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
train_num2$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL = ifelse((train_num2$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL == 0),1000000.0,train_num2$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL)
train_num2$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR = ifelse((train_num2$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR == 0),2.0,train_num2$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR)

##2.3合并train_num和train_char
train_change = left_join(train_char, train_num1, by = "CUST_UID");train_change = left_join(train_change, train_num2, by = "CUST_UID")
str(train_change)
summary(train_change)
data = train_change
rm(train_char,train_num,train_num1,train_num2,i,train_change)

##2.3重要特征构造
#data$CUR_Mon_COR_DMND_DPS_DAY_AVG_BAL = data$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL/12
#data$CUR_Mon_COR_DPS_YEAR_DAY_AVG_INCR = data$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR/12
data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL = data$CUR_MON_COR_DPS_MON_DAY_AVG_BAL*12
#data$CUR_Mon_MID_BUS_INC = data$CUR_YEAR_MID_BUS_INC/12
#data$CUR_add1 = data$CUR_Mon_COR_DMND_DPS_DAY_AVG_BAL + data$CUR_MON_COR_DPS_MON_DAY_AVG_BAL
#data$CUR_add2 = data$CUR_Mon_COR_DMND_DPS_DAY_AVG_BAL + data$CUR_Mon_COR_DPS_YEAR_DAY_AVG_INCR
#data$CUR_add3 = data$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL + data$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR
#data$CUR_add4 = data$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL + data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL
#data$CUR_add5 = data$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR + data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL
#data$CUR_add6 = data$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL + data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL + data$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR
#data$CUR_add7 = data$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL + data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL + data$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR + data$CUR_YEAR_MID_BUS_INC
#data$CUR_net1 = data$CUR_Mon_COR_DMND_DPS_DAY_AVG_BAL - data$CUR_MON_COR_DPS_MON_DAY_AVG_BAL
#data$CUR_net2 = data$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL - data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL
#data$CUR_net3 = data$CUR_Mon_COR_DPS_YEAR_DAY_AVG_INCR - data$CUR_Mon_MID_BUS_INC
#data$CUR_net4 = data$CUR_YEAR_MID_BUS_INC -  data$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR
data$CUR_add1 = data$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL + data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL
data$CUR_net1 = data$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL - data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL
data$CUR_add2 = data$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR + data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL
data$CUR_net2 = data$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR - data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL


data$HLD_add1 = data$HLD_DMS_CCY_ACT_NBR + data$HLD_FGN_CCY_ACT_NBR
data$HLD_net1 = data$HLD_DMS_CCY_ACT_NBR - data$HLD_FGN_CCY_ACT_NBR


#data$LAST_12_MON_COR_DPS_DAY_AVG_BAL_add1 = data$LAST_12_MON_COR_DPS_DAY_AVG_BAL + data$LAST_12_MON_COR_DPS_TM_PNT_BAL_PEAK_VAL
#data$LAST_12_MON_COR_DPS_DAY_AVG_BAL_net1 = data$LAST_12_MON_COR_DPS_DAY_AVG_BAL - data$LAST_12_MON_COR_DPS_TM_PNT_BAL_PEAK_VAL
#data$LAST_12_MON_MON_AVG_TRX_AMT_NAV_add1 = data$LAST_12_MON_MON_AVG_TRX_AMT_NAV + data$LAST_12_MON_DIF_NM_MON_AVG_TRX_AMT_NAV
#data$LAST_12_MON_MON_AVG_TRX_AMT_NAV_net1 = data$LAST_12_MON_MON_AVG_TRX_AMT_NAV - data$LAST_12_MON_DIF_NM_MON_AVG_TRX_AMT_NAV
#data$LAST_12_MON_add1 = data$LAST_12_MON_COR_DPS_TM_PNT_BAL_PEAK_VAL + data$LAST_12_MON_MON_AVG_TRX_AMT_NAV
data$LAST_12_MON_add1 = data$LAST_12_MON_COR_DPS_TM_PNT_BAL_PEAK_VAL + data$LAST_12_MON_COR_DPS_DAY_AVG_BAL
data$LAST_12_MON_net1 = data$LAST_12_MON_COR_DPS_TM_PNT_BAL_PEAK_VAL - data$LAST_12_MON_COR_DPS_DAY_AVG_BAL
data$LAST_12_MON_add2 = data$LAST_12_MON_DIF_NM_MON_AVG_TRX_AMT_NAV + data$LAST_12_MON_MON_AVG_TRX_AMT_NAV
data$LAST_12_MON_net2 = data$LAST_12_MON_DIF_NM_MON_AVG_TRX_AMT_NAV - data$LAST_12_MON_MON_AVG_TRX_AMT_NAV


#data$MON_12_ACM_add1 = data$MON_12_ACM_ENTR_ACT_CNT + data$MON_12_ACM_LVE_ACT_CNT
#data$MON_12_ACM_net1 = data$MON_12_ACM_LVE_ACT_CNT - data$MON_12_ACM_ENTR_ACT_CNT
#data$MON_12_AGV_add1 = data$MON_12_AGV_ENTR_ACT_CNT + data$MON_12_AGV_LVE_ACT_CNT
#data$MON_12_AGV_net1 = data$MON_12_AGV_LVE_ACT_CNT - data$MON_12_AGV_ENTR_ACT_CNT

#data$MON_12_EXT_SAM_TRSF_net_AMT = data$MON_12_EXT_SAM_TRSF_IN_AMT - data$MON_12_EXT_SAM_TRSF_OUT_AMT
#data$MON_12_EXT_SAM_TRSF_add_AMT = data$MON_12_EXT_SAM_TRSF_IN_AMT + data$MON_12_EXT_SAM_TRSF_OUT_AMT
#data$MON_12_EXT_SAM_AMT_net1 = data$MON_12_EXT_SAM_AMT - data$MON_12_EXT_SAM_NM_TRSF_OUT_CNT + data$MON_12_EXT_SAM_TRSF_IN_AMT - data$MON_12_EXT_SAM_TRSF_OUT_AMT

data$REG_add = data$REG_CPT + data$REG_DT
data$REG_net = data$REG_CPT - data$REG_DT


data = data[,c(-56:-59,-70:-74)]
data = data[,c(-9:-26)]
data = data[,c(-3:-8)]
#data = data[,c(-3:-28,-30:-35,-40:-43,-49:-54)]
#data1 = data
#data = data[,c(-28:-58)]
#data = data[,c("AI_STAR_SCO","COUNTER_CUR_YEAR_CNT_AMT","CUR_MON_EXT_SAM_CUST_TRSF_IN_AMT","CUR_MON_EXT_SAM_CUST_TRSF_OUT_AMT",
#               "CUR_YEAR_COUNTER_ENCASH_CNT","CUR_YEAR_MON_AGV_TRX_CNT","ICO_CUR_MON_ACM_TRX_TM","ICO_CUR_MON_ACM_TRX_AMT",
#               "MON_12_ACT_IN_50_UP_CNT_PTY_QTY","MON_12_ACT_OUT_50_UP_CNT_PTY_QTY","MON_12_CUST_CNT_PTY_ID","MON_6_50_UP_ENTR_ACT_CNT",
#               "MON_6_50_UP_LVE_ACT_CNT","NB_CTC_HLD_IDV_AIO_CARD_SITU","SHH_BCK","WTHR_OPN_ONL_ICO")]

##-------------------------------------------------------------------------------------------------------------------------------------------------
##3.模型选择和训练
##3.1 RandomForest模型
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

formula <- Formula::as.Formula(train[2:49])
random <- randomForest(formula,data = train, mtry = 15,ntree=400 ,importance = FALSE,na.action = na.omit)
#plot.roc(as.factor(train$LABEL), random$votes[,1],percent=TRUE, lwd=1, print.auc=TRUE, add=TRUE, print.auc.y=40)
varImpPlot(random, n.var = min(47, nrow(random$importance)),
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

bla1 <- data.matrix(train[3:49])
bla2 <- train$LABEL
dtrain <- lgb.Dataset(data = bla1, label = bla2)
bla3 <- data.matrix(test[3:49])
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
lgb <- lightgbm(data = dtrain,nrounds = 60,early_stopping_rounds = 2,num_threads = 2,objective = "regression")
prob1 <- predict(lgb,bla2);prob1 = (prob1 - min(prob1))/(max(prob1) - min(prob1))
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
xgb <- xgboost(params = params,data = train_matrix,label = train_label,nrounds = 500,early_stopping_rounds = 11,num_threads = 3)
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
              iterations = 1200,
              metric_period=10,
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
#data = data[,c(-3:-4,-41:-76)]
data$LABEL = as.numeric(data$LABEL)
for(i in names(data[,c(-1:-2)])){data[[i]] = (data[[i]] - min(data[[i]]))/(max(data[[i]]) - min(data[[i]]))}
index <- sample(nrow(data),round(0.7*nrow(data)))
train <- data[index,]
test <- data[-index,]

formula <- Formula::as.Formula(train[2:58])
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
prob = (1*pred1+2*prob1+1*prob2+2*prob3)/5
roccurve<- plot(roc(response = as.numeric(test$LABEL), predictor = as.numeric(prob)),
                legacy.axes = TRUE, print.auc=TRUE, main = "ROC Curve")
roc0 = roc(response = as.numeric(test$LABEL), predictor = as.numeric(prob));auc0 = auc(roc0);auc0

##-------------------------------------------------------------------------------------------------------------------------------------------------
##4.全部数据训练模型
##4.1 RandomForest模型
par(mfrow = c(2,2))
formula <- Formula::as.Formula(data[2:27])
random <- randomForest(formula,data = data, mtry = 15,ntree=400 ,importance = FALSE,na.action = na.omit)

pred1 <- predict(random,data)
roccurve<- plot(roc(response = as.numeric(data$LABEL), predictor = as.numeric(pred1)),
                legacy.axes = TRUE, print.auc=TRUE, main = "ROC Curve")
roc1 = roc(response = as.numeric(data$LABEL), predictor = as.numeric(pred1));auc1 = auc(roc1);auc1

##4.2 LightGBM模型
bla1 <- data.matrix(data[3:73])
bla2 <- data$LABEL
dtrain <- lgb.Dataset(data = bla1, label = bla2)
lgb <- lightgbm(data = dtrain,nrounds = 60,early_stopping_rounds = 2,num_threads = 2,objective = "regression")
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
xgb <- xgboost(params = params,data = train_matrix,label = train_label,nrounds = 500,early_stopping_rounds = 11,num_threads = 3)
prob2 <- predict(xgb, train_matrix);prob2 = (prob2 - min(prob2))/(max(prob2) - min(prob2))
roccurve<- plot(roc(response = as.numeric(data$LABEL), predictor = as.numeric(prob2)),
                legacy.axes = TRUE, print.auc=TRUE, main = "ROC Curve")
roc3 = roc(response = as.numeric(data$LABEL), predictor = as.numeric(prob2));auc3 = auc(roc3);auc3

##4.4 Catboost模型
train_feactures <- data[,-1:-2]
train_label <- as.numeric(data$LABEL)-1
train_pool <- catboost.load_pool(data = train_feactures,label = train_label)
params = list(loss_function = 'Logloss',
              iterations = 1200,
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

#4.8 神经网络
library(RSNNS)
library(Rcpp)

data1$LABEL = as.factor(data1$LABEL) 
data = data1
data = data[,c(-28:-58)]
data = data[sample(1:nrow(data),length(1:nrow(data))),1:ncol(data)]
dataValues = data[,c(-1:-2)]
datatargets = decodeClassLabels(data$LABEL)
data = splitForTrainingAndTest(dataValues, datatargets, ratio=0.15)
data = normTrainingAndTestSet(data)
model = mlp(data$inputsTrain, data$targetsTrain, size=5, learnFunc="Quickprop", 
            learnFuncParams=c(0.1, 2.0, 0.0001, 0.1),maxit=50, inputsTest=data$inputsTest, targetsTest=data$targetsTest)
predictions = predict(model,data$inputsTest)
#confusionMatrix(as.factor(data$targetsTest),as.factor(predictions)) 
confusionMatrix(data$targetsTest,predictions) 

ind = sample(2,nrow(data),replace = TRUE,prob = c(0.7,0.3))
trainset = data[ind == 1,]
testset = data[ind == 2,]

data.nn = nnet(LABEL ~ .,data = trainset[,c(-1)],size = 2,rang = 0.1,decay = 5e-4,maxit = 200)
summary(data.nn)
data.predict = predict(data.nn,testset,type = "class")
nn.table = table(testset$LABEL,data.predict)
nn.table
data.predict

##4.7 结果组合
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
#data$CUR_Mon_COR_DMND_DPS_DAY_AVG_BAL = data$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL/12
#data$CUR_Mon_COR_DPS_YEAR_DAY_AVG_INCR = data$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR/12
data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL = data$CUR_MON_COR_DPS_MON_DAY_AVG_BAL*12
#data$CUR_Mon_MID_BUS_INC = data$CUR_YEAR_MID_BUS_INC/12
#data$CUR_add1 = data$CUR_Mon_COR_DMND_DPS_DAY_AVG_BAL + data$CUR_MON_COR_DPS_MON_DAY_AVG_BAL
#data$CUR_add2 = data$CUR_Mon_COR_DMND_DPS_DAY_AVG_BAL + data$CUR_Mon_COR_DPS_YEAR_DAY_AVG_INCR
#data$CUR_add3 = data$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL + data$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR
#data$CUR_add4 = data$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL + data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL
#data$CUR_add5 = data$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR + data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL
#data$CUR_add6 = data$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL + data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL + data$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR
#data$CUR_add7 = data$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL + data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL + data$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR + data$CUR_YEAR_MID_BUS_INC
#data$CUR_net1 = data$CUR_Mon_COR_DMND_DPS_DAY_AVG_BAL - data$CUR_MON_COR_DPS_MON_DAY_AVG_BAL
#data$CUR_net2 = data$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL - data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL
#data$CUR_net3 = data$CUR_Mon_COR_DPS_YEAR_DAY_AVG_INCR - data$CUR_Mon_MID_BUS_INC
#data$CUR_net4 = data$CUR_YEAR_MID_BUS_INC -  data$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR
data$CUR_add1 = data$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL + data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL
data$CUR_net1 = data$CUR_YEAR_COR_DMND_DPS_DAY_AVG_BAL - data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL
data$CUR_add2 = data$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR + data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL
data$CUR_net2 = data$CUR_YEAR_COR_DPS_YEAR_DAY_AVG_INCR - data$CUR_Year_COR_DPS_MON_DAY_AVG_BAL


data$HLD_add1 = data$HLD_DMS_CCY_ACT_NBR + data$HLD_FGN_CCY_ACT_NBR
data$HLD_net1 = data$HLD_DMS_CCY_ACT_NBR - data$HLD_FGN_CCY_ACT_NBR


#data$LAST_12_MON_COR_DPS_DAY_AVG_BAL_add1 = data$LAST_12_MON_COR_DPS_DAY_AVG_BAL + data$LAST_12_MON_COR_DPS_TM_PNT_BAL_PEAK_VAL
#data$LAST_12_MON_COR_DPS_DAY_AVG_BAL_net1 = data$LAST_12_MON_COR_DPS_DAY_AVG_BAL - data$LAST_12_MON_COR_DPS_TM_PNT_BAL_PEAK_VAL
#data$LAST_12_MON_MON_AVG_TRX_AMT_NAV_add1 = data$LAST_12_MON_MON_AVG_TRX_AMT_NAV + data$LAST_12_MON_DIF_NM_MON_AVG_TRX_AMT_NAV
#data$LAST_12_MON_MON_AVG_TRX_AMT_NAV_net1 = data$LAST_12_MON_MON_AVG_TRX_AMT_NAV - data$LAST_12_MON_DIF_NM_MON_AVG_TRX_AMT_NAV
#data$LAST_12_MON_add1 = data$LAST_12_MON_COR_DPS_TM_PNT_BAL_PEAK_VAL + data$LAST_12_MON_MON_AVG_TRX_AMT_NAV
data$LAST_12_MON_add1 = data$LAST_12_MON_COR_DPS_TM_PNT_BAL_PEAK_VAL + data$LAST_12_MON_COR_DPS_DAY_AVG_BAL
data$LAST_12_MON_net1 = data$LAST_12_MON_COR_DPS_TM_PNT_BAL_PEAK_VAL - data$LAST_12_MON_COR_DPS_DAY_AVG_BAL
data$LAST_12_MON_add2 = data$LAST_12_MON_DIF_NM_MON_AVG_TRX_AMT_NAV + data$LAST_12_MON_MON_AVG_TRX_AMT_NAV
data$LAST_12_MON_net2 = data$LAST_12_MON_DIF_NM_MON_AVG_TRX_AMT_NAV - data$LAST_12_MON_MON_AVG_TRX_AMT_NAV


#data$MON_12_ACM_add1 = data$MON_12_ACM_ENTR_ACT_CNT + data$MON_12_ACM_LVE_ACT_CNT
#data$MON_12_ACM_net1 = data$MON_12_ACM_LVE_ACT_CNT - data$MON_12_ACM_ENTR_ACT_CNT
#data$MON_12_AGV_add1 = data$MON_12_AGV_ENTR_ACT_CNT + data$MON_12_AGV_LVE_ACT_CNT
#data$MON_12_AGV_net1 = data$MON_12_AGV_LVE_ACT_CNT - data$MON_12_AGV_ENTR_ACT_CNT

#data$MON_12_EXT_SAM_TRSF_net_AMT = data$MON_12_EXT_SAM_TRSF_IN_AMT - data$MON_12_EXT_SAM_TRSF_OUT_AMT
#data$MON_12_EXT_SAM_TRSF_add_AMT = data$MON_12_EXT_SAM_TRSF_IN_AMT + data$MON_12_EXT_SAM_TRSF_OUT_AMT
#data$MON_12_EXT_SAM_AMT_net1 = data$MON_12_EXT_SAM_AMT - data$MON_12_EXT_SAM_NM_TRSF_OUT_CNT + data$MON_12_EXT_SAM_TRSF_IN_AMT - data$MON_12_EXT_SAM_TRSF_OUT_AMT

data$REG_add = data$REG_CPT + data$REG_DT
data$REG_net = data$REG_CPT - data$REG_DT

#data = data[,c(-3:-28,-30:-35,-40:-43,-49:-54)]
data = data[,c(-55:-58,-69:-73)]
data = data[,c(-8:-25)]
data = data[,c(-2:-7)]
test_all = data

##5.2结果预测
pred1 <- predict(random,test_all);pred1 = as.factor(ifelse((is.na(pred1)),1,pred1))
summary(pred1)
#pred1 = as.factor(ifelse((is.na(pred1)),1,pred1))
#test_all$LABEL = pred1

bla3 <- data.matrix(test_all[2:48])
prob1 <- predict(lgb,bla3);prob1 = (prob1 - min(prob1))/(max(prob1) - min(prob1))

test_pool <- catboost.load_pool(test_all[,-1])
prob3 <- catboost.predict(cat,test_pool);prob3 = (prob3 - min(prob3))/(max(prob3) - min(prob3))

test_matrix <- data.matrix(test_all[,c(-1)])
prob2 <- predict(xgb, test_matrix);prob2 = (prob2 - min(prob2))/(max(prob2) - min(prob2))

pred1 = as.numeric(pred1)-1;
prob = (1*pred1+2*prob1+1*prob2+4*prob3 )/8
pred = as.factor(ifelse((prob>0.5),1,0));summary(pred)
test_all$LABEL = prob
result = test_all[,c(1,49)]

#test_all$LABEL = prob
#test_all$prob1 = prob1
#test_all$prob2 = prob2
#test_all$prob3 = prob3
#test_all$pred1 = pred1
#result = test_all[,c(1,76:79)]
#result$queshi = ifelse((is.na(result$pred1)),0,1)
#result$LABEL = ifelse((is.na(result$LABEL)),(2*result$prob1 + result$prob2 + 2*result$prob3)/5,result$LABEL);summary(result$LABEL)
#result$LABEL = (result$LABEL - min(result$LABEL))/(max(result$LABEL) - min(result$LABEL));summary(result$LABEL)
write.csv(result,"C:\\Users\\13407\\Desktop\\招商银行比赛\\result.csv")
repl_python()

prob = (prob1 + prob1 +prob1)/3;pred = as.factor(ifelse((prob > 0.5),1,0));summary(pred)
pred = test_all$pred
pred = gl(2,6000,length=12000); pred = as.factor(ifelse((pred == 2),0,pred));summary(pred)
pred = test_all$pred;pred = as.factor(ifelse((pred == 0),0,1));summary(pred)
first_test = read.table("D:\\13407\\浏览器下载\\0511提交第一次.txt");names(first_test) = c("GUIT","prob")
first_test = read.table("D:\\13407\\浏览器下载\\0510提交.txt");names(first_test) = c("GUIT","prob")
first_test$label = ifelse((first_test$prob>0.5),1,0);summary(as.factor(first_test$label));a <- table(pred,first_test$label);a;(2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]))
roccurve<- plot(roc(response = as.numeric(pred), predictor = as.numeric(first_test$prob)),
                legacy.axes = TRUE, print.auc=TRUE, main = "ROC Curve")
roc0 = roc(response = as.numeric(pred), predictor = as.numeric(first_test$prob));auc0 = auc(roc0);auc0
first_test$label = ifelse((first_test$prob>0.5),1,0);summary(as.factor(first_test$label));a <- table(pred,first_test$label);a;(2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]))
test_all$pred = as.factor(ifelse((pred == 1 & first_test$label == 1),1,0));summary(test_all$pred)



first_test = read.table("D:\\13407\\浏览器下载\\预测结果(1).txt");names(first_test) = c("GUIT","prob")
first_test$label = ifelse((first_test$prob>0.5),1,0);summary(as.factor(first_test$label));a <- table(pred,first_test$label);a;(2*a[2,2]/(2*a[2,2]+a[1,2]+a[2,1]))
roccurve<- plot(roc(response = as.numeric(pred), predictor = as.numeric(first_test$prob)),
                legacy.axes = TRUE, print.auc=TRUE, main = "ROC Curve")
test_all$pred = as.factor(ifelse((pred == 1 & first_test$label == 1),1,0));summary(test_all$pred)
Actual_label = test_all$pred
result = test_all[,c(1,28)];names(result) = c("CUST_UID","LABEL")


data = test_all
for(i in names(data[,c(-1)])){data[[i]] = (data[[i]] - min(data[[i]]))/(max(data[[i]]) - min(data[[i]]))}
prob4<-predict(object =logmodel,newdata=data);summary(prob4)
pred4 = as.factor(ifelse((prob4 >=0.5),1,0));summary(pred4)
roccurve<- plot(roc(response = as.numeric(result$LABEL), predictor = as.numeric(prob4)),
                legacy.axes = TRUE, print.auc=TRUE, main = "ROC Curve")
roc5 = roc(response = as.numeric(test$LABEL), predictor = as.numeric(prob4));auc5 = auc(roc5);auc5






test_all$LABEL = prob
result = test_all[,c(1,49)]
write.csv(result,"C:\\Users\\13407\\Desktop\\招商银行比赛\\result.csv")
repl_python()




