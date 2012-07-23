setwd('C:/My Projects/Zheng - Neural network - RAD - 254/')
bureaData<-read.csv("NovDevSample_full.csv",header=TRUE,sep=",",na.strings="ND",stringsAsFactors=FALSE);



bureaData$APPLSUMMARY_COUNTS_BB<-ifelse(bureaData$APPLSUMMARY_COUNTS_BB==-999997,15,bureaData$APPLSUMMARY_COUNTS_BB);
bureaData$BAD45<-factor(bureaData$BAD45);
bureaData$FULL_BOTH_MG<-ifelse(bureaData$FULL_BOTH_MG=='U',0.5,bureaData$FULL_BOTH_MG);
bureaData$FULL_BOTH_MG<-ifelse(bureaData$FULL_BOTH_MG=='D',7,bureaData$FULL_BOTH_MG);
bureaData$FULL_BOTH_LG<-ifelse(bureaData$FULL_BOTH_LG=='U',0.5,bureaData$FULL_BOTH_LG);
bureaData$FULL_BOTH_LG<-ifelse(bureaData$FULL_BOTH_LG=='D',7,bureaData$FULL_BOTH_LG);
bureaData$FULL_ACTHCSTATUS_RYB<-ifelse(bureaData$FULL_ACTHCSTATUS_RYB=='U',0.5,bureaData$FULL_ACTHCSTATUS_RYB);
bureaData$FULL_ACTHCSTATUS_RYB<-ifelse(bureaData$FULL_ACTHCSTATUS_RYB=='D',7,bureaData$FULL_ACTHCSTATUS_RYB);
bureaData$FULL_ACTHCSTATUS_RYB<-ifelse(bureaData$FULL_ACTHCSTATUS_RYB=='?',-1,bureaData$FULL_ACTHCSTATUS_RYB);
bureaData$APPLSUMMARY_COUNTS_CB<-ifelse(bureaData$APPLSUMMARY_COUNTS_CB==-999997,NA,bureaData$APPLSUMMARY_COUNTS_CB);
bureaData[,9][bureaData[,9]==-999997]<-NA

for (i in 10:60)
  bureaData[,i][bureaData[,i]==-999997]<-NA
head(bureaData)

table(bureaData$BAD45,bureaData$PARTIAL_NUMDEFBAL_PT,useNA="ifany")


a<-rep(0,190)
for (i in 1:190)
  a[i]=length(bureaData[,i][bureaData[,i]==-999999])
write.csv(a.file="ob.csv")

pvar<-read.csv(file="predvars.csv",header=FALSE,sep=",",stringsAsFactors=FALSE);#should include bad45 as target label.
myvars<-names(bureaData) %in% pvar;
bdata<-bureaData[myvars];

bdata$isMDSPLL<-ifelse(is.na(bdata$DaysSincePrevLastLoan),1,0)#handle missing value, convert into 2 dummy variables.
bdata$DSPLLValue<-ifelse(is.na(bdata$DaysSincePrevLastLoan),-1,bdata$DaysSincePrevLastLoan)




trainindex<-index[1:index2];
validindex<-index[87154:index3]
testindex<-index[130729:174305]
#testindex<-index[122015:174305];
bdata$BAD45f<-as.factor(bdata$BAD45)

train<-bdata[trainindex,];
test<-bdata[testindex,];

corr1<-rep(0,69);
for (i in 1:69)
{
  corr1[i]<-cor(train[,i],train[,3],use="complete.obs")  
}

for (i in 1:69)
{
  corr1[i]<-cor(train[,i],train[,3],use="complete.obs")  
}

nnindex<-corr>0&!is.na(corr)

library(nnet);
library(ROCR)
nn<-nnet(BAD45~AllArrears+AllDelinquency+DaysSinceLastLoan+LastArrears+MaxDelinquency+Prev_TopUpAmount+PrevDel15+PrevDel20+PrevDelDue+PrevDelinquency+PreviousArrears+PreviousCash+PreviousInterestRate+PrevTerm+TimesExtended+TopUpAmount+TotalActualTerm+TotalInterestRate+AnyPrevD027+AnyPrevD027_30+AnyPrevD030+AnyPrevDelDue+AnyPrevDel15+AnyPrevDel20+AvgArrears+AvgCash+AvgDelinquency+AvgEarlyPayOff_num+AvgTerm+AvgTimesExtended+AvgTopUp+AvgTopUpPerTerm+Trend_PrevAvgTerm+Trend_PrevAvgCash,data=train,size=20,linout=FALSE,decay=0.025,maxit=5000)

normbdata<-scale(bdata[,c('AllArrears','AllDelinquency','DaysSinceLastLoan','LastArrears','MaxDelinquency','Prev_TopUpAmount','PrevDel15','PrevDel20','PrevDelDue','PrevDelinquency','PreviousArrears','PreviousCash','PreviousInterestRate','PrevTerm','TimesExtended','TopUpAmount','TotalActualTerm','TotalInterestRate','AnyPrevD027','AnyPrevD027_30','AnyPrevD030','AnyPrevDelDue','AnyPrevDel15','AnyPrevDel20','AvgArrears','AvgCash','AvgDelinquency','AvgEarlyPayOff_num','AvgTerm','AvgTimesExtended','AvgTopUp','AvgTopUpPerTerm','Trend_PrevAvgTerm','Trend_PrevAvgCash')]);
normbdata<-(normbdata+1)/2#removed
normbdata<-cbind(normbdata,bdata$BAD45);
colnames(normbdata)[35]<-"BAD45"
train<-normbdata[trainindex,];
valid<-normbdata[validindex,];
test<-normbdata[testindex,];

nn.pred<-predict(nn,bdata$isMDSPLL<-ifelse(is.na(bdata$DaysSincePrevLastLoan),1,0)
                 valid)
pred<-prediction(nn.pred,valid$BAD45);
perf<-performance(pred,"tpr","fpr")
auc<-performance(pred,"auc")
auc

nn.pred<-predict(nn,test)
pred<-prediction(nn.pred,test$BAD45);
perf<-performance(pred,"tpr","fpr")
auc<-performance(pred,"auc")
auc

lr.pred<-predict(lr,test)
pred<-prediction(lr.pred,test$BAD45);
perf<-performance(pred,"tpr","fpr")
auc<-performance(pred,"auc")
auc

------------------------------------------------------------------------------------
  normalize
> i<-2
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-6
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-7
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-11
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-12
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-15
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-16
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-17
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-18
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-19
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-20
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-21
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-27
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-32
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-33
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-34
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-38
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-39
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-40
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-41
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-42
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-43
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-44
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-45
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-52
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-55
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-56
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-57
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-58
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-59
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-60
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-61
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-62
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-66
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-67
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-70
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)
> i<-71
> results2<-trimstats(bdata[,i]); bdata[,i]<-mynorm(bdata[,i],results2)

train<-bdata[which(bdata$RND==1),]
test<-bdata[which(bdata$RND==0),]

nncom<-data.frame(n2n.pred,n3n.pred,n4n.pred);
r<-apply(nncom,1,max);
pred<-prediction(r,test$BAD45);
perf<-performance(pred,"tpr","fpr")
auc<-performance(pred,"auc")
auc

r<-apply(nncom,1,mean)
pred<-prediction(r,test$BAD45);
perf<-performance(pred,"tpr","fpr")
auc<-performance(pred,"auc")
auc

------------------------------------------------------------------------
  #use new variables
  pvar<-read.csv(file="predvars2.csv",header=FALSE,sep=",",stringsAsFactors=FALSE);#should include bad45 as target label.
myvars<-names(bureaData) %in% pvar;
bdata<-bureaData[myvars];

bdata$isTMBOB<-ifelse(bdata$FULL_INDEBT_TMB==-999999,1,0);
bdata$isTMBND<-ifelse(bdata$FULL_INDEBT_TMB==-999997,1,0);
bdata$TMBValue<-ifelse(bdata$FULL_INDEBT_TMB==-999999,-2,bdata$FULL_INDEBT_TMB);
bdata$TMBValue<-ifelse(bdata$TMBValue==-999997,-1,bdata$TMBValue);

bdata$isEIND<-ifelse(bdata$FULL_WORST0_EI==-999997,1,0);
min(bdata$FULL_WORST0_EI[bdata$FULL_WORST0_EI!=-999997])
bdata$EIValue<-ifelse(bdata$FULL_WORST0_EI==-999997,-1,bdata$FULL_WORST0_EI);

bdata$isUTND<-ifelse(bdata$PARTIAL_BOTH_UT==-999997,1,0);
bdata$UTValue<-ifelse(bdata$PARTIAL_BOTH_UT==-999997,-1,bdata$PARTIAL_BOTH_UT);

bdata$isBFBND<-ifelse(bdata$SEARCH_ALLCREDIT_BFB==-999997,1,0);
bdata$BFBValue<-ifelse(bdata$SEARCH_ALLCREDIT_BFB==-999997,-1,bdata$SEARCH_ALLCREDIT_BFB)

#bdata$LoanAmount[is.na(bdata$LoanAmount)]<-286#286=mean(bdata$LoanAmount,na.rm=TRUE) 25 missing value
bdata$Term[is.na(bdata$Term)]<-18#18=median(bdata$bdata$Term,na.rm=TRUE) 25 missing value
bdata$LoanToIncome[is.na(bdata$LoanToIncome)]<-57.14#57.14=max(bdata$LoanToIncome,na.rm=TRUE) 25 missing value
bdata$PrevDelinquency[is.na(bdata$PrevDelinquency)]<-0#0 is median and 0.4462 is mean, only one missing value
bdata$LoanAmount[is.na(bdata$LoanAmount)]<-200;#200 is the median and the histogram is skew, so not mean.
bdata$LoanAmountRatioBetweenNowPrev<-bdata$LoanAmount/bdata$PreviousCash;
bdata$TermRatioBetweenNowPrev<-bdata$Term/bdata$PrevTerm;


train<-bdata[which(bdata$RND==1),]
test<-bdata[which(bdata$RND==0),]

set.seed(4321); 
n4n1<-nnet(BAD45~DaysSincePayOff+AvgDelinquency+MaxDelinquency+LastArrears+NumberOfStringLoans3+AllArrears+AvgTimesExtended+PrevDel20+EarlyPayOff_num+AvgCash+DaysSinceLastProposal+FULL_INDEBT_TMB+FULL_WORST0_EI+LoanAmount+LoanNumber+PrevD760+RegNumberPresent+SEARCH_ALLCREDIT_BFB+Term+LoanToIncome+PARTIAL_BOTH_UT+LoanAmountRatioBetweenNowPrev+TermRatioBetweenNowPrev,data=train,size=4,linout=FALSE,decay=0.025,maxit=800)


n2n.pred<-predict(n2n,test)
pred<-prediction(n2n.pred,test$BAD45);
perf<-performance(pred,"tpr","fpr")
auc<-performance(pred,"auc")
auc

n15n1<-nnet(BAD45~DaysSincePayOff+AvgDelinquency+MaxDelinquency+LastArrears+NumberOfStringLoans3+AllArrears+AvgTimesExtended+PrevDel20+EarlyPayOff_num+AvgCash+DaysSinceLastProposal+isTMBOB+isTMBND+TMBValue+isEIND+EIValue+LoanAmount+LoanNumber+PrevD760+RegNumberPresent+isBFBND+BFBValue+Term+LoanToIncome+isUTND+UTValue+LoanAmountRatioBetweenNowPrev+TermRatioBetweenNowPrev,data=train,size=15,linout=FALSE,decay=0.025,maxit=800)
nncom<-data.frame(n15n1.pred,n15n2.pred,n15n3.pred);
r<-apply(nncom,1,mean);
auc<-performance(pred,"auc")
auc

------------------------------------------------------------------------------------------------------------------------------------------------
scores<-read.csv("ModelPredict_LR.csv",header=TRUE,sep=",",stringsAsFactors=FALSE);
bbdata<-merge(bdata,scores,by="ProposalID");
train<-bbdata[which(bbdata$RND.x==1&bbdata$Scorelr<901),]
test<-bbdata[which(bbdata$RND.x==0&bbdata$Scorelr<901),]
n4n1l<-nnet(BAD45.x~DaysSincePayOff+AvgDelinquency+MaxDelinquency+LastArrears+NumberOfStringLoans3+AllArrears+AvgTimesExtended+PrevDel20+EarlyPayOff_num+AvgCash+DaysSinceLastProposal+isTMBOB+isTMBND+TMBValue+isEIND+EIValue+LoanAmount+LoanNumber+PrevD760+RegNumberPresent+isBFBND+BFBValue+Term+LoanToIncome+isUTND+UTValue+LoanAmountRatioBetweenNowPrev+TermRatioBetweenNowPrev,data=train,size=4,linout=FALSE,decay=0.025,maxit=800)

test$pred<-n4ntest.pred;
train$pred<-n4ntrain.pred;
total<-rbind(train,test);
preds<-data.frame(total$ProposalID,total$pred);
final<-merge(bureaData,preds, by="ProposalID");
colnames(preds)<-c('ProposalID','Pred')
final<-merge(bureaData,preds, by="ProposalID");
output<-data.frame(final$ProposalID,final$Pred);

set.seed(4321); #The final model we used
n4n1<-nnet(BAD45~DaysSincePayOff+AvgDelinquency+MaxDelinquency+LastArrears+NumberOfStringLoans3+AllArrears+AvgTimesExtended+PrevDel20+EarlyPayOff_num+AvgCash+DaysSinceLastProposal+isTMBOB+isTMBND+TMBValue+isEIND+EIValue+LoanAmount+LoanNumber+PrevD760+RegNumberPresent+isBFBND+BFBValue+Term+LoanToIncome+isUTND+UTValue+LoanAmountRatioBetweenNowPrev+TermRatioBetweenNowPrev,data=train,size=4,linout=FALSE,decay=0.025,maxit=800)

---------------------------------------------------------------------------------------------------
#build prediction from nn for training and test data, 9 models are built and train lr model above them.
trainpred<-data.frame(n15n3train.pred,n15n2train.pred,n15n1train.pred,n10n3train.pred,n10n2train.pred,n10n1train.pred,n4n3train.pred,n4n2train.pred,n4n1train.pred,train$BAD45);
colnames(trainpred)<-c('n15n3','n15n2','n15n1','n10n3','n10n2','n10n1','n4n3','n4n2','n4n1','BAD45');

testpred<-data.frame(n15n3test.pred,n15n2test.pred,n15n1test.pred,n10n3test.pred,n10n2test.pred,n10n1test.pred,n4n3test.pred,n4n2test.pred,n4n1test.pred,test$BAD45);
colnames(testpred)<-c('n15n3','n15n2','n15n1','n10n3','n10n2','n10n1','n4n3','n4n2','n4n1','BAD45')

lrtrain<-glm(BAD45~n15n3+n15n2+n15n1+n10n3+n10n2+n10n1+n4n3+n4n2+n4n1,family=binomial("logit"),data=trainpred);
lrtest.pred<-predict(lrtrain,testpred);
pred<-prediction(lrtest.pred,testpred$BAD45);
auc<-performance(pred,"auc")
auc
--------------------------------------------------------------------------------------------------------
trainsco<-scores[which(scores$RND==1),]
testsco<-scores[which(scores$RND==0),]
l2<-lm(BAD45~P2lr+Pred,data=scores)
l2; # give me the weights and normalize them i get the weights for lr is 0.46 and for nn is 0.54;
scores<-scores[scores(complete.cases),]
nscores<-scores[order(scores$comsco),]
badnscores<-nscores[1:13402,]
sum(badnscores$BAD45)/13402;
sum(badnscores$BAD45)/sum(nscores$BAD45);

------------------------------------------------------------------------------------------------------------
library(session);
save.session("n4n1.Rda");
restore.session("n4n1.Rda");




