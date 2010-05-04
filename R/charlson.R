charlson <-
function(Sequence,icd9) {
d <- cbind.data.frame(Sequence,icd9)

nobs<-nrow(d)

# initialize
d$code<-rep(NA,nobs)

# Create a vector of disease codes for each of the 23 categories (based on ICD9 codes):
# Acute Myocardial Infarction, Congestive Heart Failure, Peripheral Vascular Disease, Cerebrovascular Disease, Dementia, Chronic Pulmonary Disease
# Rheumatologic Disease, Peptic Ulcer Disease, Mild Liver Disease, Diabetes without complications, Diabetes with chronic complications, 
# Hemiplegia or Paraplegia, Renal Disease, Cancer, Moderate or Severe Liver Disease, Metastatic Carcinoma, AIDS/HIV
# diseases classes based on ICD9 coding
mi  <- c(410,412)
chf <- c(428)
pvd <- c(4439,7854)
cvd <- c(430:438)
dem <- c(290)
cpd <- c(490:496,500,501)
rhm <- c(7100,7101,7104,7140:7142)
pep <- c(531:534)
mld <- c(5712,5714:5716)
dnc <- c(2500:2503,2507)
dwc <- c(2504:2506)
ple <- c(342)
ren <- c(5830:5832,5834,5836,5837)
can <- c(172)
liv <- c(5722:5724,5728)
met <- c(196:199)
hiv <- c(42:44)

# check for each disease category, assign corresponding disease classification to icd9 code

for (i in 1:nobs) {
  for (j in 1:length(mi)) {
  if (d[i,2] == mi[j]) {
    d$code[i] <- "ch1"}
  }

  for (j in 1:length(chf)) {
  if (d[i,2] == chf[j]) {
    d$code[i] <- "ch2"}
  }

  for (j in 1:length(pvd)) {
  if (d[i,2] == pvd[j]) {
    d$code[i] <- "ch3"}
  }

  for (j in 1:length(cvd)) {
  if (d[i,2] == cvd[j]) {
    d$code[i] <- "ch4"}
  }

  for (j in 1:length(dem)) {
  if (d[i,2] == dem[j]) {
    d$code[i] <- "ch5"}
  }

  for (j in 1:length(cpd)) {
  if (d[i,2] == cpd[j]) {
    d$code[i] <- "ch6"}
  }

  for (j in 1:length(rhm)) {
  if (d[i,2] == rhm[j]) {
    d$code[i] <- "ch7"}
  }

  for (j in 1:length(pep)) {
  if (d[i,2] == pep[j]) {
    d$code[i] <- "ch8"}
  }

  for (j in 1:length(mld)) {
  if (d[i,2] == mld[j]) {
    d$code[i] <- "ch9"}
  }

  for (j in 1:length(dnc)) {
  if (d[i,2] == dnc[j]) {
    d$code[i] <- "ch10"}
  }

  for (j in 1:length(dwc)) {
  if (d[i,2] == dwc[j]) {
    d$code[i] <- "ch11"}
  }

  for (j in 1:length(ple)) {
  if (d[i,2] == ple[j]) {
    d$code[i] <- "ch12"}
  }

  for (j in 1:length(ren)) {
  if (d[i,2] == ren[j]) {
    d$code[i] <- "ch13"}
  }

  for (j in 1:length(can)) {
  if (d[i,2] == can[j]) {
    d$code[i] <- "ch14"}
  }

  for (j in 1:length(liv)) {
  if (d[i,2] == liv[j]) {
    d$code[i] <- "ch15"}
  }

  for (j in 1:length(met)) {
  if (d[i,2] == met[j]) {
    d$code[i] <- "ch16"}
  }

  for (j in 1:length(hiv)) {
  if (d[i,2] == hiv[j]) {
    d$code[i] <- "ch17"}
  }
}

# Select case number and disease class
#Need to add this to the end of dvars so all codes will be there
ch <- c("ch1", "ch2", "ch3" ,"ch4","ch5","ch6","ch7","ch8","ch9","ch10","ch11","ch12","ch13","ch14","ch15","ch16","ch17")
k <- data.frame(cbind(x="xxxREMOVEXXX", y=ch, z=ch))
# renaming vars in k so i can do a row merge
names(k) <- names(d)
d_vars <- rbind(d,k)


mydata <- melt(d_vars, id=c("Sequence","code")) 
mydata$value <-mydata$code
mydata2<-cast(mydata, Sequence~code)


# Then replace >1 with 1, and sum across ch1-ch17
 
mydata2$ch1<-ifelse(mydata2$ch1> 0, 1,0)
mydata2$ch2<-ifelse(mydata2$ch2> 0, 1,0)
mydata2$ch3<-ifelse(mydata2$ch3> 0, 1,0)
mydata2$ch4<-ifelse(mydata2$ch4> 0, 1,0)
mydata2$ch5<-ifelse(mydata2$ch5> 0, 1,0)
mydata2$ch6<-ifelse(mydata2$ch6> 0, 1,0)
mydata2$ch7<-ifelse(mydata2$ch7> 0, 1,0)
mydata2$ch8<-ifelse(mydata2$ch8> 0, 1,0)
mydata2$ch9<-ifelse(mydata2$ch9> 0, 1,0)
mydata2$ch10<-ifelse(mydata2$ch10> 0, 1,0)
mydata2$ch11<-ifelse(mydata2$ch11> 0, 1,0)
mydata2$ch12<-ifelse(mydata2$ch12> 0, 1,0)
mydata2$ch13<-ifelse(mydata2$ch13> 0, 1,0)
mydata2$ch14<-ifelse(mydata2$ch14> 0, 1,0)
mydata2$ch15<-ifelse(mydata2$ch15> 0, 1,0)
mydata2$ch16<-ifelse(mydata2$ch16> 0, 1,0)
mydata2$ch17<-ifelse(mydata2$ch17> 0, 1,0)

#
#get weights

# the last 6 comorbidities have increased weights assigned to them


 for (i in 1:nrow(mydata2)) {
 mydata2$comorbidity.n[i]<-sum(mydata2$ch1[i],mydata2$ch2[i],mydata2$ch3[i],mydata2$ch4[i],mydata2$ch5[i],mydata2$ch6[i],mydata2$ch7[i],mydata2$ch8[i],mydata2$ch9[i],mydata2$ch10[i],mydata2$ch11[i],mydata2$ch12[i],mydata2$ch13[i],mydata2$ch14[i],mydata2$ch15[i],mydata2$ch16[i],mydata2$ch17[i])
 mydata2$charleson[i]<-sum(mydata2$ch1[i],mydata2$ch2[i],mydata2$ch3[i],mydata2$ch4[i],mydata2$ch5[i],mydata2$ch6[i],mydata2$ch7[i],mydata2$ch8[i],mydata2$ch9[i],mydata2$ch10[i],mydata2$ch11[i],2*mydata2$ch12[i],2*mydata2$ch13[i],2*mydata2$ch14[i],3*mydata2$ch15[i],6*mydata2$ch16[i],6*mydata2$ch17[i])
 }


mydata3 <- mydata2[-nrow(mydata2),]

return(mydata3)
}

