library(caret)

#Open file
data_houseprice <- read.csv('train.csv')
head(data_houseprice)
View(data_houseprice)

#Checking missing file
is.na(data_houseprice)
colSums(is.na(data_houseprice))

#1. Handling Missing Value
## fill LotFrontage with average value (mean)
data_houseprice [is.na(data_houseprice$LotFrontage),'LotFrontage'] <- mean(data_houseprice$LotFrontage,na.rm = TRUE)
LotFrontageedit <- data_houseprice$LotFrontage

#2. Normalization 
norm_scaler <- preProcess(data_houseprice['LotArea'],method = c('range'))#preproses untuk mendapat nilai min dan max
data_houseprice['LotArea'] <- predict(norm_scaler,data_houseprice['LotArea'])#menyimpan nilai" dan rumus, yang diperlukan
LotAreaedit <- data_houseprice$LotArea
View(data_houseprice)

#3. Data Transformation 
saleprice <- data$SalePrice
hist(saleprice)
hist(saleprice[saleprice < 400000])
# Boxcox #
lambda <- BoxCoxTrans(abs_saleprice)$lambda
boxcox_saleprice <- (abs_saleprice**lambda - 1)/lambda
hist(boxcox_saleprice[boxcox_saleprice>3.22])
data_houseprice['SalesPrice_edit'] <- boxcox_saleprice
View(data_houseprice)

#4. Outlier handling
SalesPrice_edit <- data_houseprice$SalesPrice_edit
boxplot(SalesPrice_edit)

# IQR Method  #
# get upper bound and lower bound
q1 <- quantile(SalesPrice_edit,0.25)
q3 <- quantile(SalesPrice_edit,0.75)
iqr <- q3 - q1
upper_bound <- q3 + 1.5 * iqr
lower_bound <- q1 - 1.5 * iqr
# filter 
#data_houseprice_iqr <- data_houseprice[SalesPrice_edit > lower_bound & SalesPrice_edit < upper_bound,]
filter_non_outlier <- (SalesPrice_edit > lower_bound & SalesPrice_edit < upper_bound)
data_houseprice[filter_non_outlier,]
data_houseprice_iqr <- data_houseprice[filter_non_outlier,]
boxplot(data_houseprice_iqr$SalesPrice_edit)
SalesPrice_edited <- data_houseprice_iqr$SalesPrice_edit
View(data_houseprice_iqr)

#5. Categorical Data Encoding

# One-hot Encoding #
table(data$GarageType)

dummy <- dummyVars("~.",data=data_houseprice_iqr['GarageType'])
dummy_var <- data.frame(predict(dummy, newdata = data['GarageType']))
View(dummy_var)

#6. Menggabungkan semua feature
Newdata <- cbind(LotFrontageedit,LotAreaedit,SalesPrice_edit,dummy_var)
View(Newdata)


