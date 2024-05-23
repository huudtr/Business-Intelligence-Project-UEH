#import dữ liệu 
Fact_StockHolding <- read_excel("C:/Users/ASUS/OneDrive/Documents/WWI.xlsx", sheet = "Fact-StockHolding")

#Cài đặt thư viện hỗ trợ 
install.packages("psych") 
library(psych)
install.packages("ggplot")
library(ggplot2)
library(readxl)

#Fact_StockHolding 
#Target Stock Level
###Thống kê mô tả 
result_describe <- describe(Fact_StockHolding$`Target Stock Level`)
result_describe_formatted <- format(result_describe, scientific = FALSE)
Target_Stock_Level <- t(result_describe_formatted)
colnames(Target_Stock_Level) <- c("Target_Stock_Level")
print(Target_Stock_Level)
t(quantile(Fact_StockHolding$`Target Stock Level`, probs = c(0, 0.25, 0.5, 0.75, 1)))
###Vẽ biểu đồ 
hist(Fact_StockHolding$`Target Stock Level`, col = "skyblue", main = "Biểu đồ phân phối của Target Stock Level", xlab = "Target Stock Level")
boxplot(Fact_StockHolding$`Target Stock Level`, 
        main = "Biểu đồ hộp của Target Stock Level", 
        ylab = "Target Stock Level", 
        ylim = c(1,500),
        boxfill = "skyblue")

#Reorder Level
###Thống kê mô tả 
result_describe <- describe(Fact_StockHolding$`Reorder Level`)
result_describe_formatted <- format(result_describe, scientific = FALSE)
Reorder_Level <- t(result_describe_formatted)
colnames(Reorder_Level) <- c("Reorder_Level")
print(Reorder_Level)
t(quantile(Fact_StockHolding$`Reorder Level`, probs = c(0, 0.25, 0.5, 0.75, 1)))
###Vẽ biểu đồ 
hist(Fact_StockHolding$`Reorder Level`, col = "skyblue", main = "Biểu đồ phân phối của Reorder Level", xlab = "Target Stock Level")
boxplot(Fact_StockHolding$`Reorder Level`, 
        main = "Biểu đồ hộp của Reorder Level", 
        ylab = "Reorder Level", 
        ylim = c(1,250),
        boxfill = "skyblue")

#Last Cost Price 
unique(Fact_StockHolding$`Last Cost Price`)
Fact_StockHolding$`Last Cost Price` <- gsub("\\$", "", Fact_StockHolding$`Last Cost Price`)
head(Fact_StockHolding$`Last Cost Price`)
Fact_StockHolding$`Last Cost Price`<- as.numeric(Fact_StockHolding$`Last Cost Price`)
###Thống kê mô tả 
result_describe <- describe(Fact_StockHolding$`Last Cost Price`)
result_describe_formatted <- format(result_describe, scientific = FALSE)
Last_Cost_Price <- t(result_describe_formatted)
colnames(Last_Cost_Price) <- c("Last_Cost_Price")
print(Last_Cost_Price)
t(quantile(Fact_StockHolding$`Last Cost Price`, probs = c(0, 0.25, 0.5, 0.75, 1)))
###Vẽ biểu đồ 
hist(Fact_StockHolding$`Last Cost Price`, col = "skyblue", main = "Biểu đồ phân phối của Last Cost Price", xlab = "Last Cost Price")
boxplot(Fact_StockHolding$`Last Cost Price`, 
        main = "Biểu đồ hộp của Last Cost Price", 
        ylab = "Last Cost Price", 
        ylim = c(0,210),
        boxfill = "skyblue")

#Last Stocktake Quantity
###Thống kê mô tả 
result_describe <- describe(Fact_StockHolding$`Last Stocktake Quantity`)
result_describe_formatted <- format(result_describe, scientific = FALSE)
Last_Stocktake_Quantity <- t(result_describe_formatted)
colnames(Last_Stocktake_Quantity) <- c("Last_Stocktake_Quantity")
print(Last_Stocktake_Quantity)
t(quantile(Fact_StockHolding$`Last Stocktake Quantity`, probs = c(0, 0.25, 0.5, 0.75, 1)))
###Vẽ biểu đồ 
hist(Fact_StockHolding$`Last Stocktake Quantity`, col = "skyblue", main = "Biểu đồ phân phối của Last Stocktake Quantity", xlab = "Last Stocktake Quantity")
boxplot(Fact_StockHolding$`Last Stocktake Quantity`, 
        main = "Biểu đồ hộp của Last Stocktake Quantity", 
        ylab = "Last Stocktake Quantity", 
        ylim = c(0,1010000),
        boxfill = "skyblue")

#Quantity On Hand 
###Thống kê mô tả 
result_describe <- describe(Fact_StockHolding$`Quantity On Hand`)
result_describe_formatted <- format(result_describe, scientific = FALSE)
Quantity_On_Hand <- t(result_describe_formatted)
colnames(Quantity_On_Hand) <- c("Quantity_On_Hand")
print(Quantity_On_Hand)
t(quantile(Fact_StockHolding$`Quantity On Hand`, probs = c(0, 0.25, 0.5, 0.75, 1)))
###Vẽ biểu đồ 
hist(Fact_StockHolding$`Quantity On Hand`, col = "skyblue", main = "Biểu đồ phân phối của Quantity On Hand", xlab = "Last Stocktake Quantity")
boxplot(Fact_StockHolding$`Quantity On Hand`, 
        main = "Biểu đồ hộp của Quantity On Hand", 
        ylab = "Quantity On Hand", 
        ylim = c(0,1010000),
        boxfill = "skyblue")

