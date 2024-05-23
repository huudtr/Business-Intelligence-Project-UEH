#import dữ liệu
Fact_Sales <- read_excel("C:/Users/ASUS/OneDrive/Documents/WWI.xlsx", sheet = "Fact-Sales")

#Cài đặt thư viện 
install.packages("psych") 
library(psych)
install.packages("ggplot")
library(ggplot2)
library(readxl)

#Unit_Price
###Chuyển đổi dữ liệu 
unique(Fact_Sales$`Total Excluding Tax`)
Fact_Sales$`Unit Price` <- gsub(",", ".", Fact_Sales$`Unit Price`)
Fact_Sales$`Unit Price` <- gsub("\\$", "", Fact_Sales$`Unit Price`)
Fact_Sales$`Unit Price` <- gsub("\\.(?=.*\\.)", "", Fact_Sales$`Unit Price`, perl = TRUE)
head(Fact_Sales$`Unit Price`)
Fact_Sales$`Unit Price`<- as.numeric(Fact_Sales$`Unit Price`)
###Thống kê mô tả 
result_describe <- describe(Fact_Sales$`Unit Price`)
result_describe_formatted <- format(result_describe, scientific = FALSE)
Unit_Price <- t(result_describe_formatted)
colnames(Unit_Price) <- c("Unit_Price")
print(Unit_Price)
t(quantile(Fact_Sales$`Unit Price`, probs = c(0, 0.25, 0.5, 0.75, 1)))
###Vẽ biểu đồ 
# Vẽ biểu đồ histogram với màu sắc được chỉ định
hist(Fact_Sales$`Unit Price`, col = "skyblue", main = "Biểu đồ phân phối của Unit Price", xlab = "Unit Price")
boxplot(Fact_Sales$`Unit Price`, 
        main = "Biểu đồ hộp của Unit Price", 
        ylab = "Unit_Price", 
        ylim = c(1,150),
        boxfill = "skyblue"
        )

#Total_Excluding_Tax
###Chuyển đổi dữ liệu 
Fact_Sales$`Total Excluding Tax` <- gsub(",", ".", Fact_Sales$`Total Excluding Tax`)
Fact_Sales$`Total Excluding Tax` <- gsub("\\$", "", Fact_Sales$`Total Excluding Tax`)
Fact_Sales$`Total Excluding Tax` <- gsub("\\.(?=.*\\.)", "", Fact_Sales$`Total Excluding Tax`, perl = TRUE)
head(Fact_Sales$`Total Excluding Tax`)
Fact_Sales$`Total Excluding Tax`<- as.numeric(Fact_Sales$`Total Excluding Tax`)
###Thống kê mô tả 
result_describe <- describe(Fact_Sales$`Total Excluding Tax`)
result_describe_formatted <- format(result_describe, scientific = FALSE)
Total_Excluding_Tax <- t(result_describe_formatted)
colnames(Total_Excluding_Tax) <- c("Total_Excluding_Tax")
print(Total_Excluding_Tax)
t(quantile(Fact_Sales$`Total Excluding Tax`, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))
###Vẽ biểu đồ 
hist(Fact_Sales$`Total Excluding Tax`, col = "skyblue", main = "Biểu đồ phân phối của Total Excluding Tax", xlab = "Total Excluding Tax")
boxplot(Fact_Sales$`Total Excluding Tax`, 
        main = "Biểu đồ hộp của Total Excluding Tax", 
        ylab = "Total Excluding Tax", 
        ylim = c(1,990),
        boxfill = "skyblue")


#Total_Dry_Items
###Thống kê mô tả 
result_describe <- describe(Fact_Sales$`Total Dry Items`)
result_describe_formatted <- format(result_describe, scientific = FALSE)
Total_Dry_Items <- t(result_describe_formatted)
colnames(Total_Dry_Items) <- c("Total_Dry_Items")
t(quantile(Fact_Sales$`Total Dry Items`, probs = c(0, 0.25, 0.5, 0.75, 1)))
###Vẽ biểu đồ 
hist(Fact_Sales$`Total Dry Items`, col = "skyblue", main = "Biểu đồ phân phối của Total Dry Items", xlab = "Total Dry Items")
boxplot(Fact_Sales$`Total Dry Items`, 
        main = "Biểu đồ hộp của Total Dry Items", 
        ylab = "Total Dry Items", 
        ylim = c(1,400),
        boxfill = "skyblue")


#Profit
###Chuyển đổi dữ liệu 
Fact_Sales$Profit <- gsub(",", ".", Fact_Sales$Profit)
Fact_Sales$Profit <- gsub("\\$", "", Fact_Sales$Profit)
Fact_Sales$Profit <- gsub("\\.(?=.*\\.)", "", Fact_Sales$Profit, perl = TRUE)
Fact_Sales$Profit <- gsub("\\(|\\)", "", Fact_Sales$Profit)
head(Fact_Sales$Profit)
Fact_Sales$Profit<- as.numeric(Fact_Sales$Profit)
###Thống kê mô tả 
result_describe <- describe(Fact_Sales$Profit)
result_describe_formatted <- format(result_describe, scientific = FALSE)
Profit <- t(result_describe_formatted)
colnames(Profit) <- c("Profit")
print(Profit)
t(quantile(Fact_Sales$Profit, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))
###Vẽ biểu đồ 
hist(Fact_Sales$Profit, col = "skyblue", main = "Biểu đồ phân phối của Profit", xlab = "Profit")
boxplot(Fact_Sales$Profit, 
        main = "Biểu đồ hộp của Profit", 
        ylab = "Profit", 
        ylim = c(1,1400),
        boxfill = "skyblue")

#Total_Including_Tax 
###Chuyển đổi dữ liệu 
Fact_Sales$`Total Including Tax` <- gsub(",", ".", Fact_Sales$`Total Including Tax`)
Fact_Sales$`Total Including Tax` <- gsub("\\$", "", Fact_Sales$`Total Including Tax`)
Fact_Sales$`Total Including Tax` <- gsub("\\.(?=.*\\.)", "", Fact_Sales$`Total Including Tax`, perl = TRUE)
head(Fact_Sales$`Total Including Tax`)
Fact_Sales$`Total Including Tax`<- as.numeric(Fact_Sales$`Total Including Tax`)
###Thống kê mô tả 
result_describe <- describe(Fact_Sales$`Total Including Tax`)
result_describe_formatted <- format(result_describe, scientific = FALSE)
Total_Including_Tax <- t(result_describe_formatted)
colnames(Total_Including_Tax) <- c("Total_Including_Tax")
print(Total_Including_Tax)
t(quantile(Fact_Sales$`Total Including Tax`, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))
###Vẽ biểu đồ 
hist(Fact_Sales$`Total Including Tax`, col = "skyblue", main = "Biểu đồ phân phối của Total Including Tax", xlab = "Total Including Tax")
boxplot(Fact_Sales$`Total Including Tax`, 
        main = "Biểu đồ hộp của Total Including Tax", 
        ylab = "Total Including Tax", 
        ylim = c(1,2000),
        boxfill = "skyblue")

#Tax_Amount
###Chuyển đổi dữ liệu 
Fact_Sales$`Tax Amount` <- gsub(",", ".", Fact_Sales$`Tax Amount`)
Fact_Sales$`Tax Amount` <- gsub("\\$", "", Fact_Sales$`Tax Amount`)
Fact_Sales$`Tax Amount` <- gsub("\\.(?=.*\\.)", "", Fact_Sales$`Tax Amount`, perl = TRUE)
head(Fact_Sales$`Tax Amount`)
Fact_Sales$`Tax Amount`<- as.numeric(Fact_Sales$`Tax Amount`)
###Thống kê mô tả
result_describe <- describe(Fact_Sales$`Tax Amount`)
result_describe_formatted <- format(result_describe, scientific = FALSE)
Tax_Amount <- t(result_describe_formatted)
colnames(Tax_Amount) <- c("Tax_Amount")
print(Tax_Amount)
t(quantile(Fact_Sales$`Tax Amount`, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))
###Vẽ biểu đồ 
hist(Fact_Sales$`Tax Amount`, col = "skyblue", main = "Biểu đồ phân phối của Tax Amount", xlab = "Tax Amount")
boxplot(Fact_Sales$`Tax Amount`, 
        main = "Biểu đồ hộp của Tax Amount", 
        ylab = "Tax Amount",
        ylim = c(0,1500),
        boxfill = "skyblue")

#Quantity
###Thống kê mô tả
result_describe <- describe(Fact_Sales$Quantity)
result_describe_formatted <- format(result_describe, scientific = FALSE)
Quantity <- t(result_describe_formatted)
colnames(Quantity) <- c("Quantity")
print(Quantity)
t(quantile(Fact_Sales$`Quantity`, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))
###Vẽ biểu đồ 
hist(Fact_Sales$`Quantity`, col = "skyblue", main = "Biểu đồ phân phối của Quantity", xlab = "Quantity")
boxplot(Fact_Sales$`Tax Amount`, 
        main = "Biểu đồ hộp của Quantity", 
        ylab = "Quantity",
        ylim = c(0,400),
        boxfill = "skyblue")

#Tax_Rate
table <- table(Fact_Sales$`Tax Rate`)
Tax_Rate <- as.data.frame(table)
colnames(Tax_Rate) <- c("Tax Rate", "Số lượng")
print(Tax_Rate)
###Vẽ biểu đồ 
ggplot(data = Tax_Rate, aes(x = "", y = `Số lượng`, fill = `Tax Rate`)) + 
  geom_bar(stat = "identity", color = "white") + 
  coord_polar("y", start = 0) + 
  ggtitle("Biểu đồ tròn Tax Rate") + 
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank()
  ) +
  geom_text(aes(label = paste0(round(`Số lượng` / sum(`Số lượng`) * 100), "%")), 
            position = position_stack(vjust = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE)) + 
  scale_fill_brewer(palette = "Blues", name = "Tax Rate")




