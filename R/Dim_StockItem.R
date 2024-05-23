#import dữ liệu
Dim_StockItem <- read_excel("C:/Users/ASUS/OneDrive/Documents/WWI.xlsx", sheet = "Dimension-StockItem")

#Cài đặt thư viện hỗ trợ
install.packages("psych") 
library(psych)
install.packages("ggplot")
library(ggplot2)
library(readxl)

#Typical Weight Per Unit
#Chuyển đổi dữ liệu
Dim_StockItem$`Typical Weight Per Unit` <- gsub(",", ".", Dim_StockItem$`Typical Weight Per Unit`)
head(Dim_StockItem$`Typical Weight Per Unit`)
Dim_StockItem$`Typical Weight Per Unit`<- as.numeric(Dim_StockItem$`Typical Weight Per Unit`)
###Thống kê mô tả 
result_describe <- describe(Dim_StockItem$`Typical Weight Per Unit`)
result_describe_formatted <- format(result_describe, scientific = FALSE)
Typical_Weight_Per_Unit <- t(result_describe_formatted)
colnames(Typical_Weight_Per_Unit) <- c("Typical_Weight_Per_Unit")
print(Typical_Weight_Per_Unit)
t(quantile(Dim_StockItem$`Typical Weight Per Unit`, probs = c(0, 0.25, 0.5, 0.75, 1)))
###Vẽ biểu đồ 
hist(Dim_StockItem$`Typical Weight Per Unit`, col = "skyblue", main = "Biểu đồ phân phối của Typical Weight Per Unit", xlab = "Typical Weight Per Unit")
boxplot(Dim_StockItem$`Typical Weight Per Unit`, 
        main = "Biểu đồ hộp cho Typical Weight Per Unit", 
        ylab = "Typical Weight Per Unit", 
        ylim = c(0,7),
        boxfill = "skyblue")

#Recommended Retail Price
###Chuyển đổi dữ liệu 
Dim_StockItem$`Recommended Retail Price` <- gsub("\\$", "", Dim_StockItem$`Recommended Retail Price`)
Dim_StockItem$`Recommended Retail Price` <- gsub(",", ".", Dim_StockItem$`Recommended Retail Price`)
Dim_StockItem$`Recommended Retail Price` <- gsub("\\.(?=.*\\.)", "", Dim_StockItem$`Recommended Retail Price`, perl = TRUE)
head(Dim_StockItem$`Recommended Retail Price`)
Dim_StockItem$`Recommended Retail Price` <- as.numeric(Dim_StockItem$`Recommended Retail Price`)
###Thống kê mô tả
result_describe <- describe(Dim_StockItem$`Recommended Retail Price`)
result_describe_formatted <- format(result_describe, scientific = FALSE)
Recommended_Retail_Price <- t(result_describe_formatted)
colnames(Recommended_Retail_Price) <- c("Recommended_Retail_Price")
print(Recommended_Retail_Price)
t(quantile(Dim_StockItem$`Recommended Retail Price`, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))
###Vẽ biểu đồ 
hist(Dim_StockItem$`Recommended Retail Price`, col = "skyblue", main = "Biểu đồ phân phối của Recommended Retail Price", xlab = "Recommended Retail Price")
boxplot(Dim_StockItem$`Recommended Retail Price`, 
        main = "Biểu đồ hộp của Recommended Retail Price", 
        ylab = "Recommended Retail Price",
        ylim = c(0,700),
        boxfill = "skyblue")

#Unit Price 
###Chuyển đổi dữ liệu 
Dim_StockItem$`Unit Price` <- gsub("\\$", "", Dim_StockItem$`Unit Price`)
Dim_StockItem$`Unit Price` <- gsub(",", ".", Dim_StockItem$`Unit Price`)
Dim_StockItem$`Unit Price` <- gsub("\\.(?=.*\\.)", "", Dim_StockItem$`Unit Price`, perl = TRUE)
head(Dim_StockItem$`Unit Price`)
Dim_StockItem$`Unit Price` <- as.numeric(Dim_StockItem$`Unit Price`)
###Thống kê mô tả
result_describe <- describe(Dim_StockItem$`Unit Price`)
result_describe_formatted <- format(result_describe, scientific = FALSE)
Unit_Price <- t(result_describe_formatted)
colnames(Unit_Price) <- c("Unit_Price")
print(Unit_Price)
t(quantile(Dim_StockItem$`Unit Price`, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))
###Vẽ biểu đồ 
hist(Dim_StockItem$`Unit Price`, col = "skyblue", main = "Biểu đồ phân phối của Unit Price", xlab = "Unit Price")
boxplot(Dim_StockItem$`Unit Price`, 
        main = "Biểu đồ hộp của Unit Price", 
        ylab = "Unit Price",
        ylim = c(0,350),
        boxfill = "skyblue")

#Tax_Rate
table <- table(Dim_StockItem$`Tax Rate`)
Tax_Rate <- as.data.frame(table)
colnames(Tax_Rate) <- c("Tax Rate", "Số lượng")
print(Tax_Rate)
###Vẽ biểu đồ 
pie(Tax_Rate$`Số lượng`, 
    labels = paste(Tax_Rate$`Tax Rate`, "%"), 
    main = "Biểu đồ tròn Tax Rate", 
    col = rainbow(length(Tax_Rate$`Số lượng`)))
legend("topright", 
       legend = paste(Tax_Rate$`Tax Rate`, "% - ", Tax_Rate$`Số lượng`), 
       fill = rainbow(length(Tax_Rate$`Số lượng`)), 
       cex = 0.8, 
       title = "Tax Rate")


#Quantity Per Outer
###Thống kê mô tả
result_describe <- describe(Dim_StockItem$`Quantity Per Outer`)
result_describe_formatted <- format(result_describe, scientific = FALSE)
Quantity_Per_Outer<- t(result_describe_formatted)
colnames(Quantity_Per_Outer) <- c("Quantity_Per_Outer")
print(Quantity_Per_Outer)
t(quantile(Dim_StockItem$`Quantity Per Outer`, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))
###Vẽ biểu đồ 
hist(Dim_StockItem$`Quantity Per Outer`, col = "skyblue", main = "Biểu đồ phân phối cho Quantity Per Outer", xlab = "Quantity Per Outer")
boxplot(Dim_StockItem$`Quantity Per Outer`, 
        main = "Biểu đồ hộp cho Quantity Per Outer", 
        ylab = "Quantity Per Outer",
        ylim = c(0,36),
        boxfill = "skyblue")

#Lead Time Days
table <- table(Dim_StockItem$`Lead Time Days`)
Lead_Time_Days <- as.data.frame(table)
colnames(Lead_Time_Days) <- c("Lead Time Days", "Số lượng")
print(Lead_Time_Days)
###Vẽ biểu đồ 
ggplot(data = Lead_Time_Days, aes(x = `Lead Time Days`, y = `Số lượng`)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Biểu đồ cột Lead Time Days", x = "Lead Time Days", y = "Số lượng") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))


#Brand
table <- table(Dim_StockItem$Brand)
Brand <- as.data.frame(table)
colnames(Brand) <- c("Brand", "Số lượng")
print(Brand)
###Vẽ biểu đồ 
ggplot(data = Brand, aes(x = "", y = `Số lượng`, fill = `Brand`)) + 
  geom_bar(stat = "identity", color = "white") + 
  coord_polar("y", start = 0) + 
  ggtitle("Biểu đồ tròn Brand") + 
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
  scale_fill_brewer(palette = "Blues", name = "Brand")


#Buying Package
table <- table(Dim_StockItem$`Buying Package`)
Buying_Package <- as.data.frame(table)
colnames(Buying_Package) <- c("Buying Package", "Số lượng")
print(Buying_Package)
### Vẽ biểu đồ 
ggplot(data = Buying_Package, aes(x = "", y = `Số lượng`, fill = `Buying Package`)) + 
  geom_bar(stat = "identity", color = "white") + 
  coord_polar("y", start = 0) + 
  ggtitle("Biểu đồ tròn Buying Package") + 
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
  scale_fill_brewer(palette = "Blues", name = "Buying Package")


#Selling Package
table <- table(Dim_StockItem$`Selling Package`)
Selling_Package <- as.data.frame(table)
colnames(Selling_Package) <- c("Selling Package", "Số lượng")
print(Selling_Package)
### Vẽ biểu đồ 
ggplot(data = Selling_Package, aes(x = "", y = `Số lượng`, fill = `Selling Package`)) + 
  geom_bar(stat = "identity", color = "white") + 
  coord_polar("y", start = 0) + 
  ggtitle("Biểu đồ tròn Selling Package") + 
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
  scale_fill_brewer(palette = "Blues", name = "Selling Package")


#Color
table <- table(Dim_StockItem$Color)
Color <- as.data.frame(table)
colnames(Color) <- c("Color", "Số lượng")
print(Color)
### Vẽ biểu đồ 
ggplot(data = Color, aes(x = `Color`, y = `Số lượng`)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Biểu đồ cột Color", x = "Color", y = "Số lượng") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



