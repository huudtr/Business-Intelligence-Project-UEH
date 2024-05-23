#import dữ liệu
Dim_Employee <- read_excel("C:/Users/ASUS/OneDrive/Documents/WWI.xlsx", 
                         sheet = "Dimension-Employee")
Dim_Customer <- read_excel("C:/Users/ASUS/OneDrive/Documents/WWI.xlsx", 
                         sheet = "Dimension-Customer")
Dim_City <- read_excel("C:/Users/ASUS/OneDrive/Documents/WWI.xlsx", 
                         sheet = "Dimension-City")
#Cài đặt thư viện hỗ trợ
install.packages("psych") 
library(psych)
install.packages("ggplot")
library(ggplot2)
library(readxl)

#Dim_Employee
#Is salesperson
table <- table(Dim_Employee$`Is Salesperson`)
Is_Salesperson <- as.data.frame(table)
colnames(Is_Salesperson) <- c("Is Salesperson", "Số lượng")
print(Is_Salesperson)
### Vẽ biểu đồ 
ggplot(data = Is_Salesperson, aes(x = "", y = `Số lượng`, fill = `Is Salesperson`)) + 
  geom_bar(stat = "identity", color = "white") + 
  coord_polar("y", start = 0) + 
  ggtitle("Biểu đồ tròn Is_Salesperson") + 
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
  scale_fill_brewer(palette = "Blues", name = "Is Salesperson")


#Dim_Customer
#Is HO
table <- table(Dim_Customer$`is HO`)
Is_HO <- as.data.frame(table)
colnames(Is_HO) <- c("Is HO", "Số lượng")
print(Is_HO)
### Vẽ biểu đồ 
ggplot(data = Is_HO, aes(x = "", y = `Số lượng`, fill = `Is HO`)) + 
  geom_bar(stat = "identity", color = "white") + 
  coord_polar("y", start = 0) + 
  ggtitle("Biểu đồ tròn Is_HO") + 
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
  scale_fill_brewer(palette = "Blues", name = "Is HO")

#Buying Group
table <- table(Dim_Customer$`Buying Group`)
Buying_Group <- as.data.frame(table)
colnames(Buying_Group) <- c("Buying Group", "Số lượng")
print(Buying_Group)
### Vẽ biểu đồ 
ggplot(data = Buying_Group, aes(x = "", y = `Số lượng`, fill = `Buying Group`)) + 
  geom_bar(stat = "identity", color = "white") + 
  coord_polar("y", start = 0) + 
  ggtitle("Biểu đồ tròn Buying Group") + 
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
  scale_fill_brewer(palette = "Blues", name = "Buying Group")

#Dim_City
#Sales Territory
table <- table(Dim_City$`Sales Territory`)
Sales_Territory <- as.data.frame(table)
colnames(Sales_Territory) <- c("Sales Territory", "Số lượng")
print(Sales_Territory)
###Vẽ biểu đồ
ggplot(data = Sales_Territory, aes(x = `Sales Territory`, y = `Số lượng`)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Biểu đồ cột Sales Territory", x = "Sales Territory", y = "Số lượng") +
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))