# 16/05/2018

#Analisi dati food

#Import e calcolo medie prodotti Mc Donald's
md_menu_2 <- read.csv("/Users/Tornetto/R/Data_viz/menù/Mc_menu 2.csv", header = TRUE, sep = ',')
bp_md_menu <- md_menu_2[4:18,]
cf_md_menu <- md_menu_2[md_menu_2$Category== "Chicken & Fish",]
desserts_md_menu <- md_menu_2[md_menu_2$Category== "Desserts",]
salads_md_menu <- md_menu_2[md_menu_2$Category== "Salads",]
ss_md_menu <- md_menu_2[md_menu_2$Category== "Snacks & Sides",]
beverages_md_menu <- md_menu_2[md_menu_2$Category== "Beverages",]

bp_means<-c(apply(bp_md_menu[,4:18], 2, mean))
cf_means<-c(apply(cf_md_menu[,4:18], 2, mean))
desserts_means<-c(apply(desserts_md_menu[,4:18], 2, mean))
salads_means<-c(apply(salads_md_menu[,4:18], 2, mean))
ss_means<-c(apply(ss_md_menu[,4:18], 2, mean))
beverages_means<-c(apply(beverages_md_menu[,4:18], 2, mean))

categories <- c("Beef & Pork", "Chicken & Fish", "Desserts", "Salads", "Snacks & Sides", "Beverages")

#Creo un nuovo dataset con i valori di queste medie
md_means <- data.frame(Food_Category=character(), Calories=integer(), Calories_from_Fat=integer(),
                       Total_Fat.g=integer(), Saturated_Fat.g=integer(), Trans_Fat.g=integer(),
                       Cholesterol.mg=integer(), Sodium.mg=integer(), Total_Carbs.g=integer(),
                       Fiber.g=integer(), Sugars.g=integer(), Protein.g=integer(),
                       Vitamin_A.gda=integer(), Vitamin_C.gda=integer(), Calcium.gda=integer(),
                       Iron.gda=integer(),stringsAsFactors=FALSE)

md_means[1,1] <- categories[1]
md_means[2,1] <- categories[2]
md_means[3,1] <- categories[3]
md_means[4,1] <- categories[4]
md_means[5,1] <- categories[5]
md_means[6,1] <- categories[6]
md_means[1,2:16] <- bp_means
md_means[2,2:16] <- cf_means
md_means[3,2:16] <- desserts_means
md_means[4,2:16] <- salads_means
md_means[5,2:16] <- ss_means
md_means[6,2:16] <- beverages_means

num_disp <- c(3, 5, 7, 9, 11, 13, 15)

for (i in num_disp) {
  md_means[,i] <- as.double(md_means[,i])
}

md_means[,2:16] <- round(md_means[,2:16])

ff_means <- md_means
ff_means$Fast_Food <- "McDonalds"

write.csv(ff_means, file = "Fast_Food_Means.csv")

#Import e calcolo medie prodotti Burger King

bk_menu <- read.csv("/Users/Tornetto/R/Data_viz/menù/BK_menu.csv", header = TRUE, sep = ',')

bp_bk_menu <- bk_menu[1:26,]
cf_bk_menu <- bk_menu[c(27:34, 36, 40, 42:45, 67:76),]
desserts_bk_menu <- bk_menu[c(94, 95, 107, 117:129, 133, 136, 139, 142, 145, 164, 167),]
salads_bk_menu <- bk_menu[59:65,]
ss_bk_menu <- bk_menu[c(50,54,58),]

bp_means<-c(apply(bp_bk_menu[,3:14], 2, mean))
cf_means<-c(apply(cf_bk_menu[,3:14], 2, mean))
desserts_means<-c(apply(desserts_bk_menu[,3:14], 2, mean))
salads_means<-c(apply(salads_bk_menu[,3:14], 2, mean))
ss_means<-c(apply(ss_bk_menu[,3:14], 2, mean))

categories <- c("Beef & Pork", "Chicken & Fish", "Desserts", "Salads", "Snacks & Sides")

bk_means <- data.frame(Food_Category=character(), Calories=integer(), Proteins.g=integer(),
                       Total_Fat.g=integer(), Total_Carbs.g=integer(), Sodium.mg=integer(),
                       Calories_From_Fat=integer(), Saturated_Fat.g=integer(), Trans_Fat.g=integer(),
                       Cholesterol.mg=integer(), Fiber.g=integer(), Sugars.g=integer(),
                       Weight_Watchers=integer(), stringsAsFactors=FALSE)

bk_means[1,1] <- categories[1]
bk_means[2,1] <- categories[2]
bk_means[3,1] <- categories[3]
bk_means[4,1] <- categories[4]
bk_means[5,1] <- categories[5]
bk_means[1,2:13] <- bp_means
bk_means[2,2:13] <- cf_means
bk_means[3,2:13] <- desserts_means
bk_means[4,2:13] <- salads_means
bk_means[5,2:13] <- ss_means

str(bk_means)

bk_means[,2:13] <- round(bk_means[,2:13])

bk_means$Fast_Food <- "Burger King"

write.csv(bk_means, file = "Fast_Food_Burger_King_Means.csv")

#Import e calcolo medie prodotti Taco Bell

tb_menu <- read.csv("/Users/Tornetto/R/Data_viz/menù/tb_menu_off.csv", header = TRUE, sep = ',')
tb_menu <- tb_menu[,-c(1)]

bp_tb_menu <- tb_menu[tb_menu$Category == "Beef & Pork",]
cf_tb_menu <- tb_menu[tb_menu$Category == "Chicken & Fish",]
desserts_tb_menu <- tb_menu[tb_menu$Category == "Desserts",]
salads_tb_menu <- tb_menu[tb_menu$Category == "Salads",]
ss_tb_menu <- tb_menu[tb_menu$Category == "Snacks & Sides",]
beverages_tb_menu <- tb_menu[tb_menu$Category == "Beverages",]

str(bp_tb_menu)

bp_means<-c(apply(bp_tb_menu[,2:14], 2, mean))
cf_means<-c(apply(cf_tb_menu[,2:14], 2, mean))
desserts_means<-c(apply(desserts_tb_menu[,2:14], 2, mean))
salads_means<-c(apply(salads_tb_menu[,2:14], 2, mean))
ss_means<-c(apply(ss_tb_menu[,2:14], 2, mean))
beverages_means<-c(apply(beverages_tb_menu[,2:14], 2, mean))

str(bp_means)

categories <- c("Beef & Pork", "Chicken & Fish", "Desserts", "Salads", "Snacks & Sides", "Beverages")

tb_means <- data.frame(Food_Category=character(), Serving_Size.g=integer(), Calories=integer(),
                       Protein.g=integer(), Total_Fat.g=integer(), Total_Carbs.g=integer(),
                       Sodium.mg=integer(), Calories_from_Fat=integer(), Saturated_Fat.g=integer(),
                       Trans_Fat.g=integer(), Cholesterol.mg=integer(), Fiber.g=integer(),
                       Sugars.g=integer(), Weight_Watchers=integer(), stringsAsFactors=FALSE)

tb_means[1,1] <- categories[1]
tb_means[2,1] <- categories[2]
tb_means[3,1] <- categories[3]
tb_means[4,1] <- categories[4]
tb_means[5,1] <- categories[5]
tb_means[1,2:14] <- bp_means
tb_means[2,2:14] <- cf_means
tb_means[3,2:14] <- desserts_means
tb_means[4,2:14] <- salads_means
tb_means[5,2:14] <- ss_means

tb_means <- tb_means[-c(6),]

tb_means[,2:14] <- round(tb_means[,2:14])

tb_means$Fast_Food <- "Taco Bell"

write.csv(tb_means, file = "Fast_Food_Taco_Bell_Means.csv")

#Import e calcolo medie prodotti Wendy's

we_menu <- read.csv("/Users/Tornetto/R/Data_viz/menù/wendys_menu.csv", header = TRUE, sep = ',')
we_menu <- we_menu[,-c(1,2)]
we_menu <- we_menu[,-c(2)]

bp_we_menu <- we_menu[we_menu$Category == "Beef & Pork",]
cf_we_menu <- we_menu[we_menu$Category == "Chicken & Fish",]
desserts_we_menu <- we_menu[we_menu$Category == "Desserts",]
salads_we_menu <- we_menu[we_menu$Category == "Salads",]
ss_we_menu <- we_menu[we_menu$Category == "Snacks & Sides",]

str(bp_we_menu)

bp_means<-c(apply(bp_we_menu[,2:12], 2, mean))
cf_means<-c(apply(cf_we_menu[,2:12], 2, mean))
desserts_means<-c(apply(desserts_we_menu[,2:12], 2, mean))
salads_means<-c(apply(salads_we_menu[,2:12], 2, mean))
ss_means<-c(apply(ss_we_menu[,2:12], 2, mean))
beverages_means<-c(apply(beverages_we_menu[,2:12], 2, mean))

str(bp_means)

categories <- c("Beef & Pork", "Chicken & Fish", "Desserts", "Salads", "Snacks & Sides")

we_means <- data.frame(Food_Category=character(), Calories=integer(),
                       Protein.g=integer(), Total_Fat.g=integer(), Total_Carbs.g=integer(),
                       Sodium.mg=integer(), Saturated_Fat.g=integer(), Trans_Fat.g=integer(),
                       Cholesterol.mg=integer(), Fiber.g=integer(),
                       Sugars.g=integer(), Weight_Watchers=integer(), stringsAsFactors=FALSE)

we_means[1,1] <- categories[1]
we_means[2,1] <- categories[2]
we_means[3,1] <- categories[3]
we_means[4,1] <- categories[4]
we_means[5,1] <- categories[5]
we_means[1,2:12] <- bp_means
we_means[2,2:12] <- cf_means
we_means[3,2:12] <- desserts_means
we_means[4,2:12] <- salads_means
we_means[5,2:12] <- ss_means

we_means[,2:12] <- round(we_means[,2:12])

we_means$Fast_Food <- "Wendy's"

write.csv(we_means, file = "Fast_Food_Wendys_Means.csv")

#Unione delle medie dei prodotti dei Fast Food

#Import dei data set delle medie
md_means <- read.csv("/Users/Tornetto/R/Data_viz/Media_Prodotti/Fast_Food_McDonalds_Means.csv", header = TRUE, sep = ',')
bk_means <- read.csv("/Users/Tornetto/R/Data_viz/Media_Prodotti/Fast_Food_Burger_King_Means.csv", header = TRUE, sep = ',')
tb_means <- read.csv("/Users/Tornetto/R/Data_viz/Media_Prodotti/Fast_Food_Taco_Bell_Means.csv", header = TRUE, sep = ',')
we_means <- read.csv("/Users/Tornetto/R/Data_viz/Media_Prodotti/Fast_Food_Wendys_Means.csv", header = TRUE, sep = ',')

#Creo il nuovo dataset con le colonne e gli spazi giusti
ff_means <- data.frame(Fast_Food=character(), Food_Category=character(),
                       Calories=integer(), Total_Fat.g=integer(), Satured_Fat.g=integer(),
                       Trans_Fat.g=integer(), Total_Carbs.g=integer(), Protein.g=integer(),
                       Fiber.g=integer(), Sugars.g=integer(), Cholesterol.mg=integer(), 
                       Sodium.mg=integer(), stringsAsFactors=FALSE)

Fast_Food_Name <- md_means$Fast_Food[1]

ff_means[20,1] <- "Wendy's"  
ff_means[20,2] <- "Snacks & Sides"
ff_means[5,3] <- md_means$Calories[5]
ff_means[5,4] <- md_means$Total_Fat.g[5]
ff_means[5,5] <- md_means$Saturated_Fat.g[5]
ff_means[5,6] <- md_means$Trans_Fat.g[5]
ff_means[5,7] <- md_means$Total_Carbs.g[5]
ff_means[5,8] <- md_means$Protein.g[5]
ff_means[5,9] <- md_means$Fiber.g[5]
ff_means[5,10] <- md_means$Sugars.g[5]
ff_means[5,11] <- md_means$Cholesterol.mg[5]
ff_means[5,12] <- md_means$Sodium.mg[5]

write.csv(ff_means, file = "Fast_Food_Means.csv")
