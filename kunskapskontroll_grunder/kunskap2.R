library(dplyr)
library(readxl)
library(caret)
library(glmnet)
library(MASS)
library(randomForest)
library(mgcv)
library(Hmisc)
library(corrplot)
options(scipen = 999)
library(PerformanceAnalytics)
library(GGally)
library(pxweb)
library(httr)
library(stringr)


####################################
# PXWEB API                       ##
####################################
d <- pxweb_interactive()

#alternativ 1, *, 1, *, y, n, y, y, y
bast <- pxweb_interactive("https://api.scb.se/OV0104/v1/doris/sv/ssd/TK/TK1001/TK1001A/PersBilarDrivMedel")

scb_data <- bast$data


moddad_scb <- scb_data
str(moddad_scb)
#unique(moddad_scb$månad)


oddad_scb <- moddad_scb %>%
  mutate(år = substr(månad, 1, 4),
         månad = as.numeric(substr(månad, 6, 7)))

oddad_scb <- oddad_scb %>%
  rename("Nyregistrerade_personbilar" = "Nyregistrerade personbilar")

oddad_scb_summerad <- oddad_scb %>%
  group_by(drivmedel, år) %>%
  summarise(Ackumulerad_Nyregistrerade = sum(Nyregistrerade_personbilar)) %>%
  ungroup()



ggplot(oddad_scb_summerad, aes(x = år, y = Ackumulerad_Nyregistrerade, color = drivmedel, group = drivmedel)) +
  geom_line() +
  geom_point(data = oddad_scb_summerad %>% filter(drivmedel == "el"), aes(x = år, y = Ackumulerad_Nyregistrerade), color = "red", size = 3) +
  labs(x = "År", y = "Ackumulerade nyregistrerade personbilar", color = "Drivmedel", title = "Ackumulerade nyregistrerade personbilar per drivmedel och år")



elbilar_data <- oddad_scb_summerad %>% filter(drivmedel == "el")

# Skapa din ggplot
ggplot(oddad_scb_summerad, aes(x = år, y = Ackumulerad_Nyregistrerade, color = drivmedel, group = drivmedel)) +
  geom_line() +
  geom_point(data = elbilar_data, aes(x = år, y = Ackumulerad_Nyregistrerade), color = "red", size = 3) +
  geom_text(data = elbilar_data, aes(x = år, y = Ackumulerad_Nyregistrerade, label = Ackumulerad_Nyregistrerade), vjust = -1, color = "red") +
  labs(x = "År", y = "Ackumulerade nyregistrerade personbilar", color = "Drivmedel", title = "Ackumulerade nyregistrerade personbilar per drivmedel och år")






####################################
# Data                            ##
####################################


file_path <- "C:/Users/Umut_/OneDrive/Skrivbord/ec_utbildning/Rstudio/r_prog_ds23-main/kunskapskontroll/blocket_alla_bilar.xlsx"
car_data <- read_excel(file_path)






# Skapa korrelationsplotten
ggpairs(car_mod_data, columns = c("Miltal", "Modellår", "Drivning", "Hästkrafter", "Pris"))
car_mod_data
str(car_mod_data)

#Gör om variablerna till lowercases på alla & as factor
car_mod_data <- car_data %>%
  mutate(
    Färg = tolower(Färg),
    Län = tolower(Län),
    Modell = tolower(Modell),
    Märke = tolower(Märke),
    Färg = gsub("\\[|\\]|mörk|ljus", "", Färg),
    Modellår = as.factor(Modellår),
    Biltyp = as.factor(Biltyp),
    Drivning = as.factor(Drivning),
    Färg = as.factor(Färg),
    Märke = as.factor(Märke),
    Modell = as.factor(Modell),
    Län = as.factor(Län)
    )

#1-hot-encoding - sket i detta
#car_mod_data_1hot <- model.matrix(~ . - 1, data = car_mod_data)

#df_car_mod <- as.data.frame(car_mod_data_1hot)

#Multipel linjär
lm_model <- lm(Pris ~ Miltal + Modellår + Biltyp + Hästkrafter + Märke, data = car_mod_data)
res <- lm_model$residuals
RMSE_lm <- sqrt(mean(residuals^2))

summary(model)

par(mfrow=c(2,2))
plot(model)
sqrt(mean(model$residuals^2))


#Preppning för Ridge & Lasso
x <- model.matrix(Pris ~ Miltal + Modellår + Biltyp + Hästkrafter + Märke, data = car_mod_data)[, -1]
y <- car_mod_data$Pris
grid <- 10^seq(10, -2, length = 100)


set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]

corrmatrix <- rcorr(as.matrix(x))
#plottar korrelationen
corrplot(corrmatrix$r,         diag=FALSE,          method="number",          order="AOE",          tl.col="black", main = "Figur 1", mar=c(0,0,1,0)
)



#Ridge
ridge_mod <- glmnet(x[train, ], y[train], alpha = 0,
                    lambda = grid, thresh = 1e-12)

cv_out <- cv.glmnet(x[train, ], y[train], alpha = 0, nfolds = 10)
bestlam <- cv_out$lambda.min
ridge_pred <- predict(ridge_mod, s = bestlam, newx = x[test, ])

out <- glmnet(x, y, alpha = 0)
dim(coef(out))
ridge.coef <- predict(out, type = "coefficients", s = bestlam)[1:dim(coef(out))[1], ]
ridge.coef

#RMSE ridge
RMSE_ridge <- sqrt(mean((ridge_pred - y.test)^2))


#Lasso
lasso_mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid)

cv_out_lass <- cv.glmnet(x[train, ], y[train], alpha = 1)
bestlam_lass <- cv_out_lass$lambda.min
lasso_pred <- predict(lasso_mod, s = bestlam_lass, newx = x[test, ])

out_lass <- glmnet(x, y, alpha = 1, lambda = grid)

lasso.coef <- predict(out_lass, type = "coefficients", s = bestlam_lass)[1:dim(coef(out_lass))[1], ]
lasso.coef
lasso.coef[lasso.coef != 0]

#RMSE Lasso
RMSE_lasso <- sqrt(mean((lasso_pred - y.test)^2))






#RandomForest
rf <- randomForest(Pris ~ Miltal + Modellår + Biltyp + Hästkrafter + Märke, data = car_mod_data, ntrees = 1000, keep.forest = FALSE)

(rf$importance)/100000000000
RMSE_rf <- sqrt(mean(rf$mse))

#GAM

gam_mod <- gam(Pris ~ s(Miltal) + Modellår + Biltyp + s(Hästkrafter) + Märke, data = car_mod_data)
gam_pred <- predict(gam_mod, newdata = car_mod_data[test, ], type = "response")
gam_residuals <- car_mod_data$Pris[test] - gam_pred
par(mfrow=c(1,2))
plot(gam_mod)
# RMSE för GAM
RMSE_gam <- sqrt(mean(gam_residuals^2))

?gam

summary(gam_mod)
plot(gam_mod, se= TRUE, col = "blue", pages = 1)

RMSE_lm
RMSE_ridge
RMSE_lasso
RMSE_rf
RMSE_gam

confusionMatrix(lm_model)


dim(car_mod_data)
asd <- cbind(RMSE_lm, RMSE_ridge, RMSE_lasso, RMSE_rf, RMSE_gam)
?corrplot
