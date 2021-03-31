library(readr)
library(dplyr)
library(ggplot2)
library(broom)

# REGRESSION - Linear ----

# NC BIRTHS: How does weight vary by the week they were born ?
# Data 
ncbirths <- openintro::ncbirths %>% na.omit()
# - eda
ncbirths %>% select_if(is.numeric) %>% cor() %>% 
  corrplot::corrplot(method = "number", type = "upper")
# - visual: Simple Linear Regression
ncbirths %>% 
  ggplot(aes(weeks, weight)) +
  geom_point() +
  geom_smooth() +
  ggtitle("How does weight vary by week they were born ?")



# BEAUTY SCORES: What factors effect teaching score

# Data
evals <- openintro::evals %>% select(course_id, score, gender, bty_avg, age)
# EDA
evals %>% glimpse()
evals %>% select(-course_id) %>% skimr::skim()
evals %>% select(-c(course_id, gender)) %>% cor() %>% round(2)
# - visuals
evals %>% 
  ggplot(aes(bty_avg, score)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("How does Teaching Score vary by Beauty ?")
# - score: simple linear regression
lm(score ~ bty_avg, data = evals) %>% tidy(conf.int = TRUE)
evals %>% 
  ggplot(aes(age, score)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("How does Teaching Score vary by Age ?")
# - age: simple linear regression
lm(score ~ age, data = evals) %>% tidy(conf.int = TRUE)
evals %>% 
  ggplot(aes(age, score)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE,
              aes(color = gender)) +
  ggtitle("How does Teaching Score vary by Age accounting for gender ?")
# - Interaction
lm(score ~ age*gender, data = evals) %>% summary() 
# - Parrelel Slopes
lm(score ~ age + gender, data = evals) %>% summary()


# TAIWAN REAL ESTATE: What effects the price of houses in Taiwan
# Data: 
# - clean names
taiwan_raw <- read_csv("Data/Taiwan_Real_Estate.csv",
                       col_names = c("No","trans_date","house_age","dist_to_mrt_m","n_conv_store","latitude","longitude","price"))
taiwan <- taiwan_raw[-1,-1]
# - datatype conversion
taiwan <- taiwan %>% 
  mutate(trans_date = as.integer(trans_date),
         house_age = as.integer(house_age),
         dist_to_mrt_m = as.double(dist_to_mrt_m),
         n_conv_store = as.integer(n_conv_store),
         price = as.double(price)) %>% 
  select(-latitude, -longitude)

# EDA
# - price: Normal
taiwan %>% 
  ggplot(aes(price)) +
  geom_histogram()
# - corr matrix: Distance to Market (-0.67), Num of Conv Stores (0.57), House Age (-0.21)
taiwan %>% cor() %>% corrplot::corrplot(method = "number", type = "lower")

# Visual: Distance to Market
taiwan %>% 
  ggplot(aes(dist_to_mrt_m, price)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE) +
  ggtitle("How does price vary by distance to Metro ?")
# - distance to market explains around 50% of the variation around price
lm(price ~ dist_to_mrt_m, data = taiwan) %>% glance() %>% select(r.squared)
lm(price ~ poly(dist_to_mrt_m,2), data = taiwan) %>% glance() %>% select(r.squared)
lm(price ~ log(dist_to_mrt_m), data = taiwan) %>% glance() %>% select(r.squared)

# Visual: Num of Convient Stores near by 
taiwan %>% 
  ggplot(aes(n_conv_store, price)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE) +
  ggtitle("How does price vary by # of Conv_Stores near ?")
# - num of convient stores explains around 30% of the variation around price
lm(price ~ n_conv_store, data = taiwan) %>% glance() %>% select(r.squared)

# MODELING
lm(price ~ poly(dist_to_mrt_m,2) + n_conv_store + house_age:n_conv_store, data = taiwan) %>% glance() %>% select(r.squared)
# - final model
mod <- lm(price ~ poly(dist_to_mrt_m,2) + n_conv_store, data = taiwan)
mod %>% tidy()
mod %>% glance() %>% select(r.squared, rse = sigma, F_Stat = statistic, p.value)
# Residual Analysis
mod %>% plot(which = 1) # Model Check
mod %>% plot(which = 2) # Normality Check
mod %>% plot(which = 4) # Outlier Check
mod %>% augment() %>% arrange(desc(.hat)) %>% head() %>% select(.hat, everything())
mod %>% augment() %>% arrange(desc(.cooksd)) %>% head() %>% select(.cooksd, everything())




# MARIOKART:

# Data
mariokart <- openintro::mariokart
# - clean: remove outlier
mariokart <- mariokart %>% filter(total_pr < 100)
mariokart <- mariokart %>% 
  select(total_pr, everything(), -id)

# EDA: Categorical
mariokart %>% skimr::skim()
mariokart %>% glimpse()

# Visual: Price ~ Condition
mariokart %>% 
  ggplot(aes(cond, total_pr)) +
  geom_boxplot() +
  ggtitle("How does price vary by COND ?")
# - stat test: significant
t.test(total_pr ~ cond, data = mariokart)

# Visual: Price ~ Ship
mariokart %>% 
  ggplot(aes(ship_sp, total_pr)) +
  geom_boxplot() +
  ggtitle("How does price vary by Ship ?")
# - stat test: significant
lm(total_pr ~ ship_sp, data = mariokart, family = "binomial") %>% tidy() %>% mutate(p.value = round(p.value,3))

# Visual: Price ~ Stock Photo
mariokart %>% 
  ggplot(aes(stock_photo, total_pr)) +
  geom_boxplot() +
  ggtitle("How does price vary by Stock Photo ?")
# - stat test: significant
t.test(total_pr ~ cond, data = mariokart)

# EDA: Numerical
# Corrplot
mariokart %>% select(total_pr, duration, n_bids, start_pr, ship_pr, seller_rate, wheels) %>% 
  cor() %>% corrplot::corrplot(method = "number", type = "upper")
# Visual: wheels (0.8)
mariokart %>% 
  ggplot(aes(wheels, total_pr, color = cond)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("How does Price vary by # of Wheels ?")
# - stat test: wheels and Condition accounts for 71.7% of variation on Total Price
lm(total_pr ~ wheels + cond, data = mariokart) %>% glance() %>% select(r.squared)

# Visual: durartion (-0.37)
mariokart %>% 
  ggplot(aes(duration, total_pr, color = cond)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("How does Price vary by duration accting for cond ?")
# - stat test: Duration and Condition accounts for 43.5% of variation on Total Price
lm(total_pr ~ duration*cond, data = mariokart) %>% glance() %>% select(r.squared)

# Visual: start price (-0.37)
mariokart %>% 
  ggplot(aes(start_pr, total_pr, color = cond)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("How does Price vary by Start_Pr accting for cond ?")
# - stat test: Start Price and Condition accounts for 44% of variation on Total Price
lm(total_pr ~ start_pr + cond, data = mariokart) %>% glance() %>% select(r.squared)

# MODELING
lm(total_pr ~ wheels + duration + cond, data = mariokart) %>% 
  glance %>% select(r.squared)
lm(total_pr ~ wheels + duration + cond + duration:cond, data = mariokart) %>% 
  glance %>% select(r.squared)
lm(total_pr ~ wheels + duration + start_pr + cond + duration:cond, data = mariokart) %>% 
  glance %>% select(r.squared)

# Fit
mod <- lm(total_pr ~ wheels + duration + start_pr + cond + duration:cond, data = mariokart)
mod %>% tidy() %>% mutate(p.value = round(p.value, 3))
# - mod: duration:condused NOT significant
mod_2 <- lm(total_pr ~ wheels + duration + start_pr + cond, data = mariokart)
mod_2 %>% tidy() %>% mutate(p.value = round(p.value, 3))
# - mod 2: duration NOT significant
mod_3 <- lm(total_pr ~ wheels + start_pr + cond, data = mariokart)
mod_3 %>% tidy() %>% mutate(p.value = round(p.value, 3))
mod_3 %>% glance() %>% select(r.squared, rse = sigma)
# - mod 3: r2 = 75.8% | RSE = 4.54

# Residual Analysis
mod_3 %>% plot(which = 3) # Model Check
mod_3 %>% plot(which = 2) # Normality check
mod_3 %>% plot(which = 4) # Outlier Check


# -- CREDIT
credit <- ISLR::Credit %>% 
  select(ID, debt = Balance, credit_limit = Limit, income = Income, credit_rating = Rating, age = Age) %>% 
  as_tibble()
# eda
credit %>% glimpse()
credit %>% select(debt, credit_limit, income) %>% skimr::skim()

credit %>% select(debt, credit_limit, income) %>% cor() %>% round(2)
credit %>% 
  ggplot(aes(credit_limit, debt)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("How does Debt vary by Credit Limit ?")
lm(debt ~ credit_limit, data = credit) %>% tidy()
lm(debt ~ credit_limit, data = credit) %>% glance()

credit %>% 
  ggplot(aes(income, debt)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("How does Debt vary by Income ?")
lm(debt ~ income, data = credit) %>% tidy()
lm(debt ~ income, data = credit) %>% glance()

credit %>% 
  ggplot(aes(age, debt)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("How does Debt vary by Age ?")
lm(debt ~ age, data = credit) %>% tidy()
lm(debt ~ age, data = credit) %>% glance()

credit %>% 
  ggplot(aes(income, debt)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, size = 2, linetype = 2) +
  geom_smooth(method = "lm", se = FALSE,
              aes(color = cut(credit_limit, 4))) +
  labs(
    title = "How does Debt vary by Income acounting for Credit Limit ?",
    subtitle = "Simpsons Paradox")

lm(debt ~ credit_limit + income, data = credit) %>% tidy()
lm(debt ~ credit_limit + income, data = credit) %>% glance()

credit %>% 
  ggplot(aes(age, debt)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE, size = 2, linetype = 2) +
  geom_smooth(method = "lm", se = FALSE,
              aes(color = cut(credit_limit, 4))) +
  labs(
    title = "How does Debt vary by Age acounting for Credit Limit ?",
    subtitle = "Simpsons Paradox")
lm(debt ~ credit_limit + age, data = credit) %>% tidy()

# -- BOSTON HOUSING
boston <- MASS::Boston %>% as_tibble()
boston <- boston %>% mutate(chas = factor(chas))
# eda
boston %>% glimpse()
boston %>% skimr::skim()
boston %>% select(medv, everything(), -chas) %>% 
  cor() %>% corrplot::corrplot(method = "number", type = "upper")

boston %>% 
  ggplot(aes(chas, medv)) +
  geom_boxplot() +
  ggtitle("How does medv vary by if bouds Charles River ?")
t.test(medv ~ chas, data = boston)

boston %>% 
  ggplot(aes(lstat, medv)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("How does Medv vary by lower status Percentage ?")
lm(medv ~ lstat, data = boston) %>% summary()
lm(medv ~ poly(lstat,2), data = boston) %>% summary()

boston %>% 
  ggplot(aes(rm, medv)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("How does Medv vary by Avg num of Rooms ?")
lm(medv ~ rm, data = boston) %>% tidy()
lm(medv ~ rm, data = boston) %>% glance()

boston %>% 
  ggplot(aes(ptratio, medv)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("How does Medv vary by Pupil-Teacher Rato ?")
lm(medv ~ ptratio, data = boston) %>% glance()

# fit
lm(medv ~ poly(lstat,2) + chas, data = boston) %>% tidy()
lm(medv ~ poly(lstat,2) + chas, data = boston) %>% glance()
lm(medv ~ poly(lstat,2) + rm + chas, data = boston) %>% tidy()
lm(medv ~ poly(lstat,2) + rm + chas, data = boston) %>% glance()
lm(medv ~ poly(lstat,2) + rm + ptratio + chas, data = boston) %>% tidy()
lm(medv ~ poly(lstat,2) + rm + ptratio + chas, data = boston) %>% glance()
lm(medv ~ ., data = boston) %>% tidy()
lm(medv ~ ., data = boston) %>% glance()
lm(medv ~ .-indus-age, data = boston) %>% tidy()
lm(medv ~ .-indus-age, data = boston) %>% glance()

# regularization
set <- sample(1:2, size = nrow(boston), prob = c(0.7,0.3), replace = TRUE)
boston_train <- boston[set == 1,]
boston_test <- boston[set == 2,]
ctrl_repeatedCV <- trainControl(method = "repeatedcv",
                                number = 10,
                                repeats = 5,
                                verboseIter = TRUE)

mod_lm <- train(medv ~ ., data = boston_train,
                method = "lm",
                trControl = ctrl_repeatedCV)
mod_lm$results
mod_lm$finalModel %>% tidy() %>% mutate(p.value = round(p.value, 3))
mod_lm$finalModel %>% plot()

mod_ridge <- train(medv ~ ., data = boston_train,
                   method = "glmnet",
                   tuneGrid = expand.grid(alpha = 0,
                                          lambda = seq(0.0001,1, length.out = 5)),
                   trControl = ctrl_repeatedCV)
mod_ridge %>% plot()
mod_ridge$finalModel %>% plot(xvar = "lambda")
mod_ridge$finalModel %>% plot(xvar = "dev")
mod_ridge %>% varImp() %>% plot()

mod_lasso <- train(medv ~ ., data = boston_train,
                   method = "glmnet",
                   tuneGrid = expand.grid(alpha = 1,
                                          lambda = seq(0.0001,0.2, length.out = 5)),
                   trControl = ctrl_repeatedCV)
mod_lasso %>% plot()
mod_lasso$finalModel %>% plot(xvar = "lambda", label = TRUE)
mod_lasso$finalModel %>% plot(xvar = "dev", label = TRUE)
mod_lasso %>% varImp() %>% plot()

mod_elastic <- train(medv ~ ., data = boston_train,
                     method = "glmnet",
                     tuneGrid = expand.grid(alpha = seq(0,1, length.out = 10),
                                            lambda = seq(0.0001,0.2, length.out = 5)),
                     trControl = ctrl_repeatedCV)
mod_elastic %>% plot()
mod_elastic$finalModel %>% plot(xvar = "lambda", label = TRUE)
mod_elastic$finalModel %>% plot(xvar = "dev", label = TRUE)
mod_elastic %>% varImp() %>% plot()

model_list <- list(Linear = mod_lm,
                   Ridge = mod_ridge,
                   Lasso = mod_lasso,
                   Elastic = mod_elastic)
res <- resamples(model_list)
res %>% bwplot()
res %>% summary()

# -- HITTERS 
hitters <- ISLR::Hitters %>% as_tibble()
# eda
hitters %>% glimpse()
hitters %>% skimr::skim()
hitters <- hitters %>% na.omit()

mod_full <- lm(Salary ~., data = hitters)
# subset selection
mod_subset <- leaps::regsubsets(Salary ~ ., nbest = 1, nvmax = 19, data = hitters, method = "exhaustive") %>% summary()
mod_subset$adjr2 %>% plot(xlab = "Number of Variables", ylab = "Adjr2")
mod_subset$adjr2 %>% which.max()
mod_subset$cp %>% plot(xlab = "Number of Variables", ylab = "Cp")
mod_subset$cp %>% which.min()
mod_subset$bic %>% plot(xlab = "Number of Variables", ylab = "BIC")
mod_subset$bic %>% which.min()
leaps::regsubsets(Salary ~ ., data = hitters, nvmax = 19) %>% 
  coef(11)

mod_foward <- leaps::regsubsets(Salary ~ ., nvmax = 19, data = hitters, method = "forward") %>% summary()
mod_foward$adjr2 %>% plot(xlab = "Number of Variables", ylab = "Adjr2")
mod_foward$adjr2 %>% which.max()
mod_foward$cp %>% plot(xlab = "Number of Variables", ylab = "Cp")
mod_foward$cp %>% which.min()
mod_foward$bic %>% plot(xlab = "Number of Variables", ylab = "BIC")  
mod_foward$bic %>% which.min()
leaps::regsubsets(Salary ~ ., data = hitters, nvmax = 19, method = "forward") %>% 
  coef(6)

mod_step_AIC <- mod_full %>% MASS::stepAIC(direction = "both", k = 2)
mod_step_AIC$anova
mod_step_BIC <- mod_full %>% MASS::stepAIC(direction = "both", k = log(length(hitters)))
mod_step_BIC$anova
# regularization
library(glmnet)
x <- model.matrix(Salary ~ .-1, data = hitters)
y <- hitters$Salary

mod_ridge <- glmnet(x = x, y = y, alpha = 0)
mod_ridge %>% plot(xvar = "lambda", label = TRUE)
mod_ridge %>% plot(xvar = "dev", label = TRUE)
cv_ridge <- cv.glmnet(x,y, alpha = 0)
cv_ridge %>% plot()
cv_ridge %>% coef()

mod_lasso <- glmnet(x,y, alpha = 1)
mod_lasso %>% plot(xvar = "lambda", label = TRUE)
mod_lasso %>% plot(xvar = "dev", label = TRUE)
cv_lasso <- cv.glmnet(x,y, alpha = 1)
cv_lasso %>% plot()
cv_lasso %>% coef()


# -- BOSTON
dat <- tibble(MASS::Boston)
# eda
dat %>% cor() %>% corrplot::corrplot(method = "number", type = "lower")
dat %>% 
  ggplot(aes(lstat, medv)) +
  geom_point() +
  geom_smooth(method = "lm")
dat %>% 
  ggplot(aes(age, medv)) +
  geom_point() +
  geom_smooth(method = "lm")
# fit
mod <- lm(medv ~ lstat, data = dat)
mod_2 <- lm(medv ~ lstat + age, data = dat)
mod_3 <- update(mod_full, ~. -age-indus)
mod_4 <- lm(medv ~ lstat*age, data = dat)
mod_5 <- lm(medv ~ poly(lstat, 2), data = dat)
mod_full <- lm(medv ~ ., data = dat)
# assessment
mod %>% summary()
mod_2 %>% summary()
mod_3 %>% summary()
mod_4 %>% summary()
mod_full %>% summary()
# predict
mod %>% predict(data.frame(lstat = c(5,10,15)), interval = "confidence")



# -- LA HOMES --
homes <- read_csv("https://assets.datacamp.com/production/repositories/848/datasets/96a4003545f7eb48e1c14b855df9a97ab8c84b1d/LAhomes.csv")


# fit
lm(price ~ bed, data = homes) %>% broom::tidy()
lm(log(price) ~ bed, data = homes) %>% broom::tidy()
lm(log(price) ~ log(sqft), data = homes) %>% broom::tidy()
lm(log(price) ~ log(bath), data = homes) %>% broom::tidy()
lm(log(price) ~ log(sqft) + log(bath), data = homes) %>% broom::tidy()


# -- NY RESTURANTS --
rest <- read_csv("https://assets.datacamp.com/production/repositories/848/datasets/4ff34a40bd4e636556494f83cf40bdc10c33d49e/restNYC.csv")
explanatory_data <- tibble(weight = seq(2000,3000, by = 75))
# eda
ggplot(rest, aes(Food, Price)) +
  geom_point()
# fit
lm(Price ~ Service, data = rest) %>% broom::tidy()
lm(Price ~ Service + Food + Decor, data = rest) %>% broom::tidy()


# - MILES PER GALLON --
data("mpg", package = "gamair")
mpg <- tibble(mpg)
# eda
#   - price ~ weight
ggplot(mpg, aes(weight, price)) +
  geom_point() +
  # Fit
  geom_smooth(method = "lm", se = FALSE) +
  # Predictions
  geom_point(data = prediction_data, color = "red")
mpg %>%   
  ggplot(aes(weight^2, price)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
mpg %>% filter(!is.na(price)) %>% summarise(
  cor = cor(weight, price),
  cor_sqrt = cor(weight, sqrt(price)),
  cor_log = cor(weight, log(price)),
  cor_weight2 = cor(I(weight^2), price),
  cor_sqrt_both = cor(sqrt(weight), sqrt(price)),
  cor_log_both = cor(log(weight), log(price))
)

ggplot(mpg, aes(cylinders, price)) +
  geom_boxplot()
mpg %>% group_by(cylinders) %>% summarise(
  mean = mean(price)
)
# fit
mod_lm <- lm(price ~ weight, data = mpg)
mod_lm %>% broom::tidy()            # Coefficent level
mod_lm %>% broom::augment()         # Observation level
mod_lm %>% broom::glance()          # Model level

lm(price ~ cylinders + 0, data = mpg)
# predictions
prediction_data <- explanatory_data %>% 
  mutate(
    price = predict(mod_lm, newdata = explanatory_data)
  )
# assesment
autoplot(mod_lm, which = 1:3, ncol = 1) # Linear-Normality-Hetroscedatic-NoEndogeneity-NoAutocorrelation
mod_lm %>% broom::augment() %>% select(price, weight, .hat) %>% 
  arrange(desc(.hat))                   # Outliers-Leverage
mod_lm %>% broom::augment() %>% select(price, weight, .cooksd) %>%
  arrange(desc(.cooksd))                # Outliers-Influential
autoplot(mod_lm, which = 4:6, ncol = 1)


# -- NYC FOOD
dat <- read_csv("https://assets.datacamp.com/production/repositories/845/datasets/639a7a3f9020edb51bcbc4bfdb7b71cbd8b9a70e/nyc.csv",
                col_types = cols(
                  East = col_factor()
                ))
# eda
dat %>% select(-Case,-Restaurant) %>% GGally::ggpairs(aes(color = East))
dat %>% 
  ggplot(aes(Food, Price)) +
  geom_point()
dat %>% 
  ggplot(aes(Food, Price, color = East)) +
  geom_point()
# fit
mod <- lm(Price ~ Food, data = dat)
mod_2 <- lm(Price ~ Food + East, data = dat)
mod_3 <- lm(Price ~ Food + Service + East, data = dat)
# assessment
mod %>% summary()
mod_2 %>% summary()
mod_3 %>% summary()




#
# REGRESSION - Generalized Linear Models ----

# -- CREDIT DEFAULT: (Class Imbalance)
default <- ISLR::Default %>% tibble()
# eda
table(default$default) %>% prop.table() %>% round(2)

# -- TITANIC: (Missing Values) 
titanic_raw <- titanic::titanic_train %>% as_tibble()
# eda - clean
titanic_raw %>% skimr::skim()
titanic_raw[titanic_raw == ""] <- NA
titanic <- titanic_raw %>% select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked)
titanic <- titanic %>% mutate(Pclass = factor(Pclass),
                              Sex = factor(Sex),
                              Embarked = factor(Embarked))
titanic %>% glimpse()
table(titanic$Survived) %>% prop.table() %>% round(3)
# eda - categories
titanic %>% 
  ggplot(aes(Pclass, fill = factor(Survived))) +
  geom_bar(position = "fill") +
  ggtitle("Total Survivial Rate ~ Pclass")
table(titanic$Survived, titanic$Pclass) %>% prop.table(margin = 2) %>% round(3)
glm(Survived ~ Sex, family = "binomial", data = titanic) %>% tidy() %>% 
  mutate(estimate = plogis(estimate))

titanic %>% 
  ggplot(aes(Sex, fill = factor(Survived))) +
  geom_bar(position = "fill") +
  ggtitle("Total Survivial Rate ~ Sex")
table(titanic$Survived, titanic$Sex) %>% prop.table(margin = 2) %>% round(3)
glm(Survived ~ Pclass, family = "binomial", data = titanic) %>% tidy() %>% 
  mutate(estimate = plogis(estimate))
# eda - numeric
titanic %>% select(Survived, Age, SibSp, Parch, Fare) %>% cor(method = "spearman")
titanic %>% 
  ggplot(aes(factor(Survived), Fare)) +
  geom_boxplot() +
  ggtitle("Total Survivial Rate ~ Fare")
titanic %>% 
  ggplot(aes(Fare, Survived)) +
  geom_jitter(height = 0.02, alpha = 0.3) +
  geom_smooth(method = "glm", se = FALSE,
              method.args = list(family = "binomial"))
ggtitle("Total Survivial Rate ~ Fare")
glm(Survived ~ Fare, family = "binomial", data = titanic) %>% tidy() %>% 
  mutate(estimate = plogis(estimate))
glm(Survived ~ Fare, family = "binomial", data = titanic) %>% glance()


# fit
mod <- glm(Survived ~ ., data = titanic,
           family = "binomial")
mod %>% tidy() %>% mutate(estimate = plogis(estimate),
                          p.value = round(p.value, 3))
mod_2 <- mod %>% update(.~. -Parch-Fare-Embarked)
mod_2 %>% tidy() %>% mutate(estimate = plogis(estimate),
                            p.value = round(p.value, 3))
# scales
mod %>% augment()
mod %>% augment(type.predict = "response") %>% 
  ggplot(aes(Fare, .fitted)) +
  geom_point() + geom_line() +
  scale_y_continuous("Probability of Surviving")
mod %>% augment(type.predict = "class") %>% 
  mutate(odds = .fitted / (1 - .fitted)) %>% 
  ggplot(aes(Fare, odds)) +
  geom_point() + geom_line() +
  scale_y_continuous("Odds of Surviving")
mod %>% augment() %>% 
  ggplot(aes(Fare, .fitted)) +
  geom_point() + geom_line() +
  scale_y_continuous("LogOdds of Surviving")
mod %>% augment(type.predict = "class") %>% 
  mutate(pred_Survived = round(.fitted)) %>% 
  select(pred_Survived, Survived) %>% 
  table()



# -- BUS COMMUTUE 
bus <- read_csv("https://assets.datacamp.com/production/repositories/2698/datasets/e368234a66bbabc19b8da1fb42d3e1027508d710/busData.csv")
bus <- bus %>% mutate(Bus = factor(Bus))
# eda
table(bus$Bus) %>% prop.table() %>% round(2)
bus %>% 
  ggplot(aes(factor(Bus), MilesOneWay)) +
  geom_boxplot()
bus %>% mutate(Bus = ifelse(Bus == "Yes",1,0)) %>% 
  ggplot(aes(MilesOneWay, Bus)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.05) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"))
glm(Bus ~ CommuteDays, data = bus, family = "binomial") %>% 
  tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
  select(term, estimate, conf.low, conf.high) # conf.int does not include 1
# fit
mod <- glm(Bus ~ CommuteDays + MilesOneWay, data = bus,
           family = "binomial")
mod %>% tidy(exponentiate = TRUE, conf.int = TRUE)
mod %>% tidy() %>% mutate(prob = plogis(estimate))


# -- HEART DIEASE
heart <- read_csv("http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",
                  col_names = c("age","sex","cp","trestbps","chol","fbs","restecg",
                                "thalach","exang","oldpeak","slope","ca","thal","hd"))
# eda - clean
heart[heart == "?"] <- NA
heart <- heart %>% 
  mutate(sex = factor(ifelse(sex == 0, "F","M")),
         cp = factor(cp),
         fbs = factor(fbs),
         restecg = factor(restecg),
         exang = factor(exang),
         slope = factor(slope),
         ca = factor(as.integer(ca)),
         thal = factor(as.integer(thal)),
         hd = factor(ifelse(hd == 0, "Healthy", "Unhealthy")))
heart %>% glimpse()
heart %>% skimr::skim()
heart <- heart %>% filter(!is.na(ca) | !is.na(thal))
heart <- heart[!(is.na(heart$ca) | is.na(heart$thal)),]
# eda - categorical
table(heart$hd, heart$sex)
glm(hd ~ sex, data = heart, family = "binomial") %>% summary()
glm(hd ~ sex, data = heart, family = "binomial") %>% glance()

table(heart$hd, heart$cp)
table(heart$hd, heart$fbs)
table(heart$hd, heart$restecg) #*
table(heart$hd, heart$exang)
table(heart$hd, heart$slope)
table(heart$hd, heart$ca)
table(heart$hd, heart$thal)

# fit
mod <- glm(hd ~ ., data = heart, family = "binomial")
mod %>% glance()
# fit - Visuals
mod %>% augment(type.predict = "response") %>% 
  arrange(.fitted) %>% 
  mutate(rank = 1:nrow(heart)) %>%
  select(rank, hd, .fitted) %>% 
  ggplot(aes(rank, .fitted, color = hd)) +
  geom_point(alpha = 0.1, size = 4) +
  scale_y_continuous("Probability of Surviving")
mod %>% augment(type.predict = "class") %>%
  arrange(.fitted) %>% 
  mutate(odds = .fitted / (1 - .fitted),
         rank = 1:nrow(heart)) %>% 
  select(rank, hd, odds, .fitted) %>% 
  ggplot(aes(rank, odds, color = hd)) +
  geom_point(alpha = 0.1, size = 4) +
  scale_y_continuous("Odds of Surviving")
mod %>% augment() %>%
  arrange(.fitted) %>% 
  mutate(rank = 1:nrow(heart)) %>% 
  select(rank, hd, .fitted) %>% 
  ggplot(aes(rank, .fitted, color = hd)) +
  geom_point(alpha = 0.1, size = 4) +
  scale_y_continuous("LogOdds of Surviving")
mod %>% augment(type.predict = "class") %>% 
  mutate(pred_Healthy = round(.fitted)) %>% 
  select(pred_Healthy, hd) %>% 
  table()



# -- DONORS
dat <- read_csv("https://assets.datacamp.com/production/repositories/718/datasets/9055dac929e4515286728a2a5dae9f25f0e4eff6/donors.csv")
dat_mutate <- dat %>% 
  mutate(
    wealth_levels = factor(wealth_rating,
                           levels = c(0,1,2,3),
                           labels = c("Unknown", "Low", "Medium", "High"))
  ) %>% 
  mutate(
    wealth_levels = relevel(wealth_levels,
                            ref = "Medium")
  )
dat_mutate <- dat %>% 
  mutate(
    imputed_age = ifelse(is.na(age),
                         round(mean(age, na.rm = TRUE),2),
                         age),
    missing_age = ifelse(is.na(age),
                         1,0)
  )
# eda
table(dat_mutate$donated) %>% prop.table()
dat_mutate %>% 
  group_by(bad_address, interest_religion) %>% 
  summarise(donated = mean(donated == 1))
dat_mutate %>% 
  group_by(bad_address, interest_veterans) %>% 
  summarise(donated = mean(donated == 1))
dat_mutate %>% 
  group_by(wealth_levels) %>% 
  summarise(donated = mean(donated == 1))
dat_mutate %>% 
  group_by(recency, frequency, money) %>% 
  summarise(donated = mean(donated == 1))
# fit
mod <- glm(donated ~ bad_address + interest_religion + interest_veterans,
           data = dat_mutate, family = "binomial")
mod_2 <- glm(donated ~ wealth_levels, data = dat_mutate,
             family = "binomial")
mod_rfm <- glm(donated ~ money + recency*frequency,
               data = dat_mutate, family = "binomial")
#   - step
mod_null <- glm(donated ~ 1, 
                data = donors, family = "binomial")
mod_full <- glm(donated ~ .,
                data = donors, family = "binomial")
mod_step_fwd <- step(mod_null, 
                     scope = list(lower = mod_null, 
                                  upper = mod_full),
                     direction = "forward")
# asessment
mod %>% tidy(exponentiate = TRUE); mod %>% glance()
mod_2 %>% tidy(exponentiate = TRUE); mod_2 %>% glance()
mod_rfm %>% tidy(exponentiate = TRUE); mod_rfm %>% glance() 
mod_step_fwd %>% tidy()
mod_step_fwd %>% glance()
# predictions
mod_prob <- predict(mod, type = "response")
mod_pred <- ifelse(mod_prob > 0.0505,1,0)
mod_ROC <- roc(response = dat_mutate$donated, 
               predictor = mod_prob)
plot(mod_ROC, col = "blue"); auc(mod_ROC)

mod_rfm_prob <- predict(mod_rfm, type = "response")
mod_rfm_ROC <- roc(response = dat_mutate$donated, 
                   predictor = mod_rfm_prob)
plot(mod_rfm_ROC, col = "blue"); auc(mod_rfm_ROC)

mod_step_prob <- mod_step_fwd %>% predict(type = "response")
mod_step_ROC <-  roc(donors$donated, mod_step_prob)
plot(mod_step_ROC, col = "blue"); auc(mod_step_ROC)


# -- DEFAULT
dat <- read_csv2("https://assets.datacamp.com/production/repositories/1861/datasets/0b0772985a8676c3613e8ac2c6053f5e60a3aebd/defaultData.csv")
# eda
table(dat$PaymentDefault) %>% prop.table()
dat %>% 
  ggplot(aes(PaymentDefault)) +
  geom_bar()
# fit
mod_full <- glm(PaymentDefault ~ .-ID, data = dat,
                family = "binomial")
#     - step
mod_step <- MASS::stepAIC(mod_full, trace = 0)
formula_step <- mod_step$formula
# assessment
mod_full %>% tidy(exponentiate = TRUE)
mod_full %>% glance()
mod_step %>% tidy(exponentiate = TRUE)
mod_step %>% glance()
# prediction
mod_full_prob <- predict(mod_full, type = "response")
mod_full_pred <- ifelse(mod_full_prob > 0.5,1,0) %>% factor()
caret::confusionMatrix(data = factor(dat$PaymentDefault),
                       reference = mod_full_pred)
mod_step_prob <- predict(mod_step, type = "response")
mod_step_pred <- ifelse(mod_step_prob > 0.5,1,0) %>% factor()
caret::confusionMatrix(data = factor(dat$PaymentDefault),
                       reference = mod_step_pred)

##  - Poisson --
dat <- read_csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
dat <- dat %>% mutate(prog = case_when(
  prog == 1 ~ "General",
  prog == 2 ~ "Academic",
  prog == 3 ~ "Vocational"
)) %>% 
  mutate(
    prog = factor(prog),
    id = factor(id)
  )
# eda
dat %>% 
  ggplot(aes(num_awards, fill = prog)) +
  geom_histogram(binwidth = .5, position = "dodge")
dat %>% 
  ggplot(aes(math, num_awards)) +
  geom_jitter(width = 0.2, height = 0.2) +
  geom_smooth(method = "glm",
              method.args = list(family = "poisson"))


#

# Model Selection ----
dat <- ISLR::Hitters
dat <- dat %>% mutate(Name = rownames(dat)) %>% 
  select(Name, everything()) %>% 
  tibble()
dat <- na.omit(dat)


# -- BEST SUBSET SELECTION --
# fit
mod_full <- leaps::regsubsets(Salary ~ .-Name, data = dat,
                              nvmax = 19)
mod_step_fwd <- leaps::regsubsets(Salary ~ .-Name, data = dat,
                                  nvmax = 19, method = "forward")
# assesment
mod_full_summary <- mod_full %>% summary()
mod_full_summary$adjr2 %>% plot(xlab = "Number of Variables", ylab = "Adjr2")
mod_full_summary$cp %>% plot(xlab = "Number of Variables", ylab = "Cp")
mod_full_summary$bic %>% plot(xlab = "Number of Variables", ylab = "BIC")
mod_full %>% plot(scale = "adjr2")
mod_full %>% coef(10)

mod_fwd_summary <- mod_step_fwd %>% summary()
mod_fwd_summary$adjr2 %>% plot(xlab = "Number of Variables", ylab = "adjr2")
mod_fwd_summary$cp %>% plot(xlab = "Number of Variables", ylab = "cp")
mod_fwd_summary$bic %>% plot(xlab = "Number of Variables", ylab = "bic")
mod_step_fwd %>% plot(scale = "adjr2")
mod_step_fwd %>% coef(10)


# -- REGULARIZATION --
x <- model.matrix(Salary ~.-Name-1, data = dat)
y <- dat$Salary
# fit
mod_ridge <- glmnet::glmnet(x = x, y = y, alpha = 0) # Ridge
mod_lasso <- glmnet::glmnet(x = x, y = y, alpha = 1) # Lasso
# assessment
mod_ridge %>% plot(xvar = "lambda", label = TRUE)
mod_ridge %>% plot(xvar = "dev", label = TRUE)
cv_ridge <- glmnet::cv.glmnet(x,y, alpha = 0)
cv_ridge %>% plot()
cv_ridge %>% coef()

mod_lasso %>% plot(xvar = "lambda", label = TRUE)
mod_lasso %>% plot(xvar = "dev", label = TRUE)
cv_lasso <- glmnet::cv.glmnet(x,y, alpha = 1)
cv_lasso %>% plot()
cv_lasso %>% coef()
