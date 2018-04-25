library(tidyverse)

# Remove classes with less than N occurences, and transform feature to factor
remove_classes <- function(col, N) {
  col <- as.factor(col)
  keep <- sum(summary(col) > N)
  col <- fct_lump(col, n = keep)
}

<<<<<<< HEAD
# Create a function that indicates which features satisfy the two points on p. 45 in Applied Predictive Modeling!
=======
# Remove classes with less than N occurances in each feature
N <- 20
>>>>>>> 72185fc4b470eb2814257406acb7398ba25b8c70

ames <- read_csv("ames_train.csv")

# Remove insignificant features
ames <- ames %>%
  select(-(Street:Alley), -Utilities, -Condition2, -RoofMatl, -BsmtFinSF2, -Heating, -`1stFlrSF`, 
<<<<<<< HEAD
         -`2ndFlrSF`, -LowQualFinSF, -`3SsnPorch`, -PoolArea, -PoolQC, -MiscFeature, -MiscVal) %>% 
  mutate(MSSubClass = remove_classes(MSSubClass, 20))
=======
         -`2ndFlrSF`, -LowQualFinSF, -`3SsnPorch`, -PoolArea, -PoolQC, -MiscFeature, -MiscVal)

ames <- ames %>%
  mutate(MSSubClass = remove_classes(MSSubClass, N),
         MSZoning = remove_classes(MSZoning, N),
         LotShape = fct_collapse(LotShape,
                                 IR = c("IR1", "IR2", "IR3")),
         LandContour = remove_classes(LandContour, N),
         LotConfig = remove_classes(LotConfig, N),
         LandSlope = remove_classes(LandSlope, N),
         Neighborhood = remove_classes(Neighborhood, N),
         Condition1 = remove_classes(Condition1, N),
         BldgType = remove_classes(BldgType, N),
         HouseStyle = remove_classes(HouseStyle, N),
         YrSinceRemod = ifelse(YearRemodAdd > 1950, YrSold - YearRemodAdd, YrSold - YearBuilt)
         )
>>>>>>> 72185fc4b470eb2814257406acb7398ba25b8c70

dim(ames)
View(ames)

# MSSubClass - Building class, 15 levels, Identifies the type of dwelling involved in the sale.
ames %>% count(MSSubClass) %>% arrange(n)
ames %>% ggplot(aes(reorder(as.ordered(MSSubClass), SalePrice, FUN=median), SalePrice), SalePrice) + geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ as.factor(MSSubClass), data = ames))

# MSZoning, Identifies the general zoning classification of the sale, 5 levels
ames %>% count(MSZoning) %>% arrange(n)
ames %>% ggplot(aes(as.factor(MSZoning), SalePrice)) + geom_boxplot(varwidth = T)

# LotFrontage, integer values, Linear feet of street connected to property
# 50, 60, 65, 70, 75, 80, 85 most common
ames %>% ggplot(aes(LotFrontage)) + geom_histogram(binwidth = 1)
ames %>% count(LotFrontage) %>% na.omit() %>% arrange(desc(n))
ames %>% 
  ggplot(aes(LotFrontage, SalePrice)) + 
  geom_point(alpha = 0.1, width = 1, height = 1) +
  geom_smooth(method = "lm")

# LotArea, integer values, Lot size in square feet
ames %>% ggplot(aes(LotArea)) + geom_histogram(binwidth = 500) + xlim(0, 30000)
ames %>% count(LotArea) %>% na.omit() %>% arrange(desc(n))
ames %>% ggplot(aes(LotArea, SalePrice)) + 
  geom_point(alpha = 0.1, width = 1, height = 1) + 
  geom_smooth() +
  xlim(0, 30000)

# Street, street type pave or gravel, not enough diversity in data => remove feature
ames %>% count(Street) %>% na.omit() %>% arrange(desc(n))

# Alley, Type of alley access, not enough diversity in data => remove feature
ames %>% count(Alley) %>% arrange(desc(n))
ames %>% ggplot(aes(Alley, SalePrice)) + geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ Alley, data = ames))

# General shape of property, 4 levels (Reg Regular, IR1	Slightly irregular, IR2	Moderately Irregular,
# IR3	Irregular), collapse all irregular to one
ames %>% count(LotShape) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(LotShape), SalePrice)) + geom_boxplot(varwidth = T)
ames %>% ggplot(aes(SalePrice, y = ..density.., col = as.factor(LotShape))) + geom_freqpoly()
summary(lm(SalePrice ~ LotShape, data = ames))

# LandContour, Flatness of the property, 4 levels (Lvl - Near Flat/Level, 
# Bnk	Banked - Quick and significant rise from street grade to building,
# HLS	Hillside - Significant slope from side to side, Low -	Depression)
ames %>% count(LandContour) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(LandContour), SalePrice)) + geom_boxplot(varwidth = T)
ames %>% ggplot(aes(SalePrice, y = ..density.., col = as.factor(LandContour))) + geom_freqpoly(binwidth = 50000)
summary(lm(SalePrice ~ LandContour, data = ames))

# Utilities, Type of utilities available, not enough diversity in data => remove feature
ames %>% count(Utilities) %>% arrange(desc(n))

# LotConfig, Lot configuration, 5 levels (Inside - Inside lot, Corner - Corner lot, CulDSac - Cul-de-sac,
# FR2	Frontage on 2 sides of property, FR3	Frontage on 3 sides of property)
ames %>% count(LotConfig) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(LotConfig), SalePrice)) + geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ LotConfig, data = ames))

# LandSlope, Slope of property, not enough diversity in data => remove feature
ames %>% count(LandSlope) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(LandSlope), SalePrice)) + geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ LandSlope, data = ames))

# Neighborhood, Physical locations within Ames city limits, 25 levels
ames %>% count(Neighborhood) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(Neighborhood, SalePrice, FUN=median), SalePrice)) + geom_boxplot(varwidth = T) + coord_flip()
summary(lm(SalePrice ~ Neighborhood, data = ames))

# Condition1, Proximity to various conditions, 9 levels
ames %>% count(Condition1) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(Condition1, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T) + 
  coord_flip()
summary(lm(SalePrice ~ Condition1, data = ames))

# Condition2, Proximity to various conditions (if more than one is present), 9 levels,
# not enough diversity in data => remove feature
ames %>% count(Condition2) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(Condition2, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T) + 
  coord_flip()

# BldgType, Type of dwelling, 5 levels (1Fam - Single-family Detached, 
# 2FmCon - Two-family Conversion; originally built as one-family dwelling
# Duplx	- Duplex, TwnhsE - Townhouse End Unit, Twnhs - Townhouse Inside Unit)
ames %>% count(BldgType) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(BldgType, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T) + 
  coord_flip()
summary(lm(SalePrice ~ BldgType, data = ames))

# HouseStyle, Style of dwelling, 8 levels
ames %>% count(HouseStyle) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(HouseStyle, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T) + 
  coord_flip()
summary(lm(SalePrice ~ HouseStyle, data = ames))

# OverallQual, Rates the overall material and finish of the house, Integers 1-10
# Keep feature intact as integer for now
ames %>% count(OverallQual) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(OverallQual, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T) + 
  coord_flip()
summary(lm(SalePrice ~ (OverallQual), data = ames))

# OverallCond, Rates the overall condition of the house, Integers 1-9
# Keep feature intact as integer for now
ames %>% count(OverallCond) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(OverallCond, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T) + 
  coord_flip()
summary(lm(SalePrice ~ as.factor(OverallCond), data = ames))

# YearBuilt, Original construction date, integer value
ames %>% count(YearBuilt) %>% arrange(desc(n))
ames %>% ggplot(aes(YearBuilt, SalePrice)) + 
  geom_jitter(aes(col = as.factor(OverallQual)), width = 0.1, height = 0, alpha = 1) +
  geom_smooth(span = 0.7, method = "loess")
summary(lm(SalePrice ~ YearBuilt, data = ames))

# YearRemodAdd, Remodel date (same as construction date if no remodeling or additions), integer value
# Seems strange that the lower limit is 1950 with a lot of houses. Seems like an error. Probably
# a good idea to combine with YearBuilt in some way.
ames %>% count(YearRemodAdd) %>% arrange(desc(n))
ames %>% filter(YearRemodAdd > 1950) %>% ggplot(aes(YearRemodAdd)) + 
  geom_histogram(binwidth = 1)
ames %>% filter(YearRemodAdd > 1950) %>% ggplot(aes(YearRemodAdd, SalePrice)) + 
  geom_jitter(aes(col = as.factor(OverallQual)), width = 0.1, height = 0, alpha = 1) +
  geom_smooth(method = "loess")
summary(lm(SalePrice ~ YearRemodAdd, data = ames))

# YrSold: Year Sold (YYYY)
# Perhaps combine with year to get year+month
ames %>% count(YrSold) %>% arrange(desc(n))
ames %>% ggplot(aes(as.ordered(YrSold), SalePrice)) + 
  geom_boxplot(varwidth = T)
ames %>% ggplot(aes(YrSold)) + 
  geom_histogram(binwidth = 1)
summary(lm(SalePrice ~ as.factor(YrSold), data = ames))

# YrSinceRemod
ames %>% count(YrSinceRemod) %>% arrange(desc(n))
ames %>% ggplot(aes(YrSinceRemod)) + 
  geom_histogram(binwidth = 1)
summary(lm(SalePrice ~ YrSinceRemod, data = ames))
ames %>% ggplot(aes(YrSinceRemod, SalePrice)) + 
  geom_jitter(aes(col = as.factor(OverallQual)), width = 0.1, height = 0, alpha = 1) +
  geom_smooth(span = 0.7, method = "loess")






# RoofStyle: Type of roof, 6 levels
ames %>% count(RoofStyle) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(RoofStyle, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T) + 
  coord_flip()
summary(lm(SalePrice ~ RoofStyle, data = ames))

# RoofMatl: Roof material, 8 levels, not enough diversity in data => remove feature
ames %>% count(RoofMatl) %>% arrange(desc(n))

# Exterior1st: Exterior covering on house, 15 levels
ames %>% count(Exterior1st) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(Exterior1st, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T) + 
  coord_flip()
summary(lm(SalePrice ~ Exterior1st, data = ames))

# Exterior2nd: Exterior covering on house, (if more than one material) 16 levels
ames %>% count(Exterior2nd) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(Exterior2nd, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T) + 
  coord_flip()
summary(lm(SalePrice ~ Exterior2nd, data = ames))

# MasVnrType: Masonry veneer type, 4 levels (BrkCmn -	Brick Common, BrkFace - Brick Face,
# CBlock - Cinder Block, None -	None, Stone - Stone)
ames %>% count(MasVnrType) %>% arrange(desc(n))
ames %>% filter(!is.na(MasVnrType)) %>% ggplot(aes(reorder(MasVnrType, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ MasVnrType, data = ames))

# MasVnrArea: Masonry veneer area in square feet, integer value
# A lot of it at zero.
ames %>% count(MasVnrArea) %>% arrange(desc(n))
ames %>% ggplot(aes(MasVnrArea, SalePrice)) + 
  geom_jitter(width = 0.1, height = 0, alpha = 0.2) +
  geom_smooth()
summary(lm(SalePrice ~ MasVnrArea, data = ames))

# ExterQual: Evaluates the quality of the material on the exterior, 5 levels (Po poor, Fa fair, 
# TA typical/average, Gd Good, Excellent Ex)
ames %>% count(ExterQual) %>% arrange(desc(n)) 
ames %>% filter(!is.na(ExterQual)) %>% ggplot(aes(reorder(ExterQual, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ ExterQual, data = ames))

# ExterCond: Evaluates the present condition of the material on the exterior, 5 levels (Po poor,
# Fa fair, TA typical/average, Gd Good, Excellent Ex)
ames %>% count(ExterCond) %>% arrange(desc(n)) 
ames %>% ggplot(aes(reorder(ExterCond, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ ExterCond, data = ames))

# Foundation: Type of foundation, 5 levels (BrkTil	Brick & Tile, CBlock Cinder Block, 
# PConc	Poured Concrete, Slab Slab, Stone Stone, Wood, Wood)
ames %>% count(Foundation) %>% arrange(desc(n)) 
ames %>% ggplot(aes(reorder(Foundation, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ Foundation, data = ames))

# BsmtQual: Evaluates the height of the basement, 6 levels (Po poor,
# Fa fair, TA typical/average, Gd Good, Excellent Ex, NA No Basement) 
# How best handle the NA here? New variable HasBasement? Replace NA:s?
ames %>% count(BsmtQual) %>% arrange(desc(n)) 
ames %>% ggplot(aes(reorder(BsmtQual, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ BsmtQual, data = ames))

# BsmtCond: Evaluates the general condition of the basement, 6 levels (Po poor,
# Fa fair, TA typical/average, Gd Good, Excellent Ex, NA No Basement)
# How best handle the NA here? New variable HasBasement? Replace NA:s?
ames %>% count(BsmtCond) %>% arrange(desc(n)) 
ames %>% ggplot(aes(reorder(BsmtCond, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ BsmtCond, data = ames))

# BsmtExposure: Refers to walkout or garden level walls, 5 levels (Gd	Good Exposure,
# Av	Average Exposure (split levels or foyers typically score average or above),	
# Mn	Mimimum Exposure, No	No Exposure, NA	No Basement)
# How best handle the NA here? New variable HasBasement? Replace NA:s?
ames %>% count(BsmtExposure) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(BsmtExposure, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ BsmtExposure, data = ames))

###### Combine these variables in some other way ######
# BsmtFinType1: Rating of basement finished area, 7 levels
ames %>% count(BsmtFinType1) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(BsmtFinType1, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ BsmtFinType1, data = ames))

# BsmtFinSF1: Type 1 finished square feet, integer
ames %>% count(BsmtFinSF1) %>% arrange(desc(n))
ames %>% ggplot(aes(BsmtFinSF1, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")
summary(lm(SalePrice ~ BsmtFinSF1, data = ames))

# BsmtFinType2: Rating of basement finished area (if multiple types), 7 levels, remove?
ames %>% count(BsmtFinType2) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(BsmtFinType2, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ BsmtFinType2, data = ames))

# BsmtFinSF2: Type 2 finished square feet, integer, doesn't add much => remove variable
ames %>% count(BsmtFinSF2) %>% arrange(desc(n))
ames %>% ggplot(aes(BsmtFinSF2, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")
summary(lm(SalePrice ~ BsmtFinSF2, data = ames))

# BsmtUnfSF: Unfinished square feet of basement area
ames %>% count(BsmtUnfSF) %>% arrange(desc(n))
ames %>% ggplot(aes(BsmtUnfSF, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")
summary(lm(SalePrice ~ BsmtUnfSF, data = ames))

# TotalBsmtSF: Total square feet of basement area
ames %>% count(TotalBsmtSF) %>% arrange(desc(n))
ames %>% ggplot(aes(TotalBsmtSF, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess", span = 1)
summary(lm(SalePrice ~ TotalBsmtSF, data = ames))

######################################################

# Heating: Type of heating, 6 levels (Floor	Floor Furnace, GasA	Gas forced warm air furnace,
# GasW	Gas hot water or steam heat, Grav	Gravity furnace, OthW	Hot water or steam heat other than gas,
# Wall	Wall furnace). Not enough variety in feature => remove
ames %>% count(Heating) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(Heating, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)

# HeatingQC: Heating quality and condition (5 levels from Poor to Excellent)
ames %>% count(HeatingQC) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(HeatingQC, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ HeatingQC, data = ames))

# CentralAir: Central air conditioning, 2 levels(N No, Y	Yes)
ames %>% count(CentralAir) %>% arrange(desc(n))
ames %>% ggplot(aes(CentralAir, SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ CentralAir, data = ames))

# Electrical: Electrical system (5 levels, SBrkr	Standard Circuit Breakers & Romex,
# FuseA	Fuse Box over 60 AMP and all Romex wiring (Average), FuseF 60 AMP Fuse Box and 
# mostly Romex wiring (Fair), FuseP	60 AMP Fuse Box and mostly knob & tube wiring (poor),
# Mix	Mixed)
ames %>% count(Electrical) %>% arrange(desc(n))
ames %>% ggplot(aes(Electrical, SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ Electrical, data = ames))

####### Combine in any way? Also with Basement? #######
# 1stFlrSF: First Floor square feet, integer
ames %>% count(`1stFlrSF`) %>% arrange(desc(n))
ames %>% ggplot(aes(`1stFlrSF`, SalePrice)) + 
  geom_point(aes(col = as.ordered(OverallQual)), alpha = 1) +
  geom_smooth(method = "lm")
summary(lm(SalePrice ~ `1stFlrSF`, data = ames))

# 2ndFlrSF: Second floor square feet, integer
# How to deal with zeros?
ames %>% count(`2ndFlrSF`) %>% arrange(desc(n))
ames %>% ggplot(aes(`2ndFlrSF`, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")
summary(lm(SalePrice ~ `2ndFlrSF`, data = ames))

# LowQualFinSF: Low quality finished square feet (all floors), weak relation => remove feature
ames %>% count(LowQualFinSF) %>% arrange(desc(n))
ames %>% ggplot(aes(LowQualFinSF, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")
summary(lm(SalePrice ~ LowQualFinSF, data = ames))

# GrLivArea: Above grade (ground) living area square feet. Basically 1st+2nd floor area
ames %>% count(GrLivArea) %>% arrange(desc(n))
ames %>% ggplot(aes(GrLivArea, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")
summary(lm(SalePrice ~ GrLivArea, data = ames))

######################################################################

####### Combine in any way? ############

# BsmtFullBath: Basement full bathrooms, 4 levels 0-3. Replace with BasementHasBathroom
ames %>% count(BsmtFullBath) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(BsmtFullBath), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ as.factor(BsmtFullBath), data = ames))

# BsmtHalfBath: Basement half bathrooms, 3 levels 0-2
ames %>% count(BsmtHalfBath) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(BsmtHalfBath), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ as.factor(BsmtHalfBath), data = ames))

# FullBath: Full bathrooms above grade, 4 levels 0-3
ames %>% count(FullBath) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(FullBath), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ as.factor(FullBath), data = ames))

# HalfBath: Half baths above grade, 3 levels 0-2
ames %>% count(HalfBath) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(HalfBath), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ as.factor(HalfBath), data = ames))

######################################################################

# BedroomAbvGr: Bedrooms above grade (does NOT include basement bedrooms), 7 levels from 0-8 
ames %>% count(BedroomAbvGr) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(BedroomAbvGr), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ as.factor(BedroomAbvGr), data = ames))

# KitchenAbvGr: Kitchens above grade
ames %>% count(KitchenAbvGr) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(KitchenAbvGr), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ as.factor(KitchenAbvGr), data = ames))

# KitchenQual: Kitchen quality, 5 levels (Poor to Excellent)
ames %>% count(KitchenQual) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(KitchenQual, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ KitchenQual, data = ames))

# TotRmsAbvGrd: Total rooms above grade (does not include bathrooms)
ames %>% count(TotRmsAbvGrd) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(TotRmsAbvGrd), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ as.factor(TotRmsAbvGrd), data = ames))

# Functional: Home functionality (Assume typical unless deductions are warranted), 7 levels
# Collapse dysfunctional levels?
ames %>% count(Functional) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(Functional, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ Functional, data = ames))

# Fireplaces: Number of fireplaces
ames %>% count(Fireplaces) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(Fireplaces), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ as.factor(Fireplaces), data = ames))

# FireplaceQu: Fireplace quality, 6 levels (NA No fireplace, Poor to Excellent)
# How to deal with NA?
ames %>% count(FireplaceQu) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(FireplaceQu, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ as.factor(FireplaceQu), data = ames))

# GarageType: Garage location, 7 levels (NA is no garage)
# How to deal with NA?
ames %>% count(GarageType) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(GarageType, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ GarageType, data = ames))

# GarageYrBlt: Year garage was built, integer (NA no garage)
# Largely the same as YearBuilt (i.e. the house)
# How to deal with NA?
ames %>% count(GarageYrBlt) %>% arrange(desc(n))
ames %>% ggplot(aes(GarageYrBlt, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")
summary(lm(SalePrice ~ GarageYrBlt, data = ames))

# GarageFinish: Interior finish of the garage, 4 levels (Fin	Finished, RFn	Rough Finished,	
# Unf	Unfinished, NA	No Garage)
# How to deal with NA?
ames %>% count(GarageFinish) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(GarageFinish, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ GarageFinish, data = ames))

# GarageArea: Size of garage in square feet
# How to deal with zeros?
ames %>% count(GarageArea) %>% arrange(desc(n))
ames %>% ggplot(aes(GarageArea, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth()
summary(lm(SalePrice ~ GarageArea, data = ames))

# GarageQual: Garage quality, 4 levels (Poor to Excellent, NA	No Garage)
ames %>% count(GarageQual) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(GarageQual, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ GarageQual, data = ames))

# GarageCond: Garage condition, 4 levels (Poor to Excellent, NA	No Garage)
ames %>% count(GarageCond) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(GarageCond, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ GarageCond, data = ames))

# PavedDrive: Paved driveway, 3 levels (Y	Paved, P Partial Pavement, N Dirt/Gravel)
ames %>% count(PavedDrive) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(PavedDrive, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ PavedDrive, data = ames))

# WoodDeckSF: Wood deck area in square feet
ames %>% count(WoodDeckSF) %>% arrange(desc(n))
ames %>% ggplot(aes(WoodDeckSF, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth()
summary(lm(SalePrice ~ WoodDeckSF, data = ames))

# OpenPorchSF: Open porch area in square feet
ames %>% count(OpenPorchSF) %>% arrange(desc(n))
ames %>% ggplot(aes(OpenPorchSF, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth()
summary(lm(SalePrice ~ OpenPorchSF, data = ames))

# EnclosedPorch: Enclosed porch area in square feet
ames %>% count(EnclosedPorch) %>% arrange(desc(n))
ames %>% ggplot(aes(EnclosedPorch, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth()
summary(lm(SalePrice ~ EnclosedPorch, data = ames))

# 3SsnPorch: Three season porch area in square feet.
# Few houses that has it (24) => Remove variable
ames %>% count(`3SsnPorch`) %>% arrange(desc(n))
ames %>% ggplot(aes(`3SsnPorch`, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth()
summary(lm(SalePrice ~ `3SsnPorch`, data = ames))

# ScreenPorch: Screen porch area in square feet
ames %>% count(ScreenPorch) %>% arrange(desc(n))
ames %>% ggplot(aes(ScreenPorch, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")
summary(lm(SalePrice ~ ScreenPorch, data = ames))

# PoolArea: Pool area in square feet, too few > 0 => remove
ames %>% count(PoolArea) %>% arrange(desc(n))
ames %>% ggplot(aes(PoolArea, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")

# PoolQC: Pool quality, 4 levels (Fair to Excellent, NA	No Pool), too many NA's => remove
ames %>% count(PoolQC) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(PoolQC, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)

# Fence: Fence quality, 5 levels (GdPrv	Good Privacy, MnPrv	Minimum Privacy, GdWo	Good Wood,
# MnWw	Minimum Wood/Wire, NA	No Fence)
# Fix NA's
ames %>% count(Fence) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(Fence, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ Fence, data = ames))

# MiscFeature: Miscellaneous feature not covered in other categories, 6 levels (Elev	Elevator,
# Gar2	2nd Garage (if not described in garage section), Othr	Other, Shed	Shed (over 100 SF),
# TenC	Tennis Court, NA	None). Doesn't seem to affect price => remove
ames %>% count(MiscFeature) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(MiscFeature, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ MiscFeature, data = ames))

# MiscVal: $Value of miscellaneous feature. Doesn't seem to affect price => remove
ames %>% count(MiscVal) %>% arrange(desc(n))
ames %>% ggplot(aes(MiscVal, SalePrice)) + 
  geom_point(aes(col = MiscFeature)) +
  geom_smooth(method = "lm")
summary(lm(SalePrice ~ MiscVal, data = ames))

# MoSold: Month Sold (MM)
# Run through lubridate to get month names e.g.
# Perhaps combine with year to get year+month
ames %>% count(MoSold) %>% arrange(desc(n))
ames %>% ggplot(aes(as.ordered(MoSold), SalePrice)) + 
  geom_boxplot(varwidth = T)
ames %>% ggplot(aes(MoSold)) + 
  geom_histogram(binwidth = 1)
summary(lm(SalePrice ~ as.factor(MoSold), data = ames))

# SaleType: Type of sale, 10 levels
ames %>% count(SaleType) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(SaleType, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ as.factor(YrSold), data = ames))


# SaleCondition: Condition of sale, 6 levels
ames %>% count(SaleCondition) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(SaleCondition, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)
summary(lm(SalePrice ~ SaleCondition, data = ames))

# Add feature House
ames %>% mutate(HouseAge = YrSold - YearBuilt) %>% 
  ggplot(aes(HouseAge, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth()






# Remove rare levels from data, automate it
# Transform to ordered factors when needed
# Add feature age of house
# Remove known rooms from total rooms above grade?
# There are some pretty amazing outliers, take a closer look at them! 
# PCA possible on categorical data?: https://stats.stackexchange.com/questions/5774/can-principal-component-analysis-be-applied-to-datasets-containing-a-mix-of-cont













