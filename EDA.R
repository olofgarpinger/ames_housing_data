library(tidyverse)

ames <- read_csv("ames_train.csv")
dim(ames)
View(ames)

# MSSubClass - Building class, 15 levels, what does it mean?
ames %>% count(MSSubClass) %>% arrange(n)
ames %>% ggplot(aes(as.ordered(MSSubClass), SalePrice)) + geom_boxplot(varwidth = T)

# MSZoning, 5 levels
# Residential Low Density Zone "RL" (sec 29.701)
# Residential Medium Density Zone "RM" (sec 29.702)
# Residential High Density Zone "RH" (sec 29.704)
# Floating Village Residential (FV)
# C (all)? Commercial districts?
ames %>% count(MSZoning) %>% arrange(n)
ames %>% ggplot(aes(as.factor(MSZoning), SalePrice)) + geom_boxplot(varwidth = T)

# LotFrontage, integer values, Linear feet of street connected to property
# 50, 60, 65, 70, 75, 80, 85 most common
ames %>% ggplot(aes(LotFrontage)) + geom_histogram(binwidth = 1)
ames %>% count(LotFrontage) %>% na.omit() %>% arrange(desc(n))
ames %>% ggplot(aes(LotFrontage, SalePrice)) + geom_point(alpha = 0.1, width = 1, height = 1)

# LotArea, integer values, Lot size in square feet
ames %>% ggplot(aes(LotArea)) + geom_histogram(binwidth = 500) + xlim(0, 30000)
ames %>% count(LotArea) %>% na.omit() %>% arrange(desc(n))
ames %>% ggplot(aes(LotArea, SalePrice)) + geom_point(alpha = 0.1, width = 1, height = 1) + xlim(0, 30000)

# Street, street type pave or gravel, not enough diversity in data => remove feature
ames %>% count(Street) %>% na.omit() %>% arrange(desc(n))

# Alley, Type of alley access, not enough diversity in data => remove feature
ames %>% count(Alley) %>% arrange(desc(n))

# General shape of property, 4 levels (Reg Regular, IR1	Slightly irregular, IR2	Moderately Irregular,
# IR3	Irregular), collapse all irregular to one?
ames %>% count(LotShape) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(LotShape), SalePrice)) + geom_boxplot(varwidth = T)
ames %>% ggplot(aes(SalePrice, y = ..density.., col = as.factor(LotShape))) + geom_freqpoly()

# LandContour, Flatness of the property, 4 levels (Lvl - Near Flat/Level, 
# Bnk	Banked - Quick and significant rise from street grade to building,
# HLS	Hillside - Significant slope from side to side, Low -	Depression)
ames %>% count(LandContour) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(LandContour), SalePrice)) + geom_boxplot(varwidth = T)
ames %>% ggplot(aes(SalePrice, y = ..density.., col = as.factor(LandContour))) + geom_freqpoly(binwidth = 50000)

# Utilities, Type of utilities available, not enough diversity in data => remove feature
ames %>% count(Utilities) %>% arrange(desc(n))

# LotConfig, Lot configuration, 5 levels (Inside - Inside lot, Corner - Corner lot, CulDSac - Cul-de-sac,
# FR2	Frontage on 2 sides of property, FR3	Frontage on 3 sides of property)
ames %>% count(LotConfig) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(LotConfig), SalePrice)) + geom_boxplot(varwidth = T)

# LandSlope, Slope of property, not enough diversity in data => remove feature
ames %>% count(LandSlope) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(LandSlope), SalePrice)) + geom_boxplot(varwidth = T)

# Neighborhood, Physical locations within Ames city limits, 25 levels
ames %>% count(Neighborhood) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(Neighborhood, SalePrice, FUN=median), SalePrice)) + geom_boxplot(varwidth = T) + coord_flip()

# Condition1, Proximity to various conditions, 9 levels
ames %>% count(Condition1) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(Condition1, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T) + 
  coord_flip()

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

# HouseStyle, Style of dwelling, 8 levels
ames %>% count(HouseStyle) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(HouseStyle, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T) + 
  coord_flip()

# OverallQual, Rates the overall material and finish of the house, Integers 1-10
ames %>% count(OverallQual) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(OverallQual, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T) + 
  coord_flip()

# OverallCond, Rates the overall condition of the house, Integers 1-9
ames %>% count(OverallCond) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(OverallCond, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T) + 
  coord_flip()

# YearBuilt, Original construction date, integer value
ames %>% count(YearBuilt) %>% arrange(desc(n))
ames %>% ggplot(aes(YearBuilt, SalePrice)) + 
  geom_jitter(aes(col = as.factor(OverallQual)), width = 0.1, height = 0, alpha = 1) +
  geom_smooth(method = "lm")

# YearRemodAdd, Remodel date (same as construction date if no remodeling or additions), integer value
# Seems strange that the lower limit is 1950 with a lot of houses. Seems like an error.
ames %>% count(YearRemodAdd) %>% arrange(desc(n))
ames %>% ggplot(aes(YearRemodAdd, SalePrice)) + 
  geom_jitter(aes(col = as.factor(OverallQual)), width = 0.1, height = 0, alpha = 1) +
  geom_smooth(method = "lm")

# RoofStyle: Type of roof, 6 levels
ames %>% count(RoofStyle) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(RoofStyle, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T) + 
  coord_flip()

# RoofMatl: Roof material, 8 levels, not enough diversity in data => remove feature
ames %>% count(RoofMatl) %>% arrange(desc(n))

# Exterior1st: Exterior covering on house, 15 levels
ames %>% count(Exterior1st) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(Exterior1st, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T) + 
  coord_flip()

# Exterior2nd: Exterior covering on house, (if more than one material) 16 levels
ames %>% count(Exterior2nd) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(Exterior2nd, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T) + 
  coord_flip()

# MasVnrType: Masonry veneer type, 4 levels (BrkCmn -	Brick Common, BrkFace - Brick Face,
# CBlock - Cinder Block, None -	None, Stone - Stone)
ames %>% count(MasVnrType) %>% arrange(desc(n))
ames %>% filter(!is.na(MasVnrType)) %>% ggplot(aes(reorder(MasVnrType, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T) + 
  coord_flip()

# MasVnrArea: Masonry veneer area in square feet, integer value
# A lot of it at zero.
ames %>% count(MasVnrArea) %>% arrange(desc(n))
ames %>% ggplot(aes(MasVnrArea, SalePrice)) + 
  geom_jitter(width = 0.1, height = 0, alpha = 0.2) +
  geom_smooth(method = "lm")

# ExterQual: Evaluates the quality of the material on the exterior, 5 levels (Po poor, Fa fair, 
# TA typical/average, Gd Good, Excellent Ex)
ames %>% count(ExterQual) %>% arrange(desc(n)) 
ames %>% filter(!is.na(ExterQual)) %>% ggplot(aes(reorder(ExterQual, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)

# ExterCond: Evaluates the present condition of the material on the exterior, 5 levels (Po poor,
# Fa fair, TA typical/average, Gd Good, Excellent Ex)
ames %>% count(ExterCond) %>% arrange(desc(n)) 
ames %>% ggplot(aes(reorder(ExterCond, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)

# Foundation: Type of foundation, 5 levels (BrkTil	Brick & Tile, CBlock Cinder Block, 
# PConc	Poured Concrete, Slab Slab, Stone Stone, Wood, Wood)
ames %>% count(Foundation) %>% arrange(desc(n)) 
ames %>% ggplot(aes(reorder(Foundation, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)

# BsmtQual: Evaluates the height of the basement, 6 levels (Po poor,
# Fa fair, TA typical/average, Gd Good, Excellent Ex, NA No Basement) 
# How best handle the NA here? New variable HasBasement? Replace NA:s?
ames %>% count(BsmtQual) %>% arrange(desc(n)) 
ames %>% ggplot(aes(reorder(BsmtQual, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)

# BsmtCond: Evaluates the general condition of the basement, 6 levels (Po poor,
# Fa fair, TA typical/average, Gd Good, Excellent Ex, NA No Basement)
# How best handle the NA here? New variable HasBasement? Replace NA:s?
ames %>% count(BsmtCond) %>% arrange(desc(n)) 
ames %>% ggplot(aes(reorder(BsmtCond, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)

# BsmtExposure: Refers to walkout or garden level walls, 5 levels (Gd	Good Exposure,
# Av	Average Exposure (split levels or foyers typically score average or above),	
# Mn	Mimimum Exposure, No	No Exposure, NA	No Basement)
# How best handle the NA here? New variable HasBasement? Replace NA:s?
ames %>% count(BsmtExposure) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(BsmtExposure, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)

# BsmtFinType1: Rating of basement finished area, 7 levels
ames %>% count(BsmtFinType1) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(BsmtFinType1, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)

# BsmtFinSF1: Type 1 finished square feet, integer
ames %>% count(BsmtFinSF1) %>% arrange(desc(n))
ames %>% ggplot(aes(BsmtFinSF1, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")

# BsmtFinType2: Rating of basement finished area (if multiple types), 7 levels
ames %>% count(BsmtFinType2) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(BsmtFinType2, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)

# BsmtFinSF2: Type 2 finished square feet, integer, doesn't add much => remove variable
ames %>% count(BsmtFinSF2) %>% arrange(desc(n))
ames %>% ggplot(aes(BsmtFinSF2, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")

# BsmtUnfSF: Unfinished square feet of basement area
ames %>% count(BsmtUnfSF) %>% arrange(desc(n))
ames %>% ggplot(aes(BsmtUnfSF, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")

# TotalBsmtSF: Total square feet of basement area
ames %>% count(TotalBsmtSF) %>% arrange(desc(n))
ames %>% ggplot(aes(TotalBsmtSF, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")

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

# CentralAir: Central air conditioning, 2 levels(N	No, Y	Yes)
ames %>% count(CentralAir) %>% arrange(desc(n))
ames %>% ggplot(aes(CentralAir, SalePrice)) + 
  geom_boxplot(varwidth = T)

# Electrical: Electrical system (5 levels, SBrkr	Standard Circuit Breakers & Romex,
# FuseA	Fuse Box over 60 AMP and all Romex wiring (Average), FuseF	60 AMP Fuse Box and 
# mostly Romex wiring (Fair), FuseP	60 AMP Fuse Box and mostly knob & tube wiring (poor),
# Mix	Mixed)
ames %>% count(Electrical) %>% arrange(desc(n))
ames %>% ggplot(aes(Electrical, SalePrice)) + 
  geom_boxplot(varwidth = T)

# 1stFlrSF: First Floor square feet, integer
ames %>% count(`1stFlrSF`) %>% arrange(desc(n))
ames %>% ggplot(aes(`1stFlrSF`, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")

# 2ndFlrSF: Second floor square feet, integer
# How to deal with zeros?
ames %>% count(`2ndFlrSF`) %>% arrange(desc(n))
ames %>% ggplot(aes(`2ndFlrSF`, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")

# LowQualFinSF: Low quality finished square feet (all floors), weak relation => remove feature
ames %>% count(LowQualFinSF) %>% arrange(desc(n))
ames %>% ggplot(aes(LowQualFinSF, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")

# GrLivArea: Above grade (ground) living area square feet. Basically 1st+2nd floor area
ames %>% count(GrLivArea) %>% arrange(desc(n))
ames %>% ggplot(aes(GrLivArea, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")

# BsmtFullBath: Basement full bathrooms, 4 levels 0-3
ames %>% count(BsmtFullBath) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(BsmtFullBath), SalePrice)) + 
  geom_boxplot(varwidth = T)

# BsmtHalfBath: Basement half bathrooms, 3 levels 0-2, doesn't seem significant => remove
ames %>% count(BsmtHalfBath) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(BsmtHalfBath), SalePrice)) + 
  geom_boxplot(varwidth = T)

# FullBath: Full bathrooms above grade, 4 levels 0-3
ames %>% count(FullBath) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(FullBath), SalePrice)) + 
  geom_boxplot(varwidth = T)

# HalfBath: Half baths above grade, 3 levels 0-2
ames %>% count(HalfBath) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(HalfBath), SalePrice)) + 
  geom_boxplot(varwidth = T)

# BedroomAbvGr: Bedrooms above grade (does NOT include basement bedrooms), 7 levels from 0-8 
ames %>% count(BedroomAbvGr) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(BedroomAbvGr), SalePrice)) + 
  geom_boxplot(varwidth = T)

# KitchenAbvGr: Kitchens above grade
ames %>% count(KitchenAbvGr) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(KitchenAbvGr), SalePrice)) + 
  geom_boxplot(varwidth = T)

# KitchenQual: Kitchen quality, 5 levels (Poor to Excellent)
ames %>% count(KitchenQual) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(KitchenQual, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)

# TotRmsAbvGrd: Total rooms above grade (does not include bathrooms)
ames %>% count(TotRmsAbvGrd) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(TotRmsAbvGrd), SalePrice)) + 
  geom_boxplot(varwidth = T)

# Functional: Home functionality (Assume typical unless deductions are warranted), 7 levels
# Collapse dysfunctional levels?
ames %>% count(Functional) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(Functional, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)

# Fireplaces: Number of fireplaces
ames %>% count(Fireplaces) %>% arrange(desc(n))
ames %>% ggplot(aes(as.factor(Fireplaces), SalePrice)) + 
  geom_boxplot(varwidth = T)

# FireplaceQu: Fireplace quality, 6 levels (NA No fireplace, Poor to Excellent)
# How to deal with NA?
ames %>% count(FireplaceQu) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(FireplaceQu, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)

# GarageType: Garage location, 7 levels (NA is no garage)
# How to deal with NA?
ames %>% count(GarageType) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(GarageType, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)

# GarageYrBlt: Year garage was built, integer (NA no garage)
# Largely the same as YearBuilt (i.e. the house)
# How to deal with NA?
ames %>% count(GarageYrBlt) %>% arrange(desc(n))
ames %>% ggplot(aes(GarageYrBlt, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")

# GarageFinish: Interior finish of the garage, 4 levels (Fin	Finished, RFn	Rough Finished,	
# Unf	Unfinished, NA	No Garage)
# How to deal with NA?
ames %>% count(GarageFinish) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(GarageFinish, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)

# GarageArea: Size of garage in square feet
# How to deal with zeros?
ames %>% count(GarageArea) %>% arrange(desc(n))
ames %>% ggplot(aes(GarageArea, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")

# GarageQual: Garage quality, 4 levels (Poor to Excellent, NA	No Garage)
ames %>% count(GarageQual) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(GarageQual, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)

# GarageCond: Garage condition, 4 levels (Poor to Excellent, NA	No Garage)
ames %>% count(GarageCond) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(GarageCond, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)

# PavedDrive: Paved driveway, 3 levels (Y	Paved, P Partial Pavement, N Dirt/Gravel)
ames %>% count(PavedDrive) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(PavedDrive, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)

# WoodDeckSF: Wood deck area in square feet
ames %>% count(WoodDeckSF) %>% arrange(desc(n))
ames %>% ggplot(aes(WoodDeckSF, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth()

# OpenPorchSF: Open porch area in square feet
ames %>% count(OpenPorchSF) %>% arrange(desc(n))
ames %>% ggplot(aes(OpenPorchSF, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth()

# EnclosedPorch: Enclosed porch area in square feet
ames %>% count(EnclosedPorch) %>% arrange(desc(n))
ames %>% ggplot(aes(EnclosedPorch, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth()

# 3SsnPorch: Three season porch area in square feet.
# Few houses that has it (24)
ames %>% count(`3SsnPorch`) %>% arrange(desc(n))
ames %>% ggplot(aes(`3SsnPorch`, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth()

# ScreenPorch: Screen porch area in square feet
ames %>% count(ScreenPorch) %>% arrange(desc(n))
ames %>% ggplot(aes(ScreenPorch, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")

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

# MiscFeature: Miscellaneous feature not covered in other categories, 6 levels (Elev	Elevator,
# Gar2	2nd Garage (if not described in garage section), Othr	Other, Shed	Shed (over 100 SF),
# TenC	Tennis Court, NA	None). Doesn't seem to affect price => remove
ames %>% count(MiscFeature) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(MiscFeature, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)

# MiscVal: $Value of miscellaneous feature. Doesn't seem to affect price => remove
ames %>% count(MiscVal) %>% arrange(desc(n))
ames %>% ggplot(aes(MiscVal, SalePrice)) + 
  geom_point(aes(col = MiscFeature)) +
  geom_smooth(method = "lm")

# MoSold: Month Sold (MM)
# Run through lubridate to get month names e.g.
# Perhaps combine with year to get year+month
ames %>% count(MoSold) %>% arrange(desc(n))
ames %>% ggplot(aes(as.ordered(MoSold), SalePrice)) + 
  geom_boxplot(varwidth = T)

# YrSold: Year Sold (YYYY)
# Perhaps combine with year to get year+month
ames %>% count(YrSold) %>% arrange(desc(n))
ames %>% ggplot(aes(as.ordered(YrSold), SalePrice)) + 
  geom_boxplot(varwidth = T)

# SaleType: Type of sale, 10 levels
ames %>% count(SaleType) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(SaleType, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)


# SaleCondition: Condition of sale, 6 levels
ames %>% count(SaleCondition) %>% arrange(desc(n))
ames %>% ggplot(aes(reorder(SaleCondition, SalePrice, FUN=median), SalePrice)) + 
  geom_boxplot(varwidth = T)

# Add feature House
ames %>% mutate(HouseAge = YrSold - YearBuilt) %>% 
  ggplot(aes(HouseAge, SalePrice)) + 
  geom_point(alpha = 0.1) +
  geom_smooth()






# Remove rare levels from data, automate it
# Transform to ordered factors when needed
# Add feature age of house
# Remove known rooms from total rooms above grade?
# PCA possible on categorical data?: https://stats.stackexchange.com/questions/5774/can-principal-component-analysis-be-applied-to-datasets-containing-a-mix-of-cont

ames %>% 
  select(-(Street:Alley), -Utilities, -LandSlope, -Condition2, -RoofMatl, -BsmtFinSF2, -Heating,
         -`1stFlrSF`, -`2ndFlrSF`, -LowQualFinSF, -BsmtHalfBath, -PoolArea, -PoolQC, -MiscFeature,
         -MiscVal)












