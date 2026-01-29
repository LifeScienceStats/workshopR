library(tidyverse)

# load data
penguins <- datasets::penguins
# object class is data.frame
class(penguins)
# print the data
penguins
# convert to tibble
penguins <- tibble::as_tibble(penguins)
# object class is tibble now
class(penguins)
# print the data
penguins
# we can also do the same with dataframes by uisnd head()
head(as.data.frame(penguins))


penguins_df <- as.data.frame(penguins)
penguins_tibble <- tibble::as_tibble(penguins)


# works on dataframes
penguins_df$sp
# does not work on tibbles
penguins_tibble$sp
penguins_tibble$species


penguins_tibble[,"species"]
class(penguins_tibble[,"species"]) # will stay a tibble

penguins_df[,"species"]
class(penguins_df[,"species"]) # will beocme a vector


# will return an error
penguins_tibble$new_var <- 5:6
# will recycle the input across the rows
penguins_df$new_var <- 5:6
penguins_df$new_var


data1 <- readxl::read_excel("filename.xlsx", sheet=2, skip=1)
data2 <- readxl::read_excel("filename.xlsx", sheet=3, skip=1)

data_combined <- merge(data1, data2[c("SubjectId", "OF_MP_STATCD")], by="SubjectId")

data_combined <- data1 %>% merge(y=data2, by="SubjectId")
data_combined <- data1 %>% full_join(y=data2, by="SubjectId")

data1[1:5, "SiteSeq"]

filter(data1, as.numeric(data1$REG_AGE)>60)

test <- data1 %>%
  mutate(
    REG_AGE = as.numeric(REG_AGE),
    AGE_MEAN = mean(REG_AGE, na.rm=TRUE)) %>%
  filter(REG_AGE > 60)

data1 %>% 
  select(contains("REG"))

data$new_var <- 1

names(data1)
names(data1)[2] <- "new_name"

test <- data1 %>% rename_with(~ gsub("REG_", "XYZ", .x))

names(test)

x <- 1:190

data1 %>% mutate(new_var = x * 2)

formula_model <- as.formula("as.numeric(REG_AGE) ~ SubjectSeq")

model <- lm(formula_model, data=data1)

summary(model)

