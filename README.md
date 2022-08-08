# Association-Rule-Mining-using-R
Mining the Zoo dataset to obtain association rules that contain a mix of positive and negative terms.


# Installing Packages
>install.packages("tidyverse")
>install.packages("arules")
>install.packages("arulesViz")
>install.packages("mlbench")
>install.packages("dplyr")

# Loading Libraries
>library(tidyverse)
>library(arules)
>library(arulesViz)
>library(dplyr)

# Loading Dataset
>data(Zoo, package = "mlbench")
>head(Zoo)
>trans <- transactions(Zoo)

# The conversion gives a warning because only discrete features (factor and logical) can be directly translated into items. Continuous features need to be discretized first.

# Discretizing Continuous Features
>Zoo_has_legs <- Zoo %>% dplyr::mutate(legs = legs > 0)
>Zoo_discretized_legs <- Zoo %>% dplyr::mutate(legs = discretize(legs, breaks = 2, method="interval"))

# Converting data into a set of transactions
>trans <- transactions(Zoo_discretized_legs)
>trans

# Inspecting Transactions
>summary(trans)
>inspect(trans[1:3])

# Vertical Layout
>vertical <- as(trans, "tidLists")
>as(vertical, "matrix")[1:10, 1:5]

# Mining Frequent Itemsets
>2^ncol(trans)
>its <- apriori(trans, parameter=list(target = "frequent"))
>its
>5/nrow(trans)
>its <- apriori(trans, parameter=list(target = "frequent", support = 0.05))
>its

>its <- sort(its, by = "support")
>inspect(head(its, n = 10))

# Representing Itemsets
>its_max <- its[is.maximal(its)]
>its_max
inspect(head(its_max, by = "support"))

>its_closed <- its[is.closed(its)]
>its_closed
>inspect(head(its_closed, by = "support"))

# Mining association rules using Apriori
>rules <- apriori(trans, parameter = list(support = 0.05, confidence = 0.9))
>length(rules)
>inspect(head(rules))
>quality(head(rules))

# Checking Rules with highest lift
>rules <- sort(rules, by = "lift")
>inspect(head(rules, n = 10))

# Calculating Additional Interest Measures
>interestMeasure(rules[1:10], measure = c("phi", "gini"),
  trans = trans)
>interestMeasure(rules[1:10], measure = c("phi", "gini"),
  trans = trans)

# Adding Measures
>quality(rules) <- cbind(quality(rules),
  interestMeasure(rules, measure = c("phi", "gini"),
    trans = trans))

# Identifying best rules
>inspect(head(rules, by = "phi"))

# Visualization
>plot(rules)
>plot(rules, control = list(jitter = 0))
>plot(rules, method = "graph")
>plot(head(rules, by = "phi", n = 100), method = "graph")
