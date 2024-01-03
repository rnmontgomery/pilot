# test data for predtest

weights = c(0.25, 0.25, 0.5, 0.5, 0.5, 0.75, 0.75, 0.75, 0.75)
results = c(1, 1, 0, 1, 1, 0, 1, 1, 0)
small_weights = c(.25, .5, .75)
small_results = c(1, 0, 1)
big_weights = c(0.01836526, 0.18172667, 0.93965174, 0.78673775, 0.70447507, 0.4173096,
                0.52666668, 0.60833636, 0.85556757, 0.90690988, 0.3314088,  0.30168604,
                0.71713002, 0.33582719, 0.79148375, 0.80775352, 0.78635773, 0.44854763,
                0.63254025, 0.73845882)
big_results = c(1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1)


# datasets for predweights
ID <- 1:30
buildtesting <- as.data.frame(ID)
buildtesting$group <- c(rep(0,15), rep(1,15))
buildtesting$v1 <- rnorm(30, 0, 1)
buildtesting$v2 <- rnorm(30, 0, 1)
buildtesting$v3 <- rnorm(30, 0, 1)
buildtesting$v4 <- rnorm(30, 0, 1)
buildtesting


# predweights(dataset= buildtesting, variables = c("v1", "v2", "v3", "v4"),
#  id = "ID2", type = "group", gtvar = "group")

ID2 <- c(rep(1:15,2))
buildtesting2 <- as.data.frame(ID2)
buildtesting2$time2 <- c(rep(0,15), rep(12,15))
buildtesting2$v1 <- rnorm(30, 0, 1)
buildtesting2$v2 <- rnorm(30, 0, 1)
buildtesting2$v3 <- rnorm(30, 0, 1)
buildtesting2$v4 <- rnorm(30, 0, 1)
buildtesting2
