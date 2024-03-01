#Attempt to Run DiD packages from Kyle Butts-CU Boulder


event_study = function(data, yname, idname, gname, tname,
                       xformla = NULL, weights = NULL,
                       estimator = c("all", "TWFE", "did2s", "did", "impute", "sunab",
                                     "staggered")
){