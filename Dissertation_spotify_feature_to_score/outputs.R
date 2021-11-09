capture.output(round(cbind(mae,rmse),4), file = "reg.txt")
capture.output(cartfit, cartfit$finalModel, cnfcart, file= "cartfit.txt")
capture.output(glmfit_0, file = "glmfit_cv.txt")
capture.output(glm_final, glm_final$df, glm_final$beta, glm_final$a0, file = "glm.txt")

