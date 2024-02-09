caluclate_churn_prob <- function(dataset, id){
  # The function should check if the customer id provided exists in the dataset and throw an error otherwise.
  if (!(id %in% dataset$CustomerId)) {
    stop("Customer ID not found in the dataset.")
  }
  # create a prediciton model
  model <- glm(Exited ~ CreditScore + Gender + Age + Tenure + Balance +
                 NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary,
               data = dataset, family = binomial)
  # predict churn
  dataset$churn_prob <- predict(model, dataset, type = "response")
  # return churn of specific customer
  return(dataset$churn_prob[dataset$CustomerId==id])
}
