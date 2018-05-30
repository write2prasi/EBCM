PlotG <- function(history)
{
  plot(history)
  plot(
    history$metrics$loss,
    main = "Model Loss for Training Data",
    xlab = "epoch",
    ylab = "loss",
    col = "blue",
    type = "l"
  )
  lines(history$metrics$val_loss, col = "green")
  legend(
    "topright",
    c("train", "test"),
    col = c("blue", "green"),
    lty = c(1, 1)
  )
  
  plot(
    history$metrics$val_loss,
    main = "Model Loss for Testing Data",
    xlab = "epoch",
    ylab = "val_loss",
    col = "green",
    type = "l"
  )
  lines(history$metrics$loss, col = "blue")
  legend(
    "topright",
    c("train", "test"),
    col = c("blue", "green"),
    lty = c(1, 1)
  )
  
  
  plot(
    history$metrics$acc,
    main = "Model Accuracy with Training Data",
    xlab = "epoch",
    ylab = "accuracy",
    col = "blue",
    type = "l"
  )
  lines(history$metrics$val_acc, col = "green")
  legend(
    "bottomright",
    c("train", "test"),
    col = c("blue", "green"),
    lty = c(1, 1)
  )
  
  plot(
    history$metrics$val_acc,
    main = "Model Accuracy with Testing Data",
    xlab = "epoch",
    ylab = "accuracy",
    col = "green",
    type = "l"
  )
  lines(history$metrics$acc, col = "blue")
  legend(
    "bottomright",
    c("train", "test"),
    col = c("blue", "green"),
    lty = c(1, 1)
  )
  
}