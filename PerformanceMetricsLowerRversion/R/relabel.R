relabel <- function (labels, input_name) {

  input_labels <- levels(labels)

  cond_temp <- (identical(input_labels, c("case", "non-case"))   |
                  identical(input_labels, c("Case", "Non-case")) |
                  identical(input_labels, c("case", "noncase"))  |
                  identical(input_labels, c("Case", "Noncase")))

  if (cond_temp) {
    levels(labels) <- c("1", "0")
    message("Note: Class labels from '", input_name, "' have been switched from (",
            paste(input_labels[1], input_labels[2], sep = ","), ") to (1,0).")
  } else {
    levels(labels) <- c("0", "1")
    if (!(identical(input_labels, c("0", "1")))) {
      message("Note: Class labels from '", input_name, "' have been switched from (",
              paste(input_labels[1], input_labels[2], sep = ","), ") to (0,1).")
    }
  }
  labels <- as.numeric(as.character(labels)) # turn into numeric vector of 0s and 1s
  return(labels)
}
