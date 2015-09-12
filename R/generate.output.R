# generate Y according to a particular design given X and W
generate.output <- function(input, design, seed) {
  if (missing(seed)) {
    set.seed(seed)
  }
  
  num.obs <- length(input$W)
  output <- rep(0, num.obs)
  w0Indices <- which(input$W == 0)
  w1Indices <- which(input$W == 1)
  num.w0 <- length(w0Indices)
  num.w1 <- length(w1Indices)
  num.vars <- ncol(input$X)
  if (design == 1) {
    output[w0Indices] <- rnorm(num.w0,  0 + input$X[w0Indices,1] + input$X[w0Indices,2], 1)
    output[w1Indices] <- rnorm(num.w1,  1 - input$X[w1Indices,1] + input$X[w1Indices,2], 1)
  } else if (design == 2) {
    output[w0Indices] <- rnorm(num.w0,  0 + input$X[w0Indices,1] + input$X[w0Indices,2] + input$X[w0Indices,3] + input$X[w0Indices,4] + input$X[w0Indices,5] + input$X[w0Indices,6] + input$X[w0Indices,7] + input$X[w0Indices,8] + input$X[w0Indices,9] + input$X[w0Indices,10], 1)
    output[w1Indices] <- rnorm(num.w1,  1 - input$X[w1Indices,1] + input$X[w1Indices,2] + input$X[w0Indices,3] + input$X[w0Indices,4] + input$X[w0Indices,5] + input$X[w0Indices,6] + input$X[w0Indices,7] + input$X[w0Indices,8] + input$X[w0Indices,9] + input$X[w0Indices,10], 1)
  } else if (design == 3) {
    output[w0Indices] <- rnorm(num.w0,  0 + input$X[w0Indices,1] + input$X[w0Indices,2], 0.1)
    output[w1Indices] <- rnorm(num.w1,  1 - input$X[w1Indices,1] + input$X[w1Indices,2], 0.1)
  } else {
    stop("design must be 1, 2, or 3")
  }
  output
}