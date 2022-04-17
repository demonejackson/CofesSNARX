#'  CoFESNARX
#'
#'  \code{CoFESNARX} returns a compiled NARX model via Keras
#'
#'  This function takes target variable series, exogenous variables series, and the number of desired
#'  lags for each group respectively and then returns the requested lag series for each in a single
#'  data set.  Ydelay/Xdelay are vectors that holds the consecutive delays required for the target
#'  variable/exogenous series (ex 1:3 or 4:5)
#'
#' @param y,x numeric vectors for the target and exegenous variables
#' @param dim numberic vector for the dimension of the deisred netowrk
#' @param dr_o drop out rate used between hidden layers
#' @param KL2 the L2 kernel_regularizerregularization factor for hidden layer kernal regularization
#' @param BL2 the L2 bias_regularizer regularization factor for hidden layer bias regularization
#' @param act activation funciton used, if not specified 'relu' used
#' @return returns the target combined with the original and lagged exegenous
#'  variables in one database.
#'
#'
#' @export
CoFESNARX <-  function(x, y, dim, KL2 = .01, BL2 = .01,  dr_o = 0, act = 'relu'){
  # This function determine the size of the model and builds the same model with
  # different number of requested nodes.  Dim should be entered using c() i.e.
  # c(10,5) for a two hidden layers with 10 nodes in the first layer and 5
  # nodes in the second.
  model <- keras::keras_model_sequential()
  if (length(dim)==1) {
    model %>%
      keras::layer_dense(units = dim[1], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2),
                  input_shape = ncol(x)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = 1, activation = 'linear',
                  kernel_regularizer= keras::regularizer_l2(l = KL2))

  } else if  (length(dim)==2) {
    model %>%
      keras::layer_dense(units = dim[1], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2),
                  input_shape = ncol(x)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[2], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = 1, activation = 'linear',
                  kernel_regularizer= keras::regularizer_l2(l = KL2))
  } else if  (length(dim)==3) {
    model %>%
      keras::layer_dense(units = dim[1], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2),
                  input_shape = ncol(x)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[2], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[3], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = 1, activation = 'linear',
                  kernel_regularizer= keras::regularizer_l2(l = KL2))
  } else if  (length(dim)==4) {
    model %>%
      keras::layer_dense(units = dim[1], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2),
                  input_shape = ncol(x)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[2], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[3], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[4], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = 1, activation = 'linear',
                  kernel_regularizer= keras::regularizer_l2(l = KL2))

  } else if  (length(dim)==5) {
    model %>%
      keras::layer_dense(units = dim[1], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2),
                  input_shape = ncol(x)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[2], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[3], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[4], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[5], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = 1, activation = 'linear',
                  kernel_regularizer= keras::regularizer_l2(l = KL2))
  } else if  (length(dim)==6) {
    model %>%
      keras::layer_dense(units = dim[1], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2),
                  input_shape = ncol(x)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[2], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[3], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[4], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[5], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[6], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = 1, activation = 'linear',
                  kernel_regularizer= keras::regularizer_l2(l = KL2))

  } else if  (length(dim)==7) {
    model %>%
      keras::layer_dense(units = dim[1], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2),
                  input_shape = ncol(x)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[2], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[3], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[4], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[5], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[6], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[7], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = 1, activation = 'linear',
                  kernel_regularizer= keras::regularizer_l2(l = KL2))

  } else if  (length(dim)==8) {
    model %>%
      keras::layer_dense(units = dim[1], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2),
                  input_shape = ncol(x)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[2], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[3], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[4], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[5], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[6], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[7], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[8], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = 1, activation = 'linear',
                  kernel_regularizer= keras::regularizer_l2(l = KL2))

  } else if  (length(dim)==9) {
    model %>%
      keras::layer_dense(units = dim[1], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2),
                  input_shape = ncol(x)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[2], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[3], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[4], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[5], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[6], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[7], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[8], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[9], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = 1, activation = 'linear',
                  kernel_regularizer= keras::regularizer_l2(l = KL2))

  } else if  (length(dim)==10) {
    model %>%


      keras::layer_dense(units = dim[1], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2),input_shape = ncol(x)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[2], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[3], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[4], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[5], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[6], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[7], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[8], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[9], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[10], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = 1, activation = 'linear',
                  kernel_regularizer= keras::regularizer_l2(l = KL2))
  } else if  (length(dim)==11) {
    model %>%
      keras::layer_dense(units = dim[1], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2),
                  input_shape = ncol(x)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[2], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[3], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[4], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[5], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[6], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[7], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[8], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[9], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[10], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[11], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = 1, activation = 'linear',
                  kernel_regularizer= keras::regularizer_l2(l = KL2))

  } else if  (length(dim)==12) {
    model %>%
      keras::layer_dense(units = dim[1], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2),
                  input_shape = ncol(x)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[2], activation =
                    act, kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[3], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[4], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[5], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[6], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[7], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[8], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[9], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[10], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[11], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[12], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = 1, activation = 'linear',
                  kernel_regularizer= keras::regularizer_l2(l = KL2))


  } else if  (length(dim)==13) {
    model %>%
      keras::layer_dense(units = dim[1], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2),
                  input_shape = ncol(x)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[2], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[3], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[4], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[5], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[6], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[7], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[8], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[9], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[10], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[11], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[12], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[13], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = 1, activation = 'linear',
                  kernel_regularizer= keras::regularizer_l2(l = KL2))

  } else if  (length(dim)==14) {

    model %>%
      keras::layer_dense(units = dim[1], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2),
                  input_shape = ncol(x)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[2], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[3], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[4], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[5], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[6], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[7], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[8], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[9], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[10], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[11], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[12], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[13], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[14], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = 1, activation = 'linear',
                  kernel_regularizer= keras::regularizer_l2(l = KL2))

  } else if  (length(dim)==15) {
    model %>%
      keras::layer_dense(units = dim[1], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2),input_shape = ncol(x)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[2], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[3], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[4], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[5], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[6], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[7], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[8], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[9], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[10], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[11], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[12], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[13], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[14], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[15], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = 1, activation = 'linear',
                  kernel_regularizer= keras::regularizer_l2(l = KL2))

  } else if  (length(dim)==16) {

    model %>%
      keras::layer_dense(units = dim[1], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2),
                  input_shape = ncol(x)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[2], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[3], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[4], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[5], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[6], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[7], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[8], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[9], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[10], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[11], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[12], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[13], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[14], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[15], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[16], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = 1, activation = 'linear',
                  kernel_regularizer= keras::regularizer_l2(l = KL2))

  } else if  (length(dim)==17) {

    model %>%
      keras::layer_dense(units = dim[1], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2),
                  input_shape = ncol(x)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[2], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[3], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[4], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[5], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[6], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[7], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[8], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[9], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[10], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[11], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[12], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[13], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[14], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[15], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[16], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[17], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = 1, activation = 'linear',
                  kernel_regularizer= keras::regularizer_l2(l = KL2))

  } else if  (length(dim)==18) {

    model %>%
      keras::layer_dense(units = dim[1], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2),input_shape = ncol(x)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[2], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[3], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[4], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[5], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[6], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[7], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[8], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[9], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[10], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[11], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[12], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[13], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[14], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[15], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[16], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[17], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[18], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = 1, activation = 'linear',
                  kernel_regularizer= keras::regularizer_l2(l = KL2))


  } else if  (length(dim)==19) {

    model %>%

      keras::layer_dense(units = dim[1], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2),
                  input_shape = ncol(x)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[2], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[3], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[4], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[5], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[6], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[7], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[8], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[9], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[10], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[11], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[12], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[13], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[14], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[15], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[16], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[17], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[18], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[19], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = 1, activation = 'linear',
                  kernel_regularizer= keras::regularizer_l2(l = KL2))


  } else if (length(dim)==20) {
    model %>%
      keras::layer_dense(units = dim[1], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2),
                  input_shape =  ncol(x) ) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[2], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[3], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[4], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[5], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[6], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[7], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[8], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[9], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[10], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[11], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[12], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[13], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[14], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[15], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[16], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[17], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[18], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[19], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = dim[20], activation = act,
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2)) %>%
      keras::layer_dropout(rate = dr_o) %>%
      keras::layer_dense(units = 1, activation = 'linear',
                  kernel_regularizer= keras::regularizer_l2(l = KL2),
                  bias_regularizer = keras::regularizer_l2(l = BL2))
  } else {
    print("Your specification is too large")}
  summary(model)
  return(model)
}


# Activations:
#'softmax', 'elu', 'softplus', 'softsign', 'relu', 'tanh', 'sigmoid', 'hard_sigmoid', linear'


# Optimizers
#SGD(lr = 0.01, momentum = 0, decay = 0, nesterov = FALSE, clipnorm = -1, clipvalue = -1)

#RMSprop(lr = 0.001, rho = 0.9, epsilon = 1e-08, decay = 0,clipnorm = -1, clipvalue = -1)

#Adagrad(lr = 0.01, epsilon = 1e-08, decay = 0, clipnorm = -1,clipvalue = -1)

#Adadelta(lr = 1, rho = 0.95, epsilon = 1e-08, decay = 0,clipnorm = -1, clipvalue = -1)

#Adam(lr = 0.001, beta_1 = 0.9, beta_2 = 0.999, epsilon = 1e-08, decay = 0, clipnorm = -1, clipvalue = -1)

#Adamax(lr = 0.002, beta_1 = 0.9, beta_2 = 0.999, epsilon = 1e-08, decay = 0, clipnorm = -1, clipvalue = -1)

#Nadam(lr = 0.002, beta_1 = 0.9, beta_2 = 0.999, epsilon = 1e-08,schedule_decay = 0.004, clipnorm = -1, clipvalue = -1)

## Parameters:

#lr	float >= 0. Learning rate.

#momentum	float >= 0. Parameter updates momentum.

#decay	float >= 0. Learning rate decay over each update.

#nesterov	boolean. Whether to apply Nesterov momentum.

#clipnorm	float >= 0. Gradients will be clipped when their L2 norm exceeds this value. Set to -1 to disable.

#clipvalue	float >= 0. Gradients will be clipped when their absolute value exceeds this value. Set to -1 to disable.

#rho	float >= 0 to be used in RMSprop

#epsilon	float >= 0. Fuzz factor.

#beta_1	float, 0 < beta < 1. Generally close to 1.

#beta_2	float, 0 < beta < 1. Generally close to 1.

#schedule_decay	float >= 0. Learning rate decay over each schedule in Nadam.



## Loss Functions

#loss_binary_crossentropy(
#  y_true,
#  y_pred,
#  from_logits = FALSE,
#  label_smoothing = 0,
#  axis = -1L,
#  ...,
#  reduction = "auto",
#  name = "binary_crossentropy"
#)

#loss_categorical_crossentropy(
  #y_true,
  #y_pred,
  #from_logits = FALSE,
  #label_smoothing = 0L,
  #axis = -1L,
  #...,
  #reduction = "auto",
  #name = "categorical_crossentropy"
#)

#loss_categorical_hinge(
#  y_true,
#  y_pred,
#  ...,
#  reduction = "auto",
#  name = "categorical_hinge"
#)

#loss_cosine_similarity(
#  y_true,
#  y_pred,
#  axis = -1L,
#  ...,
#  reduction = "auto",
#  name = "cosine_similarity"
#)

#loss_hinge(y_true, y_pred, ..., reduction = "auto", name = "hinge")
#
#loss_huber(
#  y_true,
#  y_pred,
#  delta = 1,
#  ...,
#  reduction = "auto",
#  name = "huber_loss"
#)

#loss_kullback_leibler_divergence(
#  y_true,
#  y_pred,
#  ...,
#  reduction = "auto",
#  name = "kl_divergence"
#)

#loss_kl_divergence(
#  y_true,
#  y_pred,
#  ...,
#  reduction = "auto",
#  name = "kl_divergence"
#)

#loss_logcosh(y_true, y_pred, ..., reduction = "auto", name = "log_cosh")

#loss_mean_absolute_error(
#  y_true,
#  y_pred,
#  ...,
#  reduction = "auto",
#  name = "mean_absolute_error"
#)

#loss_mean_absolute_percentage_error(
#  y_true,
#  y_pred,
#  ...,
#  reduction = "auto",
#  name = "mean_absolute_percentage_error"
#)

#loss_mean_squared_error(
#  y_true,
#  y_pred,
#  ...,
#  reduction = "auto",
#  name = "mean_squared_error"
#)

#loss_mean_squared_logarithmic_error(
#  y_true,
 # y_pred,
#  ...,
#  reduction = "auto",
#  name = "mean_squared_logarithmic_error"
#)

#loss_poisson(y_true, y_pred, ..., reduction = "auto", name = "poisson")

#loss_sparse_categorical_crossentropy(
 # y_true,
  #y_pred,
  #from_logits = FALSE,
#  axis = -1L,
 # ...,
  #reduction = "auto",
  #name = "sparse_categorical_crossentropy"
#)

#loss_squared_hinge(
#  y_true,
#  y_pred,
#  ...,
#  reduction = "auto",
#  name = "squared_hinge"
# )
# Keras (>=2.2.5.0)
# dplyr (>=1.0.7)
