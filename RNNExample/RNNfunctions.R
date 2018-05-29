# Recurrent Neural Network functions
initialize_layers_rnn <- function(NN) {
  set.seed(20)
  NN$layer = list()
  NN$bias_layer = list()
  NN$trans_layer = list()
  NN$bias_trans_layer = list()
  NN$SSE_last = 1e12
  NN$nlayers = 2

  for (i in 1:NN$nlayers) {

    NN$layer[[i]] = list()
    NN$layer[[i]]$reg = NN$reg
    NN$layer[[i]]$lambda = NN$lambda
    NN$bias_layer[[i]] = list()
    NN$bias_layer[[i]]$reg = 'None'
    NN$trans_layer[[1]] = list()
    NN$trans_layer[[1]]$reg = NN$reg
    NN$trans_layer[[1]]$lambda = NN$lambda

    NN$bias_trans_layer[[1]] = list()
    NN$bias_trans_layer[[1]]$reg = 'None'
    NN$layer[[i]]$w = matrix(.01*rnorm(NN$n_neurons[i + 1]*NN$n_neurons[i]),
                             nrow = NN$n_neurons[i + 1], ncol = NN$n_neurons[i])
    NN$bias_layer[[i]]$w = matrix(.01*rnorm(NN$n_neurons[i + 1]),
                                  nrow = NN$n_neurons[i + 1],ncol = 1)
    NN$trans_layer[[1]]$w = matrix(.01*rnorm(NN$n_neurons[2]*NN$n_neurons[2]),
                                   nrow = NN$n_neurons[2], ncol = NN$n_neurons[2])
    NN$bias_trans_layer[[1]]$w = matrix(.01*rnorm(NN$n_neurons[2]*NN$n_neurons[2]),
                                        nrow = NN$n_neurons[2], ncol = 1)

    NN$layer[[i]]$dEdw = NN$layer[[i]]$w * 0
    NN$bias_layer[[i]]$dEdw = NN$bias_layer[[i]]$w * 0
    NN$trans_layer[[1]]$dEdw = NN$trans_layer[[1]]$w * 0
    NN$bias_trans_layer[[1]]$dEdw = NN$bias_trans_layer[[1]]$w * 0

    ### Rprop specific properties
    NN$bias_layer[[i]]$delta = NN$bias_layer[[i]]$dEdw*0 + .5
    NN$bias_layer[[i]]$dEdw_last = NN$bias_layer[[i]]$dEdw + 0
    NN$bias_layer[[i]]$d_w = NN$bias_layer[[i]]$dEdw*0

    NN$bias_trans_layer[[1]]$delta = NN$bias_trans_layer[[1]]$dEdw*0 + .5
    NN$bias_trans_layer[[1]]$dEdw_last = NN$bias_trans_layer[[1]]$dEdw*0
    NN$bias_trans_layer[[1]]$d_w = NN$bias_trans_layer[[1]]$dEdw*0

    NN$layer[[i]]$delta = NN$layer[[i]]$dEdw*0 + .5
    NN$layer[[i]]$dEdw_last = NN$layer[[i]]$dEdw + 0
    NN$layer[[i]]$d_w = NN$layer[[i]]$dEdw*0

    NN$trans_layer[[1]]$delta = NN$trans_layer[[1]]$dEdw*0 + .5
    NN$trans_layer[[1]]$dEdw_last = NN$trans_layer[[1]]$dEdw + 0
    NN$trans_layer[[1]]$d_w = NN$trans_layer[[1]]$dEdw*0

    ## adam specific properties
    NN$layer[[i]]$t = 0
    NN$bias_layer[[i]]$t = 0
    NN$bias_trans_layer[[1]]$t = 0
    NN$trans_layer[[1]]$t = 0

    NN$layer[[i]]$m = 0
    NN$bias_layer[[i]]$m = 0
    NN$bias_trans_layer[[1]]$m = 0
    NN$trans_layer[[1]]$m = 0

    NN$layer[[i]]$v = 0
    NN$bias_layer[[i]]$v = 0
    NN$bias_trans_layer[[1]]$v = 0
    NN$trans_layer[[1]]$v = 0

    # RMSprop parameters
    NN$layer[[i]]$gamma = .9
    NN$bias_layer[[i]]$gamma = .9
    NN$bias_trans_layer[[1]]$gamma = .9
    NN$trans_layer[[1]]$gamma = .9

    NN$layer[[i]]$beta = .45
    NN$bias_layer[[i]]$beta = .45
    NN$bias_trans_layer[[1]]$beta = .45
    NN$trans_layer[[1]]$beta = .45

    NN$layer[[i]]$r = NN$layer[[i]]$w*0
    NN$bias_layer[[i]]$r = NN$bias_layer[[i]]$w*0
    NN$bias_trans_layer[[1]]$r = NN$bias_trans_layer[[1]]$w*0
    NN$trans_layer[[1]]$r = NN$trans_layer[[1]]$w*0

    NN$layer[[i]]$V = NN$layer[[i]]$w*0
    NN$bias_layer[[i]]$V = NN$bias_layer[[i]]$w*0
    NN$bias_trans_layer[[1]]$V = NN$bias_trans_layer[[1]]$w*0
    NN$trans_layer[[1]]$V = NN$trans_layer[[1]]$w*0



  }
  NN$h0 = NN$bias_trans_layer[[1]]$w
  #NN$bias_layer[[2]]$w = NN$bias_layer[[2]]$w*0
  #NN$layer[[2]]$w = NN$layer[[2]]$w*0 + 1
  return(NN)
}

update_layers_rnn <- function(NN, eta = .1) {
  for (i in 1:NN$nlayers) {
    NN$layer[[i]] = update_layer(NN$layer[[i]],SSE = NN$SSE, SSE_last = NN$SSE_last,
                                 method = NN$method, eta = NN$eta, eta_1 = 0.05,
                                 eta_2 = 1.2, delta_min = 1e-5, delta_max = 50, n = NN$Tmax)
    NN$bias_layer[[i]] = update_layer(NN$bias_layer[[i]],SSE = NN$SSE, SSE_last = NN$SSE_last,
                                      method = NN$method, eta = NN$eta, eta_1 = 0.05,
                                      eta_2 = 1.2, delta_min = 1e-5, delta_max = 50, n = NN$Tmax)
  }

  NN$trans_layer[[1]] = update_layer(NN$trans_layer[[1]],SSE = NN$SSE, SSE_last = NN$SSE_last,
                                     method = NN$method, eta = NN$eta, eta_1 = 0.05,
                                     eta_2 = 1.2, delta_min = 1e-5, delta_max = 50, n = NN$Tmax)

  if (any(abs(NN$bias_trans_layer[[1]]$dEdw) > 0)) {
    NN$bias_trans_layer[[1]] = update_layer(NN$bias_trans_layer[[1]],SSE = NN$SSE, SSE_last = NN$SSE_last,
                                            method = NN$method, eta = NN$eta, eta_1 = 0.05,
                                            eta_2 = 1.2, delta_min = 1e-5, delta_max = 50, n = NN$Tmax)
  }
  return(NN)
}

bptt_rnn2 <- function(NN, batch_size = 150, k2 = 15) {
  n_batches = dim(NN$X_train)[1] %/% batch_size
  seq_length = dim(NN$X_train)[2]
  NN$Tmax = dim(NN$X_train)[1]
  Tmax = NN$Tmax
  #batch_size = NN$Tmax
  NN$h0 = NN$bias_trans_layer[[1]]$w

  nh = NN$n_neurons[2]; ni = NN$n_inputs; no = NN$n_outputs


  #batch_size = sample.int(15,1)
  NN$SSE_test = 1e6
  for (ne in 1:NN$n_epochs) {
    if (ne %% 1 == 0) {
      # https://arxiv.org/pdf/1711.00489.pdf
      batch_size0 = batch_size
      batch_size = min(round(1.02*batch_size),dim(NN$X_train)[1])
      n_batches = dim(NN$X_train)[1] %/% batch_size
      if (n_batches < 5) {
        batch_size = batch_size0
        n_batches = dim(NN$X_train)[1] %/% batch_size
        NN$eta = max(NN$eta*.95, .00001)
      }
    }

    #NN = reset_rprop_params(NN)
    print(paste('batch size =',batch_size))
    pb <- txtProgressBar(min=0,max=n_batches,style=3)
    if (NN$batch_shuffle == FALSE) {
      b_ind = 1:n_batches
    } else {
      b_ind = sort(runif(n_batches),index.return = TRUE)$ix
    }
    for (b in 1:n_batches) {
      b0 = b
      b = b_ind[b]

      NN$Whh = NN$trans_layer[[1]]$w; NN$Whv = NN$layer[[1]]$w; NN$Woh = NN$layer[[2]]$w

      tind0 = ((b - 1) * batch_size + 1):(b * batch_size + seq_length)
      # reset gradients for each batch
      NN$dWoh = NN$Woh * 0;
      NN$dWhv = NN$Whv * 0;
      NN$dWhh = NN$Whh * 0;

      NN$dbo = matrix(0, ncol = 1, nrow = no)
      NN$dbh = matrix(0, ncol = 1, nrow = nh)
      NN$dh0 = NN$bias_trans_layer[[1]]$w*0
      E = NULL
      if (NN$seq_shuffle == FALSE) {
        bbind = 1:batch_size
      } else {
        bbind = sort(runif(batch_size),index.return = TRUE)$ix
      }
      for (bb in bbind) {

        seq_ind = (b - 1)*batch_size + bb

        if (seq_ind > dim(NN$X_train)[1]) browser()
        X = matrix(NN$X_train[seq_ind,,], nrow = ni, ncol = seq_length, byrow = TRUE)
        Y = matrix(NN$Y_train[seq_ind,,], nrow = no, ncol = 1, byrow = TRUE)
        NN = rnn_fprop2(NN, X, Y, seq_length)
        E[bb] = NN$E
        # compute gradients for sequence
        # adds on to previous gradients from this batch
        NN = rnn_bprop(NN, X, seq_length, k2, nh, no, ni, bb)

      } # samples
      setTxtProgressBar(pb, b0)

      NN$SSE_last = NN$SSE
      NN$SSE = sum(E^2)
      #print(paste('Epoch ', ne,' Training Batch ',b,' SSE = ',NN$SSE))
      NN$trans_layer[[1]]$w
      NN = assign_gradients(NN, batch_size)
      NN$trans_layer[[1]]$w


    } # batches
    close(pb)
    E_test = NULL
    y_tar = y_test = matrix(0,nrow = no, ncol = dim(NN$X_test)[1])
    for (bb in 1:dim(NN$X_test)[1]) {
      X = matrix(NN$X_test[bb,,], nrow = ni, ncol = seq_length, byrow = TRUE)
      Y = matrix(NN$Y_test[bb,,], nrow = no, ncol = 1, byrow = TRUE)
      NN = rnn_fprop2(NN, X, Y, seq_length)
      E_test[bb] = NN$E
      y_test[, bb] = NN$z[,seq_length]
      y_tar[, bb] = Y
    }
    NN$SSE_test_last = NN$SSE_test
    NN$SSE_test = sum(E_test^2)/dim(NN$X_test)[1]
    print(paste('Epoch', ne, 'Test Data MSE =', NN$SSE_test))
    plot(y_test[1,], col = 'green', ylim = c(-1.1,1.1),pch=15)
    matplot(y_tar[1,], col = 'blue', add = TRUE,pch=16)

    if (NN$SSE_test < 1e-5) {
      print('Accuracy met, exiting')
      return(NN)
    }

  } # epochs
  print('Iterations complete, exiting')
  return(NN)
}

rnn_fprop2 <- function(NN,X,Y,seq_length) {
  nh = NN$n_neurons[2]; ni = NN$n_inputs; no = NN$n_outputs
  Whh = NN$trans_layer[[1]]$w
  Whv = NN$layer[[1]]$w
  Woh = NN$layer[[2]]$w

  NN$u = matrix(0, ncol = seq_length, nrow = nh)
  NN$h = matrix(0, ncol = (seq_length + 1), nrow = nh)
  NN$o = matrix(0, ncol = seq_length, nrow = no)
  NN$z = matrix(0, ncol = seq_length, nrow = no)
  NN$E = matrix(0, ncol = 1, nrow = no)


  for (t in 1:seq_length) {

    if (t == 1) { # was t==1 when working
      NN$u[, t] = Whv %*% X[, t] + NN$Whh %*% NN$h0 + NN$bias_layer[[1]]$w * 1
    } else {
      NN$u[, t] = Whv %*% X[, t] + NN$Whh %*% NN$h[, t - 1] + NN$bias_layer[[1]]$w * 1
    }

    NN$h[, t] = NN$f[[1]](NN$u[, t])#*rbinom(NN$n_neurons[2],1,.5) #dropout
    NN$o[, t] = NN$Woh %*% NN$h[, t] + NN$bias_layer[[2]]$w * 1
    NN$z[, t] = NN$f[[2]](NN$o[, t])

  }
  NN$E = Y - NN$z[, t]
  return(NN)
}

rnn_predict <- function(NN,v0,tind) {

  Tmax = NN$Tmax
  nh = NN$n_neurons[2]
  ni = NN$n_inputs
  no = NN$n_outputs

  Whh = NN$trans_layer[[1]]$w
  Whv = NN$layer[[1]]$w
  Woh = NN$layer[[2]]$w
  #print(is.null(NN$u))

  NN$u = matrix(0, ncol = Tmax, nrow = nh)
  NN$h = matrix(0, ncol = (Tmax + 1), nrow = nh)
  NN$o = matrix(0, ncol = Tmax, nrow = no)
  NN$z = matrix(0, ncol = Tmax, nrow = no)
  NN$E = matrix(0, ncol = Tmax, nrow = no)

  for (t in tind) {

    if (t <= dim(v0)[2]) {
      if (t == tind[1]) {

        NN$u[, t] = NN$layer[[1]]$w %*% v0[,t] + NN$trans_layer[[1]]$w %*% NN$h0 + NN$bias_layer[[1]]$w * 1
      } else {

        NN$u[, t] = NN$layer[[1]]$w %*% v0[,t] + NN$trans_layer[[1]]$w %*% NN$h[, t - 1] + NN$bias_layer[[1]]$w * 1
      }
    }
    else {
      z = matrix(NN$z[, (t - 1):(t - NN$n_inputs)], nrow = NN$n_inputs, ncol = 1)
      if (t == 1) {

        NN$u[, t] = NN$layer[[1]]$w %*% z + NN$trans_layer[[1]]$w %*% NN$h0 + NN$bias_layer[[1]]$w * 1
      } else {

        NN$u[, t] = NN$layer[[1]]$w %*% z + NN$trans_layer[[1]]$w %*% NN$h[, t - 1] + NN$bias_layer[[1]]$w * 1
      }
    }
    #u[,t] = u[,t]/sum(abs(u[,t]))
    NN$h[, t] = NN$f[[1]](NN$u[, t])#*rbinom(NN$n_neurons[2],1,.5) #dropout
    NN$o[, t] = NN$layer[[2]]$w %*% NN$h[, t] + NN$bias_layer[[2]]$w * 1
    #o[,t] = o[,t]/sum(abs(o[,t]))
    NN$z[, t] = NN$f[[2]](NN$o[, t])
    #NN$E[, t] = z_tar[, t] - NN$z[, t]
  }

  #NN$SSE_last = NN$SSE
  #NN$SSE = sum(NN$E[,tind]^2)
  #print(NN$SSE)

  print('finished iterations')
  plot(NN$z[1,], ylim = c(-1,1))
  matplot(z_tar[1,], add = TRUE, col = 'blue')
  return(NN)
}

getdLdu <- function(NN, Woh, nh, no, tt) {

  dLdo = -NN$E * NN$g[[2]](NN$o[, tt])
  dLdh = t(Woh) %*% dLdo
  dLdu = dLdh * NN$g[[1]](NN$u[, tt]) # commented out above matches this
  return(dLdu)
}
getdudu <- function(NN, Whh, nh, tt, nn) {
  dudu = matrix(1, nrow = nh, ncol = 1)
  if (nn > 0) {
    for (mm in 1:nn) {
      #dudu = dudu * matrix(colSums(Whh), nrow = nh, ncol = 1) *
      #  matrix(NN$g[[1]](NN$u[, tt - mm]), nrow = nh, ncol = 1)
      ##dudu = dudu * rowSums(Whh * matrix(NN$g[[1]](NN$u[, tt - mm]), nrow = nh, ncol = nh))
      dudu = (t(Whh) %*% NN$g[[1]](NN$u[, tt - mm]))
    }
  }
  return(dudu)
}
getdWhh_0 <- function(NN, dLdu, nh, tt) {
  if (tt == 1) { # have to use h0 instead of h[tt-1]
    dWhh = dLdu %*% t(matrix(NN$h0, nrow = nh, ncol = 1))
  } else {
    dWhh = dLdu %*% t(matrix(NN$h[, tt - 1], nrow = nh, ncol = 1))
  }
  return(dWhh)
}

getdWhh <- function(NN, dWhh, dLdu, dudu, nh, ii) {
  if (ii == 1) {
    dWhh = dWhh + (dLdu*dudu) %*% t(matrix(NN$h0, nrow = nh, ncol = 1))
  } else {
    # the thesis doesn't seem to included dudu
    dWhh = dWhh + (dLdu*dudu) %*% t(matrix(NN$h[, ii - 1], nrow = nh, ncol = 1))
  }

  return(dWhh)
}

getdWhv_0 <- function(dLdu, v, nh, ni, tt) {
  dWhv = matrix(dLdu, nrow = nh, ncol = ni) *
    matrix(v[, tt], nrow = nh, ncol = ni, byrow = TRUE)
  return(dWhv)
}

getdWhv <- function(dWhv, dudu, dLdu, v, nh, ni, ii) {
  dWhv = dWhv + matrix(dLdu*dudu, nrow = nh, ncol = ni) *
    #matrix(dudu, nrow = nh, ncol = ni) *
    matrix(v[, ii], nrow = nh, ncol = ni, byrow = TRUE)
}

rnn_bprop <- function(NN,X,seq_length, k2, nh, no, ni, bb) {


  # update output layer params
  dLdo = NN$g[[2]](NN$o[, seq_length]) * -(NN$E) / NN$Tmax
  NN$dbo = NN$dbo + matrix(dLdo, nrow = no, ncol = 1)

  #dWoh = dWoh + matrix(dLdo, nrow = no, ncol = nh) *
  #  matrix(NN$h[, tt], nrow = no, ncol = nh, byrow = TRUE)
  NN$dWoh = NN$dWoh + dLdo %*% t(matrix(NN$h[, seq_length], nrow = nh, ncol = 1)) ##equivalent
  dLdu <- getdLdu(NN, NN$Woh, nh, no, seq_length)

  # if inds includes 1, then we can evaluate dh0 to update the initial hidden state
  inds = seq_length:max(seq_length - k2, 1)
  #inds = tt:tind0[1]
  # for each tt we work k2 steps back in time from tt
  nn = 0
  for (ii in inds) {
    # We already have Woh, boh, and dL/du
    dudu <- getdudu(NN, NN$Whh, nh, seq_length, nn) # du[t]/du[t-1] moves us back in time
    if (any(is.null(dudu)) | any(is.na(dudu)) | any(is.infinite(dudu))) {
      dudu[is.na(dudu)] = .01}
    if (mean(abs(dudu)) < 1e-10) break

    #if (any(inds == 1)) { #update initial state weights
    #if (tt == 1) { #update initial state weights

    ### Need to count down to the beginning of the sequence!
    if (ii <= 25) {
      dh0_inc = t(NN$Whh) %*% dLdu
      if ((ii - 1) > 0) {
        for (jj in (ii - 1):1) {
          dh0_inc = dh0_inc * (t(NN$Whh) %*% NN$g[[1]](NN$u[,jj]))
        }
      }
      NN$dh0 = NN$dh0 + dh0_inc
    }

    if (bb == 1 & ii == seq_length) { # if it is the first time step in the backprop
      dWhv0 <- getdWhv_0(dLdu, X, nh, ni, ii) #don't need du/du
      NN$dWhv = NN$dWhv + dWhv0
      dWhh0 <- getdWhh_0(NN, dLdu, nh, ii) # don't need du/du
      NN$dWhh = NN$dWhh + dWhh0
      dbh0 = matrix(dLdu, nrow = nh, ncol = 1)
      NN$dbh = NN$dbh + dbh0
      #dLhv21 = -NN$E[,tt]*NN$g[[2]](NN$o[,tt])*Woh[1,2]*NN$g[[1]](NN$u[2,tt])*v[,tt]
    } else {# previous timesteps in the backprop
      NN$dWhv <- getdWhv(NN$dWhv, dudu, dLdu, X, nh, ni, ii) # need du/du
      NN$dWhh <- getdWhh(NN, NN$dWhh, dLdu, dudu, nh, ii) # need du/du
      NN$dbh = NN$dbh + matrix(dLdu*dudu, nrow = nh, ncol = 1) #* dudu # thesis doesnt include dudu
      # dL/dz[,tt]* dz[,tt]/do[,tt] * do[,tt]/dh[2,tt] * dh[2,tt]/du[2,tt] * du[]
      # -E[,tt]   * g'(o[,tt])      * Woh[1,2]         * e'(u[2,tt]) *
      #dLhv21b = dLhv21 -NN$E[,tt]*NN$g[[2]](NN$o[,tt])*Woh[1,2]*NN$g[[1]](NN$u[2,tt])*
    }
    nn = nn + 1
  } #end ii
  return(NN)

}

assign_gradients <- function(NN, batch_size) {
  lim = 500
  lim2 = 500
  NN$dbo = NN$dbo/batch_size
  NN$dbh = NN$dbh/batch_size
  NN$dh0 = NN$dh0/batch_size
  NN$dWoh = NN$dWoh/batch_size
  NN$dWhh = NN$dWhh/batch_size
  NN$dWhv = NN$dWhv/batch_size
  NN$dbo[NN$dbo > lim] = lim; NN$dbo[NN$dbo < -lim] = -lim
  NN$dbh[NN$dbh > lim] = lim; NN$dbh[NN$dbh < -lim] = -lim

  #dWhv[dWhv > 5] = 5; dWhv[dWhv < -5] = -5
  NN$dWoh[NN$dWoh > lim] = lim; NN$dWoh[NN$dWoh < -lim]  = -lim
  #dWhh[dWhh > 5] = 5; dWoh[dWoh < -5] = -5
  NN$dh0[NN$dh0 > lim2] = lim2; NN$dh0[NN$dh0 < -lim2] = -lim2
  # save to lists
  dW = list(); db = list();
  switch(NN$reg,'None' = {
    dW[[1]] <- (NN$dWhv);
    dW[[2]] <- NN$dWoh;
    dW[[3]] <- NN$dWhh;
  }, 'L1' = {
    dW[[1]] <- NN$dWhv + NN$lambda*sign(NN$Whv)#/dim(NN$X_train)[1];
    dW[[2]] <- NN$dWoh + NN$lambda*sign(NN$Woh)#/dim(NN$X_train)[1];
    dW[[3]] <- NN$dWhh + NN$lambda*sign(NN$Whh)#/dim(NN$X_train)[1];
  }, 'L2' = {
    dW[[1]] <- NN$dWhv + NN$lambda*(NN$Whv)#/dim(NN$X_train)[1];
    dW[[2]] <- NN$dWoh + NN$lambda*(NN$Woh)#/dim(NN$X_train)[1];
    dW[[3]] <- NN$dWhh + NN$lambda*(NN$Whh)#/dim(NN$X_train)[1];
  })

  db[[1]] <- (NN$dbh); db[[2]]  <- NN$dbo

  # save derivatives to the NN
  for (i in 1:NN$nlayers) {
    if (any(is.na(db[[i]]))) {db[[i]][is.na(db[[i]])] = .1}
    if (any(is.na(dW[[i]]))) {dW[[i]][is.na(dW[[i]])] = .1}

    NN$layer[[i]]$dEdw = dW[[i]] #Whv and Woh
    NN$bias_layer[[i]]$dEdw = db[[i]] #bh and bo
  }

  if (any(is.na(dW[[3]]))) {browser()}
  if (any(is.na(NN$dh0))) {
    dh0[which(is.na(NN$dh0))] = 0
  }

  NN$trans_layer[[1]]$dEdw = dW[[3]] # Whh
  NN$bias_trans_layer[[1]]$dEdw = NN$dh0 #h0

  NN = update_layers_rnn(NN)

  #NN$h0 = NN$bias_trans_layer[[1]]$w*0
  return(NN)
}