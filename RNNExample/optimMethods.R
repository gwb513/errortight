update_layer <- function(S,SSE,SSE_last, method = 'Rprop_plus', eta = .1, eta_1 = 0.15,
                         eta_2 = 1.2, delta_min = 0, delta_max = 50, n) {
  # methods: Rprop_plus (default), Rprop_minus, SGD
  # eta is only by SGD (basic stochastic gradient descent)
  # eta_1, eta_2, delta_min, delta_max used by all Rprop methods
  # 0 < eta_1 < 1 < eta_2

  #S = list(w,delta,dEdw,dEdw_last,d_w)
  switch(method,
         'Rprop_plus' = {
           for (i in 1:length(S$w)) {
             if (is.infinite(S$dEdw[i])) {S$dEdw[i] = 0}
             if (S$dEdw_last[i]*S$dEdw[i] > 0) {
               if (is.na(min(eta_2*S$delta[i], delta_max))) {browser()}
               S$delta[i] = min(eta_2*S$delta[i], delta_max)
               S$d_w[i] = -sign(S$dEdw[i])*S$delta[i]
               S$w[i] = S$w[i] + S$d_w[i]
             } else if (S$dEdw_last[i]*S$dEdw[i] < 0) {
               if (is.na(max(eta_1*S$delta[i], delta_min))) {browser()}
               S$delta[i] = max(eta_1*S$delta[i], delta_min)
               S$w[i] = S$w[i] - S$d_w[i]
               S$dEdw[i] = 0
             } else {
               S$d_w[i] = -sign(S$dEdw[i])*S$delta[i]

               S$w[i] = S$w[i] + S$d_w[i]
               if (is.na(S$w[i])) {browser()}
             }


           }
           S$dEdw_last = S$dEdw
           return(S)
         },
         'iRprop_plus' = {
           for (i in 1:length(S$w)) {
             if (S$dEdw_last[i]*S$dEdw[i] > 0) {
               S$delta[i] = min(eta_2*S$delta[i], delta_max)
               S$d_w[i] = -sign(S$dEdw[i])*S$delta[i]
               S$w[i] = S$w[i] + S$d_w[i]
             } else if (S$dEdw_last[i]*S$dEdw[i] < 0) {
               S$delta[i] = max(eta_1*S$delta[i], delta_min)
               if (SSE > SSE_last) {S$w[i] = S$w[i] - S$d_w[i];} #print('Et > Et-1')}
               S$dEdw[i] = 0
             } else {
               S$d_w[i] = -sign(S$dEdw[i])*S$delta[i]
               S$w[i] = S$w[i] + S$d_w[i]
             }

           }
           S$dEdw_last = S$dEdw
           return(S)
         },
         'Rprop_minus' = {
           for (i in 1:length(S$w)) {
             if (S$dEdw_last[i]*S$dEdw[i] > 0) {
               S$delta[i] = min(eta_2*S$delta[i], delta_max)

             } else if (S$dEdw_last[i]*S$dEdw[i] < 0) {
               S$delta[i] = max(eta_1*S$delta[i], delta_min)
               #dEdw[i] = 0
             } else  {
               #stop('rprop- error')
             }
             S$w[i] = S$w[i] - sign(S$dEdw[i])*S$delta[i]
           }
           S$dEdw_last = S$dEdw
           return(S)
         },
         'iRprop_minus' = {
           for (i in 1:length(S$w)) {
             if (S$dEdw_last[i]*S$dEdw[i] > 0) {
               S$delta[i] = min(eta_2*S$delta[i], delta_max)

             } else if (S$dEdw_last[i]*S$dEdw[i] < 0) {
               S$delta[i] = max(eta_1*S$delta[i], delta_min)
               S$dEdw[i] = 0
             } else  {
               #stop('rprop- error')
             }
             S$w[i] = S$w[i] - sign(S$dEdw[i])*S$delta[i]
           }
           S$dEdw_last = S$dEdw
           return(S)
         },
         'SGD' = {
           S$w = S$w - eta*S$dEdw
           return(S)
         },
         'adam' = {
           beta1 = 0.9; beta2 = 0.999; ep = 10e-8
           S$t = S$t + 1
           S$m = beta1*S$m + (1 - beta1)*S$dEdw # update biased first moment estimate
           S$v = beta2*S$v + (1 - beta2)*S$dEdw^2
           mhat = S$m/(1 - beta1^S$t)
           vhat = S$v/(1 - beta2^S$t)
           dw = eta*mhat/(vhat + ep)
           S$w = S$w - dw


           #if (any(s$w > 5)) browser()
           return(S)
         },
         'quickprop' = {
           #S$delta_last * S$w * ((S$dEdw * SSE)/(S$dEdw*SSE_last - S$dEdw))
           #return(S)
           stop()
         },
         'rmsprop' = {
           S$r = (1 - S$gamma)*S$dEdw^2 + S$gamma*S$r
           S$V = eta/sqrt(S$r)*S$dEdw
           S$w = S$w - S$V
           return(S)
         },
         'rmsprop_m' = {
           S$dEdW = S$dEdw - S$beta*S$V
           S$r = (1 - S$gamma)*S$dEdw^2 + S$gamma*S$r
           S$V = S$beta*S$V + eta/sqrt(S$r)*S$dEdw
           S$w = S$w - S$V
           return(S)
         }

  )


}