## Function to step forward in time to next event and update states:

event_sirspill <- function(time, S, I, R, params, t_end, count.inf, count.spill) {
	N <- S+I+R
  with(as.list(params), {
    
    rates <- c(spillover = lambda*S/N, 
               infect = beta * I * S / N,
               recover = gamma * I
    )
    
    total_rate <- sum(rates)
    
    if (total_rate == 0) {
      
      event_time <- t_end
      
    } else {
      
      event_time <- time + rexp(1, total_rate)
      event_type <- sample(c("Spillover","Infect","Recover"), 1, prob = rates / total_rate)
      
      switch(event_type,
             "Spillover" = {
               S <- S-1
               I <- I+1
               count.spill = count.spill+1
             },
             "Infect" = {
               S <- S-1
               I <- I+1
               count.inf = count.inf+1
             },
             "Recover" = {
               I <- I-1
               R <- R+1
             })
    }
    
    return(data.frame(time = event_time, S = S, I = I, R = R, count.inf = count.inf, count.spill = count.spill))
  })
}

## Function to simulate states from time 0 to t_end:

simulate_sirspill <- function(t_end, y, params) {
  with(as.list(y), {
    
    count.inf <-  I
    count.spill <-  0
    ts <- data.frame(time = 0, S = S, I = I, R = R, count.inf = I, count.spill = 0)
    next_event <- ts
    
    while (next_event$time < t_end) {
      next_event <- event_sirspill(next_event$time, next_event$S, next_event$I, next_event$R, params, t_end, count.inf, count.spill)
      ts <- rbind(ts, next_event)
    }
    
    return(ts)
  })
}

