library(QuickShinyApp)

slider_params = list(
  d = c(0,-1.5,1.5),
  q = c(0.5,0.01,0.99),
  jitter = c(0.3,0,1)
)

plot_fun = function(d,q,jitter) {
  n = 100
  p = 1-q
  alpha = 1 + d*(q-p)
  A2A2 = round(n*q^2)
  A1A2 = round(2*n*p*q)
  A1A1 = round(n*p^2)
  area = 0.25^2*pi
  data = rbind(data.frame(Genotype = 'A2A2',X = 0,value = -1)[rep(1,A2A2),],
               data.frame(Genotype = 'A1A2',X = 1,value = d)[rep(1,A1A2),],
               data.frame(Genotype = 'A2A2',X = 2,value = 1)[rep(1,A1A1),]
        )
  lm1 = lm(value~X,data)
  plot(NA,NA,xlim = c(-0.5,2.5),ylim = c(-1,1)*max(c(1,max(abs(slider_params$d)))),xlab = '# A1',ylab = 'genetic value')
  # with(data, points(jitter(X,jitter),jitter(value,jitter)))
  symbols(c(0,1,2),y=c(-1,d,1),circles = sqrt(area*c(q^2,2*p*q,p^2)/pi),add=T,inches=F)
  abline(lm1)
  # abline(h=mean(data$value)+c(2*q*alpha,-2*p*alpha,(q-p)*alpha))

  text(1,d - 0.5*c(1,-1)[(d<0)+1],sprintf('alpha = %0.2f',alpha))
}

call = 'plot_fun(d,q,jitter)'
run_shiny(call,slider_params)
