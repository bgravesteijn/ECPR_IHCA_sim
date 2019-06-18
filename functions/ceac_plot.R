ce_plot <- function(ce, plot, wtp_line=20000,vline=NULL){
  ktop <- 100000 
  icea.dt <-  icea(ce, k = seq(0, ktop, 500), sim = "sim", strategy = "strategy",
                   grp="grp", e = "QALY", c = "cost")
  icea.pw.dt <-  icea_pw(ce,  k = seq(0, ktop, 500), comparator = "NE",
                         sim = "sim", strategy = "strategy", e = "QALY", c = "cost")
  
  ylim <- max(icea.pw.dt$delta[, ic])
  xlim <- ceiling(max(icea.pw.dt$delta[, ie]))
  
  if(plot==1){
    ggplot(icea.pw.dt$delta, aes(x = ie, y = ic, col = factor(strategy))) + 
      geom_point(size = .2) + 
      xlab("Incremental QALYs") + ylab("Incremental cost") +
      # scale_y_continuous(label = euro, limits = c(-ylim, ylim)) +
      scale_x_continuous(limits = c(-xlim, xlim), breaks = seq(-6, 6, 2)) +
      theme(legend.position = "bottom") + scale_colour_discrete(name = "Strategy") +
      geom_abline(slope = wtp_line, linetype = "dashed") +
      geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
  }
  if(plot==2){
    if(is.null(vline)){
      ggplot(icea.dt$mce, aes(x = k, y = prob, col = factor(strategy))) +
        geom_line(cex=1.5) + xlab("Willingess to pay") +
        ylab("Probability most cost-effective") +
        theme(legend.position = "bottom") + scale_colour_discrete(name = "Strategy")
    }
    if(length(vline)==1){
      ggplot(icea.dt$mce, aes(x = k, y = prob, col = factor(strategy))) +
        geom_line(cex=1.5) + xlab("Willingess to pay") +
        ylab("Probability most cost-effective") +
        geom_vline(xintercept = vline, lty=2)+
        theme(legend.position = "bottom") + scale_colour_discrete(name = "Strategy")
    }
    if(length(vline)==2){
      ggplot(icea.dt$mce, aes(x = k, y = prob, col = factor(strategy))) +
        geom_line(cex=1.5) + xlab("Willingess to pay") +
        ylab("Probability most cost-effective") +
        geom_vline(xintercept = vline[1], lty=2)+
        geom_vline(xintercept = vline[2], lty=2)+
        theme(legend.position = "bottom") + scale_colour_discrete(name = "Strategy")
    }
    
  }
  
}