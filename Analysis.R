library(boot)
library(survey)
library(foreign)
library(stargazer)
library(corrplot)
library(lmtest)
library(Matching)
library(effects)
library(sandwich)
library(effects)
library(RColorBrewer)


load("TermsConditionsData.RData")


design <- svydesign(ids=~1, data=data.ca[data.ca$manip.correct == 1,])
design2 <- svydesign(ids=~1, data=data.ca)

design.w <- svydesign(ids=~1, data=data.ca[data.ca$manip.correct == 1 & data.ca$r.white == 1  & data.ca$lgbt == 0,])
design.poc <- svydesign(ids=~1, data=data.ca[data.ca$manip.correct == 1 & data.ca$r.white == 0  & data.ca$lgbt == 0,])

design.wmn <- svydesign(ids=~1, data=data.ca[data.ca$manip.correct == 1 & data.ca$gender.lim == 1  & data.ca$lgbt == 0,])
design.men <- svydesign(ids=~1, data=data.ca[data.ca$manip.correct == 1 & data.ca$gender.lim == 0  & data.ca$lgbt == 0,])
##########################################################################
#
# Functions
#
##########################################################################
add.bars <- function(svyby.object, bar.plot){
    if(grepl("as.factor", colnames(svyby.object)[1]) == TRUE){
        ind <- grep("_treat", colnames(svyby.object)) 
        means <- svyby.object[ind[1:4]]
        #means <- means[1:nrow(means), names(means)[1]]
        lower.ci <- means - (svyby.object[ind[5:8]] * 1.96)
        #lower.ci <- lower.ci[1:nrow(lower.ci), names(lower.ci)[1]]
        upper.ci <- means + (svyby.object[ind[5:8]] * 1.96)
        #upper.ci <- upper.ci[1:nrow(upper.ci), names(upper.ci)[1]]
        arrows(x0=bar.plot,
               y0=t(as.matrix(lower.ci)),
               y1=t(as.matrix(upper.ci)), 
               lwd=2, col='red', code=3, angle=90, length=.05)
    } else if(('racial_resentment' %in% colnames(svyby.object) & 'modern_sexism' %in% colnames(svyby.object))|
              ('racial_resentment.s' %in% colnames(svyby.object) & 'modern_sexism.s' %in% colnames(svyby.object))){
        ind <- length(svyby.object) - 1
        means <- svyby.object[,2:3]
        lower.ci <- means - (svyby.object[,4:5] * 1.96)
        upper.ci <- means + (svyby.object[,4:5] * 1.96)
        arrows(x0=bar.plot,
               y0=t(as.matrix(lower.ci)),
               y1=t(as.matrix(upper.ci)),
               lwd=2, col='red', code=3, angle=90, length=.05)
    } else if(('resentment1' %in% colnames(svyby.object) & 'resentment2' %in% colnames(svyby.object))|
              ('sexism1' %in% colnames(svyby.object) & 'sexism2' %in% colnames(svyby.object))){
      ind <- length(svyby.object) - 1
      means <- svyby.object[,grepl('resentment\\d|sexism\\d', colnames(svyby.object))]
      ses <- means[,(ncol(means)/2 + 1):ncol(means)]
      means <- means[,1:(ncol(means)/2 )]
      lower.ci <- means - (ses * 1.96)
      upper.ci <- means + (ses * 1.96)
      arrows(x0=bar.plot,
             y0=t(as.matrix(lower.ci)),
             y1=t(as.matrix(upper.ci)),
             lwd=2, col='red', code=3, angle=90, length=.05)
    } else {
        ind <- length(svyby.object) - 1
        means <- svyby.object[ind]
        means <- means[1:nrow(means), names(means)[1]]
        lower.ci <- svyby.object[ind] - (svyby.object[ind + 1] * 1.96)
        lower.ci <- lower.ci[1:nrow(lower.ci), names(lower.ci)[1]]
        upper.ci <- svyby.object[ind] + (svyby.object[ind + 1] * 1.96)
        upper.ci <- upper.ci[1:nrow(upper.ci), names(upper.ci)[1]]
        arrows(x0=bar.plot,
               y0=lower.ci,
               y1=upper.ci, 
               lwd=2, col='red', code=3, angle=90, length=.05)
    }
}


ScoreBarPlot <- function(x, treat, main='', data=data.ca, ylims=c(0,.45), by.rg = FALSE){
  data = data[data$manip.correct == 1,]
  treatment <- data[,treat]
  att <- data[,x]
  att[att == 0] <- NA
  treatment[att == 0] <- NA  
  rgl <- data$race_gender_lim
  if(by.rg == FALSE){
    #layout(mat=matrix(c(1, 2, 2, 2)))  
    par(mar=c(1,2.2,2,.5), mgp=c(1.25, .15, 0), family='sans', 
        cex.main=1, las=1, tck=-.001, xaxs='i', yaxs='i', cex.axis=1)  
    bp <- boxplot(att ~ treatment, horizontal = TRUE, axes=F, ylab='', main=main, xlab='', col=grey.colors(n=4),
                  ylim=c(0.5, 7.5))
    par(mar=c(3,2.2,0,.5))
    leg.labs <- levels(treatment)
    tab <- table(treatment, att)
    tab <- tab/rowSums(tab)
    bp <-barplot(tab, beside=TRUE, ylim=ylims, col=grey.colors(n=4), main='',
                 ylab='Percent')
    legend('top', leg.labs, col=grey.colors(n=4), fill=grey.colors(n=4),
           ncol=4, bty='n')
  } else {
    par(mar=c(3,1.2,2,.5), mgp=c(1.25, .15, 0), family='sans', 
        cex.main=1, las=1, tck=-.001, xaxs='i', yaxs='i', cex.axis=1)  
    bp <- boxplot(att ~ treatment + rgl, horizontal = F,ylab='Score', main=main, xlab='', col=grey.colors(n=4),
                  ylim=c(0.9, 7.8), xaxt='n')
    axis(1, at=c(2.5, 6.5, 10.5, 14.5), labels=c("MOC", "WOC", "White Men", "White Women"))
    abline(v=c(4.5, 8.5, 12.5))
    leg.labs <- levels(treatment)
    legend('top', leg.labs, col=grey.colors(n=4), fill=grey.colors(n=4),
           ncol=4)
  }
  
}


GetTreatBalance <- function(variable, treatment, data, across.groups=FALSE){
    treat_levels <- levels(data[,treatment])
    v <- data[,variable]
    tr <- data[,treatment]
    out1 <- t.test(x=v[tr == treat_levels[1]],
                   y=v[tr == treat_levels[2]])
    out2 <- t.test(x=v[tr == treat_levels[1]],
                   y=v[tr == treat_levels[3]])
    out3 <- t.test(x=v[tr == treat_levels[1]],
                   y=v[tr == treat_levels[4]])
    out.m <- c(out1$estimate[1], out1$estimate[2], out2$estimate[2], out3$estimate[2])
    out.m <- as.character(round(out.m, 3))
    ps <- c(out1$p.value, out2$p.value, out3$p.value)
    ps <- round(ps, 4)
    out <- c(out.m[1], paste(out.m[2:4], ' (', ps, ')', sep=''))
    names(out) <- paste("Mean: ", treat_levels, sep='')
    return(out)
}


plot.p.test <- function(perms, estimate, outcome, ylim, xlim, xlab){
    par(mar=c(2.2,2.2,1.75,.5), mgp=c(1.1, .1, 0), family='sans', 
        cex.main=1, tck=-.01, cex.axis=.75)
    plot(density(perms[1,], cut=2, na.rm=T), xlab=xlab, main=outcome, ylim=ylim, xlim=xlim)
    abline(v=estimate[1])
    lines(density(perms[2,], cut=2, na.rm=T), lty=2, col='red')
    abline(v=estimate[2], col='red')
    lines(density(perms[3,], cut=2, na.rm=T), lty=3, col='blue')
    abline(v=estimate[3], col='blue')
    if(nrow(perms) == 4){
        lines(density(perms[4,], cut=2, na.rm=T), lty=4, col='purple')
        abline(v=estimate[4], col='blue')
        #legend('topleft', rownames(perms), col=c('black', 'red', 'blue', 'purple'), lty=1:4,
        #       bty='n', cex=.75)
    } else if(nrow(perms) == 6){
        lines(density(perms[4,], cut=2, na.rm=TRUE), lty=4, col='purple')
        abline(v=estimate[4], col='blue')
        lines(density(perms[5,], cut=2, na.rm=TRUE), lty=5, col='green')
        abline(v=estimate[5], col='green')
        lines(density(perms[6,], cut=2, na.rm=TRUE), lty=6, col='yellow')
        abline(v=estimate[6], col='yellow')
        legend('topleft', legend=rownames(perms), col=c('black', 'red', 'blue', 'purple', 'green', 'yellow'), lty=1:6,
               bty='n', cex=.75)
    } else {
        legend('topleft', rownames(perms), col=c('black', 'red', 'blue'), lty=1:3,
               bty='n', cex=.75) 
    }
}


p.test <- function(data, outcome, treatment, test.type, outcome.adj='', n=5000){
    perms <- matrix(nrow=3, ncol=n)
    Tr <- data[, treatment]
    Tr.levels <- levels(Tr)
    rownames(perms) <- Tr.levels[2:4]
    if(test.type == "dim"){
        perms <- matrix(nrow=6, ncol=n)
        rownames(perms) <- c(Tr.levels[2:4], paste(Tr.levels[3:4], Tr.levels[2], sep='-'), paste(Tr.levels[4], Tr.levels[2], sep='-'))
        if(outcome.adj == 'abs'){
            Y <- abs(data[, outcome] - 4)
        } else if(outcome.adj == 'extreme'){
            Y <- ifelse(data[, outcome] == 1 | data[, outcome] == 7, 1, 0)
        } else if(outcome.adj == 'mid'){
            Y <- ifelse(data[, outcome] == 4, 1, 0)
        } else{
            Y <- data[, outcome] 
        }
        mean.c <- mean(Y[Tr==Tr.levels[1]], na.rm=T)
        mean.2 <- mean(Y[Tr==Tr.levels[2]], na.rm=T)
        mean.3 <- mean(Y[Tr==Tr.levels[3]], na.rm=T)
        mean.4 <- mean(Y[Tr==Tr.levels[4]], na.rm=T)
        out.1 <- (mean.2 - mean.c)
        out.2 <- (mean.3 - mean.c)
        out.3 <- (mean.4 - mean.c)
        out.4 <- (mean.3 - mean.2)
        out.5 <- (mean.4 - mean.2)
        out.6 <- (mean.4 - mean.3)
        estimate <- rbind(out.1, out.2, out.3, out.4, out.5, out.6)
        rownames(estimate) <- rownames(perms) 
        for(i in 1:n){
            Tr.p <- sample(Tr, length(Tr), replace=F)
            mean.c <- mean(Y[Tr.p==Tr.levels[1]], na.rm=T)
            mean.2 <- mean(Y[Tr.p==Tr.levels[2]], na.rm=T)
            mean.3 <- mean(Y[Tr.p==Tr.levels[3]], na.rm=T)
            mean.4 <- mean(Y[Tr.p==Tr.levels[4]], na.rm=T)
            perms[1, i] <- (mean.2 - mean.c)
            perms[2, i] <- (mean.3 - mean.c)
            perms[3, i] <- (mean.4 - mean.c)
            perms[4, i] <- (mean.3 - mean.2)
            perms[5, i] <- (mean.4 - mean.2)
            perms[6, i] <- (mean.4 - mean.3)
        }
        p.value.1 <- mean(abs(perms[1,]) >= abs(estimate[1]))
        p.value.2 <- mean(abs(perms[2,]) >= abs(estimate[2]))
        p.value.3 <- mean(abs(perms[3,]) >= abs(estimate[3]))
        p.value.4 <- mean(abs(perms[4,]) >= abs(estimate[4]))
        p.value.5 <- mean(abs(perms[5,]) >= abs(estimate[5]))
        p.value.6 <- mean(abs(perms[6,]) >= abs(estimate[6]))
        estimate <- cbind(estimate, 
                          c(p.value.1, p.value.2, p.value.3, p.value.4, p.value.5,p.value.6))
        colnames(estimate) <- c(paste(outcome, 'Estimate'), 'P Value')
        plot.p.test(perms, estimate, outcome, ylim=c(0, 3), 
                    xlab='Difference in Means', xlim=c(-1, 1))
    }   
    if(test.type == "ks"){
        Y <- data[, outcome]
        out.1 <- ks.test(x=Y[Tr==Tr.levels[2]], y=Y[Tr==Tr.levels[1]], exact=F)$statistic
        out.2 <- ks.test(x=Y[Tr==Tr.levels[3]], y=Y[Tr==Tr.levels[1]], exact=F)$statistic
        out.3 <- ks.test(x=Y[Tr==Tr.levels[4]], y=Y[Tr==Tr.levels[1]], exact=F)$statistic
        estimate <- rbind(out.1, out.2, out.3)
        rownames(estimate) <- Tr.levels[2:4]
        for(i in 1:n){
            Tr.p <- sample(Tr, length(Tr), replace=F)
            perms[1, i] <- ks.test(x=Y[Tr.p==Tr.levels[2]], y=Y[Tr.p==Tr.levels[1]], exact=F)$statistic
            perms[2, i] <- ks.test(x=Y[Tr.p==Tr.levels[3]], y=Y[Tr.p==Tr.levels[1]], exact=F)$statistic
            perms[3, i] <- ks.test(x=Y[Tr.p==Tr.levels[4]], y=Y[Tr.p==Tr.levels[1]], exact=F)$statistic
        }
        p.value.1 <- mean(abs(perms[1,]) >= abs(estimate[1]))
        p.value.2 <- mean(abs(perms[2,]) >= abs(estimate[2]))
        p.value.3 <- mean(abs(perms[3,]) >= abs(estimate[3]))
        estimate <- cbind(estimate, 
                          c(p.value.1, p.value.2, p.value.3))
        colnames(estimate) <- c(paste(outcome, 'Estimate'), 'P Value')
        plot.p.test(perms, estimate, outcome,ylim=c(0, 3), 
                    xlab='KS Statistic', xlim=c(-1, 1))
    }
    if(test.type == "normal"){
        Y <- data[, outcome]
        perms <- matrix(nrow=4, ncol=n)
        rownames(perms) <- Tr.levels
        out <- ks.test(x=Y[Tr==Tr.levels[1]], y='pnorm', mean=mean(Y), sd=sd(Y), exact=F)$statistic
        out.1 <- ks.test(x=Y[Tr==Tr.levels[2]], y='pnorm', mean=mean(Y), sd=sd(Y), exact=F)$statistic
        out.2 <- ks.test(x=Y[Tr==Tr.levels[3]], y='pnorm', mean=mean(Y), sd=sd(Y), exact=F)$statistic
        out.3 <- ks.test(x=Y[Tr==Tr.levels[4]], y='pnorm', mean=mean(Y), sd=sd(Y), exact=F)$statistic
        estimate <- rbind(out, out.1, out.2, out.3)
        rownames(estimate) <- Tr.levels
        for(i in 1:n){
            Tr.p <- sample(Tr, length(Tr), replace=F)
            perms[1, i] <- ks.test(x=Y[Tr.p==Tr.levels[1]], y='pnorm', mean=mean(Y), sd=sd(Y), exact=F)$statistic
            perms[2, i] <- ks.test(x=Y[Tr.p==Tr.levels[2]], y='pnorm', mean=mean(Y), sd=sd(Y), exact=F)$statistic
            perms[3, i] <- ks.test(x=Y[Tr.p==Tr.levels[3]], y='pnorm', mean=mean(Y), sd=sd(Y), exact=F)$statistic
            perms[4, i] <- ks.test(x=Y[Tr.p==Tr.levels[4]], y='pnorm', mean=mean(Y), sd=sd(Y), exact=F)$statistic
        }
        p.value <- mean(abs(perms[1,]) >= abs(estimate[1]))
        p.value.1 <- mean(abs(perms[2,]) >= abs(estimate[2]))
        p.value.2 <- mean(abs(perms[3,]) >= abs(estimate[3]))
        p.value.3 <- mean(abs(perms[4,]) >= abs(estimate[4]))
        estimate <- cbind(estimate, 
                          c(p.value.1, p.value.2, p.value.3))
        colnames(estimate) <- c(paste(outcome, 'Estimate'), 'P Value')
        plot.p.test(perms, estimate, outcome,ylim=c(0, 3), 
                    xlab='KS Statistic', xlim=c(-.25, .25))
    }
    if(test.type == "sd"){
        Y <- data[, outcome]
        perms <- matrix(nrow=3, ncol=n)
        rownames(perms) <- Tr.levels[2:4]
        out.1 <- var.test(x=Y[Tr==Tr.levels[1]], y=Y[Tr==Tr.levels[2]])$statistic
        out.2 <- var.test(x=Y[Tr==Tr.levels[1]], y=Y[Tr==Tr.levels[3]])$statistic
        out.3 <- var.test(x=Y[Tr==Tr.levels[1]], y=Y[Tr==Tr.levels[4]])$statistic
        estimate <- rbind(out.1, out.2, out.3)
        rownames(estimate) <- Tr.levels[2:4]
        for(i in 1:n){
            Tr.p <- sample(Tr, length(Tr), replace=F)
            perms[1, i] <- var.test(x=Y[Tr.p==Tr.levels[1]], y=Y[Tr.p==Tr.levels[2]])$statistic
            perms[2, i] <- var.test(x=Y[Tr.p==Tr.levels[1]], y=Y[Tr.p==Tr.levels[3]])$statistic
            perms[3, i] <- var.test(x=Y[Tr.p==Tr.levels[1]], y=Y[Tr.p==Tr.levels[4]])$statistic
        }
        p.value.1 <- mean(abs(perms[1,]) >= abs(estimate[1]))
        p.value.2 <- mean(abs(perms[2,]) >= abs(estimate[2]))
        p.value.3 <- mean(abs(perms[3,]) >= abs(estimate[3]))
        estimate <- cbind(estimate, 
                          c(p.value.1, p.value.2, p.value.3))
        colnames(estimate) <- c(paste(outcome, 'Estimate'), 'P Value')
        plot.p.test(perms, estimate, outcome,ylim=c(0, 1), 
                    xlab='F Statistic', xlim=c(0, 5))
    }
    return(list("estimate" = estimate, "perms"=perms))
}


test.wrap <- function(variable, test.type, data, outcome.adj='', n=10000, full=FALSE){
    if(variable == 'sexism'){
        par(mfrow=c(3, 2))
        s <- p.test(data, 'modern_sexism', 'sexism_treat', outcome.adj, test.type=test.type, n)$estimate
        if(full == TRUE){
            s1 <- p.test(data, 'sexism1', 'sexism_treat', outcome.adj, test.type=test.type, n)$estimate
            s2 <- p.test(data, 'sexism2', 'sexism_treat', outcome.adj, test.type=test.type, n)$estimate
            s3 <- p.test(data, 'sexism3', 'sexism_treat', outcome.adj, test.type=test.type, n)$estimate
            s4 <- p.test(data, 'sexism4', 'sexism_treat', outcome.adj, test.type=test.type, n)$estimate
            s5 <- p.test(data, 'sexism5', 'sexism_treat', outcome.adj, test.type=test.type, n)$estimate
            att.out <- cbind(s, s1, s2, s3, s4, s5)
        } else{
            att.out <- cbind(s)
        }
    }
    if(variable == 'resentment'){
        par(mfrow=c(3, 2))
        r <- p.test(data, 'racial_resentment', 'resentment_treat', outcome.adj, test.type=test.type, n)$estimate
        if(full == TRUE){
            r1 <- p.test(data, 'resentment1', 'resentment_treat', outcome.adj, test.type=test.type, n)$estimate
            r2 <- p.test(data, 'resentment2', 'resentment_treat', outcome.adj, test.type=test.type, n)$estimate
            r3 <- p.test(data, 'resentment3', 'resentment_treat', outcome.adj, test.type=test.type, n)$estimate
            r4 <- p.test(data, 'resentment4', 'resentment_treat', outcome.adj, test.type=test.type, n)$estimate
            att.out <- cbind(r, r1, r2, r3, r4)
        } else {
            att.out <- cbind(r)
        }
    }
    return(att.out)
}


cdf.plot <- function(data, variable, main=''){
    if(variable == 'racial_resentment'){
        par(mar=c(1.5,2,1.75,.5), mgp=c(1.2, .1, 0), family='sans', 
            cex.main=1, las=1, tck=-.001, yaxs='i', cex.axis=.75)
        if(main == ''){main='Resentment Empirical CDFs'}
        plot(ecdf(data[data$resentment_treat == 'Control', variable]), xlim=c(1, 7), verticals=T, main=main, ylab='Cumulative Density')
        lines(ecdf(data[data$resentment_treat == 'Men', variable]), xlim=c(1, 7), pch=2, col='red', verticals=T, lty=2)
        lines(ecdf(data[data$resentment_treat == 'Women', variable]), xlim=c(1, 7), pch=3, col='blue', verticals=T, lt=3)
        lines(ecdf(data[data$resentment_treat == 'LGBT', variable]), xlim=c(1, 7), pch=4, col='slategrey', verticals=T, lty=4)
        legend('bottomright', c('Control', 'Men', 'Women', 'LGBT'), lty=1:4, pch=1:4, col=c('black', 'red', 'blue', 'slategrey'), bty='n')
        ks.m <- round(ks.boot(Co=data[data$resentment_treat == 'Control', variable], 
                              Tr=data[data$resentment_treat == 'Men', variable], nboots=10000)$ks.boot.pvalue, 4)
        ks.w <- round(ks.boot(Co=data[data$resentment_treat == 'Control', variable], 
                              Tr=data[data$resentment_treat == 'Women', variable], nboots=10000)$ks.boot.pvalue, 4)
        ks.l <- round(ks.boot(Co=data[data$resentment_treat == 'Control', variable], 
                              Tr=data[data$resentment_treat == 'LGBT', variable], nboots=10000)$ks.boot.pvalue, 4)
        ks.l1 <- round(ks.boot(Co=data[data$resentment_treat == 'Men', variable], 
                              Tr=data[data$resentment_treat == 'Women', variable], nboots=10000)$ks.boot.pvalue, 4)
        ks.l2 <- round(ks.boot(Co=data[data$resentment_treat == 'Men', variable], 
                              Tr=data[data$resentment_treat == 'LGBT', variable], nboots=10000)$ks.boot.pvalue, 4)
        text(x=2, y=.8, paste("KS Men:", ks.m), cex=.75)
        text(x=2, y=.75, paste("KS Women:", ks.w), cex=.75)
        text(x=2, y=.7, paste("KS LGBT:", ks.l), cex=.75)
        text(x=2, y=.65, paste("KS Women-Men:", ks.l1), cex=.75)
        text(x=2, y=.6, paste("KS LGBT-Men:", ks.l2), cex=.75)
        box()
    } else if(variable == 'modern_sexism'){
        par(mar=c(1.5,2,1.75,.5), mgp=c(1.2, .1, 0), family='sans', 
            cex.main=1, las=1, yaxs='i', tck=-.001, cex.axis=.75)
        if(main == ''){main='Sexism Empirical CDFs'}  
        plot(ecdf(data[data$sexism_treat == 'Control', variable]), xlim=c(1, 7), verticals=T, main=main, ylab='Cumulative Density')
        lines(ecdf(data[data$sexism_treat == 'White', variable]), xlim=c(1, 7), pch=2, col='red', verticals=T, lty=2)
        lines(ecdf(data[data$sexism_treat == 'Black', variable]), xlim=c(1, 7), pch=3, col='blue', verticals=T, lty=3)
        lines(ecdf(data[data$sexism_treat == 'LGBT', variable]), xlim=c(1, 7), pch=4, col='slategrey', verticals=T, lty=4)
        legend('bottomright', c('Control', 'White', 'Black', 'LGBT'), lty=1:4, pch=1:4, col=c('black', 'red', 'blue', 'slategrey'), bty='n')
        ks.m <- round(ks.boot(Co=data[data$sexism_treat == 'Control', variable], 
                              Tr=data[data$sexism_treat == 'White', variable], nboots=10000)$ks.boot.pvalue, 4)
        ks.w <- round(ks.boot(Co=data[data$sexism_treat == 'Control', variable], 
                              Tr=data[data$sexism_treat == 'Black', variable], nboots=10000)$ks.boot.pvalue, 4)
        ks.l <- round(ks.boot(Co=data[data$sexism_treat == 'Control', variable], 
                              Tr=data[data$sexism_treat == 'LGBT', variable], nboots=10000)$ks.boot.pvalue, 4)
        ks.l1 <- round(ks.boot(Co=data[data$sexism_treat == 'Black', variable], 
                              Tr=data[data$sexism_treat == 'LGBT', variable], nboots=10000)$ks.boot.pvalue, 4)
        ks.l2 <- round(ks.boot(Co=data[data$sexism_treat == 'White', variable], 
                              Tr=data[data$sexism_treat == 'LGBT', variable], nboots=10000)$ks.boot.pvalue, 4)
        text(x=2, y=.8, paste("KS White:", ks.m))
        text(x=2, y=.75, paste("KS Black:", ks.w))
        text(x=2, y=.7, paste("KS LGBT:", ks.l))
        text(x=2, y=.65, paste("KS Black-White:", ks.l1))
        text(x=2, y=.6, paste("KS LGBT-White:", ks.l2))
        box()
    } else if(variable == 'sexism'){
        par(mfrow=c(3,2), mar=c(1.5,2,1.75,.5), mgp=c(1.2, .1, 0), family='sans', 
            cex.main=1, las=1, tck=-.01, cex.axis=.75)
        plot(ecdf(data[data$sexism_treat == 'Control', paste(variable, "1", sep="")]), xlim=c(1, 7), verticals=T)
        lines(ecdf(data[data$sexism_treat == 'White', paste(variable, "1", sep="")]), xlim=c(1, 7), pch=2, col='red', verticals=T)
        lines(ecdf(data[data$sexism_treat == 'Black', paste(variable, "1", sep="")]), xlim=c(1, 7), pch=3, col='blue', verticals=T)
        lines(ecdf(data[data$sexism_treat == 'LGBT', paste(variable, "1", sep="")]), xlim=c(1, 7), pch=4, col='green', verticals=T)
        plot(ecdf(data[data$sexism_treat == 'Control', paste(variable, "2", sep="")]), xlim=c(1, 7), verticals=T)
        lines(ecdf(data[data$sexism_treat == 'White', paste(variable, "2", sep="")]), xlim=c(1, 7), pch=2, col='red', verticals=T)
        lines(ecdf(data[data$sexism_treat == 'Black', paste(variable, "2", sep="")]), xlim=c(1, 7), pch=3, col='blue', verticals=T)
        lines(ecdf(data[data$sexism_treat == 'LGBT', paste(variable, "2", sep="")]), xlim=c(1, 7), pch=4, col='green', verticals=T)
        plot(ecdf(data[data$sexism_treat == 'Control', paste(variable, "3", sep="")]), xlim=c(1, 7), verticals=T)
        lines(ecdf(data[data$sexism_treat == 'White', paste(variable, "3", sep="")]), xlim=c(1, 7), pch=2, col='red', verticals=T)
        lines(ecdf(data[data$sexism_treat == 'Black', paste(variable, "3", sep="")]), xlim=c(1, 7), pch=3, col='blue', verticals=T)
        lines(ecdf(data[data$sexism_treat == 'LGBT', paste(variable, "3", sep="")]), xlim=c(1, 7), pch=4, col='green', verticals=T)
        plot(ecdf(data[data$sexism_treat == 'Control', paste(variable, "4", sep="")]), xlim=c(1, 7), verticals=T)
        lines(ecdf(data[data$sexism_treat == 'White', paste(variable, "4", sep="")]), xlim=c(1, 7), pch=2, col='red', verticals=T)
        lines(ecdf(data[data$sexism_treat == 'Black', paste(variable, "4", sep="")]), xlim=c(1, 7), pch=3, col='blue', verticals=T)
        lines(ecdf(data[data$sexism_treat == 'LGBT', paste(variable, "4", sep="")]), xlim=c(1, 7), pch=4, col='green', verticals=T)
        plot(ecdf(data[data$sexism_treat == 'Control', paste(variable, "5", sep="")]), xlim=c(1, 7), verticals=T)
        lines(ecdf(data[data$sexism_treat == 'White', paste(variable, "5", sep="")]), xlim=c(1, 7), pch=2, col='red', verticals=T)
        lines(ecdf(data[data$sexism_treat == 'Black', paste(variable, "5", sep="")]), xlim=c(1, 7), pch=3, col='blue', verticals=T)
        lines(ecdf(data[data$sexism_treat == 'LGBT', paste(variable, "5", sep="")]), xlim=c(1, 7), pch=4, col='green', verticals=T)
    } else{
        par(mfrow=c(2,2), mar=c(1.5,2,1.75,.5), mgp=c(1.2, .1, 0), family='sans', 
            cex.main=1, las=1, tck=-.01, cex.axis=.75)
        plot(ecdf(data[data$resentment_treat == 'Control', paste(variable, "1", sep="")]), xlim=c(1, 7), verticals=T)
        lines(ecdf(data[data$resentment_treat == 'Men', paste(variable, "1", sep="")]), xlim=c(1, 7), pch=2, col='red', verticals=T)
        lines(ecdf(data[data$resentment_treat == 'Women', paste(variable, "1", sep="")]), xlim=c(1, 7), pch=3, col='blue', verticals=T)
        lines(ecdf(data[data$resentment_treat == 'LGBT', paste(variable, "1", sep="")]), xlim=c(1, 7), pch=4, col='green', verticals=T)
        plot(ecdf(data[data$resentment_treat == 'Control', paste(variable, "2", sep="")]), xlim=c(1, 7), verticals=T)
        lines(ecdf(data[data$resentment_treat == 'Men', paste(variable, "2", sep="")]), xlim=c(1, 7), pch=2, col='red', verticals=T)
        lines(ecdf(data[data$resentment_treat == 'Women', paste(variable, "2", sep="")]), xlim=c(1, 7), pch=3, col='blue', verticals=T)
        lines(ecdf(data[data$resentment_treat == 'LGBT', paste(variable, "2", sep="")]), xlim=c(1, 7), pch=4, col='green', verticals=T)
        plot(ecdf(data[data$resentment_treat == 'Control', paste(variable, "2", sep="")]), xlim=c(1, 7), verticals=T)
        lines(ecdf(data[data$resentment_treat == 'Men', paste(variable, "3", sep="")]), xlim=c(1, 7), pch=2, col='red', verticals=T)
        lines(ecdf(data[data$resentment_treat == 'Women', paste(variable, "3", sep="")]), xlim=c(1, 7), pch=3, col='blue', verticals=T)
        lines(ecdf(data[data$resentment_treat == 'LGBT', paste(variable, "3", sep="")]), xlim=c(1, 7), pch=4, col='green', verticals=T)
        plot(ecdf(data[data$resentment_treat == 'Control', paste(variable, "4", sep="")]), xlim=c(1, 7), verticals=T)
        lines(ecdf(data[data$resentment_treat == 'Men', paste(variable, "4", sep="")]), xlim=c(1, 7), pch=2, col='red', verticals=T)
        lines(ecdf(data[data$resentment_treat == 'Women', paste(variable, "4", sep="")]), xlim=c(1, 7), pch=3, col='blue', verticals=T)
        lines(ecdf(data[data$resentment_treat == 'LGBT', paste(variable, "4", sep="")]), xlim=c(1, 7), pch=4, col='green', verticals=T)
    }
}


get.lim.adj <- function(attitude, data, outcome=NULL, outcome.adj = '', var=NULL, manip.var='manip.correct', out.type='lm', boot.n=10000){
  data = data[which(data[,manip.var] == 1),]
  set.seed(08540)
  if(is.null(outcome) == TRUE){ # tab:black RR lgbt
    if(attitude == 'resentment'){ 
      outcome <- 'racial_resentment' 
      Tr.levels <- levels(data$resentment_treat)
      Tr <- data$resentment_treat
    }
    if(attitude == 'sexism'){ 
      outcome <- 'modern_sexism' 
      Tr.levels <- levels(data$sexism_treat)
      Tr <- data$sexism_treat
    }
  }
  if(is.null(manip.var) == TRUE){
    if(attitude == 'resentment'){ manip.var <- 'manip.correct.resentment' }
    if(attitude == 'sexism'){ manip.var <- 'manip.correct.sexism' }
  }
  if(outcome.adj == 'abs'){
    if(outcome == 'racial_resentment.s' | outcome == 'modern_sexism.s'){
      data[,'outcome'] <- abs(data[, outcome])
      Y <- data$outcome
    } else {
      data[,'outcome'] <- abs(data[, outcome] - 4)
      Y <- data$outcome
    }
  } else if(outcome.adj == 'abs mean'){
    data[,'outcome'] <- abs(data[, outcome] - mean(data[, outcome], na.rm=T))
    Y <- data$outcome
  } else if(outcome.adj == 'extreme'){
    #data[,'outcome'] <- ifelse(data[, outcome] == 1 | data[, outcome] == 7, 1, 0)
    data[,'outcome'] <- ifelse(data[, outcome] <= 2 | data[, outcome] >= 6, 1, 0)
    Y <- data$outcome
  } else if(outcome.adj == 'extreme low'){
    #data[,'outcome'] <- ifelse(data[, outcome] == 1 | data[, outcome] == 7, 1, 0)
    data[,'outcome'] <- ifelse(data[, outcome] <= 2, 1, 0)
    Y <- data$outcome
  } else if(outcome.adj == 'extreme high'){
    #data[,'outcome'] <- ifelse(data[, outcome] == 1 | data[, outcome] == 7, 1, 0)
    data[,'outcome'] <- ifelse(data[, outcome] >= 6, 1, 0)
    Y <- data$outcome
  } else if(outcome.adj == 'less 4'){
    #data[,'outcome'] <- ifelse(data[, outcome] == 1 | data[, outcome] == 7, 1, 0)
    data[,'outcome'] <- ifelse(data[, outcome] < 4, 1, 0)
    data$outcome[data$outcome == 4] <- NA
    Y <- data$outcome
  } else if(outcome.adj == 'more 4'){
    #data[,'outcome'] <- ifelse(data[, outcome] == 1 | data[, outcome] == 7, 1, 0)
    data[,'outcome'] <- ifelse(data[, outcome] > 4, 1, 0)
    data$outcome[data$outcome == 4] <- NA
    Y <- data$outcome
  } else if(outcome.adj == 'mid'){
    #data[,'outcome'] <- ifelse(data[, outcome] == 4, 1, 0)
    data[,'outcome'] <- ifelse(data[, outcome] >= 3.75 & data[, outcome] <= 4.25, 1, 0)
    Y <- data$outcome
  } else{
    data[,'outcome'] <- data[,outcome]
    Y <- data$outcome
  }
  out.mat <-matrix(ncol=5, nrow=4)
  if(grepl('resentment', outcome)){
    reg.out <- lm(outcome ~ resentment_treat + question_order, data=data)
    reg.out.2 <- lm(outcome ~ I(relevel(resentment_treat, 'Men')) + question_order, data=data)
    reg.out.3 <- lm(outcome ~ I(relevel(resentment_treat, 'Women')) + question_order, data=data)
    n <- summary(data[, 'resentment_treat'])
    rownames(out.mat) <- c("Black People", "Black Men", "Black Women", "LGBT Black People")
    if(nrow(data) == nrow(data.ca[data.ca[,manip.var] == 1,])){
      colnames(out.mat) <- c("Mean", "ATE (Black People)", "ATE (Men)", "ATE (Women)",  "N")
      sub=FALSE
    } else {
      colnames(out.mat) <- c("Mean", "CATE (Black People)", "CATE (Men)", "CATE (Women)",  "N")
      sub=TRUE
    }
    means <- round(tapply(data$outcome, data$resentment_treat, mean, na.rm=T),2)
  } else {
    reg.out <- lm(outcome ~ sexism_treat + question_order, data=data)
    reg.out.2 <- lm(outcome ~ I(relevel(sexism_treat, 'White')) + question_order, data=data)
    reg.out.3 <- lm(outcome ~ I(relevel(sexism_treat, 'Black')) + question_order, data=data)
    n <- summary(data[, 'sexism_treat'])
    rownames(out.mat) <- c("Women", "White Women", "Black Women", "LBT Women")
    if(nrow(data) == nrow(data.ca[data.ca[,manip.var] == 1,])){
      colnames(out.mat) <- c("Mean", "ATE (Women)", "ATE (White)", "ATE (Black)", "N")
      sub=FALSE
    } else {
      colnames(out.mat) <- c("Mean", "CATE (Women)", "CATE (White)", "CATE (Black)", "N")
      sub=TRUE
    }
    means <- round(tapply(data$outcome, data$sexism_treat, mean, na.rm=T),2)
  }
  if(out.type == 'lm'){
    out.1 <- coeftest(reg.out, vcov=sandwich)
    out.2 <- coeftest(reg.out.2, vcov=sandwich)
    out.3 <- coeftest(reg.out.3, vcov=sandwich)
  
    est.coef <- c(out.1[1:4, 1], out.2[3:4, 1], out.3[4, 1])
    std <- c(out.1[1:4, 2], out.2[3:4, 2], out.3[4, 2])
    p.vals  <- c(out.1[1:4, 4], out.2[3:4, 4], out.3[4, 4])
    p.vals.adj <- p.adjust(p.vals, 'BH', n=15)
    #p.vals.adj <- c(p.adjust(p.vals[1:4], 'BH', n=4), p.adjust(p.vals[5:6], 'BH', n=4), p.adjust(p.vals[7], 'BH', n=4))
    est.coef <- as.character(round(est.coef, 2))
    est.coef <- paste0(est.coef, ' (', round(std, 3), ')')
    est.coef[which(p.vals <= .05)] <- paste(est.coef[which(p.vals <= .05)], '+')
    est.coef[which(p.vals.adj <= .05)] <- paste0(est.coef[which(p.vals.adj <= .05)], '*')
    
    out.mat[,1] <- means
    out.mat[,2:4] <- '-' 
    out.mat[2:4,2] <- est.coef[2:4]
    out.mat[3:4,3] <- est.coef[5:6]
    out.mat[4,4] <- est.coef[7]
    out.mat[,5] <- n
  } else if(out.type == 'boot'){
    means <- tapply(Y, Tr, mean, na.rm=T)
    out.mat[,1] <- round(means, 2)
    estimate <- rbind(means[2] - means[1], 
                      means[3] - means[1],
                      means[4] - means[1],
                      means[3] - means[2],
                      means[4] - means[2],
                      means[4] - means[3])
    boots <- matrix(nrow=6, ncol=boot.n)
    for(i in 1:boot.n){
      i.boot <- sample(1:length(Tr), length(Tr), replace=T)
      Y.boot <- Y[i.boot]
      Tr.boot <- Tr[i.boot]
      means <- tapply(Y.boot, Tr.boot, mean, na.rm=T)
      mean.c <- means[1]
      mean.2 <- means[2]
      mean.3 <- means[3]
      mean.4 <- means[4]
      boots[1, i] <- (mean.2 - mean.c)
      boots[2, i] <- (mean.3 - mean.c)
      boots[3, i] <- (mean.4 - mean.c)
      boots[4, i] <- (mean.3 - mean.2)
      boots[5, i] <- (mean.4 - mean.2)
      boots[6, i] <- (mean.4 - mean.3)
    }
    cis <- apply(boots,1 , quantile, probs = c(.025, .5, .975), na.rm=T)
    cis.adj <- apply(boots,1 , quantile, probs = c((.05/2)/15, .5, 1-(.05/2)/15, na.rm=T))
    est.coef <- cis[2,]
    std <- apply(boots, 1, sd, na.rm=T)
    sig = NULL
    sig.adj = NULL
    for(i in 1:nrow(boots)){
      if(sign(cis[1,i]) == sign(cis[3, i])){
        sig[i] <- " +"
      } else {
        sig[i] <- ''
      }
      if(sign(cis.adj[1,i]) == sign(cis.adj[3, i])){
        sig.adj[i] <- "*"
      } else {
        sig.adj[i] <- ''
      }
    }

    est.coef <- as.character(round(est.coef, 2))
    est.coef <- paste0(est.coef, ' (', round(std, 3), ')')
    est.coef <- paste0(est.coef, sig)
    est.coef <- paste0(est.coef, sig.adj)
    
    out.mat[,2:4] <- '-' 
    out.mat[2:4,2] <- est.coef[1:3]
    out.mat[3:4,3] <- est.coef[4:5]
    out.mat[4,4] <- est.coef[6]
    out.mat[,5] <- n
  } else if(out.type == 'perm'){
    means <- tapply(Y, Tr, mean, na.rm=T)
    out.mat[,1] <- round(means, 2)
    estimate <- rbind(means[2] - means[1], 
                      means[3] - means[1],
                      means[4] - means[1],
                      means[3] - means[2],
                      means[4] - means[2],
                      means[4] - means[3])
    boots <- matrix(nrow=6, ncol=boot.n)
    rownames(boots) <- c(Tr.levels[2:4], paste(Tr.levels[3], Tr.levels[2]), paste(Tr.levels[4], Tr.levels[2]), paste(Tr.levels[4], Tr.levels[3]))
    for(i in 1:boot.n){
      Tr.boot <- sample(Tr, length(Tr), replace=F)
      means <- tapply(Y, Tr.boot, mean, na.rm=T)
      mean.c <- means[1]
      mean.2 <- means[2]
      mean.3 <- means[3]
      mean.4 <- means[4]
      boots[1, i] <- (mean.2 - mean.c)
      boots[2, i] <- (mean.3 - mean.c)
      boots[3, i] <- (mean.4 - mean.c)
      boots[4, i] <- (mean.3 - mean.2)
      boots[5, i] <- (mean.4 - mean.2)
      boots[6, i] <- (mean.4 - mean.3)
    }
    est.coef <- estimate
    std <- apply(boots, 1, sd, na.rm=T)
    
    p.vals <- c(
      mean(abs(boots[1,]) >= abs(estimate[1])),
      mean(abs(boots[2,]) >= abs(estimate[2])),
      mean(abs(boots[3,]) >= abs(estimate[3])),
      mean(abs(boots[4,]) >= abs(estimate[4])),
      mean(abs(boots[5,]) >= abs(estimate[5])),
      mean(abs(boots[6,]) >= abs(estimate[6]))
      )
    p.vals.adj <- p.adjust(p.vals, "BH", n=15)
    
    est.coef <- as.character(round(est.coef, 2))
    est.coef <- paste0(est.coef, ' (', round(std, 3), ')')
    est.coef[which(p.vals <= .05)] <- paste(est.coef[which(p.vals <= .05)], '*')
    #est.coef[which(p.vals.adj <= .05)] <- paste0(est.coef[which(p.vals.adj <= .05)], '*')
    
    out.mat[,2:4] <- '-' 
    out.mat[2:4,2] <- est.coef[1:3]
    out.mat[3:4,3] <- est.coef[4:5]
    out.mat[4,4] <- est.coef[6]
    out.mat[,5] <- n
    plot.p.test(boots, estimate, outcome, ylim=c(0, 2), xlim=c(-1, 1), xlab='')
  }
  return(out.mat)
}


ReturnDescriptive <- function(var, label.names=NULL){
  tab <- table(data.ca[, var])
  if(is.null(label.names) == FALSE){
    names(tab) <- label.names
  }
  #stargazer(t(as.matrix(tab)))
  return(t(as.matrix(tab)))
}


ScoreCor <- function(data){
  scores <- cbind(data$manip.bp , data$manip.bm , data$manip.bw , data$manip.bl , data$manip.w , data$manip.ww , data$manip.wl,
                  data$resentment_treat == 'Control',
                  data$resentment_treat == 'Men',
                  data$resentment_treat == 'Women',
                  data$resentment_treat == 'LGBT',
                  data$sexism_treat == 'Control',
                  data$sexism_treat == 'Black',
                  data$sexism_treat == 'White',
                  data$sexism_treat == 'LGBT'
  )
  score.cor <- cor(scores, use='complete.obs')
  rownames(score.cor) <- colnames(score.cor) <- c('Black People', 'Black Men', 'Black Women', 'LGBT Black', 'Women', 'White Women', 'LBT Women', 
                                                  'Resent: Control', 'Resent: Men', 'Resent: Women', 'Resent: LGBT',
                                                  'Sexism: Control', 'Sexism: Black', 'Sexism: White', 'Sexism: LGBT')
  score.cor[8:15,8:15] <- NA
  return(score.cor)
}


get.cor <- function(data){
  cor.mat <- matrix(ncol=4, nrow=4)
  rownames(cor.mat) <- levels(data.ca$resentment_treat)
  colnames(cor.mat) <- levels(data.ca$sexism_treat)
  for(i in 1:4){
    for(j in 1:4){
      cor.mat[i,j] <- cor(data[data$resentment_treat == rownames(cor.mat)[i] & data$sexism_treat == colnames(cor.mat)[j], 'racial_resentment'], 
                          data[data$resentment_treat == rownames(cor.mat)[i] & data$sexism_treat == colnames(cor.mat)[j], 'modern_sexism'], use='complete.obs')
    }
  }
  return(cor.mat)
}

##########################################################################
#
# Main Text Table 1
#
##########################################################################
stargazer((lm(modern_sexism ~ I(treat_full3 == 'Mix')*racial_resentment, data=data.ca, subset = manip.correct == 1)), 
          style='ajps', type='text',
          covariate.labels = c('No Treatment Group Overlap', 'Racial Resentment', 'No Treatment Group Overlap: Racial Resentment'),
          label='tab:overlap reg')

##########################################################################
#
# Main Text Table 2
#
##########################################################################
tab <- svyby(~cross, ~treat_full5, design2, svymean, na.rm=TRUE, drop.empty.groups=F)
bar.plot <- plot(tab, legend=T,  main="Racial Resentment: ", ylab="Average Score",
                 col=hcl.colors(n=4, "Blue-Red 2", rev=F, alpha=1), xpd=F, las=3, ylim=c(0, .5))
stargazer(as.matrix(tab)[c(1, 3, 2, 4),], title='Proportion of Inconsistent Attitudes by Treatment', label='tab:crossed', type='text')
table(data.ca$treat_full5)

# Limited to drop LGBT Groups
tab <- svyby(~cross, ~treat_full5b, design2, svymean, na.rm=TRUE, drop.empty.groups=F)
bar.plot <- plot(tab, legend=T,  main="Racial Resentment: ", ylab="Average Score",
                 col=hcl.colors(n=4, "Blue-Red 2", rev=F, alpha=1), xpd=F, las=3, ylim=c(0, .5))
stargazer(as.matrix(tab)[c(1, 3, 2, 4),], title='Proportion of Inconsistent Attitudes by Treatment (Drop LGBT)', label='tab:crossed', type='text')
table(data.ca$treat_full5[data.ca$manip.correct == 1])


##########################################################################
#
# CATE Tables (Main Text 3, 4, SI "Additional Subgroup Estimates" and SI "Racial Resentment CATEs")
#
##########################################################################
## Full Sample
stargazer(get.lim.adj(data=data.ca, attitude = 'resentment'),
          get.lim.adj(data=data.ca, attitude = 'sexism'),
          type='text',
          notes = c("", "+ $p < .05$, * FDR-adjusted $p < .05$"),
          title=c('Racial Resentment: Full Data',
                  'Modern Sexism: Full Data'), 
          label=c('tab:full RR',
                  'tab:full MS'))

## By Race
stargazer(get.lim.adj(data=data.ca[data.ca$r.white == 1 & data.ca$lgbt == 0,], attitude = 'resentment'),
          get.lim.adj(data=data.ca[data.ca$r.white == 1 & data.ca$lgbt == 0,], attitude = 'sexism'),
          type='text',
          notes = c("", "+ $p < .05$, * FDR-adjusted $p < .05$"),
          title=c('Racial Resentment: White, Non-LGBTQ Respondents',
                  'Modern Sexism: White, Non-LGBTQ Respondents'), 
          label=c('tab:white RR',
                  'tab:white MS'))
stargazer(get.lim.adj(data=data.ca[data.ca$r.white == 0 & data.ca$lgbt == 0,], attitude = 'resentment'),
          get.lim.adj(data=data.ca[data.ca$r.white == 0 & data.ca$lgbt == 0,], attitude = 'sexism'),
          type='text',
          notes = c("", "+ $p < .05$, * FDR-adjusted $p < .05$"),
          title=c('Racial Resentment: POC, Non-LGBTQ Respondents',
                  'Modern Sexism: POC, Non-LGBTQ Respondents'), 
          label=c('tab:poc RR',
                  'tab:poc MS'))

## By Gender
stargazer(get.lim.adj(data=data.ca[data.ca$gender.lim == 1 & data.ca$lgbt == 0,], attitude = 'resentment'),
          get.lim.adj(data=data.ca[data.ca$gender.lim == 1 & data.ca$lgbt == 0,], attitude = 'sexism'),
          type='text',
          notes = c("", "+ $p < .05$, * FDR-adjusted $p < .05$"),
          title=c('Racial Resentment: Men, Non-LGBTQ Respondents',
                  'Modern Sexism: Men, Non-LGBTQ Respondents'), 
          label=c('tab:men RR',
                  'tab:men MS'))
stargazer(get.lim.adj(data=data.ca[data.ca$gender.lim == 0 & data.ca$lgbt == 0,], attitude = 'resentment'),
          get.lim.adj(data=data.ca[data.ca$gender.lim == 0 & data.ca$lgbt == 0,], attitude = 'sexism'),
          type='text',
          notes = c("", "+ $p < .05$, * FDR-adjusted $p < .05$"),
          title=c('Racial Resentment: Women, Non-LGBTQ Respondents',
                  'Modern Sexism: Women, Non-LGBTQ Respondents'), 
          label=c('tab:women RR',
                  'tab:women MS'))

## Black Respondents
stargazer(get.lim.adj(data=data.ca[data.ca$r.black == 1 & data.ca$lgbt == 0,], attitude = 'resentment'),
          get.lim.adj(data=data.ca[data.ca$r.black == 1 & data.ca$lgbt == 0,], attitude = 'sexism'),
          type='text',
          notes = c("", "+ $p < .05$, * FDR-adjusted $p < .05$"),
          title=c('Racial Resentment: Black, Non-LGBTQ Respondents',
                  'Modern Sexism: Black, Non-LGBTQ Respondents'), 
          label=c('tab:black RR',
                  'tab:black MS'))

## By Sexuality
stargazer(get.lim.adj(data=data.ca[data.ca$lgbt == 1,], attitude = 'resentment'),
          get.lim.adj(data=data.ca[data.ca$lgbt == 1,], attitude = 'sexism'),
          type='text',
          notes = c("", "+ $p < .05$, * FDR-adjusted $p < .05$"),
          title=c('Racial Resentment: LGBTQ Respondents',
                  'Modern Sexism: LGBTQ Respondents'), 
          label=c('tab:lgbt RR',
                  'tab:lgbt MS'))
stargazer(get.lim.adj(data=data.ca[data.ca$lgbt == 0,], attitude = 'resentment'),
          get.lim.adj(data=data.ca[data.ca$lgbt == 0,], attitude = 'sexism'),
          type='text',
          notes = c("", "+ $p < .05$, * FDR-adjusted $p < .05$"),
          title=c('Racial Resentment:Non-LGBTQ Respondents',
                  'Modern Sexism: Non-LGBTQ Respondents'), 
          label=c('tab:nonlgbt RR',
                  'tab:nonlgbt MS'))

##########################################################################
#
# Main Text Table 5
#
##########################################################################
stargazer(
  lm(sexism1 ~ sexism_treat, data=data.ca, subset = r.white==1),
  lm(sexism2 ~ sexism_treat, data=data.ca, subset = r.white==1),
  lm(sexism3 ~ sexism_treat, data=data.ca, subset = r.white==1),
  lm(sexism4 ~ sexism_treat, data=data.ca, subset = r.white==1),
  lm(sexism5 ~ sexism_treat, data=data.ca, subset = r.white==1),
  style='apsr', type='text',
  dep.var.labels = c('Disc. No Longer Problem', 'Equal Income', 'Equal Opportunity', 'Concerns too Much Attention', 'Hard Understand Concerns'),
  covariate.labels = c('White Women', 'Black Women', 'LBT Women'),
  label='tab:ms questions white',
  title='Modern Sexism  Treatment Effects by Question among White Respondents'
)


##########################################################################
#
# Main Text Table 6
#
##########################################################################
stargazer(
  lm(sexism1 ~ sexism_treat, data=data.ca, subset = r.white==0),
  lm(sexism2 ~ sexism_treat, data=data.ca, subset = r.white==0),
  lm(sexism3 ~ sexism_treat, data=data.ca, subset = r.white==0),
  lm(sexism4 ~ sexism_treat, data=data.ca, subset = r.white==0),
  lm(sexism5 ~ sexism_treat, data=data.ca, subset = r.white==0),
  style='apsr', #type='text',
  dep.var.labels = c('Disc. No Longer Problem', 'Equal Income', 'Equal Opportunity', 'Concerns too Much Attention', 'Hard Understand Concerns'),
  covariate.labels = c('White Women', 'Black Women', 'LBT Women'),
  label='tab:ms questions poc',
  title='Modern Sexism  Treatment Effects by Question among Respondents of Color'
)


##########################################################################
#
# Figure 1
#
##########################################################################
png('Figures/Figure_1_Resentment.png', width=8, height=4, units='in', res=500)
par(mfrow=c(1, 3), mar=c(2.5,1.75,.5,.5), mgp=c(.8, .1, 0), family='sans', 
    cex.main=1, las=1, tck=-.001, xaxs='i', yaxs='i', cex.axis=.65)
tab <- svyby(~racial_resentment, ~gender.lim * resentment_treat_names, design, svymean, na.rm=TRUE, drop.empty.groups=F)
bar.plot <- plot(tab, legend=F, ylim=c(1, 7), main="", ylab="Average Score", col=grey.colors(n=2), xpd=F)
legend('top', legend=c('Women', 'Men'), col=grey.colors(n=2), fill=grey.colors(n=2), bty='n', horiz=T, title='Respondent Gender')
add.bars(tab, bar.plot)
abline(h=4)
par(mar=c(2.5,.5,.5,.75))
tab <- svyby(~racial_resentment, ~race.lim * resentment_treat_names, design, svymean, na.rm=TRUE, drop.empty.groups=F)
bar.plot <- plot(tab, legend=F, ylim=c(1, 7), main="", ylab="", xlab= 'Resentment Wording Treatment', col=grey.colors(n=3), xpd=F)
legend('top', legend=c('White', 'Black', 'NBPOC'), col=grey.colors(n=3), fill=grey.colors(n=3), bty='n', horiz=T, title='Respondent Race')
add.bars(tab, bar.plot)
abline(h=4)
tab <- svyby(~racial_resentment, ~lgbt * resentment_treat_names, design, svymean, na.rm=TRUE, drop.empty.groups=F)
bar.plot <- plot(tab, legend=F, ylim=c(1, 7), main="", ylab="", col=grey.colors(n=2), xpd=F)
legend('top', legend=c("Non-LGBT", "LGBT"), col=grey.colors(n=2), fill=grey.colors(n=2), bty='n', horiz=T, title='Respondent Sexuality')
add.bars(tab, bar.plot)
abline(h=4)
dev.off()


png('Figures/Figure_1_Sexism.png', width=8, height=4, units='in', res=500)
par(mfrow=c(1, 3), mar=c(2.5,1.75,.5,.5), mgp=c(.8, .1, 0), family='sans', 
    cex.main=1, las=1, tck=-.001, xaxs='i', yaxs='i', cex.axis=.65)
tab <- svyby(~modern_sexism, ~gender.lim * sexism_treat_names, design, svymean, na.rm=TRUE, drop.empty.groups=F)
bar.plot <- plot(tab, legend=F, ylim=c(1, 7), main="", ylab="Average Score", col=grey.colors(n=2), xpd=F)
legend('top', legend=c('Women', 'Men'), col=grey.colors(n=2), fill=grey.colors(n=2), bty='n', horiz=T, title='Respondent Gender')
add.bars(tab, bar.plot)
abline(h=4)
par(mar=c(2.5,.5,.5,.75))
tab <- svyby(~modern_sexism, ~race.lim * sexism_treat_names, design, svymean, na.rm=TRUE, drop.empty.groups=F)
bar.plot <- plot(tab, legend=F, ylim=c(1, 7), main="", ylab="", col=grey.colors(n=3), xpd=F, xlab= 'Sexism Wording Treatment')
legend('top', legend=c('White', 'Black', 'NBPOC'), col=grey.colors(n=3), fill=grey.colors(n=3), bty='n', horiz=T, title='Respondent Race')
add.bars(tab, bar.plot)
abline(h=4)
tab <- svyby(~modern_sexism, ~lgbt * sexism_treat_names, design, svymean, na.rm=TRUE, drop.empty.groups=F)
bar.plot <- plot(tab, legend=F, ylim=c(1, 7), main="", ylab="", col=grey.colors(n=2), xpd=F)
legend('top', legend=c("Non-LGBT", "LGBT"), col=grey.colors(n=2), fill=grey.colors(n=2), bty='n', horiz=T, title='Respondent Sexuality')
add.bars(tab, bar.plot)
abline(h=4)
dev.off()

##########################################################################
#
# Figure 2
#
##########################################################################
png('Figures/Figure_3_Resentment.png', width=6, height=6, units='in', res=500)
cols=grey.colors(n=4, start=0, end=.7)
par(mfrow=c(2, 2), mar=c(2.2,2.5,1.75,.25), mgp=c(1.25, .15, 0), family='sans', 
    cex.main=1, las=1, tck=-.001, xaxs='i', yaxs='i', cex.axis=.75)
dat = data.ca[data.ca$r.white == 1,]
plot(density(dat$racial_resentment[dat$resentment_treat == 'Control' & dat$manip.correct == 1], cut=0, na.rm = T), ylim=c(0, .4), lwd=2, col=cols[1], main='White', xlab='Score')
lines(density(dat$racial_resentment[dat$resentment_treat == 'Men' & dat$manip.correct == 1], cut=0, na.rm = T), lty=2, lwd=2, col=cols[2])
lines(density(dat$racial_resentment[dat$resentment_treat == 'Women' & dat$manip.correct == 1], cut=0, na.rm = T), lty=3, lwd=2, col=cols[3])
lines(density(dat$racial_resentment[dat$resentment_treat == 'LGBT' & dat$manip.correct == 1], cut=0, na.rm = T), lty=4, lwd=2, col=cols[4])
legend('topleft', levels(dat$resentment_treat), col=cols, lty=1:4, bty='n')
abline(v=4, lwd=.5)
dat = data.ca[data.ca$r.white == 0,]
plot(density(dat$racial_resentment[dat$resentment_treat == 'Control' & dat$manip.correct == 1], cut=0, na.rm = T), ylim=c(0, .4), lwd=2, col=cols[1], main='People of Color', xlab='Score')
lines(density(dat$racial_resentment[dat$resentment_treat == 'Men' & dat$manip.correct == 1], cut=0, na.rm = T), lty=2, lwd=2, col=cols[2])
lines(density(dat$racial_resentment[dat$resentment_treat == 'Women' & dat$manip.correct == 1], cut=0, na.rm = T), lty=3, lwd=2, col=cols[3])
lines(density(dat$racial_resentment[dat$resentment_treat == 'LGBT' & dat$manip.correct == 1], cut=0, na.rm = T), lty=4, lwd=2, col=cols[4])
legend('topleft', levels(dat$resentment_treat), col=cols, lty=1:4, bty='n')
abline(v=4, lwd=.5)
dat = data.ca[data.ca$gender.lim == 1,]
plot(density(dat$racial_resentment[dat$resentment_treat == 'Control' & dat$manip.correct == 1], cut=0, na.rm = T), ylim=c(0, .4), lwd=2, col=cols[1], main='Men', xlab='Score')
lines(density(dat$racial_resentment[dat$resentment_treat == 'Men' & dat$manip.correct == 1], cut=0, na.rm = T), lty=2, lwd=2, col=cols[2])
lines(density(dat$racial_resentment[dat$resentment_treat == 'Women' & dat$manip.correct == 1], cut=0, na.rm = T), lty=3, lwd=2, col=cols[3])
lines(density(dat$racial_resentment[dat$resentment_treat == 'LGBT' & dat$manip.correct == 1], cut=0, na.rm = T), lty=4, lwd=2, col=cols[4])
legend('topleft', levels(dat$resentment_treat), col=cols, lty=1:4, bty='n')
abline(v=4, lwd=.5)
dat = data.ca[data.ca$gender.lim == 0,]
plot(density(dat$racial_resentment[dat$resentment_treat == 'Control' & dat$manip.correct == 1], cut=0, na.rm = T), ylim=c(0, .4), lwd=2, col=cols[1], main='Women', xlab='Score')
lines(density(dat$racial_resentment[dat$resentment_treat == 'Men' & dat$manip.correct == 1], cut=0, na.rm = T), lty=2, lwd=2, col=cols[2])
lines(density(dat$racial_resentment[dat$resentment_treat == 'Women' & dat$manip.correct == 1], cut=0, na.rm = T), lty=3, lwd=2, col=cols[3])
lines(density(dat$racial_resentment[dat$resentment_treat == 'LGBT' & dat$manip.correct == 1], cut=0, na.rm = T), lty=4, lwd=2, col=cols[4])
legend('topleft', levels(dat$resentment_treat), col=cols, lty=1:4, bty='n')
abline(v=4, lwd=.5)
dev.off()

png('Figures/Figure_3_Sexism.png', width=6, height=6, units='in', res=500)
cols=grey.colors(n=4, start=0, end=.7)
par(mfrow=c(2, 2), mar=c(2.2,2.5,1.75,.25), mgp=c(1.25, .15, 0), family='sans', 
    cex.main=1, las=1, tck=-.001, xaxs='i', yaxs='i', cex.axis=.75)
dat = data.ca[data.ca$r.white == 1,]
plot(density(dat$modern_sexism[dat$sexism_treat == 'Control' & dat$manip.correct == 1], cut=0, na.rm = T), ylim=c(0, .5), lwd=2, col=cols[1], main='White', xlab='Score')
lines(density(dat$modern_sexism[dat$sexism_treat == 'White' & dat$manip.correct == 1], cut=0, na.rm = T), lty=2, lwd=2, col=cols[2])
lines(density(dat$modern_sexism[dat$sexism_treat == 'Black' & dat$manip.correct == 1], cut=0, na.rm = T), lty=3, lwd=2, col=cols[3])
lines(density(dat$modern_sexism[dat$sexism_treat == 'LGBT' & dat$manip.correct == 1], cut=0, na.rm = T), lty=4, lwd=2, col=cols[4])
legend('topleft', levels(dat$sexism_treat), col=cols, lty=1:4, bty='n')
abline(v=4, lwd=.5)
dat = data.ca[data.ca$r.white == 0,]
plot(density(dat$modern_sexism[dat$sexism_treat == 'Control' & dat$manip.correct == 1], cut=0, na.rm = T), ylim=c(0, .5), lwd=2, col=cols[1], main='People of Color', xlab='Score')
lines(density(dat$modern_sexism[dat$sexism_treat == 'White' & dat$manip.correct == 1], cut=0, na.rm = T), lty=2, lwd=2, col=cols[2])
lines(density(dat$modern_sexism[dat$sexism_treat == 'Black' & dat$manip.correct == 1], cut=0, na.rm = T), lty=3, lwd=2, col=cols[3])
lines(density(dat$modern_sexism[dat$sexism_treat == 'LGBT' & dat$manip.correct == 1], cut=0, na.rm = T), lty=4, lwd=2, col=cols[4])
legend('topleft', levels(dat$sexism_treat), col=cols, lty=1:4, bty='n')
abline(v=4, lwd=.5)
dat = data.ca[data.ca$gender.lim == 1,]
plot(density(dat$modern_sexism[dat$sexism_treat == 'Control' & dat$manip.correct == 1], cut=0, na.rm = T), ylim=c(0, .5), lwd=2, col=cols[1], main='Men', xlab='Score')
lines(density(dat$modern_sexism[dat$sexism_treat == 'White' & dat$manip.correct == 1], cut=0, na.rm = T), lty=2, lwd=2, col=cols[2])
lines(density(dat$modern_sexism[dat$sexism_treat == 'Black' & dat$manip.correct == 1], cut=0, na.rm = T), lty=3, lwd=2, col=cols[3])
lines(density(dat$modern_sexism[dat$sexism_treat == 'LGBT' & dat$manip.correct == 1], cut=0, na.rm = T), lty=4, lwd=2, col=cols[4])
legend('topleft', levels(dat$sexism_treat), col=cols, lty=1:4, bty='n')
abline(v=4, lwd=.5)
dat = data.ca[data.ca$gender.lim == 0,]
plot(density(dat$modern_sexism[dat$sexism_treat == 'Control' & dat$manip.correct == 1], cut=0, na.rm = T), ylim=c(0, .5), lwd=2, col=cols[1], main='Women', xlab='Score')
lines(density(dat$modern_sexism[dat$sexism_treat == 'White' & dat$manip.correct == 1], cut=0, na.rm = T), lty=2, lwd=2, col=cols[2])
lines(density(dat$modern_sexism[dat$sexism_treat == 'Black' & dat$manip.correct == 1], cut=0, na.rm = T), lty=3, lwd=2, col=cols[3])
lines(density(dat$modern_sexism[dat$sexism_treat == 'LGBT' & dat$manip.correct == 1], cut=0, na.rm = T), lty=4, lwd=2, col=cols[4])
legend('topleft', levels(dat$sexism_treat), col=cols, lty=1:4, bty='n')
abline(v=4, lwd=.5)
dev.off()

##########################################################################
#
# Figure 3 (Appendix)
#
##########################################################################
png('Figures/Appendix_Figure_2_Resentment.png', width=6, height=6, units='in', res=500)
par(mar=c(2.,1.75,.75,.25), mgp=c(.9, .1, 0), family='sans', 
    cex.main=1, las=1, tck=-.001, xaxs='i', yaxs='i', cex.axis=.75)
tab <- svyby(~racial_resentment, ~resentment_treat + race_gender, design, svymean, na.rm=TRUE)
bar.plot <- plot(tab, legend=F, ylim=c(1, 7), main="", ylab="Average Score", xlab='Respondent Race-Gender',
                 col=brewer.pal(4, 'Greys'), xpd=F)
abline(h=4)
add.bars(tab, bar.plot)
legend('top', legend=c('Black People', 'Black Men', 'Black Women', 'LGBT Black People'), title='Wording Tretment',
       col=brewer.pal(4, 'Greys'), fill=brewer.pal(6, 'Greys'), ncol=4, bty='n', cex=.75)
dev.off()

png('Figures/Appendix_Figure_2_Sexism.png', width=6, height=6, units='in', res=500)
par(mar=c(2.,1.75,.75,.25), mgp=c(.9, .1, 0), family='sans', 
    cex.main=1, las=1, tck=-.001, xaxs='i', yaxs='i', cex.axis=.75)
tab <- svyby(~modern_sexism, ~sexism_treat + race_gender, design, svymean, na.rm=TRUE)
bar.plot <- plot(tab, legend=F, ylim=c(1, 7), main="", ylab="Average Score", xlab='Respondent Race-Gender',
                 col=brewer.pal(4, 'Greys'), xpd=F)
add.bars(tab, bar.plot)
abline(h=4)
legend('top', c('Women', 'White Women', 'Black Women', 'LBT Women'), title='Wording Tretment',
       col=brewer.pal(4, 'Greys'), fill=brewer.pal(6, 'Greys'), ncol=4, bty='n', cex=.75)
dev.off()

##########################################################################
#
# Figure 4 (Appendix)
#
##########################################################################
png('Figures/Figure_5_ECDF_RR.png', width=6, height=6, units='in', res=500)
cdf.plot(data.ca, 'racial_resentment', main=' ')
dev.off()
png('Figures/Figure_5_ECDF_MS.png', width=6, height=6, units='in', res=500)
cdf.plot(data.ca, 'modern_sexism', main=' ')
dev.off()

##########################################################################
#
# SI 1 Sample Collection and Demographics
#
##########################################################################
ReturnDescriptive('education', label.names = c())

data.ca$gender.desc <- data.ca$gender
levels(data.ca$gender.desc) <- c(NA, "Man", "Nonbinary", "Other", "Man", "Woman")

stargazer(
  ReturnDescriptive('race', label.names = c()),
  ReturnDescriptive('hispanic', label.names = c('Non-Hispanic', 'Hispanic')),
  ReturnDescriptive('gender.desc'),
  ReturnDescriptive('lgbt', label.names=c("Non-LGBT", "LGBT")),
  ReturnDescriptive('education', label.names = c("Some H.S", "H.S. Diploma","Vocational Training", "Some College", "Associates", "Bachelors", "Masters", "Doctorate")),
  #ReturnDescriptive('income'),
  ReturnDescriptive('pid', label.names = c("Strong Dem", "Weak Dem", "Lean Dem", "Indep", "lean Rep", "Weak Rep", "Strong Rep")),
  ReturnDescriptive('ideology', label.names = c("Very Lib", "Lib", "Slight Lib", "Moderate", "Slight Con", "Con", "Very Con")),
  title=c(
    "Race", "Hispanic", "Gender", "LGBTQ", "Education", "Income", "Party ID", "Ideology"
  )#, type='text'
)

# Add race-gender groups
stargazer(table(data.ca$race_gender), title='Respondent Race-Gender Group Size')

## Manipulation Check

summary(data.ca$manip.correct.second)
summary(data.ca$manip.correct.first)

table(data.ca$manip.correct, data.ca$manip.correct.second)
table(data.ca$manip.correct, data.ca$manip.correct.first)
table(data.ca$manip.correct.first, data.ca$manip.correct.second)

table(data.ca$manip.correct, data.ca$manip.correct.second, data.ca$question_order)

#png('Figures/Bar_Plot_Manip_Check_correct.png', width=7, height=7, units='in', res=500)
par(mar=c(5,1.5,1,.25), mgp=c(1, .15, 0), family='sans', 
    cex.main=1, las=1, tck=-.001, xaxs='i', yaxs='i', cex.axis=.75, xpd=F)
layout(matrix(nrow=2, ncol=2, c(1, 2, 3, 3), byrow=T))
tab <- svyby(~manip.correct.resentment + manip.correct.both, ~question_order * resentment_treat, design, svymean, na.rm=TRUE, drop.empty.groups=F)
bar.plot <- plot(tab, legend=T, ylim=c(0, 1), main="", ylab="Average")
add.bars(tab, bar.plot)
tab <- svyby(~manip.correct.sexism + manip.correct.both, ~question_order * sexism_treat, design, svymean, na.rm=TRUE, drop.empty.groups=F)
bar.plot <- plot(tab, legend=T, ylim=c(0, 1), main="", ylab="Average")
add.bars(tab, bar.plot)
tab <- svyby(~manip.correct.resentment + manip.correct.sexism + manip.correct.both + manip.correct, ~race_gender, design, svymean, na.rm=TRUE, drop.empty.groups=F)
bar.plot <- plot(tab, legend=T, ylim=c(0, 1), main="", ylab="Average")
add.bars(tab, bar.plot)


svyby(~manip.bp + manip.bm + manip.bw + manip.bl, ~resentment_treat, design, svymean, na.rm=TRUE, drop.empty.groups=F)
svyby(~manip.w + manip.bw+ manip.ww + manip.wl, ~sexism_treat, design, svymean, na.rm=TRUE, drop.empty.groups=F)

## Treatment Balance

GetTreatBalance(variable='white_man', treatment='resentment_treat', data=data.ca)
GetTreatBalance(variable='white_woman', treatment='resentment_treat', data=data.ca)
GetTreatBalance(variable='man_of_color', treatment='resentment_treat', data=data.ca)
GetTreatBalance(variable='woman_of_color', treatment='resentment_treat', data=data.ca)
GetTreatBalance(variable='education', treatment='resentment_treat', data=data.ca)
GetTreatBalance(variable='income', treatment='resentment_treat', data=data.ca)
GetTreatBalance(variable='pid', treatment='resentment_treat', data=data.ca)
GetTreatBalance(variable='pid.dem', treatment='resentment_treat', data=data.ca)
GetTreatBalance(variable='pid.rep', treatment='resentment_treat', data=data.ca)
GetTreatBalance(variable='ideology', treatment='resentment_treat', data=data.ca)
GetTreatBalance(variable='rel.other', treatment='resentment_treat', data=data.ca)
GetTreatBalance(variable='rel.noth', treatment='resentment_treat', data=data.ca)
GetTreatBalance(variable='rel.prot', treatment='resentment_treat', data=data.ca)
GetTreatBalance(variable='rel.cath', treatment='resentment_treat', data=data.ca)


##########################################################################
#
# SI 2 Power Analysis
#
##########################################################################
sigma <- sqrt(((7-1)^2/12))
sigma <- 1.07 # SD for racial resentment in another survey with same questions
sigma2 <- .82 # SD for modern sexism in another survey with same questions
mu = sigma*0.2
critical = qnorm((1-0.025),0,1)


p=NULL
p.m=NULL
p.l=NULL
p2=NULL
p.m2=NULL
p.l2=NULL
for (n in seq(10,300,2)){
  test = (abs(.2)*sqrt(n)/(sigma)) - critical
  power = pnorm(test,0,1)
  p=c(p,power)
  
  test = (abs(0.5)*sqrt(n)/(sigma)) - critical
  power = pnorm(test,0,1)
  p.m=c(p.m,power)
  
  test = (abs(0.75)*sqrt(n)/(sigma)) - critical
  power = pnorm(test,0,1)
  p.l=c(p.l,power)
  
  test = (abs(.2)*sqrt(n)/(sigma2)) - critical
  power = pnorm(test,0,1)
  p2=c(p2,power)
  
  test = (abs(0.5)*sqrt(n)/(sigma2)) - critical
  power = pnorm(test,0,1)
  p.m2=c(p.m2,power)
  
  test = (abs(0.75)*sqrt(n)/(sigma2)) - critical
  power = pnorm(test,0,1)
  p.l2=c(p.l2,power)
}
cbind(p, seq(10,300,2)) #224; 136
cbind(p.m, seq(10,300,2)) #36; 20
cbind(p.l, seq(10,300,2)) #16; 10

#png('Figures/SI_Power.png', width=6, height=6, units='in', res=500)
par(mar=c(2.25,2.25,1,.75), mgp=c(1, .15, 0), family='sans', 
    cex.main=1, las=1, tck=-.001, xaxs='i', yaxs='i', cex.axis=.75, xpd=F)
plot(seq(10,300,2),p,'l',xlab='N',ylab='Power', ylim=c(0, 1))
lines(seq(10,300,2),p.m,'l',xlab='n',ylab='power', col='red')
lines(seq(10,300,2),p.l,'l',xlab='n',ylab='power', col='blue')
lines(seq(10,300,2),p2,'l',xlab='n',ylab='power', lty=2)
lines(seq(10,300,2),p.m2,'l',xlab='n',ylab='power', col='red', lty=2)
lines(seq(10,300,2),p.l2,'l',xlab='n',ylab='power', col='blue', lty=2)
abline(h=0.8,lty='dotted',col='grey')
legend('bottom', c('Effect: .2; SD: 1.07', 'Effect: .2; SD: .82', 
                   'Effect: .5; SD: 1.07', 'Effect: .5; SD:.82',
                   'Effect: .75; SD: 1.07', 'Effect: .75; SD: .82'),
       col=c('black', 'black', 'red', 'red', 'blue', 'blue'), lty=1:2, bty='n', ncol=3, cex=.75)
#dev.off()


# Uniform dist
sigma <- sqrt(((7-1)^2/12))
sigma <- 1.07 # SD for racial resentment in another survey with same questions
sigma <- .82 # SD for modern sexism in another survey with same questions
mu = sigma*0.2
critical = qnorm((1-0.025),0,1)


p=NULL
p.m=NULL
p.l=NULL
for (n in seq(10,500,5)){
  test = (abs(mu)*sqrt(n)/(sigma)) - critical
  power = pnorm(test,0,1)
  p=c(p,power)
  
  test = (abs(sigma*0.5)*sqrt(n)/(sigma)) - critical
  power = pnorm(test,0,1)
  p.m=c(p.m,power)
  
  test = (abs(sigma*0.7)*sqrt(n)/(sigma)) - critical
  power = pnorm(test,0,1)
  p.l=c(p.l,power)
}
cbind(p, seq(10,500,5)) #200
cbind(p.m, seq(10,500,5)) #35
cbind(p.l, seq(10,500,5)) #15

plot(seq(10,500,5),p,'l',xlab='n',ylab='power')
lines(seq(10,500,5),p.m,'l',xlab='n',ylab='power')
lines(seq(10,500,5),p.l,'l',xlab='n',ylab='power')
abline(h=0.8,lty='dotted',col='red')
##########################################################################
#
# SI 3 Question Treatment Effects
#
##########################################################################
## Table 2
stargazer(
  lm(resentment1 ~ resentment_treat, data=data.ca),
  lm(resentment2 ~ resentment_treat, data=data.ca),
  lm(resentment3 ~ resentment_treat, data=data.ca),
  lm(resentment4 ~ resentment_treat, data=data.ca),
  style='apsr', #type='text',
  dep.var.labels = c('Work Way Up', 'Generations of Slavery', 'Less Than Deserve', 'Try Harder'),
  covariate.labels = c('Black Men', 'Black Women', 'LGBT Black People'),
  label='tab:rr questions'
)

## Table 3
stargazer(
  lm(sexism1 ~ sexism_treat, data=data.ca),
  lm(sexism2 ~ sexism_treat, data=data.ca),
  lm(sexism3 ~ sexism_treat, data=data.ca),
  lm(sexism4 ~ sexism_treat, data=data.ca),
  lm(sexism5 ~ sexism_treat, data=data.ca),
  style='apsr', type='text',
  dep.var.labels = c('Disc. No Longer Problem', 'Income Same as Men', 'Equal Opportunity', 'Media/Gov Attention too High', 'Hard to Understand Concerns'),
  covariate.labels = c('White Women', 'Black Women', 'LBT Women')
)

## Table 4
stargazer(lm(sexism1 ~ sexism_treat, data=data.ca, subset = r.black==1),
          lm(sexism2 ~ sexism_treat, data=data.ca, subset = r.black==1),
          lm(sexism3 ~ sexism_treat, data=data.ca, subset = r.black==1),
          lm(sexism4 ~ sexism_treat, data=data.ca, subset = r.black==1),
          lm(sexism5 ~ sexism_treat, data=data.ca, subset = r.black==1),
          style='apsr', #type='text',
          column.labels = c('Disc. Not Problem', 'Pay Gap', 'Equal Op.', 'Media Too Concerned', 'Undertand Concerns'),
          model.numbers = FALSE, dep.var.labels.include = FALSE,
          covariate.labels = c('White women', 'Black women', 'LBT women'),
          title='Modern Sexism Question Answers for Black Respondents',
          label='tab:int reg ms black')

## Table 5
stargazer(lm(sexism1 ~ sexism_treat, data=data.ca, subset = r.black==0 & r.white==0),
          lm(sexism2 ~ sexism_treat, data=data.ca, subset = r.black==0 & r.white==0),
          lm(sexism3 ~ sexism_treat, data=data.ca, subset = r.black==0 & r.white==0),
          lm(sexism4 ~ sexism_treat, data=data.ca, subset = r.black==0 & r.white==0),
          lm(sexism5 ~ sexism_treat, data=data.ca, subset = r.black==0 & r.white==0),
          style='apsr', #type='text',
          column.labels = c('Disc. Not Problem', 'Pay Gap', 'Equal Op.', 'Media Too Concerned', 'Undertand Concerns'),
          model.numbers = FALSE, dep.var.labels.include = FALSE,
          covariate.labels = c('White women', 'Black women', 'LBT women'),
          title='Modern Sexism Question Answers for Non-Black Respondents of Color',
          label='tab:int reg ms nbpoc')

## Table 6
stargazer(lm(modern_sexism ~ sexism_treat*race.lim, data=data.ca),
          lm(sexism1 ~ sexism_treat*race.lim, data=data.ca),
          lm(sexism2 ~ sexism_treat*race.lim, data=data.ca),
          lm(sexism3 ~ sexism_treat*race.lim, data=data.ca),
          lm(sexism4 ~ sexism_treat*race.lim, data=data.ca),
          lm(sexism5 ~ sexism_treat*race.lim, data=data.ca),
          style='apsr', #type='text',
          column.labels = c('Overall', 'Discrimination No Longer Problem', 'Lower Income', 'Equal Opportunities', 'Media Too Concerned', 'Easy to Undertand Concerns'),
          model.numbers = FALSE, dep.var.labels.include = FALSE,
          covariate.labels = c('White Resp.: White women', 'White Resp.: Black women', 'White Resp.: LBT women', 'Black Resp.', 'NBPOC Resp.',
                               'Black Resp.: White women', 'Black Resp.: Black women', 'Black Resp.: LBT women', 
                               'NBPOC Resp.: White women', 'NBPOC Resp.: Black women', 'NBPOC Resp.: LBT women'),
          title='Modern Sexism Question Answers by Respondent Race',
          label='tab:int reg ms race')

stargazer(lm(modern_sexism ~ sexism_treat*gender.lim, data=data.ca),
          lm(sexism1 ~ sexism_treat*gender.lim, data=data.ca),
          lm(sexism2 ~ sexism_treat*gender.lim, data=data.ca),
          lm(sexism3 ~ sexism_treat*gender.lim, data=data.ca),
          lm(sexism4 ~ sexism_treat*gender.lim, data=data.ca),
          lm(sexism5 ~ sexism_treat*gender.lim, data=data.ca),
          style='apsr', #type='text',
          column.labels = c('Overall', 'Discrimination No Longer Problem', 'Lower Income', 'Equal Opportunities', 'Media Too Concerned', 'Easy to Undertand Concerns'),
          model.numbers = FALSE, dep.var.labels.include = FALSE,
          covariate.labels = c('Women: White women', 'Women: Black women', 'Women: LBT women', 'Men',
                               'Men: White women', 'Men: Black women', 'Men: LBT women'),
          title='Modern Sexism Question Answers by Respondent Gender',
          label='tab:int reg ms gender')

##########################################################################
#
# SI 4 Additional Estimates for Score Consistency
#
##########################################################################
data.ca$change <- NA
data.ca$change[round(data.ca$racial_resentment,0) == round(data.ca$modern_sexism, 0)] <- 'Same'
data.ca$change[round(data.ca$racial_resentment,0) > round(data.ca$modern_sexism, 0)] <- 'Higher Racism'
data.ca$change[round(data.ca$racial_resentment,0) < round(data.ca$modern_sexism, 0)] <- 'Lower Racism'
data.ca$change <- factor(data.ca$change, c('Higher Racism', 'Same', 'Lower Racism'))

data.ca$change.deserve.income <- NA
data.ca$change.deserve.income[data.ca$resentment3 == data.ca$sexism2] <- 'Same'
data.ca$change.deserve.income[data.ca$resentment3 > data.ca$sexism2] <- 'Higher Racism'
data.ca$change.deserve.income[data.ca$resentment3 < data.ca$sexism2] <- 'Lower Racism'
data.ca$change.deserve.income <- factor(data.ca$change.deserve.income, c('Higher Racism', 'Same', 'Lower Racism'))

data.ca$change.try.equal <- NA
data.ca$change.try.equal[data.ca$resentment4 == data.ca$sexism3] <- 'Same'
data.ca$change.try.equal[data.ca$resentment4 > data.ca$sexism3] <- 'Higher Racism'
data.ca$change.try.equal[data.ca$resentment4 < data.ca$sexism3] <- 'Lower Racism'
data.ca$change.try.equal <- factor(data.ca$change.try.equal, c('Higher Racism', 'Same', 'Lower Racism'))

get.prop <- function(x='RR', y='MS', data){
  if(y == ''){
    tab <- prop.table(table(data[,x]))
  } else {
    tab <- prop.table(table(data[,y], data[,x]))
  }
  tab <- round(tab * 100, 2)
  tab <- c(as.vector(tab), round(nrow(data), 0))
  return(tab)
}

## Table 8
stargazer((lm(modern_sexism ~ I(treat_full3 == 'Mix')*racial_resentment, data=data.ca, subset=question_order == "0" & manip.correct == 1)),
          (lm(racial_resentment ~ I(treat_full3 == 'Mix')*modern_sexism, data=data.ca, subset=question_order == "1" & manip.correct == 1)), 
          style='ajps', column.labels = c('Sexism Asked Second', 'Resentment Asked Second'), type='text',
          covariate.labels = c('No Treatment Group Overlap', 'Racial Resentment', 'No Treatment Group Overlap: Racial Resentment',
                               'Modern Sexism', 'No Treatment Group Overlap: Modern Sexism'))

stargazer((lm(modern_sexism ~ I(relevel(treat_full3, 'Same'))*racial_resentment, data=data.ca, subset = manip.correct == 1)), 
          style='ajps', type='text',
          covariate.labels = c('Partial Treatment Group Overlap','No Treatment Group Overlap', 
                               'Racial Resentment', 'No Treatment Group Overlap: Racial Resentment', 'Partial Treatment Group Overlap: Racial Resentment'))
## Table 9 
mat <- prop.table(table(data.ca[data.ca$manip.correct == 1, 'change'], data.ca[data.ca$manip.correct == 1, 'treat_full3']), margin=2)
mat <- prop.table(table(data.ca[, 'change'], data.ca[, 'treat_full3']), margin=2)

## Tablw 10
stargazer(get.cor(data.ca[data.ca$manip.correct == 1 & data.ca$r.white == 1,]), title='Correlations between Treatments', label='tab:cor')

## Table 11
mat <- matrix(nrow=7, ncol=5)
mat[1,] <- get.prop(data=data.ca)
mat[2,] <- get.prop(data=data.ca[data.ca$treat_full == 'Control Control',])
mat[3,] <- get.prop(data=data.ca[data.ca$treat_full == 'Women Black',])
mat[4,] <- get.prop(data=data.ca[data.ca$treat_full == 'LGBT LGBT',])

mat[5,] <- get.prop(data=data.ca[data.ca$treat_full == 'Men Black',])
mat[6,] <- get.prop(data=data.ca[data.ca$treat_full == 'Women White',])
mat[7,] <- get.prop(data=data.ca[data.ca$treat_full == 'Men White',])

colnames(mat) <- c('High Racism, Sexism', 'High Racism, Low Sexism', 'Low Racism, High Sexism', 'Low Racism, Sexism', 'N')
rownames(mat) <- c('Full Sample', "Black People, Women", "Black Women", 'LGBT Black People, LBT Women', "Black Men, Black Women", 
                   "Black Women, White Women", "Black Men, White Women")
stargazer(mat, title='Combinations of Sexism and Racism by Treatments', label='tab:racism sexism comb', type='text')
##########################################################################
#
# SI 7 Intersectional Subgroup Estimates
#
##########################################################################
stargazer(get.lim.adj(data=data.ca[data.ca$race_gender_lim == "White Man" & data.ca$lgbt == 0,], attitude = 'resentment', boot.n=100000),
          get.lim.adj(data=data.ca[data.ca$race_gender_lim == "White Man" & data.ca$lgbt == 0,], attitude = 'sexism', boot.n=100000),
          get.lim.adj(data=data.ca[data.ca$race_gender_lim == "White Woman" & data.ca$lgbt == 0,], attitude = 'resentment', boot.n=100000),
          get.lim.adj(data=data.ca[data.ca$race_gender_lim == "White Woman" & data.ca$lgbt == 0,], attitude = 'sexism', boot.n=100000),
          get.lim.adj(data=data.ca[data.ca$race_gender_lim == "MOC" & data.ca$lgbt == 0,], attitude = 'resentment', boot.n=100000),
          get.lim.adj(data=data.ca[data.ca$race_gender_lim == "MOC" & data.ca$lgbt == 0,], attitude = 'sexism', boot.n=100000),
          get.lim.adj(data=data.ca[data.ca$race_gender_lim == "WOC" & data.ca$lgbt == 0,], attitude = 'resentment', boot.n=100000),
          get.lim.adj(data=data.ca[data.ca$race_gender_lim == "WOC" & data.ca$lgbt == 0,], attitude = 'sexism', boot.n=100000),
          #type='text',
          notes = c("+ $p < .05$, * FDR-adjusted $p < .05$"),
          title=c(
            'Racial Resentment: White Man',
            'Modern Sexism: White Man',
            'Racial Resentment: White Women',
            'Modern Sexism: White Women',
            'Racial Resentment: Men of Color',
            'Modern Sexism: Men of Color',
            'Racial Resentment: Women of Color',
            'Modern Sexism: Women of Color'
          )
)
##########################################################################
#
# SI 8 Additional Figures
#
##########################################################################
## SI Figure 2

png('Figures/Figure_BP_Sexism.png', width=8, height=6, units='in', res=500)
par(mfrow=c(1, 5), mar=c(2.5,2.5,1.5,.0), mgp=c(1, .1, 0), family='sans', 
    cex.main=1, las=1, tck=-.001, cex.axis=.75)
boxplot(modern_sexism ~sexism_treat, data=data.ca[data.ca$manip.correct == 1,], horizontal = F,
        xlab='', ylab='Score',col=brewer.pal(4, 'Greys'), xpd=F, ylim=c(.95, 7.05), main='Full Sample')
par(mar=c(2.5,0,1.5,.0))
boxplot(modern_sexism ~sexism_treat , data=data.ca[data.ca$manip.correct == 1 & data.ca$r.white == 1 & data.ca$lgbt == 0,], horizontal = F,
        xlab='', ylab='',col=brewer.pal(4, 'Greys'), xpd=F, ylim=c(.95, 7.05), main='White Respondents', yaxt='n')
boxplot(modern_sexism ~sexism_treat , data=data.ca[data.ca$manip.correct == 1 & data.ca$r.white == 0 & data.ca$lgbt == 0,], horizontal = F,
        xlab='Wording Treatment', ylab='',col=brewer.pal(6, 'Greys'), xpd=F, ylim=c(.95, 7.05), main='Respondents of Color', yaxt='n')
boxplot(modern_sexism ~sexism_treat , data=data.ca[data.ca$manip.correct == 1 & data.ca$gender.lim == 0 & data.ca$lgbt == 0,], horizontal = F,
        xlab='', ylab='',col=brewer.pal(4, 'Greys'), xpd=F, ylim=c(.95, 7.05), main='Men', yaxt='n')
boxplot(modern_sexism ~sexism_treat , data=data.ca[data.ca$manip.correct == 1 & data.ca$gender.lim == 1 & data.ca$lgbt == 0,], horizontal = F,
        xlab='', ylab='',col=brewer.pal(4, 'Greys'), xpd=F, ylim=c(.95, 7.05), main='Women', yaxt='n')
dev.off()

png('Figures/Figure_BP_Resentment.png', width=8, height=6, units='in', res=500)
par(mfrow=c(1, 5), mar=c(2.5,2.5,1.5,.0), mgp=c(1, .1, 0), family='sans', 
    cex.main=1, las=1, tck=-.001, cex.axis=.75)
boxplot(racial_resentment ~sexism_treat, data=data.ca[data.ca$manip.correct == 1,], horizontal = F,
        xlab='', ylab='Score',col=brewer.pal(4, 'Greys'), xpd=F, ylim=c(.95, 7.05), main='Full Sample')
par(mar=c(2.5,0,1.5,.0))
boxplot(racial_resentment ~resentment_treat , data=data.ca[data.ca$manip.correct == 1 & data.ca$r.white == 1 & data.ca$lgbt == 0,], horizontal = F,
        xlab='', ylab='',col=brewer.pal(4, 'Greys'), xpd=F, ylim=c(.95, 7.05), main='White Respondents', yaxt='n')
boxplot(racial_resentment ~resentment_treat , data=data.ca[data.ca$manip.correct == 1 & data.ca$r.white == 0 & data.ca$lgbt == 0,], horizontal = F,
        xlab='Wording Treatment', ylab='',col=brewer.pal(6, 'Greys'), xpd=F, ylim=c(.95, 7.05), main='Respondents of Color', yaxt='n')
boxplot(racial_resentment ~resentment_treat , data=data.ca[data.ca$manip.correct == 1 & data.ca$gender.lim == 0 & data.ca$lgbt == 0,], horizontal = F,
        xlab='', ylab='',col=brewer.pal(4, 'Greys'), xpd=F, ylim=c(.95, 7.05), main='Men', yaxt='n')
boxplot(racial_resentment ~resentment_treat , data=data.ca[data.ca$manip.correct == 1 & data.ca$gender.lim == 1 & data.ca$lgbt == 0,], horizontal = F,
        xlab='', ylab='',col=brewer.pal(4, 'Greys'), xpd=F, ylim=c(.95, 7.05), main='Women', yaxt='n')
dev.off()


## SI Figures 3, 4

png('Figures/Bar_Resentment_Questions.png', width=8, height=8, units = 'in', res=300)
#layout(mat=matrix(c(1, 3, 2, 4, 5, 7, 6, 8), ncol=2, nrow=4, byrow=T), heights=rep(c(.25, .75), 4))
par(mfrow=c(2,2))
ScoreBarPlot(x='resentment1', treat='resentment_treat', main="No Special Favors", by.rg=T)
ScoreBarPlot(x='resentment2', treat='resentment_treat', main="Generations Slavery", by.rg=T)
ScoreBarPlot(x='resentment3', treat='resentment_treat', main="Gotten Less Than Deserve", by.rg=T)
ScoreBarPlot(x='resentment4', treat='resentment_treat', main="Try Harder", by.rg=T)
dev.off()
png('Figures/Bar_Sexism_Questions.png', width=8, height=8, units = 'in', res=300)
#layout(mat=matrix(c(1, 3, 2, 4, 5, 7, 6, 8, 9, 11, 10, 12), ncol=2, nrow=6, byrow=T), heights=rep(c(.25, .75), 5))
par(mfrow=c(3,2))
ScoreBarPlot(x='sexism1', treat='sexism_treat', main="Disc. No Longer Problem", by.rg=T)
ScoreBarPlot(x='sexism2', treat='sexism_treat', main="Lower Income", by.rg=T)
ScoreBarPlot(x='sexism3', treat='sexism_treat', main="Equal Opportunity", by.rg=T)
ScoreBarPlot(x='sexism4', treat='sexism_treat', main="Showing More Concern", by.rg=T)
ScoreBarPlot(x='sexism5', treat='sexism_treat', main="Understand Concerns", by.rg=T)
dev.off()








##########################################################################
#
# SI 9 Permutation Test Estimates
#
##########################################################################
stargazer(get.lim.adj(data=data.ca, attitude = 'resentment', out.type = 'perm', boot.n=100000),
          get.lim.adj(data=data.ca, attitude = 'sexism', out.type = 'perm', boot.n=100000),
          get.lim.adj(data=data.ca[data.ca$race.bin == "White" & data.ca$lgbt == 0,], attitude = 'resentment', out.type = 'perm', boot.n=100000),
          get.lim.adj(data=data.ca[data.ca$race.bin == "White" & data.ca$lgbt == 0,], attitude = 'sexism', out.type = 'perm', boot.n=100000),
          get.lim.adj(data=data.ca[data.ca$race.bin == "POC" & data.ca$lgbt == 0,], attitude = 'resentment', out.type = 'perm', boot.n=100000),
          get.lim.adj(data=data.ca[data.ca$race.bin == "POC" & data.ca$lgbt == 0,], attitude = 'sexism', out.type = 'perm', boot.n=100000),
          get.lim.adj(data=data.ca[data.ca$r.black == 1 & data.ca$lgbt == 0,], attitude = 'resentment', out.type = 'perm', boot.n=100000),
          get.lim.adj(data=data.ca[data.ca$r.black == 1 & data.ca$lgbt == 0,], attitude = 'sexism', out.type = 'perm', boot.n=100000),
          get.lim.adj(data=data.ca[data.ca$gender.lim == 1 & data.ca$lgbt == 0,], attitude = 'resentment', out.type = 'perm', boot.n=100000),
          get.lim.adj(data=data.ca[data.ca$gender.lim == 1 & data.ca$lgbt == 0,], attitude = 'sexism', out.type = 'perm', boot.n=100000),
          get.lim.adj(data=data.ca[data.ca$gender.lim == 0 & data.ca$lgbt == 0,], attitude = 'resentment', out.type = 'perm', boot.n=100000),
          get.lim.adj(data=data.ca[data.ca$gender.lim == 0 & data.ca$lgbt == 0,], attitude = 'sexism', out.type = 'perm', boot.n=100000),
          get.lim.adj(data=data.ca[data.ca$lgbt == 1,], attitude = 'resentment', out.type = 'perm', boot.n=100000),
          get.lim.adj(data=data.ca[data.ca$lgbt == 1,], attitude = 'sexism', out.type = 'perm', boot.n=100000),
          #type='text',
          title=c('Racial Resentment: Full Sample',
                  'Modern Sexism: Full Sample',
                  'Racial Resentment: White, Non-LGBTQ Respondents',
                  'Modern Sexism: White, Non-LGBTQ Respondents',
                  'Racial Resentment: Non-LGBTQ Respondents of Color',
                  'Modern Sexism: Non-LGBTQ Respondents of Color',
                  'Racial Resentment: Black, Non-LGBTQ Respondents',
                  'Modern Sexism: Black, Non-LGBTQ Respondents',
                  'Racial Resentment: Cisgender Men, Non-LGBTQ Respondents',
                  'Modern Sexism: Cisgender Men, Non-LGBTQ Respondents',
                  'Racial Resentment: Cisgender Women, Non-LGBTQ Respondents',
                  'Modern Sexism: Cisgender Women, Non-LGBTQ Respondents',
                  'Racial Resentment: LGBTQ Respondents',
                  'Modern Sexism: LGBTQ Respondents'), 
          label=c('tab:full RR',
                  'tab:full MS',
                  'tab:white RR',
                  'tab:white MS',
                  'tab:poc RR',
                  'tab:poc MS',
                  'tab:black RR',
                  'tab:black MS',
                  'tab:men RR',
                  'tab:men MS',
                  'tab:women RR',
                  'tab:women MS',
                  'tab:lgbt RR',
                  'tab:lgbt MS'))

stargazer(get.lim.adj(data=data.ca[data.ca$race_gender_lim == "White Man" & data.ca$lgbt == 0,], out.type = 'perm', attitude = 'resentment', boot.n=250000),
          get.lim.adj(data=data.ca[data.ca$race_gender_lim == "White Man" & data.ca$lgbt == 0,], out.type = 'perm', attitude = 'sexism', boot.n=250000),
          get.lim.adj(data=data.ca[data.ca$race_gender_lim == "White Woman" & data.ca$lgbt == 0,], out.type = 'perm', attitude = 'resentment', boot.n=250000),
          get.lim.adj(data=data.ca[data.ca$race_gender_lim == "White Woman" & data.ca$lgbt == 0,], out.type = 'perm', attitude = 'sexism', boot.n=250000),
          get.lim.adj(data=data.ca[data.ca$race_gender_lim == "MOC" & data.ca$lgbt == 0,], out.type = 'perm', attitude = 'resentment', boot.n=250000),
          get.lim.adj(data=data.ca[data.ca$race_gender_lim == "MOC" & data.ca$lgbt == 0,], out.type = 'perm', attitude = 'sexism', boot.n=250000),
          get.lim.adj(data=data.ca[data.ca$race_gender_lim == "WOC" & data.ca$lgbt == 0,], out.type = 'perm', attitude = 'resentment', boot.n=250000),
          get.lim.adj(data=data.ca[data.ca$race_gender_lim == "WOC" & data.ca$lgbt == 0,], out.type = 'perm', attitude = 'sexism', boot.n=250000),
          #type='text',
          notes = c("+ $p < .05$, * FDR-adjusted $p < .05$"),
          title=c(
            'Racial Resentment: White Man',
            'Modern Sexism: White Man',
            'Racial Resentment: White Women',
            'Modern Sexism: White Women',
            'Racial Resentment: Men of Color',
            'Modern Sexism: Men of Color',
            'Racial Resentment: Women of Color',
            'Modern Sexism: Women of Color'
          )
)
