library(MASS)

aclda <- function(d,label,CV="loo"){
    ## make seat
    x <- list()
    ## browser()
    for( i in (1:ncol(d)) ){
        x[[i]] <- c(FALSE,TRUE)
    }
    seat <- expand.grid(x)
    names(seat) <- names(d)
    seat <- seat[-which(apply(seat,1,sum)<2),]

    ## run lda
    accuracy <- c()
    for( i in (1:nrow(seat) )){
        tmp.col <- which(unlist(seat[i,]))
        if(CV=="loo"){
            tmp.acu <- 0
            for( j in 1:nrow(d) ){
                e <- try(m <- lda(as.matrix(d[-(j),tmp.col]),label[-(j)]),silent=TRUE)
                if(class(e)=="try-error"){
                    tmp.acu <- tmp.acu + 0
                }else{
                    res <- predict(m,d[j,tmp.col])
                    tmp.acu <- tmp.acu + as.numeric(res$class == label[j])
                }
            }
            accuracy <- c(accuracy,tmp.acu/nrow(d))
        }else{
            m <- lda(as.matrix(d[,tmp.col]),label)
            res <- predict(m,d[,tmp.col])
            accuracy <- c(accuracy,sum(res$class == label)/length(res$class))
        }
    }
    seat <- cbind(seat,accuracy)
    
    return(seat)
}

loo <- function(){
    
}


# 入力 x(matrix) をテキスト付きプロット
image_text<- function(x,#RemNum,
                       main1=c(),dig=3,xcex=1.0,...){ #
    ## main <- c()
    logic.point <- which(x[1,]==TRUE)
    logic.point <- c(logic.point,which(x[1,]==FALSE))
    x <- as.matrix(head(x,30))
    ## x <- head(x,30)
    ## main1 <- c()
    ## for( i in 1:length(main) ){
    ##     main1 <- paste(main1,main[i],", ", sep="")
    ## }
    
    at_v <- 1/(2*(ncol(x)-1))
    at_v <- seq(0,1+2*at_v,by=at_v)-at_v
    at_h <- 1/(2*(nrow(x)-1))
    at_h <- seq(0,1+2*at_h,by=at_h)-at_h
    xy <- do.call("rbind",lapply(at_v[seq(2,length(at_v),by=2)]
                                ,function(x,y) cbind(x,y),y=rev(at_h[seq(2,length(at_h),by=2)])))
    xy[x==0] <- ""
    xy[x==1] <- ""
    x2 <- x
    ## x2[,(ncol(x)-RemNum):ncol(x)] <- FALSE
    x2[,-(logic.point)] <- FALSE
    image(t(x2[nrow(x2):1,ncol(x2):1])[ncol(x2):1,],col=c(rep("white",1000),"black"),axes=FALSE,main=main1) #axes=F で軸消去
    text(xy,labels=signif(x,digits=dig))
    mtext(text=colnames(x),side=1,at=(0:(ncol(x)-1)/(ncol(x)-1)),las=3,cex=xcex)#列名
    mtext(text=rev(row(x)[,1]),side=2,at=(0:(nrow(x)-1)/(nrow(x)-1)),las=1)#行数
    abline(v=at_v[seq(3,length(at_v)-1,by=2)]
          ,h=at_h[seq(3,length(at_h)-1,by=2)],col=8)
    return(list(v=at_v,h=at_h))
#    return(xy)
}
