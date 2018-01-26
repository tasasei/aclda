library(MASS)
library(ROCR)

aclda <- function(d,label,CV="loo",seat=c(),measure="accuracy",missprint=FALSE){
    if(!(length(seat)>0)){
        ## make seat
        x <- list()
        for( i in (1:ncol(d)) ){
            x[[i]] <- c(FALSE,TRUE)
        }
        seat <- expand.grid(x)
        names(seat) <- names(d)
        seat <- seat[-which(apply(seat,1,sum)<1),]
    }

    ## run lda
    accuracy <- c()
    for( i in (1:nrow(seat) )){
        tmp.col <- which(unlist(seat[i,]))
        accuracy <- c(accuracy,get.measure(d[,tmp.col],label,measure))
    }
    seat <- cbind(seat,accuracy)
    names(seat)[ncol(seat)] <- "measure"
    return(seat)
}

get.measure <- function(d,cls,measure="accuracy"){ # d = data.frame
    measure <- tolower(measure)
    if(measure=="auc"){
        mdl <- lda(as.matrix(d),cls)
        res <- predict(mdl,as.matrix(d))
        return(get.auc(res$x,cls))
    }

    d <- as.data.frame(d)
    if(nrow(d) != length(cls)){
        print("caution!!")
        return(0)
    }
    cls <- as.factor(cls)
    uniqcls <- unique(cls)
    if(length(uniqcls)>2){
        print("caution!!")
        return(0)
    }
    tmp <- sum(cls==uniqcls[1])
    if(sum(cls==uniqcls[2]) > tmp){
        PosClass <- uniqcls[1]
    }else{
        PosClass <- uniqcls[2]
    }
    
    pos.row <- c()
    neg.row <- c()
    for( i in 1:nrow(d) ){
        mdl <- lda(as.matrix(d[-i,]),cls[-i])
        res <- predict(mdl,d[i,])
        if( res$class == PosClass ){
            pos.row <- c(pos.row,i)
        }else{
            neg.row <- c(neg.row,i)
        }
    }
    TP <- sum(cls[pos.row] == PosClass)
    FP <- sum(cls[pos.row] != PosClass)
    TN <- sum(cls[neg.row] != PosClass)
    FN <- sum(cls[neg.row] == PosClass)
    if(measure=="accuracy"){
        return((TP+TN)/(TP+TN+FN+FP))
    }
    else if(measure=="F"){
        prec <- TP / (TP + FP)
        reca <- TP / (TP + FN)
        return((2*reca * prec) / (reca + prec))
    }
}

get.auc <- function(x,cls){
    pred <- prediction(x,cls)
    perf <- performance(pred,"auc")
    auc <- as.numeric(perf@y.values)
    return(auc)
}

# 入力 x(matrix) をテキスト付きプロット
image_text<- function(x,#RemNum,
                       main=c(),dig=3,xcex=1.0,col.label=NULL,col.las=3,col.line=0,...){ #
    ## main <- c()
    if(is.null(col.label)){
        col.label <- colnames(x)
    }
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
    image(t(x2[nrow(x2):1,ncol(x2):1])[ncol(x2):1,],col=c(rep("white",1000),"black"),axes=FALSE,main=main) #axes=F で軸消去
    text(xy,labels=signif(x,digits=dig))
    mtext(text=col.label,side=1,at=(0:(ncol(x)-1)/(ncol(x)-1)),las=col.las,cex=xcex,line=col.line)#列名
    mtext(text=rev(row(x)[,1]),side=2,at=(0:(nrow(x)-1)/(nrow(x)-1)),las=1)#行数
    abline(v=at_v[seq(3,length(at_v)-1,by=2)]
          ,h=at_h[seq(3,length(at_h)-1,by=2)],col=8)
    return(list(v=at_v,h=at_h))
#    return(xy)
}
