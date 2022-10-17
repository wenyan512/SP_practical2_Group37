# sample(1:20,replace=F)
# try<-c(1:20)
# sample(try,10,replace = T)

##1.
# strategy=c(1,2,3)
# number_of_prisoners=10
# data.frame(number_of_boxes=c(1:10),card=sample(number_of_prisoners,10,replace = F));
# k=5
# card_distribution = c(sample(1:10,10,replace=F));card_distribution###n=5
# 
# 
# set=rep(0,5)
# set[1]=card_distribution[k]
# set[2]=card_distribution[set[1]]
# set
# 
# set[j]=card_distribution[set[j-1]]

pone<-function(n,k,strategy,nreps=10000){
  number_of_prisoners=2*n
  if(strategy==1){
    nrep=rep(0,10000)###记录册
    for(i in 1:nreps){####在重复实验1：10000次时
      card_distribution = sample(1:number_of_prisoners,number_of_prisoners,replace=F)
      set=rep(0,n)####n次抽取的总卡片得到数
      set[1]=card_distribution[k]
      for(j in 2:n){ ##只可以循环n次，我这里想把所有可能均放入set里，若k编号出现在set里，则成功
        set[j]=card_distribution[set[j-1]]
      }
      if(k%in%set){
        nrep[i]=1
      }
    }
    return(sum(nrep)/nreps)
  }
  if(strategy==2){
    nrep=rep(0,10000)###记录册
    for(i in 1:nreps){####在重复实验1：10000次时
      card_distribution = c(sample(1:number_of_prisoners,number_of_prisoners,replace=F))
      set=rep(0,n)####n次抽取的总卡片得到数
      set[1]=card_distribution[sample(1:number_of_prisoners,1,replace = F)]##random 箱子（人）
      for(j in 2:n){ ##只可以循环n次，我这里想把所有可能均放入set里，若k编号出现在set里，则成功
        set[j]=card_distribution[set[j-1]]
      }
      if(k%in%set){
        nrep[i]=1
      }
    }
    return(sum(nrep)/nreps)
}
  if(strategy==3){
    nrep=rep(0,10000)###记录册
    for(i in 1:nreps){####在重复实验1：10000次时
      card_distribution = c(sample(1:number_of_prisoners,number_of_prisoners,replace=F))
      random_index_boxes = c(sample(1:number_of_prisoners),n,replace=F)
      set=rep(0,n)####n次抽取的总卡片得到数
      for(j in 1:n){ ##只可以循环n次，我这里想把所有可能均放入set里，若k编号出现在set里，则成功
        set[j]=card_distribution[random_index_boxes[j]]
      }
      if(k%in%set){
        nrep[i]=1
      }
    }
    return(sum(nrep)/nreps)
  }
}
####2.

pall<-function(n,strategy,nreps=10000){
  number_of_prisoners=2*n
  if(strategy==1){
    nrep=rep(0,nreps)###记录册
    for(i in 1:nreps){####在重复实验1：10000次时
      card_distribution = sample(1:number_of_prisoners,number_of_prisoners,replace=F)
      set_total = rep(0,2*n)###记录2n个人是否同时成功
      for(k in 1:(2*n)){####总共2n个人，k 是 index of 人
        # 记录卡number
        set=rep(0,n)
        set[1]=card_distribution[k]
        for(j in 2:n){
          set[j]=card_distribution[set[j-1]]
        }
        if(k%in%set){
          set_total[k]=1
        }
      }
      if(sum(set_total)==(2*n)){
        nrep[i]=1
        }
    }
    return(sum(nrep)/10000)
  }
  if(strategy==2){
    nrep=rep(0,nreps)###记录册
    for(i in 1:nreps){####在重复实验1：10000次时
      card_distribution = sample(1:number_of_prisoners,number_of_prisoners,replace=F)
      set_total = rep(0,2*n)###记录2n个人是否同时成功
      for(k in 1:(2*n)){####总共2n个人，k 是 index of 人
        # 记录卡number
        set=rep(0,n)
        set[1]=card_distribution[sample(1:number_of_prisoners,1,replace = F)]
        for(j in 2:n){
          set[j]=card_distribution[set[j-1]]
        }
        if(k%in%set){
          set_total[k]=1
        }
      }
      if(sum(set_total)==(2*n)){
        nrep[i]=1
      }
    }
    return(sum(nrep)/10000)
  }
  if(strategy==3){
    nrep=rep(0,nreps)###记录册
    for(i in 1:nreps){####在重复实验1：10000次时
      card_distribution = sample(1:number_of_prisoners,number_of_prisoners,replace=F)
      set_total = rep(0,2*n)###记录2n个人是否同时成功
      for(k in 1:(2*n)){####总共2n个人，k 是 index of 人
        # 记录卡number
        random_index_boxes = sample(1:number_of_prisoners,n,replace=F)
        set=rep(0,n)
        for(j in 1:n){
          
          set[j]=card_distribution[random_index_boxes[j]]
        }
        if(k%in%set){
          set_total[k]=1
        }
      }
      if(sum(set_total)==(2*n)){
        nrep[i]=1
      }
    }
    return(sum(nrep)/10000)
  }
}

##3.4 done       
        
