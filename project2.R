sample(1:20,replace=F)
try<-c(1:20)
sample(try,10,replace = T)

##1.
strategy=c(1,2,3)
number_of_prisoners=10
data.frame(number_of_boxes=c(1:10),card=sample(number_of_prisoners,10,replace = F));
k=5
card_distribution = c(sample(1:10,10,replace=F));card_distribution###n=5


set=rep(0,5)
set[1]=card_distribution[k]
set[2]=card_distribution[set[1]]
set

set[j]=card_distribution[set[j-1]]

pone<-function(n,k,strategy,nreps=10000){
  number_of_prisoners=2*n
  if(strategy==1){
    nrep=rep(0,10000)###��¼��
    for(i in 1:nreps){####���ظ�ʵ��1��10000��ʱ
      card_distribution = c(sample(1:number_of_prisoners,number_of_prisoners,replace=F))
      set=rep(0,n)####n�γ�ȡ���ܿ�Ƭ�õ���
      set[1]=card_distribution[k]
      for(j in 2:n){ ##ֻ����ѭ��n�Σ�������������п��ܾ�����set���k��ų�����set���ɹ�
        set[j]=card_distribution[set[j-1]]
      }
      if(k%in%set){
        nrep[i]=1
      }
    }
    return(sum(nrep)/nreps)
  }
  if(strategy==2){
    nrep=rep(0,10000)###��¼��
    for(i in 1:nreps){####���ظ�ʵ��1��10000��ʱ
      card_distribution = c(sample(1:number_of_prisoners,number_of_prisoners,replace=F))
      set=rep(0,n)####n�γ�ȡ���ܿ�Ƭ�õ���
      set[1]=card_distribution[sample(1:number_of_prisoners,1,replace = F)]##random ���ӣ��ˣ�
      for(j in 2:n){ ##ֻ����ѭ��n�Σ�������������п��ܾ�����set���k��ų�����set���ɹ�
        set[j]=card_distribution[set[j-1]]
      }
      if(k%in%set){
        nrep[i]=1
      }
    }
    return(sum(nrep)/nreps)
}
  if(strategy==3){
    nrep=rep(0,10000)###��¼��
    for(i in 1:nreps){####���ظ�ʵ��1��10000��ʱ
      card_distribution = c(sample(1:number_of_prisoners,number_of_prisoners,replace=F))
      random_index_boxes = c(sample(1:number_of_prisoners),n,replace=F)
      set=rep(0,n)####n�γ�ȡ���ܿ�Ƭ�õ���
      for(j in 1:n){ ##ֻ����ѭ��n�Σ�������������п��ܾ�����set���k��ų�����set���ɹ�
        set[j]=card_distribution[random_index_boxes[j]]
      }
      if(k%in%set){
        nrep[i]=1
      }
    }
    return(sum(nrep)/nreps)
  }
}

pall<-function(n,strategy,nreps=10000){
  nrep=rep(0,10000)###��¼��
  for(i in 1:)
      return("Total probablity is",)
  }
}