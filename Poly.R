#Genskaber figur 2.6 i reg bogen
x_seq<-seq(11,21,0.1)
plot(x_seq,poly(x_seq,5)[,1],type="n",ylim=c(-0.2,0.2))
lines(x_seq,poly(x_seq,5)[,1],col="blue")
lines(x_seq,poly(x_seq,5)[,2],col="red")
lines(x_seq,poly(x_seq,5)[,3],col="green")
lines(x_seq,poly(x_seq,5)[,4],col="purple")
lines(x_seq,poly(x_seq,5)[,5],col="orange")

#Poly basis i age dimensionen
t_seq<-seq(18,67,0.1)
plot(t_seq,poly(t_seq,5)[,1],type="n",ylim=c(-0.15,0.15))
lines(t_seq,poly(t_seq,5)[,1],col="blue")
lines(t_seq,poly(t_seq,5)[,2],col="red")
lines(t_seq,poly(t_seq,5)[,3],col="green")
lines(t_seq,poly(t_seq,5)[,4],col="purple")
lines(t_seq,poly(t_seq,5)[,5],col="orange")

#Poly basis i duration dimensionen
u_seq<-seq(0,3,0.01)
plot(u_seq,poly(u_seq,5)[,1],type="n",ylim=c(-0.2,0.2))
lines(u_seq,poly(u_seq,5)[,1],col="blue")
lines(u_seq,poly(u_seq,5)[,2],col="red")
lines(u_seq,poly(u_seq,5)[,3],col="green")
lines(u_seq,poly(u_seq,5)[,4],col="purple")
lines(u_seq,poly(u_seq,5)[,5],col="orange")


