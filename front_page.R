library(png)

jhu_png = readPNG("jhudsl.png")
x = rep(1:510,each=555)
y = rep(1:555,510)
val = jhu_png[,,3]
val = round(val)
rm(jhu_png)

ind1 = which(val ==1)
ind0 = which(val ==0)

n = 10e3

ind = c(sample(ind1,size=n),sample(ind0,size=n))
ind = ind[order(ind)]

x = x[ind]; y = 555-y[ind]; val = val[ind]
colvals = c("black","lightblue")
plot(x,y,pch=15,cex=0.3,col=colvals[(val + 1)])

df_small = data.frame(x=x,y=y,val=val)
df_small2 = data.frame(x=round(runif(2*n,min=0,max=510)),
                       y=round(runif(2*n,min=0,max=510)),
                       val=val)

df = tween_states(list(df_small2,df_small),
                  tweenlength=20,
                  statelength=1,
                  ease="quadratic-in",
                  nframes=20)

mxframe = max(df$.frame)
for(i in 1:5){
df = rbind(df,data.frame(df_small,
                      .frame=(mxframe+i)))
}


for(i in 1:max(df$.frame)){
  tmp = filter(df,.frame==i)
  colvals = c("black","lightblue")
  plot(tmp$x,tmp$y,pch=15,
       cex=0.3,
       col=colvals[(tmp$val + 1)],
       xaxt="n",yaxt="n",xlab="",ylab="")
}




p = vector(mode="list",length=max(df$.frame))
for(i in 1:max(df$.frame)){
  p[[i]] = ggplot(filter(df,.frame==i),
             aes(x=x, y=y)) + 
    geom_voronoi_tile(aes(fill=val),
                      show.legend=FALSE) +
    geom_voronoi_segment() +
    ggforce::theme_no_axes()
}

saveGIF(
for(i in 1:max(df$.frame)){
  print(p[[i]])
},interval = 0.2,movie.name="test.gif")

saveGIF(for(i in 1:max(df$.frame)){
  tmp = filter(df,.frame==i)
  colvals = c("black","lightblue")
  plot(tmp$x,tmp$y,pch=15,
       cex=0.3,
       col=colvals[(tmp$val + 1)],
       xaxt="n",yaxt="n",xlab="",ylab="",bty="n")
},interval=0.2,movie.name='test.gif')
