margins <-
function(object, newdata) {

#newdata debe ser el mismo que con el que se construye object
#newdata must be the same used in object


vardep <- newdata[,as.character(object$formula[[2]])]
n <- length(newdata[,1])
nclases <- nlevels(vardep)
votos<- object[[4]]

votos<- object$votes
votos/apply(votos,1,sum)->votosporc


margen<-rep(0,n) #Creo un vector para guardar los errores conforme evoluciona boosting

for (i in 1:n) {
 
k<-votosporc[i, as.numeric(vardep[i])]-votosporc[i,]
margen[i]<- min(k[k!=0])

}

output<- list( margins=margen)
}

