library(RWeka)
library(ggplot2)
library(tsne)
library(animation)
library(caret)
library(gridExtra)

#Set the appropiate path to the project directory
project.directory <- "C:\\Users\\IBM_ADMIN\\Desktop\\UCD\\Practicum\\"
setwd(project.directory)





j48.crossvalidate.data.kernel.density <- function(data.set,class.offset, classfication.formula, dataset.name){
	
	#Split the input into 10 folds 
	splits.samples <- createFolds(data.set[,class.offset])
	splits <- lapply(splits.samples, function(ind, dat) dat[ind,], dat = data.set)
	
	ind <- 1

	
	
	#We have the samples ready,prepare visualizations, 
	# run the classification, plot the results
	
	pca <- prcomp(data.set[,-class.offset], scale. = T, center = T)
	pca.graph <- data.frame(pca$x)
	pca.graph$Class <- data.set[,class.offset]

	
	d <- dist(data.set[,-class.offset])
	fit <- cmdscale(d, k=2)
	mds <- data.frame(fit)
	mds$Class <- data.set[,class.offset]
	
	tsne.data <- tsne(data.set[,-class.offset],  perplexity=50)
	tsne.graph <- data.frame(tsne.data)
	tsne.graph$Class <- data.set[,class.offset]
	
	
	#Create the training set data and the test data set
	for( test.sample in names(splits) ) {

		training.data <- data.frame()
		
		for (i in names(splits)){
			if ( i != test.sample ) {
				training.data <- rbind(training.data, splits[[i]])
			}
		}
		test.data <- splits[[test.sample]]

		
		j48.predictor <- J48(classfication.formula,data = training.data)

		results <- predict(j48.predictor, newdata = data.set)
		
		#hasCF stands for "has classification failed"
		data.set$hasCF <- results != data.set[,class.offset]
		
		
		failed.pca.objs <- pca.graph[data.set$hasCF,]
		failed.mds.objs <- mds[data.set$hasCF,]
		failed.tsne.objs <- tsne.graph[data.set$hasCF,]
		
		training.data$is.validation.set <- FALSE
		test.data$is.validation.set <- TRUE
		
		
		all.data <- rbind(training.data, test.data)
		all.data <- all.data[order(as.numeric(row.names(all.data))),]
		
		pca.graph$is.validation.set <- all.data$is.validation.set
		mds$is.validation.set <- all.data$is.validation.set
		tsne.graph$is.validation.set <- all.data$is.validation.set
		
		pca.plot.name <- paste(dataset.name,"PCA data set fold",ind,sep=" ")
		
		pca.plot <- ggplot(pca.graph, mapping = aes(PC1,PC2,color=Class)) + 
		geom_point() +
		ggtitle(pca.plot.name) +  
		annotate("segment", x = failed.pca.objs[,1], xend = failed.pca.objs[,1] + 0.1, 
						y = failed.pca.objs[,2], yend = failed.pca.objs[,2] + 0.1,
						alpha =0.2, arrow = arrow(ends="first",type="closed"))  + 
						theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank()) + stat_density2d()
		
		mds.plot.name <- paste(dataset.name,"MDS data set fold",ind, sep=" ")
		
		mds.plot <- ggplot(mds, mapping = aes(X1, X2,color=Class)) + 
		geom_point() +
		ggtitle(mds.plot.name) +  
		annotate("segment", x = failed.mds.objs[,1], xend = failed.mds.objs[,1], 
						y = failed.mds.objs[,2], yend = failed.mds.objs[,2],
						alpha =0.2, arrow = arrow(ends="first",type="closed")) +
						theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + stat_density2d()

		tsne.plot.name <- paste(dataset.name, "TSNE data set fold",ind, sep=" ")
		tsne.plot <- ggplot(tsne.graph, mapping = aes(X1, X2,color=Class)) + 
		geom_point() +
		ggtitle(tsne.plot.name) +  
		annotate("segment", x = failed.tsne.objs[,1], xend = failed.tsne.objs[,1], 
						y = failed.tsne.objs[,2], yend = failed.tsne.objs[,2],
						alpha =0.2, arrow = arrow(ends="first",type="closed")) +
						theme(legend.position="none",axis.title.x = element_blank(), axis.title.y = element_blank()) + stat_density2d()
						
		
		
		plt <- grid.arrange(pca.plot, mds.plot, tsne.plot,ncol=2, nrow =2)
		ggsave(file=paste(dataset.name," data set fold ",ind," kernel", ".svg"), plot=plt, width=297, height=210, units="mm")
		print(plt)
		ind <- ind + 1
		}
		
}

j48.crossvalidate.data <- function(data.set,class.offset, classfication.formula, dataset.name){
	
	#Split the input into 10 folds 
	splits.samples <- createFolds(data.set[,class.offset])
	splits <- lapply(splits.samples, function(ind, dat) dat[ind,], dat = data.set)
	
	ind <- 1

	
	
	#We have the samples ready,prepare visualizations, 
	# run the classification, plot the results
	
	pca <- prcomp(data.set[,-class.offset], scale. = T, center = T)
	pca.graph <- data.frame(pca$x)
	pca.graph$Class <- data.set[,class.offset]

	
	d <- dist(data.set[,-class.offset])
	fit <- cmdscale(d, k=2)
	mds <- data.frame(fit)
	mds$Class <- data.set[,class.offset]
	
	tsne.data <- tsne(data.set[,-class.offset],  perplexity=50)
	tsne.graph <- data.frame(tsne.data)
	tsne.graph$Class <- data.set[,class.offset]
	
	
	#Create the training set data and the test data set
	for( test.sample in names(splits) ) {

		training.data <- data.frame()
		
		for (i in names(splits)){
			if ( i != test.sample ) {
				training.data <- rbind(training.data, splits[[i]])
			}
		}
		test.data <- splits[[test.sample]]

		
		j48.predictor <- J48(classfication.formula,data = training.data)

		results <- predict(j48.predictor, newdata = data.set)
		
		#hasCF stands for "has classification failed"
		data.set$hasCF <- results != data.set[,class.offset]
		
		
		failed.pca.objs <- pca.graph[data.set$hasCF,]
		failed.mds.objs <- mds[data.set$hasCF,]
		failed.tsne.objs <- tsne.graph[data.set$hasCF,]
		
		training.data$is.validation.set <- FALSE
		test.data$is.validation.set <- TRUE
		
		
		all.data <- rbind(training.data, test.data)
		all.data <- all.data[order(as.numeric(row.names(all.data))),]
		
		pca.graph$is.validation.set <- all.data$is.validation.set
		mds$is.validation.set <- all.data$is.validation.set
		tsne.graph$is.validation.set <- all.data$is.validation.set
		
		pca.plot.name <- paste(dataset.name,"PCA data set fold",ind,sep=" ")

		
		pca.plot <- ggplot(pca.graph, mapping = aes(PC1,PC2,shape = is.validation.set,color=Class)) + 
		geom_point() +
		ggtitle(pca.plot.name) +  
		annotate("segment", x = failed.pca.objs[,1], xend = failed.pca.objs[,1] + 0.1, 
						y = failed.pca.objs[,2], yend = failed.pca.objs[,2] + 0.1,
						alpha =0.2, arrow = arrow(ends="first",type="closed"))  + 
						theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank())
		
		mds.plot.name <- paste(dataset.name,"MDS data set fold",ind, sep=" ")
		
		mds.plot <- ggplot(mds, mapping = aes(X1, X2, shape = is.validation.set,color=Class)) + 
		geom_point() +
		ggtitle(mds.plot.name) +  
		annotate("segment", x = failed.mds.objs[,1], xend = failed.mds.objs[,1], 
						y = failed.mds.objs[,2], yend = failed.mds.objs[,2],
						alpha =0.2, arrow = arrow(ends="first",type="closed")) +
						theme(axis.title.x = element_blank(), axis.title.y = element_blank())

		tsne.plot.name <- paste(dataset.name, "TSNE data set fold",ind, sep=" ")
		
		tsne.plot <- ggplot(tsne.graph, mapping = aes(X1, X2, shape = is.validation.set,color=Class)) + 
		geom_point() +
		ggtitle(tsne.plot.name) +  
		annotate("segment", x = failed.tsne.objs[,1], xend = failed.tsne.objs[,1], 
						y = failed.tsne.objs[,2], yend = failed.tsne.objs[,2],
						alpha =0.2, arrow = arrow(ends="first",type="closed")) +
						theme(legend.position="none",axis.title.x = element_blank(), axis.title.y = element_blank())
						
		
		
		
		plt <- grid.arrange(pca.plot, mds.plot, tsne.plot,ncol=2, nrow =2)
		ggsave(file=paste(dataset.name," data set fold ",ind,".svg"), plot=plt, width=297, height=210, units="mm")
		print(plt)
		ind <- ind + 1
		}
		
}

#save data as weka format 
write.arff(iris, "Datasets\\iris\\iris.arff")
#iris does not need any pre-processing
saveGIF(j48.crossvalidate.data(data.set = iris, class.offset = 5, classfication.formula = Species ~ ., "Iris"), interval = .5, movie.name="j48-iris.gif", ani.height = 1000, ani.width = 1618)
saveGIF(j48.crossvalidate.data.kernel.density(data.set = iris, class.offset = 5, classfication.formula = Species ~ ., "Iris"), interval = .5, movie.name="j48-iris-kernel.gif", ani.height = 1000, ani.width = 1618)



#Glass data set
glass.data <- read.csv ("Datasets\\glass\\glass.data", sep = ",")
colnames(glass.data) <- c("ID", "RI", "NA2O", "MGO", "AL2O3", "SIO2", "K2O", "CAO", "BAO", "FE2O3", "Class")
glass.data$ID <- NULL
glass.data$Class <- as.factor(glass.data$Class)
write.arff(glass.data, "Datasets\\glass\\glass.arff")
glass.data$Class <- as.factor(glass.data$Class)
saveGIF(j48.crossvalidate.data(data.set = glass.data, class.offset = 10, classfication.formula = Class ~ ., "Glass"), interval = .5, movie.name="j48-glass.gif", ani.height = 1000, ani.width = 1618)
saveGIF(j48.crossvalidate.data.kernel.density(data.set = glass.data, class.offset = 10, classfication.formula = Class ~ .,"Glass"), interval = .5, movie.name="j48-glass-kernel.gif", ani.height = 1000, ani.width = 1618)




#Wine data set
wine.data <- read.csv("Datasets\\wine\\wine.data", sep = ",")
colnames(wine.data) <- c("Type", "Alcohol", "MalicAcid", "Ash", "AlcalinityOfAsh", "Magnesium", "TotalPhenols", "Flavanoids", "NonflavanoidPhenols", "Proanthocyanins", "ColorIntensity", "Hue", "OD280OD315", "Proline")
wine.data$Type <- as.factor(wine.data$Type)
write.arff(wine.data, "Datasets\\wine\\wine.arff")


saveGIF(j48.crossvalidate.data(data.set = wine.data, class.offset = 1, classfication.formula = Type ~ .,"Wine"), interval = .5, movie.name="j48-wine.gif", ani.height = 1000, ani.width = 1618)
saveGIF(j48.crossvalidate.data.kernel.density(data.set = wine.data, class.offset = 1, classfication.formula = Type ~ .,"Wine"), interval = .5, movie.name="j48-wine-kernel.gif", ani.height = 1000, ani.width = 1618)




sonar.data <- read.csv ("Datasets\\sonar\\sonar.all-data", sep = ",")
write.arff(sonar.data, "Datasets\\sonar\\sonar.arff")						
saveGIF(j48.crossvalidate.data(data.set = sonar.data, class.offset = 61, classfication.formula = R ~ .,"Sonar"), interval = .5, movie.name="j48-sonar.gif", ani.height = 1000, ani.width = 1618)
saveGIF(j48.crossvalidate.data.kernel.density(data.set = sonar.data, class.offset = 61, classfication.formula = R ~ .,"Sonar"), interval = .5, movie.name="j48-sonar-kernel.gif", ani.height = 1000, ani.width = 1618)









iris.PCA <- prcomp(iris[c(1:ncol(iris)-1)], scale. = T)
iris.PCA.graph <- data.frame(iris.PCA$x)
iris.PCA.graph$Class <- iris$Species
iris.j48.predictor <- J48(Species~.,data = iris)
results <- predict(iris.j48.predictor)
#IsCC stands for "Is correctly classified"
iris.PCA.graph$IsCC <- results == iris$Species
iris.PCA.graph.IC <-  iris.PCA.graph[!iris.PCA.graph$IsCC,]
pca.plot.iris <- ggplot(iris.PCA.graph, mapping = aes(PC1,PC2,shape = Class,color=Class)) + 
		geom_point() +
		ggtitle("PCA Iris data set") +  stat_density2d() +
		annotate("segment", x = iris.PCA.graph.IC[,1], xend = iris.PCA.graph.IC[,1] + 1, 
						y = iris.PCA.graph.IC[,2], yend = iris.PCA.graph.IC[,2] + 1,
						alpha =0.2, arrow = arrow(ends="first",type="closed"))


d <- dist(iris[c(1,2,3,4)])
fit <- cmdscale(d,k=2)
iris.mds <- data.frame(fit)
iris.mds$Species <- iris$Species
iris.mds$IsCC <- results == iris$Species
iris.mds.IC <-  iris.mds[!iris.mds$IsCC,]
mds.plot.iris <- ggplot(data = iris.mds, mapping = aes(X1, X2, color = Species,shape=Species)) + 
		geom_point() +
		ggtitle("MDS Iris data set") +  stat_density2d() +
		annotate("segment", x = iris.mds.IC[,1], xend = iris.mds.IC[,1] + 1, 
						y = iris.mds.IC[,2], yend = iris.mds.IC[,2] + 1,
						alpha =0.2, arrow = arrow(ends="first",type="closed"))



						
iris.tsne <- tsne(iris[,1:4],  perplexity=50)
iris.tsne.graph <- data.frame(iris.tsne)
iris.tsne.graph$Class <- iris$Species
iris.tsne.graph$IsCC <- results == iris$Species
iris.tsne.graph.IC <-  iris.tsne.graph[!iris.tsne.graph$IsCC,]


tsne.plot.iris <- ggplot(data = iris.tsne.graph, mapping = aes(X1, X2, color = Class,shape=Class)) + 
		geom_point() +
		ggtitle("T-SNE Iris data set") +  stat_density2d() +
		annotate("segment", x = iris.tsne.graph.IC[,1], xend = iris.tsne.graph.IC[,1] + 1, 
						y = iris.tsne.graph.IC[,2], yend = iris.tsne.graph.IC[,2] + 1,
						alpha =0.2, arrow = arrow(ends="first",type="closed"))

						
						
wine.data <- read.csv("Datasets\\wine\\wine.data", sep = ",")
colnames(wine.data) <- c("Type", "Alcohol", "MalicAcid", "Ash", "AlcalinityOfAsh", "Magnesium", "TotalPhenols", "Flavanoids", "NonflavanoidPhenols", "Proanthocyanins", "ColorIntensity", "Hue", "OD280OD315", "Proline")
wine.data$Type <- as.factor(wine.data$Type)
write.arff(wine.data, "Datasets\\wine\\wine.arff")
wine.PCA <- prcomp(wine.data[c(2:ncol(wine.data))], scale. = T)
wine.PCA.graph <- data.frame(wine.PCA$x)
wine.PCA.graph$Class <- wine.data$Type
wine.j48.predictor <- J48(Type~.,data = wine.data)
results <- predict(wine.j48.predictor)
wine.PCA.graph$IsCC <- results == wine.data$Type
wine.PCA.graph.IC <-  wine.PCA.graph[!wine.PCA.graph$IsCC,]
pca.plot.wine <- ggplot(wine.PCA.graph, mapping = aes(PC1,PC2,shape = Class,color=Class)) + 
		geom_point() +
		ggtitle("PCA Wine data set") +  stat_density2d() +
		annotate("segment", x = wine.PCA.graph.IC[,1], xend = wine.PCA.graph.IC[,1] + 1, 
						y = wine.PCA.graph.IC[,2], yend = wine.PCA.graph.IC[,2] + 1,
						alpha =0.2, arrow = arrow(ends="first",type="closed"))



						

d <- dist(wine.data[2:ncol(wine.data)])
fit <- cmdscale(d,k=2)
wine.mds <- data.frame(fit)
wine.mds$Class <- wine.data$Type
wine.mds$IsCC <- results == wine.data$Type
wine.mds.IC <-  wine.mds[!wine.mds$IsCC,]
mds.plot.wine <- ggplot(data = wine.mds, mapping = aes(X1, X2, color = Class,shape=Class)) + 
		geom_point() +
		ggtitle("MDS Wine data set") +  stat_density2d() +
		annotate("segment", x = wine.mds.IC[,1], xend = wine.mds.IC[,1] + 50, 
						y = wine.mds.IC[,2], yend = wine.mds.IC[,2] + 50,
						alpha =0.2, arrow = arrow(ends="first",type="closed"))


wine.tsne <- tsne(wine.data[,2:ncol(wine.data)],  perplexity=50)
wine.tsne.graph <- data.frame(wine.tsne)
wine.tsne.graph$Class <- wine.data$Type
wine.tsne.graph$IsCC <- results == wine.data$Type
wine.tsne.graph.IC <-  wine.tsne.graph[!wine.tsne.graph$IsCC,]


tsne.plot.wine <- ggplot(data = wine.tsne.graph, mapping = aes(X1, X2, color = Class,shape=Class)) + 
		geom_point() +
		ggtitle("T-SNE Wine data set") +  stat_density2d() +
		annotate("segment", x = wine.tsne.graph.IC[,1], xend = wine.tsne.graph.IC[,1] + 20, 
						y = wine.tsne.graph.IC[,2], yend = wine.tsne.graph.IC[,2] + 20,
						alpha =0.2, arrow = arrow(ends="first",type="closed"));







sonar.data <- read.csv ("Datasets\\sonar\\sonar.all-data", sep = ",")
write.arff(sonar.data, "Datasets\\sonar\\sonar.arff")						
sonar.PCA <- prcomp(sonar.data[c(1:ncol(sonar.data)-1)], scale. = T)
sonar.PCA.graph <- data.frame(sonar.PCA$x)
sonar.PCA.graph$Class <- sonar.data$R



sonar.j48.predictor <- J48(R~.,data = sonar.data)
results <- predict(sonar.j48.predictor)
sonar.PCA.graph$IsCC <- results == sonar.data$R
sonar.PCA.graph.IC <-  sonar.PCA.graph[!sonar.PCA.graph$IsCC,]
pca.plot.sonar <- ggplot(sonar.PCA.graph, mapping = aes(PC1,PC2,shape = Class,color=Class)) + 
		geom_point() +
		ggtitle("PCA Sonar data set") +  stat_density2d() +
		annotate("segment", x = sonar.PCA.graph.IC[,1], xend = sonar.PCA.graph.IC[,1] + 1, 
						y = sonar.PCA.graph.IC[,2], yend = sonar.PCA.graph.IC[,2] + 1,
						alpha =0.2, arrow = arrow(ends="first",type="closed"))

						
					
d <- dist(sonar.data[1:ncol(wine.data)-1])
fit <- cmdscale(d,k=2)
sonar.mds <- data.frame(fit)
sonar.mds$Class <- sonar.data$R
sonar.mds$IsCC <- results == sonar.data$R
sonar.mds.IC <-  sonar.mds[!sonar.mds$IsCC,]
mds.plot.sonar <- ggplot(data = sonar.mds, mapping = aes(X1, X2, color = Class,shape=Class)) + 
		geom_point() +
		ggtitle("MDS Sonar data set") +  stat_density2d() +
		annotate("segment", x = sonar.mds.IC[,1], xend = sonar.mds.IC[,1] + 0.25, 
						y = sonar.mds.IC[,2], yend = sonar.mds.IC[,2] + 0.25,
						alpha =0.2, arrow = arrow(ends="first",type="closed"))

						
glass.PCA <- prcomp(glass.data[c(1:ncol(glass.data)-1)], scale. = T)
glass.PCA.graph <- data.frame(glass.PCA$x)
glass.PCA.graph$Class <- glass.data$Class
ggplot(glass.PCA.graph, mapping = aes(PC1,PC2,shape = Class,color=Class)) + 
		geom_point() +
		ggtitle("Glass data set") +  stat_density2d()

		
		
		
		
		


iris.perplexity.animate <- function(){
		for (i in 5:10) {
			iris.tsne <- tsne(iris[,1:4],  perplexity=i)
			iris.tsne.graph <- data.frame(iris.tsne)
			iris.tsne.graph$Class <- iris$Species
			p <- ggplot(iris.tsne.graph, mapping = aes(X1,X2,shape = Class,color=Class)) + 
			geom_point() +
			ggtitle("TSNEIris data set")
			
			print(p)
		}


		p <- ggplot(iris.tsne.graph, mapping = aes(X1,X2,shape = Class,color=Class)) + 
		geom_point() +
		ggtitle("TSNE Iris data set")
		i <- FALSE;
		print(p)
	
	p <- ggplot(iris.tsne.graph, mapping = aes(X1,X2,shape = Class,color=Class)) + 
		geom_point() +
		ggtitle("TSNE Iris data set") +  stat_density2d()
		print(p)
}


iris.perplexity.animate <- function(){
		for (i in 5:10) {
			iris.tsne <- tsne(iris[,1:4],  perplexity=i)
			iris.tsne.graph <- data.frame(iris.tsne)
			iris.tsne.graph$Class <- iris$Species
			p <- ggplot(iris.tsne.graph, mapping = aes(X1,X2,shape = Class,color=Class)) + 
			geom_point() +
			ggtitle(paste("T-SNE Iris data set perplexity of ",i))
			
			print(p)
		}
}
saveGIF(iris.perplexity.animate(), interval = 2.5, movie.name="iris.gif")

