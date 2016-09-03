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


saveGIF(j48.crossvalidate.data(data.set = iris, class.offset = 5, classfication.formula = Species ~ .,"Iris"), interval = .5, movie.name="iris.gif", ani.height = 1000, ani.width = 1618)


