library(RWeka)

anneal <- read.csv("C:\\Users\\IBM_ADMIN\\Desktop\\UCD\\Practicum\\Datasets\\anneal\\anneal.data", sep = ",", na.strings = "?")
colnames(anneal) <- c( "family", "product-type", "steel", "carbon", "hardness", "temper_rolling", "condition", "formability",
"strength", "non-ageing", "surface-finish", "surface-quality", "enamelability", "bc", "bf", "bt", "bw",
"bl", "m", "chrom", "phos", "cbond", "marvi", "exptl", "ferro", "corr", "blue", "lustre", "jurofm", "s",
"p", "shape", "thick", "width", "len", "oil", "bore", "packing", "classes")

#write.arff(anneal, "C:\\Users\\IBM_ADMIN\\Desktop\\UCD\\Practicum\\Datasets\\anneal\\anneal.arff")


anneal.test <- read.csv("C:\\Users\\IBM_ADMIN\\Desktop\\UCD\\Practicum\\Datasets\\anneal\\anneal.test", sep = ",", na.strings = "?")
colnames(anneal.test) <- c( "family", "product-type", "steel", "carbon", "hardness", "temper_rolling", "condition", "formability",
"strength", "non-ageing", "surface-finish", "surface-quality", "enamelability", "bc", "bf", "bt", "bw",
"bl", "m", "chrom", "phos", "cbond", "marvi", "exptl", "ferro", "corr", "blue", "lustre", "jurofm", "s",
"p", "shape", "thick", "width", "len", "oil", "bore", "packing", "classes")
#write.arff(anneal.test, "C:\\Users\\IBM_ADMIN\\Desktop\\UCD\\Practicum\\Datasets\\anneal\\anneal_test.arff")


anneal.all <- rbind(anneal,anneal.test)

write.csv(anneal.all,"C:\\Users\\IBM_ADMIN\\Desktop\\UCD\\Practicum\\Datasets\\anneal\\anneal_all.csv")
#write.arff(anneal.all, "C:\\Users\\IBM_ADMIN\\Desktop\\UCD\\Practicum\\Datasets\\anneal\\anneal_all.arff")



colic <- read.table("C:\\Users\\IBM_ADMIN\\Desktop\\UCD\\Practicum\\Datasets\\colic\\horse-colic.data",  na.strings = "?")
colnames(colic) <- c("surgery", "Age", "Hospital.Number", "rectal.temperature", "pulse", "respiratoryRate", "temperature.of.extremities",
"peripheral.pulse", "mucous.membranes", "capillary.refill.time", "pain", "peristalsis", "abdominal.distension",
"nasogastric.tube", "nasogastric.reflux", "nasogastric.reflux.PH", "rectal examination", "abdomen", "packed.cell.volume",
"total.protein", "abdominocentesis.appearance", "abdomcentesis.total.protein", "outcome", "surgical lesion",
"type.of.lesion1","type.of.lesion2", "type.of.lesion3", "cp_data")
#write.arff(colic, "C:\\Users\\IBM_ADMIN\\Desktop\\UCD\\Practicum\\Datasets\\colic\\colic-data.arff")


colic.test <- read.table("C:\\Users\\IBM_ADMIN\\Desktop\\UCD\\Practicum\\Datasets\\colic\\horse-colic.test",  na.strings = "?")
colnames(colic.test) <- c("surgery", "Age", "Hospital.Number", "rectal.temperature", "pulse", "respiratoryRate", "temperature.of.extremities",
"peripheral.pulse", "mucous.membranes", "capillary.refill.time", "pain", "peristalsis", "abdominal.distension",
"nasogastric.tube", "nasogastric.reflux", "nasogastric.reflux.PH", "rectal examination", "abdomen", "packed.cell.volume",
"total.protein", "abdominocentesis.appearance", "abdomcentesis.total.protein", "outcome", "surgical lesion",
"type.of.lesion1","type.of.lesion2", "type.of.lesion3", "cp_data")
#write.arff(colic.test, "C:\\Users\\IBM_ADMIN\\Desktop\\UCD\\Practicum\\Datasets\\colic\\colic-data-test.arff")


collic.all <- rbind(colic,colic.test)
#write.arff(colic.all, "C:\\Users\\IBM_ADMIN\\Desktop\\UCD\\Practicum\\Datasets\\colic\\colic-data-all.arff")



wine.data <- read.csv("C:\\Users\\IBM_ADMIN\\Desktop\\UCD\\Practicum\\Datasets\\wine\\wine.data", sep = ",")
colnames(wine.data) <- c("Type", "Alcohol", "MalicAcid", "Ash", "AlcalinityOfAsh", "Magnesium", "TotalPhenols", "Flavanoids", "NonflavanoidPhenols", "Proanthocyanins", "ColorIntensity", "Hue", "OD280OD315", "Proline")
write.arff(wine.data, "C:\\Users\\IBM_ADMIN\\Desktop\\UCD\\Practicum\\Datasets\\wine\\wine.arff")

write.arff(iris, "C:\\Users\\IBM_ADMIN\\Desktop\\UCD\\Practicum\\Datasets\\iris\\iris.arff")



glass.data <- read.csv ("C:\\Users\\IBM_ADMIN\\Desktop\\UCD\\Practicum\\Datasets\\Glass\\glass.data", sep = ",")
colnames(glass.data) <- c("ID", "RI", "NA2O", "MGO", "AL2O3", "SIO2", "K2O", "CAO", "BAO", "FE2O3", "Class")
glass.data$ID <- NULL

write.arff(glass.data, "C:\\Users\\IBM_ADMIN\\Desktop\\UCD\\Practicum\\Datasets\\Glass\\glass.arff")



sonar.data <- read.csv ("C:\\Users\\IBM_ADMIN\\Desktop\\UCD\\Practicum\\Datasets\\sonar\\sonar.all-data", sep = ",")

write.arff(sonar.data, "C:\\Users\\IBM_ADMIN\\Desktop\\UCD\\Practicum\\Datasets\\sonar\\sonar.arff")

