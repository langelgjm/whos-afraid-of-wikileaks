######### Network Graph #############

library(xtable)

setwd("/Users/gjm/Documents/_Works in Progress/TPP/Leak 1/")
# Have to specify number of columns because read.table only looks at first 5 rows
countries <- read.table("countries.csv", sep=",", header=FALSE,fill=TRUE,col.names=c(1:12),
		stringsAsFactors=FALSE)

# create list of country codes
codes <- c("AU","BN","CA","CL","JP","MX","MY","NZ","PE","SG","US","VN")
# create transposed matrix of unique, unordered combinations of country codes
# later we add identities to the combinations
##code_combos <- rbind(t(combn(codes,2)), matrix(rep(t(combn(codes,1)),2),12,2))
code_combos <- t(combn(codes,2))
# Create matrix of identical country codes
single_code_combos <- matrix(rep(t(combn(codes,1)),2),12,2)
# Do some tests
#code_combos[75,]
#apply(head(countries), 1, function(x) all(code_combos[75,] %in% x))
#apply(code_combos, 1, function(y) all(y %in% countries[11,]))

# This should run through each row of code_combos, and tell us if 
# it is present in each line of groups, producing a matrix
countries_matrix <- apply(countries, 1, 
		function(x) apply(code_combos, 1, 
					function(y) all(y %in% x)))
# Now handle the sole proposers
# This runs through for just the single code combos
countries_matrix_sole <- apply(countries, 1, 
		function(x) apply(single_code_combos, 1, 
					function(y) all(y %in% x[1] & nchar(x[2])==0 ) ) )

# now bind matrices together
countries_matrix <- rbind(countries_matrix,countries_matrix_sole)
code_combos <- rbind(code_combos, single_code_combos)

# Trick to convert logical matrix to numeric matrix
countries_matrix <- countries_matrix * 1
# Now row sums will provide a frequency count for each pair
countries_freq <- rowSums(countries_matrix)

# Start making a data frame
countries_df <- data.frame(country1 = code_combos[,1], country2 = code_combos[,2])
countries_df$freq <- countries_freq
# Make labels
countries_df$label <- apply(countries_df, 1, function(x) paste(x[1], x[2]))
# Reorder
countries_df <- countries_df[order(countries_df$freq),]

# Create a matrix for diplaying frequencies
A <- matrix(rep(NA,144), nrow=12, ncol=12)
dimnames(A) = list(codes, codes)
A[cbind(countries_df$country2, countries_df$country1)] <- countries_df$freq
xtable(A, digits=rep(0,13))

# Created weighted network graph
library(igraph)
gr <- graph.data.frame(countries_df, directed=FALSE)
# Play with different ways of transforming the frequencies
#E(gr)$width <- countries_df$freq / 15
E(gr)$width <- rank(countries_df$freq)/10
E(gr)$curved <- 0.2
#E(gr)$color <- ifelse(grepl("US", E(gr)$label), "blue", "grey")
# Produce color vector; add 2 for rounding down and up
colors <- heat.colors(max(rank(countries_df$freq)/10)+2)
# reverse color order
colors <- rev(colors)
E(gr)$color <- colors[round(E(gr)$width)+1]
# Delete labels for now
E(gr)$label <- ""
V(gr)$label.cex <- 2
V(gr)$label.family <- "sans"
V(gr)$label.color <- "white"
V(gr)$color <- rgb(8,104,172,maxColorValue=255)
V(gr)$frame.color <- rgb(8,104,172,maxColorValue=255)
angles <- c(0,0,0,0,-pi,-pi,-pi,0,-pi,-pi,-pi,0)
E(gr)$loop.angle[is.loop(gr)] <- angles
#pdf("tpp_network_all.pdf", width=12,height=10)
png("/Users/gjm/Documents/_Works in Progress/Leaks/eclipse/tpp_network_all.png", 
		width=1024,height=800)
par(mar=c(0,0,0,0))
plot(gr, layout=layout.circle)
#title(main="Weighted Network Graph of Country Dyad Negotiating Positions\n in TPP IP Chapter, including Sole-Country Proposals", , sub="Gabriel J. Michael, gmichael@gwu.edu")
dev.off()

############# Distance Matrix and Heat Map ############


setwd("/Users/gjm/Documents/_Works in Progress/TPP/Leak 2")

library(MASS)
library(ggplot2)
library(Matrix)
library(xtable)

tpp_change_coding <- function(positions) {
	# change coding: 0 becomes -1, 0.5 becomes NA, 1 stays 1
	positions[positions==0] <- -1
	# 0.5 to NA means making no assumptions about reserved positions
	positions[positions==0.5] <- NA
	return(positions)
}

tpp_chapter_positions <- function(positions) {
	# drop first row, since it contains proposal data not relevant to chapter comparisons
	label <- colnames(positions)[1]
	positions <- positions[-1]
	# NAs prevent calculating cosine similarity, so changing them to 0s for now
	positions[is.na(positions)] <- 0
	# cosine similarity calculates similarity between N-dimensional vectors, produces value between -1 and 1
	# -1 is diametrically opposed vectors, 0 is vectors at 90 degrees, 1 is identical vectors
	# Then the column sum will a meaningful value?
	# The higher the value, the more similarity exists between a country and others?
	# Divide column sum by number of issues to normalize it between chapters?
	# Decided against normalizing, as it is misleading?
#	cos_sim_vector <- colSums(cosine(as.matrix(positions)))/nrow(positions)
	cos_sim_vector <- colSums(cosine(as.matrix(positions)))
	return(list(label, cos_sim_vector))
}

positions <- read.csv("positions_ip.csv")[-1]
positions <- rbind(positions, read.csv("positions_competition_soe.csv")[-1])
positions <- rbind(positions, read.csv("positions_customs.csv")[-1])
positions <- rbind(positions, read.csv("positions_ecommerce.csv")[-1])
positions <- rbind(positions, read.csv("positions_environment.csv")[-1])
positions <- rbind(positions, read.csv("positions_govt_procurement.csv")[-1])
positions <- rbind(positions, read.csv("positions_investment.csv")[-1])
positions <- rbind(positions, read.csv("positions_labour_issues.csv")[-1])
positions <- rbind(positions, read.csv("positions_legal.csv")[-1])
positions <- rbind(positions, read.csv("positions_market_access.csv")[-1])
positions <- rbind(positions, read.csv("positions_rules_of_origin.csv")[-1])
positions <- rbind(positions, read.csv("positions_services.csv")[-1])
positions <- rbind(positions, read.csv("positions_sps.csv")[-1])
positions <- rbind(positions, read.csv("positions_tbt.csv")[-1])
positions <- tpp_change_coding(positions)
# reorder column names alphabetically
positions <- positions[,order(names(positions))]

# Euclidean distance
tmp <- as.matrix(dist(t(positions)))
tmp[upper.tri(tmp, diag=TRUE)] <- NA
xtable(tmp, digits=c(0,rep(1,12)))
# Manhattan distance
#dist(t(positions), method="manhattan")


library(RColorBrewer)
#library(gplots)
png("/Users/gjm/Documents/_Works in Progress/Leaks/eclipse/tpp_heatmap_clust.png", height=640,width=640)
par(mar=c(0,0,0,0))
#par(mar=c(5.1,8.1,4.1,2.1))
# Euclidean distance
tmp <- as.matrix(dist(t(positions)))
#tmp[lower.tri(tmp, diag=TRUE)] <- NA
heatmap(tmp, symm=TRUE, col=rev(brewer.pal(9,"Blues")), cexRow=1.5, cexCol=1.5)
#heatmap.2(tmp, symm=TRUE, col=rev(brewer.pal(9,"Blues")))
#heatmap(tmp, symm=TRUE, main="Heat Map of Euclidean Distances between TPP Negotiating Positions")
#heatmap(tmp, Rowv=NA, Colv=NA, col=rev(heat.colors(24)), symm = TRUE,
#		main=expression(atop("Heat Map of TPP Negotiating Position Overlap", atop("CC BY-SA 3.0, Gabriel J. Michael, gmichael@gwu.edu"))))
dev.off()

############ MDS ################

library(lsa)

setwd("/Users/gjm/Documents/_Works in Progress/TPP/Leak 2")

create_ggplot <- function(legend_title, 
		chapter_title, 
		file_name, 
		x_min, x_max, x_by,
		y_min, y_max, y_by,
		new_jitter,
		x_min_jitter, x_max_jitter, 
		y_min_jitter, y_max_jitter,
		width, height, dpi) {
	
	# Only create new jitter if explicitly told to do so
	if (new_jitter == TRUE) {
		positions_mds_gg$xj <- positions_mds_gg$x + runif(nrow(positions_mds_gg),x_min_jitter,x_max_jitter)
		positions_mds_gg$yj <- positions_mds_gg$y + runif(nrow(positions_mds_gg),y_min_jitter,y_max_jitter)
	}
	
	ggp <- ggplot(data=positions_mds_gg, aes(x=xj, y=yj, color=issue)) + 
			geom_point(size=15) +
			geom_point(size=12, 
					color="white", 
					show_guide=FALSE) +
			geom_text(size=6, 
					aes(label=nums), 
					color="black") +
			theme(axis.text = element_text(size=20), 
					axis.ticks=element_blank(), 
					axis.title = element_text(size=20), 
					plot.title=element_text(size=20), 
					legend.title = element_text(size=16), 
					legend.text=element_text(size=16),
					legend.position="bottom") +
			scale_x_continuous(limits=c(x_min,x_max), 
					breaks=seq(x_min,x_max,by=x_by), 
					name="x") + 
			scale_y_continuous(limits=c(y_min,y_max), 
					breaks=seq(y_min,y_max,by=y_by), 
					name="y") + 
			scale_color_discrete(name=legend_title) + 
			guides(colour = guide_legend(ncol=2, override.aes = list(size=7), 
							title.position="bottom", 
							title.hjust=0.5)) +
			coord_fixed() + 
			geom_vline(xintercept=0, 
					colour="#999999", 
					linetype="longdash") + 
			geom_hline(yintercept=0, 
					colour="#999999", 
					linetype="longdash") #+ 
#			ggtitle(
#					bquote(
#							atop(paste("TPP Issue Distances, ", .(chapter_title), sep=""), 
#									atop("CC BY-SA 3.0 Gabriel J. Michael, gmichael@gwu.edu", ""))))
	ggsave(file_name, ggp, width=width, height=height, dpi=dpi)
	return(ggp)
}

tpp_change_coding <- function(positions) {
	# change coding: 0 becomes -1, 0.5 becomes NA, 1 stays 1
	positions[positions==0] <- -1
	# 0.5 to NA means making no assumptions about reserved positions
	positions[positions==0.5] <- NA
	return(positions)
}

# Normalized

tpp_chapter_positions_norm <- function(positions) {
	# drop first row, since it contains proposal data not relevant to chapter comparisons
	label <- colnames(positions)[1]
	positions <- positions[-1]
	# creates intensity score, which we have to normalize?
	v <- colMeans(positions, na.rm=TRUE)
	return(list(label, v))
}

tpp_chapter_positions_nonnorm <- function(positions) {
	# drop first row, since it contains proposal data not relevant to chapter comparisons
	label <- colnames(positions)[1]
	positions <- positions[-1]
	# creates intensity score, which we have to normalize?
	v <- colSums(positions, na.rm=TRUE)
	return(list(label, v))
}

positions <- read.csv("positions_ip.csv", colClasses=c("character",rep("numeric",12)))
positions <- tpp_change_coding(positions)
tpp <- data.frame(matrix(vector(), 0, 12))
colnames(tpp) <- colnames(positions[2:13])
o <- tpp_chapter_positions_nonnorm(positions)
tpp[1,] <- o[[2]]

positions <- read.csv("positions_competition_soe.csv", colClasses=c("character",rep("numeric",12)))
positions <- tpp_change_coding(positions)
o <- tpp_chapter_positions_nonnorm(positions)
tpp[2,] <- o[[2]]

positions <- read.csv("positions_customs.csv", colClasses=c("character",rep("numeric",12)))
positions <- tpp_change_coding(positions)
o <- tpp_chapter_positions_nonnorm(positions)
tpp[3,] <- o[[2]]

positions <- read.csv("positions_ecommerce.csv", colClasses=c("character",rep("numeric",12)))
positions <- tpp_change_coding(positions)
o <- tpp_chapter_positions_nonnorm(positions)
tpp[4,] <- o[[2]]

positions <- read.csv("positions_environment.csv", colClasses=c("character",rep("numeric",12)))
positions <- tpp_change_coding(positions)
o <- tpp_chapter_positions_nonnorm(positions)
tpp[5,] <- o[[2]]

positions <- read.csv("positions_govt_procurement.csv", colClasses=c("character",rep("numeric",12)))
positions <- tpp_change_coding(positions)
o <- tpp_chapter_positions_nonnorm(positions)
tpp[6,] <- o[[2]]

positions <- read.csv("positions_investment.csv", colClasses=c("character",rep("numeric",12)))
positions <- tpp_change_coding(positions)
o <- tpp_chapter_positions_nonnorm(positions)
tpp[7,] <- o[[2]]

positions <- read.csv("positions_labour_issues.csv", colClasses=c("character",rep("numeric",12)))
positions <- tpp_change_coding(positions)
o <- tpp_chapter_positions_nonnorm(positions)
tpp[8,] <- o[[2]]

positions <- read.csv("positions_legal.csv", colClasses=c("character",rep("numeric",12)))
positions <- tpp_change_coding(positions)
o <- tpp_chapter_positions_nonnorm(positions)
tpp[9,] <- o[[2]]

positions <- read.csv("positions_market_access.csv", colClasses=c("character",rep("numeric",12)))
positions <- tpp_change_coding(positions)
o <- tpp_chapter_positions_nonnorm(positions)
tpp[10,] <- o[[2]]

positions <- read.csv("positions_rules_of_origin.csv", colClasses=c("character",rep("numeric",12)))
positions <- tpp_change_coding(positions)
o <- tpp_chapter_positions_nonnorm(positions)
tpp[11,] <- o[[2]]

positions <- read.csv("positions_services.csv", colClasses=c("character",rep("numeric",12)))
positions <- tpp_change_coding(positions)
o <- tpp_chapter_positions_nonnorm(positions)
tpp[12,] <- o[[2]]

positions <- read.csv("positions_sps.csv", colClasses=c("character",rep("numeric",12)))
positions <- tpp_change_coding(positions)
o <- tpp_chapter_positions_nonnorm(positions)
tpp[13,] <- o[[2]]

positions <- read.csv("positions_tbt.csv", colClasses=c("character",rep("numeric",12)))
positions <- tpp_change_coding(positions)
o <- tpp_chapter_positions_nonnorm(positions)
tpp[14,] <- o[[2]]

label_nums <- LETTERS[1:nrow(tpp)]
labels <- c("Intellectual Property", "Competition/SOE", "Customs", "E-Commerce", "Environment", 
		"Government Procurement", "Investment", "Labor", "Legal", 
		"Market Access", "Rules of Origin", "Services",
		"Sanitary/Phytosanitary", "Technical Barriers to Trade")
labels <- paste(label_nums, ": ", labels, sep="")
#	labels <- rep("blank", 11)

positions_dist <- dist(tpp)
# Alternatively
#tpp[is.na(tpp)] <- 0
#positions_dist <- acos(cosine(as.matrix(t(tpp))))
positions_mds <- isoMDS(positions_dist)
positions_mds_gg <- data.frame(x=positions_mds$points[,1], 
		y=positions_mds$points[,2], 
		issue=labels, 
		nums=label_nums)

positions_mds <- cmdscale(positions_dist)
cmdscale(positions_dist, eig=TRUE)
positions_mds_gg <- data.frame(x=positions_mds[,1], 
		y=positions_mds[,2], 
		issue=labels, 
		nums=label_nums)
#
create_ggplot("Chapter", "All Chapters (column sums)", 
		"/Users/gjm/Documents/_Works in Progress/Leaks/eclipse/tpp_proposals_all_col_sums.png", 
		-10, 40, 5, -10, 15, 5, TRUE, -.5, .5, -.5, .5, 9, 7, 72)
#create_ggplot("Chapter", "All Chapters (column sums)", "tpp_proposals_all_col_sums_2.png", 
#		-3, 3, 1, -3, 3, 1, TRUE, -.2, .2, -.2, .2, 10, 10, 72)
