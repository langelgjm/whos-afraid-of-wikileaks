################################################################################
# This code is licensed CC BY
#
# Author: Gabriel J. Michael
#
# Postdoctoral Associate and Resident Fellow
# Information Society Project
# Yale Law School
# 127 Wall St
# New Haven, CT 06511
#
# Personal E-mail: gabriel.j.michael@gmail.com
# Institutional E-mail: gabriel.michael@yale.edu
# Website: www.gabrieljmichael.com
# Twitter: @gabrieljmichael
# Github: github.com/langelgjm
#
# This file contains the R code used to produce the figures, tables, and 
# statistics for the article "Who's Afraid of Wikileaks? Missed Opportunities 
# in Political Science Research"
# 
# Before running this code, you should have obtained or created the necessary 
# data files by either running the supplied scripts, or downloading the provided 
# data files.
#
# This code will overwrite without warning any existing files that have the 
# following names:
# tpp_network_all.png
# tpp_heatmap_clust.png
# tpp_proposals_all_col_sums.png
#
# Tested on R version 2.15.2, running on x86_64-apple-darwin9.8.0/x86_64
#################################################################################

################################################################################
# Set your working directory and perform basic tests
################################################################################

wd <- "/Users/gjm/Documents/_Works in Progress/Leaks/eclipse/"
# Check to make sure this path is valid
if (! file_test("-d", wd)) {
	stop("You must set a valid working directory.")
} else {
	setwd(wd)
}

# Does countries.csv exist?
if (! file_test("-f", "countries.csv")) {stop("Missing countries.csv")}
# Vector of files that contain position data for each chapter
position_files <- c("positions_competition_soe.csv",
		"positions_customs.csv",
		"positions_ecommerce.csv",
		"positions_environment.csv",
		"positions_govt_procurement.csv",
		"positions_investment.csv",
		"positions_ip.csv",
		"positions_labour_issues.csv",
		"positions_legal.csv",
		"positions_market_access.csv",
		"positions_rules_of_origin.csv",
		"positions_services.csv",
		"positions_sps.csv",
		"positions_tbt.csv")
# Test for existence of each file
position_files_test <- file_test("-f", position_files)
# Tell us which files, if any, are missing
if (any(ifelse(position_files_test, FALSE, TRUE))) {
	stop(paste("Missing", 
					position_files[! position_files_test], "\n"))
} else {
	print("Found all required files.\n")
}

# Test if necessary packages are installed, and load them
required_packages <- c("xtable", "igraph", "ggplot2", "RColorBrewer")
optional_packages <- c("MASS")
required_packages_test <- sapply(required_packages, require, character.only=TRUE)
if (any(ifelse(required_packages_test, FALSE, TRUE))) {
	stop(paste("Missing package", 
					required_packages[! required_packages_test], "\n"))
} else {
	print("Loaded all required packages.\n")
} 
optional_packages_test <- sapply(optional_packages, require, character.only=TRUE)
if (any(ifelse(optional_packages_test, FALSE, TRUE))) {
	stop(paste("Missing optional package", 
					optional_packages[! optional_packages_test], "\n"))
} else {
	print("Loaded all optional packages.\n")
} 

################################################################################
# SECTION 3.1
# Read and prepare the data from the draft text
################################################################################

# Read countries.csv data file (contains all country codes, including single
# codes)
# Must specify number of columns because read.table only looks at first 5 rows
# when guessing
countries <- read.table("countries.csv", sep=",", header=FALSE, fill=TRUE, 
		col.names=c(1:12), stringsAsFactors=FALSE)
# The text uses standard ISO 3166-1 alpha-2 country codes for the 12 parties
codes <- c("AU","BN","CA","CL","JP","MX","MY","NZ","PE","SG","US","VN")
# Create transposed matrix of unique, unordered combinations of these codes
# This does not include identities (dyads between the same country)
code_combos <- t(combn(codes,2))
# Create transposed matrix of identities
single_code_combos <- matrix(rep(t(combn(codes,1)),2),12,2)
# This looks at each row of code_combos, and tell us if 
# it is present in each line of countries, producing a matrix of boolean values
countries_matrix <- apply(countries, 1, 
		function(x) apply(code_combos, 1, 
					function(y) all(y %in% x)))
# Look at each row of single_code_combos, and tell us which rows in 
# countries match each row of single_code combos
# This is how we count sole proposer, i.e., countries not joined by other 
# parties
countries_matrix_sole <- apply(countries, 1, 
		function(x) apply(single_code_combos, 1, 
					function(y) all(y %in% x[1] & nchar(x[2])==0)))
# Bind these matrices together
countries_matrix <- rbind(countries_matrix,countries_matrix_sole)
# And bind the combinations together to produce a matrix with  
# all dyads, including identities
code_combos <- rbind(code_combos, single_code_combos)
# Convert the logical (boolean) matrix to a numeric matrix
# This trick is easier than using as.numeric and recreating the matrix
countries_matrix <- countries_matrix * 1
# Count the frequency of each dyad and store it
countries_freq <- rowSums(countries_matrix)

# Create a data frame for analysis
countries_df <- data.frame(country1 = code_combos[,1], 
		country2 = code_combos[,2])
countries_df$freq <- countries_freq
# Add a label column
countries_df$label <- apply(countries_df, 1, function(x) paste(x[1], x[2]))
# Reorder
countries_df <- countries_df[order(countries_df$freq),]

################################################################################
# SECTION 3.1
# Display a matrix for dyadic frequencies
# This table is not included in the manuscript, but displays the data used in 
# the network graph
################################################################################

# Create empty matrix
A <- matrix(rep(NA,144), nrow=12, ncol=12)
# Name the rows and columns
dimnames(A) = list(codes, codes)
# Populate with frequencies
A[cbind(countries_df$country2, countries_df$country1)] <- countries_df$freq
# Display
xtable(A, digits=rep(0,13))

################################################################################
# SECTION 3.1
# Create the network graph
################################################################################

# It is not a directed network graph
gr <- graph.data.frame(countries_df, directed=FALSE)
# Transform the raw frequencies to produce a reasonable visualization
E(gr)$width <- rank(countries_df$freq)/10
# Purely for visual effect
E(gr)$curved <- 0.2
# Produce color vector; add 2 values to cover rounding range
colors <- heat.colors(max(rank(countries_df$freq)/10)+2)
# Reverse color order
colors <- rev(colors)
# Apply the colors
E(gr)$color <- colors[round(E(gr)$width)+1]
# Delete labels from the edges
E(gr)$label <- ""
# Add and format labels for the vertices
V(gr)$label.cex <- 2
V(gr)$label.family <- "sans"
V(gr)$label.color <- "white"
V(gr)$color <- rgb(8,104,172,maxColorValue=255)
V(gr)$frame.color <- rgb(8,104,172,maxColorValue=255)
# Manually specify the angles of loops so that they all extend outwards
angles <- c(0,0,0,0,-pi,-pi,-pi,0,-pi,-pi,-pi,0)
E(gr)$loop.angle[is.loop(gr)] <- angles
# Save output to file
png("tpp_network_all.png", 
		width=1024,height=800)
par(mar=c(0,0,0,0))
plot(gr, layout=layout.circle)
dev.off()

################################################################################
# SECTION 3.2
# Read data from hand coded files based on position document
################################################################################

read_positions <- function(f) {
# As we read each file, eliminate the first row (contains provision description)
	return(read.csv(f)[-1])
}
positions <- do.call("rbind", sapply(position_files, read_positions, 
				simplify = FALSE, USE.NAMES = FALSE))

# My hand coded file initially coded as follows:
# Accept = 1
# Reject = 0
# Reserved position = 0.5
# No data = (nothing)
# This function changes the coding as follows:
# Accept = 1
# Reject = -1
# Reserved position = NA (to avoid making assumptions about what this means)
# No data = NA (this is default R behavior)
change_coding <- function(positions) {
	# Order of changes is important!
	positions[positions==0] <- -1
	positions[positions==0.5] <- NA
	return(positions)
}
positions <- change_coding(positions)
# Reorder column names (country codes) alphabetically
positions <- positions[,order(names(positions))]

################################################################################
# SECTION 3.2
# Create the distance matrix
################################################################################

# Produce the distance matrix reporting Euclidean distances between parties
tmp <- as.matrix(dist(t(positions)))
# Note that we only need the lower triangle, so fill the upper triangle
# and the diagonal (identities) with NAs
tmp[upper.tri(tmp, diag=TRUE)] <- NA
xtable(tmp, digits=c(0,rep(1,12)))

################################################################################
# SECTION 3.2
# Create a heat map and clustering, neither of which appear in the manuscript
# Although I briefly reference the clustering results
################################################################################

png("tpp_heatmap_clust.png", height=640,width=640)
par(mar=c(0,0,0,0))
# Need a fresh distance matrix
tmp <- as.matrix(dist(t(positions)))
heatmap(tmp, symm=TRUE, col=rev(brewer.pal(9,"Blues")), cexRow=1.5, cexCol=1.5)
dev.off()

################################################################################
# SECTION 3.2
# Prepare data for MDS
################################################################################

# Create empty data frame to store chapter intensity scores
chap_df <- data.frame(matrix(vector(), 0, 12))	
read_chap <- function(f) {
	# Read chapter file, dropping first column which contains proposal names
	p <- read.csv(f, colClasses=c("character",rep("numeric",12)))[-1]
	p <- change_coding(p)
	# Create non-normalized intensity scores using column sums
	p <- colSums(p, na.rm=TRUE)
	# If we wanted normalized scores, use this line instead
	#p <- colMeans(p, na.rm=TRUE)
	# If the data frame has not yet been populated, provide country codes as 
	# column names
	if (nrow(chap_df) == 0) {
		colnames(chap_df) <- names(p)
	}
	# Add a row to the data frame with the scores for this chapter
	chap_df[nrow(chap_df)+1,] <- p
	chap_df
}

# Create transposed matrix storing chapter intensity scores
chap_m <- t(sapply(position_files, read_chap, USE.NAMES = FALSE))

################################################################################
# SECTION 3.2
# Perform MDS
################################################################################

# Create distance matrix
chap_dist <- dist(chap_m)
# Metric MDS
chap_mds <- cmdscale(chap_dist, eig=TRUE)
# View goodness-of-fit statistic
chap_mds$GOF

# For non-metric MDS, use this instead
#chap_mds <- isoMDS(chap_dist)

################################################################################
# SECTION 3.2
# Prepare labels and graph MDS results
################################################################################

# Create letters to serve as labels on the MDS graph
labels_letters <- LETTERS[1:nrow(chap_m)]
# Descriptive labels will also go in the legend
# Get the title of the chapter in each file
get_title <- function(f) {
	x <- read.csv(f)
	names(x)[1]
}
titles <- sapply(position_files, get_title, USE.NAMES = FALSE)
# Now match a prettier label to some of these titles
pretty_title <- function(title) {
	switch(title, 
		Competition={"Competition/SOE"},
	    E.Commerce={"E-Commerce"},
	    Gov.t.Procurement={"Government Procurement"},
		IP={"Intellectual Property"},
	    Market.Access={"Market Access"},
	    Rules.of.Origin={"Rules of Origin"},
	    SPS={"Sanitary/Phytosanitary"},
		{title}
	)
}
pretty_titles <- sapply(titles, pretty_title, USE.NAMES = FALSE)
# Create the legend labels for the MDS graph
labels_legend <- paste(labels_letters, ": ", pretty_titles, sep="")

# Create the data frame for graphing based on the results of the MDS
chap_mds_df <- data.frame(x=chap_mds$points[,1], 
		y=chap_mds$points[,2],
		labels_legend=labels_legend, 
		labels_letters=labels_letters)

# Define a plotting function
# This function includes the option to produce jitter (random disturbances in 
# X and Y coordinates). Without jitter, the points and labels will overlap and 
# be hard to read. Adding a small amount of jitter makes the plot far more 
# legible without affecting the interpretation of coordinates too much.
# Because the jitter is random, you cannot produce the same plot twice.
# Each call of this function will produce new and different jitter.
# To keep a specific set of jitter values, pass new_jitter = FALSE
make_mds_plot <- function(data, 
		legend_title, 
		file_name, 
		x_min, x_max, x_by,
		y_min, y_max, y_by,
		new_jitter,
		x_min_jitter, x_max_jitter, 
		y_min_jitter, y_max_jitter,
		width, height, dpi) {
	
	# Only create new jitter if explicitly told to do so
	if (new_jitter == TRUE) {
		data$xj <- data$x + runif(nrow(data),x_min_jitter,x_max_jitter)
		data$yj <- data$y + runif(nrow(data),y_min_jitter,y_max_jitter)
	}
	
	ggp <- ggplot(data=data, aes(x=xj, y=yj, color=labels_legend)) + 
			geom_point(size=15) +
			geom_point(size=12, 
					color="white", 
					show_guide=FALSE) +
			geom_text(size=6, 
					aes(label=labels_letters), 
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
					linetype="longdash")
	ggsave(file_name, ggp, width=width, height=height, dpi=dpi)
	return(ggp)
}

make_mds_plot(chap_mds_df, 
		"Chapter", 
		"tpp_proposals_all_col_sums.png", 
		-10, 40, 5, 
		-10, 15, 5, 
		TRUE, 
		-.5, .5, 
		-.5, .5, 
		9, 7, 72)
################################################################################
# END
################################################################################