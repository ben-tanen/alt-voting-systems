library(data.table)
library(ggplot2)

######################
### SET PARAMETERS ###
######################

# number of voters / candidates
n_voters     <- 1000
n_candidates <- 3

# parameters of distribution
mean.along_axis <- 0
sd.along_axis   <- 0.27
mean.off_axis   <- 0
sd.off_axis     <- 0.17

###########################
### GENERATE IDEOLOGIES ###
###########################

# set initial distributions along and off typical dem/rep axis
voters <- data.table(along_axis = rnorm(n_voters, mean = mean.along_axis, sd = sd.along_axis),
                     off_axis = rnorm(n_voters, mean = mean.off_axis, sd = sd.off_axis))
candidates <- data.table(along_axis = rnorm(n_candidates, mean = mean.along_axis, sd = 0.3),
                         off_axis = rnorm(n_candidates, mean = mean.off_axis, sd = 0.1))

# convert along and off axis metrics to x,y coordinates
voters$x <- cospi(1/4) * voters$along_axis + sinpi(1/4) * voters$off_axis
voters$y <- sinpi(1/4) * voters$along_axis - cospi(1/4) * voters$off_axis
candidates$x <- cospi(1/4) * candidates$along_axis + sinpi(1/4) * candidates$off_axis
candidates$y <- sinpi(1/4) * candidates$along_axis - cospi(1/4) * candidates$off_axis

##############################
### CALCULATE FAVORABILITY ###
##############################

# calculate distance as a proxy for favorability
dist_all <- as.matrix(dist(rbind(voters[, c("x", "y")], candidates[, c("x", "y")]), diag = T))
dist     <- dist_all[(n_voters + 1):(n_voters + n_candidates), 1:n_voters]

# convert distance to favorability
favor    <- apply(dist, c(1, 2), function(x) 1 - min(x, 1))
favor_dt <- data.table(t(favor))
names(favor_dt) <- paste0("candidate", 1:n_candidates, "_favor")

#######################
### DETERMINE VOTES ###
#######################

voters$single_vote <- max.col(favor_dt)

# plurality voting
plurality <- data.table(t(ifelse(apply(-favor_dt, 1, rank) == 1, 1, 0)))
names(plurality) <- paste0("candidate", 1:n_candidates, "_plurality")

# approval voting
approval <- data.table(apply(favor_dt, c(1, 2), function(x) ifelse(x >= 0.7, 1, 0)))
names(approval) <- paste0("candidate", 1:n_candidates, "_approval")

# ranked choice voting
ranked_choice <- data.table(t(apply(-favor_dt, 1, rank)))
names(ranked_choice) <- paste0("candidate", 1:n_candidates, "_rank")

# combine all voting systems
voters_plus <- cbind(voters, favor_dt, plurality, approval, ranked_choice)

############
### PLOT ###
############

ggplot() + 
    annotate("rect", xmin = Inf, xmax = 0, ymin = Inf, ymax = 0, fill= "red", alpha = 0.08)  + 
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0 , fill = "blue", alpha = 0.08) + 
    annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = -Inf, fill= "yellow", alpha = 0.08) + 
    annotate("rect", xmin = 0, xmax = -Inf, ymin = Inf, ymax = 0, fill= "green", alpha = 0.08) +
    annotate("text", x = -0.92, y = -0.9, label = "Conservative", hjust = "left") +
    annotate("text", x = -0.92, y = 0.9, label = "Libertarian", hjust = "left") +
    annotate("text", x = 0.92, y = 0.9, label = "Liberal", hjust = "right") +
    annotate("text", x = 0.92, y = -0.9, label = "Communitarian", hjust = "right") +
    geom_point(data = voters_plus, aes(x = x, y = y, color = factor(single_vote)), size = 1) + 
    geom_point(data = candidates, aes(x = x, y = y)) + 
    geom_point(data = candidates, aes(x = x, y = y), size = 56, alpha = 0.1) + 
    scale_x_continuous(name = "Fiscal Issues", limits = c(-1, 1), expand = c(0, 0)) +
    scale_y_continuous(name = "Social Issues", limits = c(-1, 1), expand = c(0, 0)) +
    scale_color_discrete(labels = paste0("vote for candidate ", 1:n_candidates))

# tabulate votes under each system
max.col(t(colSums(plurality)) / n_voters)
max.col(t(colSums(approval)) / n_voters)
as.matrix(dist(rbind(candidates[, 3:4], data.table(x = 0, y = 0))))[n_candidates+1,1:n_candidates]
