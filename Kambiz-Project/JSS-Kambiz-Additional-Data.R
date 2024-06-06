# JSS Kambiz - additional data for reviewers information


# Number of lines per parallel dimensions plots
task1.pairs <- c(41,43,42,36,32,31,35,34,32,34,32) #11, 392
task2.pairs <- c(75,77,77,65,65,63,63,66,55,65,55,66,77,60) #14, 929
task3.pairs <- c(118,131,119,132,114,130,117,129,126,134,136) #11, 1386
task4.pairs <- c(41,34,41,54,51,43,45) #7, 309


task9.triplets  <- c(120,84,120,120,84,120,84,120,56,84,120,56,56,56,84,84,56,56,84,84,84,84) #22, 1896
task10.triplets <- c(92,77,42,36,22,22) # 6, 291
task11.triplets <- c(514,748,1087,826,646,1059,513,629,698,803,652,694,1012,886,
                     522,657,691,445,449,596,912,676,661,756,466,302,422,596,435,517,389) #31, 20259
task12.triplets <- c(455,320,319,308,352,319,235,245,235,234,235,245,173,193,210) #15, 4078


number.lines.pd <- c(sum(task1.pairs),sum(task2.pairs),sum(task3.pairs),sum(task4.pairs),
                     sum(task9.triplets),sum(task10.triplets),sum(task11.triplets),sum(task12.triplets))


# Computing the number of lines for GPL running example for parallel dimensions plot

gpl.2d.pd <- c(79,92,80,89,136,104,102,106,90,91,104,91,105) # 13 1269
gpl.3d.pd <- c(582,463,472,315,305,299,372,389,346,359,274,291,309,368,260,399,474,368,
               474,358,299,257,307,396,360,242,305) # 27 9643
