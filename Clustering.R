#Grouping pupils as per their performance at two intermediate examinations----
library(tidyverse)
student <- read_csv("clustering-student-mat.csv") #In workspace
library(cluster)
#student
K <- 2:8
list_clu_res <- vector("list", length(K))

list_clu_res <- map(K, ~kmeans(student %>% select(Exam1, Exam2), centers = .))
for(i in seq_along(K)) {
  student_clu <- student %>%
    bind_cols(tibble(cluster = list_clu_res[[i]]$cluster)) %>%
    mutate(cluster = factor(cluster))
  # Filter points that lie on a cluster's convex hull
  student_hull <- student_clu %>%
    split(.$cluster) %>%
    map(~ slice(., chull(.$Exam1, .$Exam2))) %>%
    do.call("rbind", .)
  print(ggplot(student_clu, aes(Exam1, Exam2, color = cluster, fill = cluster)) +
          geom_polygon(data = student_hull, alpha = .5, color = "black") +
          geom_point(pch = 21) +
          geom_point(data = student_clu %>%
                       group_by(cluster) %>%
                       summarize_all(mean), shape = "+", color = "black",
                     size = 8) +
          guides(fill = FALSE, color = FALSE) +
          labs(title = str_c("k=", K[i])))
}

#-------------Silhouette Coefficient------------------------------------
silh <- map_dbl(list_clu_res, ~mean(silhouette(.$cluster, dist(student %>% select(Exam1, Exam2)))))

student_silh <- tibble(K = K,
                       Silh = silh)
student_silh %>%
  ggplot(aes(K, Silh)) +
  geom_line(aes(group = factor(1)))

run_opt <- which.max(student_silh$Silh)


#---------------------HAC-Dendrogram----------------------
dm <- tribble(~p1,~p2,~p3,~p4,~p5,
              0.00, 0.02, 0.90, 0.36, 0.53,
              0.02, 0.00, 0.65, 0.15, 0.24,
              0.90, 0.65, 0.00, 0.59, 0.45,
              0.36, 0.15, 0.59, 0.00, 0.56,
              0.53, 0.24, 0.45, 0.56, 0.00) %>% as.matrix()

rownames(dm) <- letters[1:5]
colnames(dm) <- letters[1:5]
knitr::kable(dm)
plot(hclust(as.dist(dm), "complete"))
plot(hclust(as.dist(dm), "single"))
plot(hclust(as.dist(dm), "average"))