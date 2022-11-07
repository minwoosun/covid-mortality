here::i_am("analysis/code/permutation_plot.R")

library(here)
library(ggplot2)

load(here::here("analysis/data/permutation_result10000.RData"))

rMSE_delta_observed = result[[2]]
null.dist = result[[3]]

df = data.frame(null=null.dist)

p <-
  ggplot(df, aes(x=null)) + 
  geom_histogram(bins=50, colour="gray61", fill="gray61") +
  geom_vline(xintercept = rMSE_delta_observed, linetype="dashed", 
             color = "red", size=0.8) +
  theme_minimal() +
  xlab("RMSE difference") +
  ylab("Count") +
  ggtitle("Permutation Null Distribution for RMSE difference (10,000 iterations)") + 
  #annotate(geom="text", x=-34, y=750, label="p-value: 0.001",
  #         color="blue") +
  annotate(geom="text", x=15, y=750, label="Observed RMSE \n difference: 8.3",
           color="red")
p

png(here::here("figures/permutation10000.png"), width = 800, height = 600)
print(p)
dev.off()

