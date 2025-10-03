library(targets)
library(sf)
library(vegan)

library(conflicted) # Conflict resolution in packages
conflict_prefer_all("dplyr", quiet = TRUE)

library(tidyverse) # For general functions (dplyr etc)

# === Interesting graphs? ===
#The distribution of alpha diversity across all cells
#plot(density(tar_read(data_grid)$alpha_diversity))
#boxplot(tar_read(data_grid)$alpha_diversity)
# ===

library(betareg)
library(marginaleffects)
library(performance)

tar_load(m_saprotroph)
tar_load(m_lichenized)
tar_load(m_ectomycorrhizal)
tar_load(data_grid)

plot(fitted(m_saprotroph), residuals(m_saprotroph))
abline(h = 0, lty = 2)

plot(check_model(m_saprotroph, residual_type = "normal"))

# --
plot(fitted(m_lichenized), residuals(m_lichenized))
abline(h = 0, lty = 2)

plot(check_model(m_lichenized, residual_type = "normal"))

# --
plot(fitted(m_ectomycorrhizal), residuals(m_ectomycorrhizal))
abline(h = 0, lty = 2)

plot(check_model(m_ectomycorrhizal, residual_type = "normal"))

tar_read(avg_slopes_saprotroph)
tar_read(avg_slopes_lichenized)
tar_read(avg_slopes_ectomycorrhizal)

tar_read(valid_obs_plot)
tar_read(alpha_diversity_plot)
tar_read(guild_distribution_plot)
