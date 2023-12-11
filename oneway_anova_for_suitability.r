oneway.test(Habitat_suitability ~ Biogeography, data = all_pop_diversity,var.equal = TRUE)
with(all_pop_diversity,pairwise.t.test(Habitat_suitability, Biogeography, pool.sd=FALSE, p.adjust.method="none"))
oneway.test(Annual_mean_fecundity ~ Biogeography, data = all_pop_diversity,var.equal = TRUE)
with(all_pop_diversity,pairwise.t.test(Annual_mean_fecundity, Biogeography, pool.sd=FALSE, p.adjust.method="none"))
