
data:
	mkdir -p $@

# make a population of populations
data/population.rds: population.R | data
	Rscript $^ $@

# simulate a population of studies
data/study_%.rds: study.R data/population.rds | data
	Rscript $^ $(subst _, ,$*) $@

studies: | data
	for i in 0.3 0.5 0.7; do for j in 0.6 0.75 0.9; do make data/study_$$i\_$$j.rds; done; done

# analyze a population of studies
data/analysis_%.rds: analyze.R data/study_%.rds | data
	Rscript $^ $@

analyses: | data
	for i in 0.3 0.4 0.5 0.6 0.7; do for j in 0.6 0.75 0.9; do make data/analysis_$$i\_$$j.rds; done; done

plot.png: plot.R $(wildcard data/analysis_*.rds)
	Rscript $^ $@