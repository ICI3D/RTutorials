# Makefile for Rabies Vaccination Extinction Simulation Pipeline

R = $(strip Rscript $^ $(1) $@)

VACS = 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9
CSVS = $(patsubst %,sim_%.csv,$(VACS))

# Default target builds the report plot and the separate presentation plots
all: plot.png presentation_prob.png presentation_time.png

# Rule to run simulations for a specific vaccination level
sim_%.csv: run_simulation.R model_functions.R
	Rscript $< $* $@

# Rule to merge CSVs into a single RDS
merged_data.rds: merge_data.R $(CSVS)
	Rscript $< . $@

# Rule to plot the combined report output (2 panels, small font)
plot.png: plot_extinction.R merged_data.rds
	$(call R)

# Rule to plot the widescreen presentation probability plot (large font)
presentation_prob.png: plot_presentation_prob.R merged_data.rds
	$(call R)

# Rule to plot the widescreen presentation time to extinction plot (large font)
presentation_time.png: plot_presentation_time.R merged_data.rds
	$(call R)

clean:
	rm -f $(CSVS) merged_data.rds plot.png presentation_prob.png presentation_time.png

.PHONY: all clean
