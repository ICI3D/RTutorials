
R = $(strip Rscript $^ $(1) $@)

TAR ?= png
TARS = png pdf jpg

target_%.csv: somescript.R
	$(call R,$*)

plot.%: plot_script.R $(patsubst %,target_%.csv,01 02 03 04)
	$(call R)

figures: $(patsubst %,plot.%,${TARS})

clean:
	rm -f target*.csv

######################################################################

# Pipeline session
## plot_script.R
## somescript.R

# Carl's other session
## participatory2025stochastic.R
