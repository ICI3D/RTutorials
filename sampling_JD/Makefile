# RTutorials
### Hooks for the editor to set the default target

## TODO: update rules annotate scripts

current: target
-include target.mk

##################################################################

## Defs

Sources += Makefile .gitignore

######################################################################

### Content

###### Influenza example (shown briefly, not participatory)

Sources += $(wildcard *.R)

## Not done annotating

fluStudy.Rout: fluStudy.R

fluTest.Rout: fluTest.R

fluExperiment.Rout: fluExperiment.R

fluExperiment_again.Rout: fluExperiment.Rout fluExperiment_again.R

###### Cotrim participatory example (not completed in class)

cotrimStudy.Rout: cotrimStudy.R

cotrimTest.Rout: cotrimTest.R

######################################################################

### Makestuff

Sources += Makefile

Ignore += makestuff
msrepo = https://github.com/dushoff

Makefile: makestuff/00.stamp
makestuff/%.stamp:
	- $(RM) makestuff/*.stamp
	(cd makestuff && $(MAKE) pull) || git clone -depth 1 $(msrepo)/makestuff
	touch $@

-include makestuff/os.mk

## -include makestuff/pipeR.mk

-include makestuff/git.mk
-include makestuff/visual.mk
