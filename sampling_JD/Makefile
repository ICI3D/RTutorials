# RTutorials
### Hooks for the editor to set the default target

current: target
-include target.mk

##################################################################

## Defs

# stuff
Sources += Makefile .ignore 
Sources += .gitignore
msrepo = https://github.com/dushoff
ms = makestuff
-include $(ms)/os.mk

# -include $(ms)/perl.def

Ignore += $(ms)
Makefile: $(ms) $(ms)/Makefile
$(ms):
	git clone $(msrepo)/$(ms)

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

-include $(ms)/git.mk
-include $(ms)/visual.mk

-include $(ms)/wrapR.mk

