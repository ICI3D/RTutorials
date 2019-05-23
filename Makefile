# RTutorials
### Hooks for the editor to set the default target

current: target
-include target.mk

##################################################################

## Defs

# stuff

Sources += Makefile 
Ignore += .gitignore

msrepo = https://github.com/dushoff
ms = makestuff
Ignore += local.mk
-include local.mk
-include $(ms)/os.mk

# -include $(ms)/perl.def

Ignore += $(ms)
## Sources += $(ms)
Makefile: $(ms) $(ms)/Makefile
$(ms):
	git clone $(msrepo)/$(ms)

######################################################################

## Content

Sources += $(wildcard *.R)

######################################################################

## Sandbox



## Labs
ICI3D_RTutorial_5_DataCleaning.Rout: ICI3D_RTutorial_5_DataCleaning.R
ICI3D_Lab7_MCMC-Binomial.Rout: ICI3D_Lab7_MCMC-Binomial.R
ICI3D_Lab8_MCMC-SI_HIV.Rout: ICI3D_Lab8_MCMC-SI_HIV.R

######################################################################

### Makestuff

-include $(ms)/git.mk
-include $(ms)/visual.mk

-include $(ms)/wrapR.mk

