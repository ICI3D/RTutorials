## This is ICI3D/RTutorials
## https://github.com/ICI3D/RTutorials

-include target.mk

-include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

Sources += $(wildcard *.R)

participatoryDesign2021.Rout: participatoryDesign2021.R
	$(pipeR)

######################################################################

Ignore += local.mk

## Your data location

datadir = ../datasets ## Override below
-include local.mk

Ignore += data
data: dir=$(datadir)
data:
	$(linkdirname)

## Processing machinery

Ignore += batch
batch:
	$(mkdir)
Sources += batch.pl
.PRECIOUS: batch/%.R
batch/%.R: %.R %.answers.R batch.pl
	$(MAKE) batch
	$(PUSHRO)

## batch/ICI3D_RTutorial_1.R: batch.pl
%.batch.Rout: batch/%.R
	$(pipeR)

Sources += batch.md

## ICI3D_RTutorial_1.batch.Rout: ICI3D_RTutorial_1.R batch.pl
## ICI3D_RTutorial_1.batch.Rout: ICI3D_RTutorial_1.answers.R

## ICI3D_RTutorial_2.batch.Rout: ICI3D_RTutorial_2.R ICI3D_RTutorial_2.answers.R

## ICI3D_RTutorial_4_VisualizingData.Rout: ICI3D_RTutorial_4_VisualizingData.R ICI3D_RTutorial_4_VisualizingData.answers.R

## batch/ICI3D_RtvTutorial_4.R: ICI3D_RtvTutorial_4.R ICI3D_RtvTutorial_4.answers.R
## ICI3D_RtvTutorial_4.batch.Rout: ICI3D_RtvTutorial_4.R ICI3D_RtvTutorial_4.answers.R
ICI3D_RtvTutorial_4.batch.Rout: batch/ICI3D_RtvTutorial_4.R
	$(pipeR)
	- mv data/visualizingData/Rplots.pdf .

######################################################################



######################################################################

## New sandbox (see also content.mk)

## Follow-up

hetero_play.Rout: HetSIR_functions.Rdata hetero_play.R 
	$(pipeR)

HetSIR.Rout: HetSIR_functions.Rdata HetSIR.R

### Makestuff

## Makefile gutted 2021 Jun 21 (Mon)
Sources += Makefile content.mk

Ignore += makestuff
msrepo = https://github.com/dushoff

Makefile: makestuff/Makefile
makestuff/Makefile:
	git clone $(msrepo)/makestuff
	ls makestuff/Makefile

-include makestuff/os.mk

-include makestuff/pipeR.mk

-include makestuff/git.mk
-include makestuff/visual.mk
