## project.Makefile
## This is ICI3D/RTutorials

current: target
-include target.mk

-include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

Sources += $(wildcard *.R)

######################################################################

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

## ICI3D_RTutorial_1.batch.Rout: ICI3D_RTutorial_1.R batch.pl
## ICI3D_RTutorial_1.batch.Rout: ICI3D_RTutorial_1.answers.R
## ICI3D_RTutorial_1.batch.rtmp ICI3D_RTutorial_1.batch.Rlog

######################################################################

### Makestuff

## Gutted 2021 Jun 21 (Mon)
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
