## This is ICI3D/RTutorials
## https://github.com/ICI3D/RTutorials

-include target.mk

-include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

Ignore += pipelines

alldirs += sampling_JD

Sources += $(wildcard *.R *.md)

particCoding_Rabies2017.Rout: particCoding_Rabies2017.R
	$(pipeR)

participatoryDesign2021.Rout: participatoryDesign2021.R
	$(pipeR)

participatoryDynamics2025.Rout: participatoryDynamics2025.R
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

######################################################################

## Branching

Ignore += branchdir

branchdir:
	git clone https://github.com/ICI3D/RTutorials $@

######################################################################

## Building macpan labs 2025 May 20 (Tue)

## mpLabCompartmental.batch.Rout: mpLabCompartmental.R testing/mpLabCompartmental.answers.R

## mpLabFitting.batch.Rout: mpLabFitting.R testing/mpLabFitting.answers.R
## git log -- mpLabFitting.R 

######################################################################

Sources += $(wildcard *.pl)

## Several answers files need to be renamed after big renaming 2026 Apr 21 (Tue)

## Processing machinery
## Make an answers file that allows batch.pl to convert the published file into something that runs
## Started with a hard-coded batch, then decided to do a weird, flexible batchdir variable. Don't do that; in cases where you want to batch from elsewhere, make new rules
Sources += $(wildcard testing/*.R)
Sources += batch.md

Ignore += batch
batch:
	$(mkdir)
.PRECIOUS: batch/%.R
batch/%.R: %.R testing/%.answers.R batch.pl
	$(MAKE) batch
	$(PUSHRO)

%.batch.Rout: batch/%.R
	$(pipeR)

## ICI3D_RTutorial_1.batch.Rout: ICI3D_RTutorial_1.R testing/ICI3D_RTutorial_1.answers.R
## ICI3D_RTutorial_2.batch.Rout: ICI3D_RTutorial_2.R testing/ICI3D_RTutorial_2.answers.R
## ICI3D_RTutorial_3.batch.Rout: ICI3D_RTutorial_3.R testing/ICI3D_RTutorial_3.answers.R

## ICI3D_Lab_ODEmodels.batch.Rout: ICI3D_Lab_ODEmodels.R testing/ICI3D_Lab_ODEmodels.answers.R
## ICI3D_Lab_ODEmodels.batch.Rlog: batch/ICI3D_Lab_ODEmodels.R

## ICI3D_Lab_EpiStudyDesign.batch.Rout: ICI3D_Lab_EpiStudyDesign.R testing/ICI3D_Lab_EpiStudyDesign.answers.R

## ICI3D_RTutorial_1.batch.Rout: testing/ICI3D_RTutorial_1.answers.R
## ICI3D_RTutorial_2.batch.Rout: ICI3D_RTutorial_2.R testing/ICI3D_RTutorial_2.answers.R

## Two Tutorial 4 files; this one is obsolete, I think
## ICI3D_previous_VisualizingData.Rout: ICI3D_previous_VisualizingData.R testing/ICI3D_previous_VisualizingData.answers.R

## Warning! This uses setwd, and thus default plots are made elsewhere
## ICI3D_RTutorial_4.batch.pdf: ICI3D_RTutorial_4.R testing/ICI3D_RTutorial_4.answers.R
Ignore += ICI3D_RTutorial_4.batch.pdf
ICI3D_RTutorial_4.batch.pdf: ICI3D_RTutorial_4.batch.Rout
	$(MV) data/visualizingData/Rplots.pdf $@
## ICI3D_RTutorial_4.batch.Rout: ICI3D_RTutorial_4.R testing/ICI3D_RTutorial_4.answers.R
## batch/ICI3D_RTutorial_4.R: ICI3D_RTutorial_4.R testing/ICI3D_RTutorial_4.answers.R

## Not working, something about where is the data!
ICI3D_RTutorial_5_DataCleaning.batch.Rout: batchdir=data/dataCleaning/
## ICI3D_RTutorial_5_DataCleaning.batch.Rout: ICI3D_RTutorial_5_DataCleaning.R testing/ICI3D_RTutorial_5_DataCleaning.answers.R

######################################################################

pipeRdesc += ICI3D_Lab_Heterogeneous_Groups.batch
## ICI3D_Lab_Heterogeneous_Groups.batch.noSpread.pdf:
## ICI3D_Lab_Heterogeneous_Groups.batch.noSpread-0.pdf:

ICI3D_Lab_Heterogeneous_Groups.batch.Rout.pdf: ICI3D_Lab_Heterogeneous_Groups.batch.Rout
	$(MV) Rplots.pdf $@
## ICI3D_Lab_Heterogeneous_Groups.batch.Rout: ICI3D_Lab_Heterogeneous_Groups.R testing/ICI3D_Lab_Heterogeneous_Groups.answers.R 
ICI3D_Lab_Heterogeneous_Groups.batch.Rout: ICI3D_Heterogeneous_Groups.R ICI3D_Lab_Heterogeneous_Groups.R
## ICI3D_Lab_Heterogeneous_Groups.batch.Rout-0.pdf: 

Ignore += labNow.*.pdf
## labNow.homoLess.pdf: labNow.R
pdfDesc += labnow
labNow.Rout: labNow.R ICI3D_Heterogeneous_Groups.R
	$(pipeR)

######################################################################

## ICI3D_Example_StochasticSpillover.batch.Rout: ICI3D_Example_StochasticSpillover.R testing/ICI3D_Example_StochasticSpillover.answers.R
ICI3D_Example_StochasticSpillover.batch.Rout: ICI3D_Example_StochasticSpillover_functions.R

## ICI3D_Lab_ODEmodels.batch.Rout: ICI3D_Lab_ODEmodels.R testing/ICI3D_Lab_ODEmodels.answers.R

## ICI3D_Lab_Heterogeneity.R
## ICI3D_Lab_EpiStudyDesign.batch.Rout: ICI3D_Lab_EpiStudyDesign.R testing/ICI3D_Lab_EpiStudyDesign.answers.R

## ICI3D_Lab_RCT.batch.Rout: ICI3D_Lab_RCT.R testing/ICI3D_Lab_RCT.answers.R
## ICI3D_Lab_LikelihoodPlusRejectionP.R
## ICI3D_Lab_introLikelihood.batch.Rout: ICI3D_Lab_introLikelihood.R testing/ICI3D_Lab_introLikelihood.answers.R

## ICI3D_Lab_LikelihoodCompare.batch.Rout: ICI3D_Lab_LikelihoodCompare.R testing/ICI3D_Lab_LikelihoodCompare.answers.R

## ICI3D_Lab_MLE_SIV_HIV.batch.Rout: ICI3D_Lab_MLE_SIV_HIV.R testing/ICI3D_Lab_MLE_SIV_HIV.answers.R

## ICI3D_Lab_MCMC-Binomial.R
## ICI3D_Lab_MCMC-SI_HIV.R
## ICI3D_Lab_SampDistrVariability.R

######################################################################

## New sandbox (see also content.mk)

## Follow-up

hetero_play.Rout: HetSIR_functions.Rdata hetero_play.R 
	$(pipeR)

HetSIR.Rout: HetSIR_functions.Rdata HetSIR.R

## Dev for MMED 2025

seir.w.seasonal.Rout: seir.w.seasonal.R
	$(pipeR)

######################################################################

## Look at labnames branch for a sketched-out implementation of the renaming
## Edit in place
Sources += names.txt

######################################################################

## Migration TEMPORARY 2026 Apr 22 (Wed). Could delete soon (or keep migrate txt for reference)

Ignore += index.out
index.out: $(wildcard *.R) Makefile
	ls $^ > $@

Sources += migrate.txt

Ignore += mv.bash
mv.bash: migrate.txt mv.pl
	$(PUSHRO)

## This was run in the testing/ subdirectory
amv.bash: migrate.txt amv.pl
	$(PUSHRO)

## perl -pi links.perl Makefile
## perl -pi links.perl testing/
Ignore += links.perl
links.perl: migrate.txt links.pl
	$(PUSHRO)

######################################################################

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
