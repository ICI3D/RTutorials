## This is ICI3D/RTutorials
## https://github.com/ICI3D/RTutorials

-include target.mk

-include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

alldirs += sampling_JD

Sources += $(wildcard *.R *.md)

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

######################################################################

## Building macpan labs 2025 May 20 (Tue)

## mpLabCompartmental.batch.Rout: mpLabCompartmental.R mpLabCompartmental.answers.R

######################################################################

## Processing machinery
## Make an answers file that allows batch.pl to convert the published file into something that runs
## Started with a hard-coded batch, then decided to do a weird, flexible batchdir variable. Don't do that; in cases where you want to batch from elsewhere, make new rules
Sources += batch.md

Ignore += batch
batch:
	$(mkdir)
Sources += batch.pl
.PRECIOUS: batch/%.R
batch/%.R: %.R %.answers.R batch.pl
	$(MAKE) batch
	$(PUSHRO)

## ICI3D_RTutorial_1.batch.Rout: ICI3D_RTutorial_1.R ICI3D_RTutorial_1.answers.R
## ICI3D_RTutorial_2.batch.Rout: ICI3D_RTutorial_2.R ICI3D_RTutorial_2.answers.R
## ICI3D_RTutorial_3.batch.Rout: ICI3D_RTutorial_3.R ICI3D_RTutorial_3.answers.R
%.batch.Rout: batch/%.R
	$(pipeR)

## ICI3D_Lab1_ODEmodels.batch.Rlog: batch/ICI3D_Lab1_ODEmodels.R
## ICI3D_Lab1_ODEmodels.batch.Rout: ICI3D_Lab1_ODEmodels.R ICI3D_Lab1_ODEmodels.answers.R
## ICI3D_Lab3_EpiStudyDesign.batch.Rout: ICI3D_Lab3_EpiStudyDesign.answers.R
## ICI3D_RTutorial_1.batch.Rout: ICI3D_RTutorial_1.answers.R
## ICI3D_RTutorial_2.batch.Rout: ICI3D_RTutorial_2.R ICI3D_RTutorial_2.answers.R

## Two Tutorial 4 files; this one is obsolete, I think
## ICI3D_RTutorial_4_VisualizingData.Rout: ICI3D_RTutorial_4_VisualizingData.R ICI3D_RTutorial_4_VisualizingData.answers.R

## ICI3D_RtvTutorial_4.batch.Rout: ICI3D_RtvTutorial_4.R ICI3D_RtvTutorial_4.answers.R
## batch/ICI3D_RtvTutorial_4.R: ICI3D_RtvTutorial_4.R ICI3D_RtvTutorial_4.answers.R

## Not working, something about where is the data!
ICI3D_RTutorial_5_DataCleaning.batch.Rout: batchdir=data/dataCleaning/
## ICI3D_RTutorial_5_DataCleaning.batch.Rout: ICI3D_RTutorial_5_DataCleaning.R ICI3D_RTutorial_5_DataCleaning.answers.R

######################################################################

## ICI3D_Lab1_ODEmodels.batch.Rout: ICI3D_Lab1_ODEmodels.R ICI3D_Lab1_ODEmodels.answers.R

## ICI3D_Lab2_Heterogeneity.R
## ICI3D_Lab3_EpiStudyDesign.batch.Rout: ICI3D_Lab3_EpiStudyDesign.R ICI3D_Lab3_EpiStudyDesign.answers.R

## ICI3D_Lab4_RCT.batch.Rout: ICI3D_Lab4_RCT.R ICI3D_Lab4_RCT.answers.R
## ICI3D_Lab5a_introLikelihoodPlusRejectionP.R
## ICI3D_Lab5_introLikelihood.R

## ICI3D_Lab6_MLE_SIV_HIV.batch.Rout: ICI3D_Lab6_MLE_SIV_HIV.R ICI3D_Lab6_MLE_SIV_HIV.answers.R

## Not the current Lab 6
## ICI3D_Lab6_HetSIR_exercise.R


## ICI3D_Lab7_MCMC-Binomial.R
## ICI3D_Lab8_MCMC-SI_HIV.R
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
