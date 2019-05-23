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

ICI3D_RTutorial_1.Rout: ICI3D_RTutorial_1.R
ICI3D_RTutorial_2.Rout: ICI3D_RTutorial_2.R
ICI3D_RTutorial_3.Rout: ICI3D_RTutorial_3.R
ICI3D_RTutorial_4_VisualizingData.Rout: ICI3D_RTutorial_4_VisualizingData.R
ICI3D_RTutorial_5_DataCleaning.Rout: ICI3D_RTutorial_5_DataCleaning.R

ICI3D_Lab1_ODEmodels.Rout: ICI3D_Lab1_ODEmodels.R
ICI3D_Lab2_Heterogeneity.Rout: ICI3D_Lab2_Heterogeneity.R
ICI3D_Lab3_EpiStudyDesign.Rout: ICI3D_Lab3_EpiStudyDesign.R
ICI3D_Lab4_RCT.Rout: ICI3D_Lab4_RCT.R
ICI3D_Lab5_introLikelihood.Rout: ICI3D_Lab5_introLikelihood.R
ICI3D_Lab6_HetSIR_exercise.Rout: ICI3D_Lab6_HetSIR_exercise.R
ICI3D_Lab6_MLE_SIV_HIV.Rout: ICI3D_Lab6_MLE_SIV_HIV.R
ICI3D_Lab7_MCMC-Binomial.Rout: ICI3D_Lab7_MCMC-Binomial.R
ICI3D_Lab8_MCMC-SI_HIV.Rout: ICI3D_Lab8_MCMC-SI_HIV.R
ICI3D_Lab8_MCMC-SI_HIV.Rout: ICI3D_Lab8_MCMC-SI_HIV.Rout
ICI3D_Lab8_MCMC-SI_HIV.Rout: ICI3D_Lab8_MCMC-SI_HIV.Rout.pdf
ICI3D_Lab_SampDistrVariability.Rout: ICI3D_Lab_SampDistrVariability.R

ICI3D_Ex1_StochasticSpillover.Rout: ICI3D_Ex1_StochasticSpillover.R
ICI3D_Example_binomialDistribution.Rout: ICI3D_Example_binomialDistribution.R
ICI3D_Example_chainBinom.Rout: ICI3D_Example_chainBinom.R
ICI3D_spillover_introductions.Rout: ICI3D_spillover_introductions.R

######################################################################

### Makestuff

-include $(ms)/git.mk
-include $(ms)/visual.mk

-include $(ms)/wrapR.mk

