
#The teaching goals are as follows:
#Students should be able to 
# Define  clinical trial
# Explain why a clinical trial permits causal inference
# Explain the role of  randomization and  masking
# Understand that not all biomedical questions require a clinical trial
# Explain the three key ethical principles which govern clinical trial conduct
# Understand how to analyze data from a simple clinical trial

#Lab

#What is a clinical trial?
#Clinical trials are widely recognized as a powerful and important design.  Evidence from
#well-designed clinical trials is almost always considered the strongest form of evidence, 
#so that clinical trials are at the top of a hierarchy of levels of evidence, followed by
#well designed cohort studies, case control studies, and ecological designs (a
#useful discussion is given in [Ho2008]).  

#In general, the goal of clinical studies, whether or observational
#or experimental, is to determine whether or not a treatment,
#intervention, or exposure is related to an outcome.  Such questions may
#take many forms, depending on the nature of the exposure or treatment, and the nature
#of the outcome.  Here is a sample of observational or experimental studies, taken
#almost at random from the literature.
# Is cigarette smoking associated with uveitis? (e.g. [Lin2010]).
# Is prenatal exposure to influenza associated with adult schizophrenia? (e.g. [Ebert2005])
# Are chili peppers associated with increased risk of stomach cancer? (e.g. [LopezCarillo1994])
# Does streptomycin treat tuberculosis? (e.g. [BMRC1948])
# Does hormone replacement therapy help prevent coronary heart disease? (e.g. [Hulley1998])
# Does topiramate help reduce the frequency of migraine? Acupuncture? (e.g. [Silberstein2009,Li2012])
# Does vitamin A supplementation reduce hearing loss in African children? (e.g. [Schmitz2012])
# Does circumcision reduce HIV incidence in men? (e.g. [Gray2007])
# Does intercessory prayer influence recovery from illness? (e.g. [Benson2006], a study funded by
#a religious foundation)
#In some of these, we are interested in a binary (yes/no) outcome, such as the occurrence of stomach cancer.  In others,
#we are interested in an event rate (such as the frequency of migraine), a continuous variable (hearing loss), or a 
#possibly censored event time (time to HIV infection).  
#
#Friedman et al. [Friedman1998] define a clinical trial as a prospective study comparing the effect of
#an intervention against a control in humans.  The key features are
# A clinical trial is prospective, so that the assignment is made and then the patients are followed forward in time.
# A clinical trial has a control group.
# A clinical trial is an experiment.
# Clinical trials are governed by ethical principles, and therefore differ from experimental studies in
#other fields where patient safety is not a factor.
#
#While some of these questions were suitable for a clinical trial, some questions of interest cannot be studied
#experimentally.  In some cases, the exposure of interest is
#known to cause other health problems, and cannot be experimentally administered (cigarette smoking), or the outcome
#is so severe that it would be unthinkable to do an experiment (schizophrenia).  In some cases, a clinical trial may
#simply be impractical because of adherence, while in others, the rationale for a clinical trial is 
#dubious on subject matter grounds (e.g., it is unclear on a priori grounds whether or not prayer is
#an appropriate subject for a clinical trial).
#
#What can clinical trials do that other designs cannot?
#Clinical trials permit causal inference.  By experimentally manipulating the exposure, confounding is (in a 
#probabilistic sense) eliminated.  Moreover, good clinical trial protocols minimize bias by using masked
#patients and investigators.  Masking and randomization are the twin cornerstones of clinical trial design.
#
#Why is masking important?
#Masking, also called  blinding, removes an important source of bias.  For example, consider a trial of
#whether or not acupuncture prevents migraine.  Patients or investigators may have a prior commitment to acupuncture,
#which is a low-tech traditional intervention.  Belief in the efficacy of the intervention may lead people to 
#inadvertently understate the frequency of migraine if the patients know they received acupuncture.  This bias may
#lead people to believe in the efficacy of the intervention.
#
#When both the
#subjects and investigators are masked, the trial is {\it double masked} (also called
# double blind).  
#
#In certain trials, masking is impossible.  For example, in a trial of a surgical procedure, it
#is impossible to mask the surgeon, and frequently impossible to mask the patients (it would be impossible
#for a patient randomized to radical prostatectomy vs external beam radiation to be masked).  Another
#example arises in dietary interventions: it would be impossible to be masked to whether or not one had
#been randomized to a low carbohydrate vs a low fat arm (for example).
#In such circumstances, it may still be possible to use biomedical measurements made by masked personnel, so that
#the inference could be based on objective measurements made by masked personnel.
#
#Unmasked trials are viewed as less credible than masked trials, because of bias.  Therefore, both subjects 
#and investigators should be masked whenever possible.  When evaluating a published clinical trial, be aware of
#subtle ways in which masking could have been compromised.  For instance, patients may be aware of side effects of
#the medication, or the medications may be different in some subtle way detectable to trained physicians.
#
#Why is randomization important?
#Randomization insures that unknown factors will be balanced in the study arms, in a probabilistic sense.  The 
#development of randomized experimental designs by R. A. Fisher early in the last century arguably inaugurated the modern
#era of biomedical research.
#
#Armitage [Armitage1982] gives three important reasons for randomization.  First, randomization achieves broad similarity
#between the treatment and control groups, with respect to known and unknown prognostic factors; it is unlikely that a
#serious imbalance will occur by chance alone.  
#Second, statistical theory (which RA Fisher played a prominent role
#in developing) may be used to compute the chance that a difference between the treatment and control groups was due to 
#chance alone.  Third, randomization supports masking; using any nonrandom method for allocation makes it harder to maintain masking.
#Yoshioka [Yoshioka1998] gives the following example from the MRC Streptomycin trial: ``using allotment by alternation, the
#clinicians would be certain that the next patient to be admitted would, say, receive the treatment, and their decision
#whether to accept this patient might consequently be affected.''  Yoshioka goes on to give a fourth reason in support of
#randomization in a clinical trial: fairness in allocation.
#
#%In Exercise 1, we will simulate a clinical trial.  Suppose that recovery from tuberculosis is governed by an unknown
#%individual factor, something we cannot measure.  Assume that the probability of recovery is 18/107, taken from 
#%both arms of the MRC tuberculosis trial of 1948.  Simulate a clinical trial in which 55 patients (110 in total) are assigned 
#%to a treatment or a control group, supposing that the intervention does not work.  
#
#Why is a control group important?
#Without a control group, we do not know what would have happened without the treatment, and therefore cannot estimate
#the effect of the treatment.  The control group may be a masked placebo, or in some cases may be an existing standard of
#care to which a new intervention is being compared.
#
#When is a clinical trial needed?
#Although a clinical trial provides the highest form of evidence, the expense of a clinical 
#trial implies that a clinical trial should only done when the question is of sufficient importance
#to warrant a trial.  
#
#Equipoise is indicated prior to beginning a clinical trial.  There must be genuine disagreement as to
#the value of the therapy.  For instance, topical application of the antifungal {\it natamycin} is commonly
#used for fungal infections of the cornea, although no controlled clinical trial ever established this.  The reason
#is that it seems better on {\it a priori} grounds to use an antifungal agent to treat a fungal infection
#than to offer no treatment to the patient.
#
#What are some common clinical trial designs?
#What is a factorial design?
#A factorial design assigns study units to all combinations of experimental factors.  In the Mycotic Ulcer 
#Therapeutic Exploratory Trial, the investigators were interested in drugs and in corneal scraping.  Two drugs were
#of interest (natamycin and voriconazole, making two levels of this factor), and two scraping protocols (either
#actually scrape, or don't scrape, making two levels of this factor).  A $2 \times 2$ factorial design was conducted.
#
#What is a noninferiority trial?
#In some cases, we are interested in a new drug for which there is already an accepted drug or treatment.  It is
#unethical to withhold treatment, so that ethical considerations preclude the presence of a placebo control.  In this case, we
#wish to test the hypothesis that the new drug is not worse than the old drug.
#
#What is a group-randomized trial?
#In some cases, the natural intervention unit is not the individual, but the group or community.
#
#How can one analyze data from a simple trial?
#Statistical analysis of clinical trial data depends on the nature of the outcome variable and the
#trial design.  In the examples below, we will illustrate analysis using data from the 
#Mycotic Ulcer Therapeutic Exploratory Trial, a $2 \times 2$ factorial trial in which
#the drug  azithromycin was compared to natamycin, and corneal scraping was compared to
#no corneal scraping.  The trial was conducted by investigators from UCSF and Aravind Eye Hospital, 
#India (Lietman, Acharya, Srinivasan) in 2008.

#Warmup Exercises
#Normal random variables
#How can we simulate data from a standard normal distribution with a given mean and standard deviation?
#To simulate a given number of random variates, use the {\tt rnorm} command.  Here, we simulate 100 
#standard normal variates with a mean of 10 and a standard deviation of 3:


v1 <- rnorm(100,mean=10,sd=3)
hist(v1)



#Simulate 30 variates with a mean of 1 and a standard deviation of 0.3.  Repeat this exercise 10 times, 
#plotting the histogram each time.

#Random selection
#How can we simulate random selection of people into a treatment and a control group?  To do so,
#we use sampling without replacement based on the sample command.  Here, we divide 20 subjects
#into a treatment group and a control group.  We select without replacement half of the numbers from
#1 to 20 (representing the people) into a treatment and a control group.


indices <- 1:20
treatment.group <- sample(indices,size=10,replace=FALSE)
treatment.group


#Repeat this exercise a few times.

#Another way to do it is to simply shuffle a vector of 0s and 1s.  


assignments <- sample( c(rep(1,10),rep(0,10)), size=20, replace=FALSE )
assignments


#The assignment of the first person is found by looking at  assignments[1], the second person
#by looking at  assignments[2], and so on.  Assume that 1 corresponds to the treatment group.
#We can print the number of the treatment subjects like this:


which(assignments==1)


#
#Working with data frames
#We will now generate a simulated data set.   Here, we assume the treatment and control groups 
#do not differ statistically---the mean and standard deviation of the outcome variable are assumed
#to be the same.


n.subjects <- 20
dset <- data.frame(study.id=1:n.subjects)
dset$treatment <- sample( c(rep(1,n.subjects/2),rep(0,n.subjects/2)), size=n.subjects, replace=FALSE )
dset$outcome <- rnorm(n.subjects, mean=1,sd=0.3)
dset


#

rm(dset,n.subjects,assignments)


#Understanding Type I Error
#In this section, we will simulate a clinical trial in which the treatment does not work.  To make
#this easy, we are going to first write a function that generates the data set.  


gen.data <- function(n.subjects, control.mean, treatment.effect, sd) {
  dset <- data.frame(study.id = 1:n.subjects)
  dset$treatment <- sample( c(rep(1,n.subjects/2),rep(0,n.subjects/2)), size=n.subjects, replace=FALSE )
  mean.vector <- ifelse(dset$treatment==1, control.mean+treatment.effect, control.mean)
  dset$outcome <- rnorm(n.subjects, mean=mean.vector, sd=sd)
  dset
}


#
#Exercise: use this function to generate a data set of size 20, with a treatment effect of 0, and a 
#mean of 1.  Assume the standard deviation is 0.3.


v1 <- gen.data(20, 1, 0, 0.3)
v1



#Exercise: conduct a T-test of the difference between the treatment and control groups.


t.test(outcome ~ treatment, data=v1)


#What do you find?  Try it a few times---generate new data, and repeat the test.  Keep doing this until
#you find $P<0.05$.  How long did it take?  When $P<0.05$, we reject the null hypothesis at a
#significance level of 0.05.
#
#Exercise: repeat this 10,000 times.  Let's build a function to do this:


repeat.test <- function(nsims, n.subjects, control.mean, treatment.effect, sd) {
  ans <- rep(NA, nsims)
  for (ii in 1:nsims) {
    v1 <- gen.data(n.subjects, control.mean, treatment.effect, sd)
    ans[ii] <- t.test(outcome ~ treatment, data=v1)$p.value
  }
  ans
}
a1 <- repeat.test(10000,20,1,0,0.3)


#Let's find out how often we reject:


mean(a1 < 0.05)


#(Why does this command work?)

#Try this experiment again.

#Now try it with a sample size of--not 20---but 100.  Repeat for 200 and 500.  How does the rejection
#probability change with the sample size?

#\subsection*{Power}
#Now, suppose the treatment does work.  Suppose that the difference between the treatment group and
#the control group is 0.15, which turns out to be one and one half lines of vision on a traditional
#chart.  
#

a2 <- repeat.test(10000, 20, 1, -0.15, 0.3)
mean(a2 < 0.05)

#
#How often do you reject for a sample size of 20?  Try it for 50, 100, 200, and 500.  Can you find the
#sample size so that you reject 80\% of the time (approximately)?  The probability of rejecting the
#null hypothesis when you should reject it is called the power, and it does depend on the sample size.
#
#Now, suppose the treatment effect is very small, only 0.025:

a3 <- repeat.test(10000, 100, 1, -0.025, 0.3)
mean(a3 < 0.05)

#
#For a sample size of 100, how often do you reject?  For a given sample size, how does the chance of
#rejecting the null hypothesis depend on the effect?  As you lower the treatment effect, the power goes
#up or down?
#
#Inference
#An important advantage of the clinical trial is that it (probabilistically) controls for confounding.  
#Randomization allocates potential confounders to the two arms with equal probability.  Let's do an
#example.  Suppose that 10\% of infected individuals have an infection by an organism that is harder to
#cure and leads to worse outcomes.  However, suppose that we don't know this at the time of enrollment
#or treatment.  Let's simulate the trial.  First, let's write a new data generation function that will
#add the simulated covariate.  Here we assume that patients with the severe organism have acuity that
#is {\tt severe.effect} worse (and default of 0.4).

gen.data.ii <- function(n.subjects, control.mean, treatment.effect, sd, severe.prob, severe.effect=0.4) {
  dset <- data.frame(study.id = 1:n.subjects)
  dset$treatment <- sample( c(rep(1,n.subjects/2),rep(0,n.subjects/2)), size=n.subjects, replace=FALSE )
  dset$severe <- rbinom(n.subjects, size=1, prob=severe.prob)
  mean.vector <- ifelse(dset$treatment==1, control.mean+treatment.effect, control.mean)
  dset$outcome <- rnorm(n.subjects, mean=mean.vector, sd=sd) + severe.effect * dset$severe
  dset
}
simdata <- gen.data.ii(20, 1.0, 0.0, 0.3, 0.1, 0.4)
simdata

#Let's simulate a modest clinical trial (say 50 individuals in each arm, 100 total) and see how well the severe cases are
#balanced:

simdata <- gen.data.ii(100, 1.0, 0.0, 0.3, 0.1, 0.4)
table(simdata$treatment, simdata$severe)

#What does this mean?  Repeat this several times.  
#
#Let's repeat the simulated trial several times under these circumstances.

repeat.test.ii <- function(nsims, n.subjects, control.mean, treatment.effect, sd, prob.severe, severe.effect) {
  ans <- rep(NA, nsims)
  for (ii in 1:nsims) {
    v1 <- gen.data.ii(n.subjects, control.mean, treatment.effect, sd, prob.severe, severe.effect)
    ans[ii] <- t.test(outcome ~ treatment, data=v1)$p.value
  }
  ans
}

#Use this function to determine how the clinical trial works even though there is an unknown covariate that
#has a substantial effect on the outcome.

#Optional exercise: modify the original {\tt repeat.test} so that it takes a data-generating function as an
#argument, instead of rewriting the function as we just did.  What is a graceful way to handle additional
#arguments required by the data generating function?  (Suggestion: ``rest'' argument {\tt ...} might work.)

#Exploring some clinical trial data
#We will examine data from the Mycotic Ulcer Therapeutic Exploratory Trial.  These data can only be used for this lab.  To
#protect confidentiality, I added a tiny random value to some of the scar values from the actual trial.

load("MuTxT.Rdata")

#The variables are:
#"patid"           -- patient ID
#"drug"            -- drug assignment: 0 is natamycin, 1 is voriconazole 
#"scrape"          -- scraping: 0 is no, 1 is yes
#"age"             -- age
#"sex"             -- sex
#"if_perf"         -- 1 if a perforation happened
#"scar_baseline"   -- scar size at baseline
#"scar_3W"         -- scar size at 3 weeks

#Try to estimate the treatment effect on scar size at 3 weeks.  Consider controlling for baseline.  Is it necessary to
#control for a covariate?  Why might one do so?
#Example:
summary(lm(scar_3W ~ drug + scrape, data=mutxt))

#\item[Armitage1982] Armitage P.  The role of randomization in clinical trials.  {\it Statistics in Medicine} 1(4):345--352, 1982.
#\item[Benson2006] Benson H, Dusek JA, Sherwood JB, Lam P, Bethea CF, Carpenter W, Levitsky S, Hill PC, Clem DW Jr, Jain MK, Drumel D, Kopecky SL, Mueller PS, Marek D, Rollins S, Hibberd PL.  Study of the Therapeutic Effects of Intercessory Prayer (STEP) in cardiac bypass patients: a multicenter randomized trial of uncertainty and certainty of receiving intercessory prayer.  American Heart Journal 151(4):934--42, 2006.
#\item[BMRC1948] British Medical Research Council. Streptomycin treatment of pulmonary tuberculosis: a Medical Research Council investigation. British Medical Journal 2:769--783, 1948.
#\item[Friedman1998] Friedman LM, Furberg CD, DeMets DL. Fundamentals of Clincal Trials. New York: Springer-Verlag, 1998.
#\item[Gray2007] Gray RH, Kigozi G, Serwadda D, Makumbi F, Watya S, Nalugoda F, Kiwanuka N, Moulton LH, Chaudhary MA, Chen MZ, Sewankambo NK, Wabwire-Mangen F, Bacon MC, Williams CF, Opendi P, Reynolds SJ, Laeyendecker O, Quinn TC, Wawer MJ.  Male circumcision for HIV prevention in men in Rakai, Uganda: a randomised trial.  {\it Lancet} 369(9562):657--66, 2007.
#\item[Ho2008] Ho PM, Peterson PN, Masoudi FA. Evaluating the evidence. Is there a rigid hierarchy? {\it Circulation} 118:1675--1684, 2008.
#\item[Hulley1998] Hulley S, Grady D, Bush T, Furberg C, Herrington D, Riggs B, Vittinghoff E. Randomized trial of estrogen plus progestin for secondary prevention of coronary heart disease in postmenopausal women. JAMA 280(7):605--613, 1998.
#\item[Li2012] Li Y, Zheng H, Witt CM, Roll S, Yu SG, Yan J, Sun GJ, Zhao L, Huang WJ, Chang XR, Zhang HX, Wang DJ, Lan L, Zou R, Liang FR.
#Acupuncture for migraine prophylaxis: a randomized controlled trial.  CMAJ. 184(4):401--410, 2012.
#\item[Lin2010] Lin P, Loh AR, Margolis TP, Acharya NR.  Cigarette smoking as a risk factor for uveitis.  {\it Ophthalmology} 117(3):585--590, 2010.
#\item[LopezCarillo1994] L{\'{o}}pez-Carillo L, Hern{\'{a}}ndez AM, Dubrow R. Chili pepper consumption and gastric cancer in Mexico: a case-control study.  {\it American Journal of Epidemiology} 139(3):263--271, 1994.
#\item[Schmitz2012] Schmitz J, West KP Jr, Khatry SK, Wu L, Leclerq SC, Karna SL, Katz J, Sommer A, Pillion J.
#Vitamin A supplementation in preschool children and risk of hearing loss as adolescents and young adults in rural Nepal: randomised trial cohort follow-up study.  {\it BMJ} 344:d7962. {\tt doi: 10.1136/bmj.d7962} 2012.
#\item[Silberstein2009] Silberstein S, Lipton R, Dodick D, Freitag F, Mathew N, Brandes J, Bigal M, Ascher S, Morein J, Wright P, Greenberg S, Hulihan J.  Topiramate treatment of chronic migraine: a randomized, placebo-controlled trial of quality of life and other efficacy measures.  {\it Headache} 49(8):1153--1162, 2009.
#\item[Yoshioka1998] Yoshioka A. Use of randomisation in the Medical Research Council's clinical trial of streptomycin in pulmonary tuberculosis in the 1940s. BMJ 317:1220--1223, 1998.

