library(shiny)

shinyUI(fluidPage(
  titlePanel('Dynamical Fever: computer exercise'),
  sidebarLayout(
    sidebarPanel(
      p('Juliet R.C. Pulliam, 2012-2016'),
      p('Some Rights Reserved'),
      p(a('CC BY-NC 3.0', 
          href = 'http://creativecommons.org/licenses/by-nc/3.0/')),
      #      code('install.packages('shiny')'),
      br(),
      br(),
      br(),
      br(),
      #       img(src = 'bigorb.png', height = 72, width = 72),
      #       'shiny is a product of ', 
      #       span('RStudio', style = 'color:blue')
      width=3),
    mainPanel(
      p(em(span('This exercise uses a simulation model to explore the dynamics and control of an
        imaginary disease in two populations. The goals of this tutorial are to (1) help
        you develop an intuition for how transmission patterns influence the outcome of
        interventions, (2) give you practice in making arguments based on indirect
        evidence, and (3) introduce the concept of a “model world.” Some of the model output
        has been generated previously, and the code for this model and its output is
        available upon request. You may want to look at this code after you have completed
        the exercise, but for now you should focus on the model output.',style='size=20px'))),
      h1('Part 1: Epidemic dynamics'),
      p('A disease called Dynamical Fever (DF) causes near-annual outbreaks in dogs and people in
Daidd County. Typical outbreaks produce similar patterns of cases in the two populations, and the outbreaks always occur simultaneously. DF became a reportable disease in Daidd County in 2008, and the Department of Health has enlisted your help to understand the data they have collected since that time.'),
      p('In 2007 and 2008 the outbreaks looked pretty similar. First, look at how the epidemics
        in dogs and people progressed in 2008:'),
      # IMAGE
      h3('2008'),
      span(img(src = 'df2008.png'),align = 'center'),
      br(),
      p('The x-axis is time (week of the year), and the y-axis is the number of new cases observed.
Overall, there were 759 canine cases and 786 human cases over a period of 16 weeks. In 2009, there were 789 canine cases and 804 human cases, and the outbreak lasted 14 weeks. You can see that the overall pattern of these outbreaks was similar by comparing the epidemic curves:'),
      # IMAGE
      h3('2009'),
      span(img(src = 'df2009.png'),align = 'center'),
      br(),
      p('In 2010, there was only one case in dogs and one in humans, but in 2011, another large
outbreak occurred, with a total of 796 canine cases and 821 human cases over a period of 20 weeks.'),
      # IMAGE
      h3('2010'),
      span(img(src = 'df2010.png'),align = 'center'),
      h3('2011'),
      span(img(src = 'df2011.png'),align = 'center'),
      br(),
      br(),
      p(strong('Based on the data from 2008-2011, what have you learned about DF?  What factors might determine the differences in epidemic size and duration from year to year? Is it possible to draw any conclusions about the natural history of the disease? Why might the epidemic die out each spring?')),
      br(),
      h1('Part 2: Introduction of a veterinary vaccine'),
      p('A veterinary vaccine against DF had been approved for use in dogs in mid-2010, but since there was not a problem that year, the Health Department did not promote it. Following the resurgence of DF in 2011, however, information on the vaccine was sent to veterinarians and pet owners in an effort to encourage vaccine uptake. By the beginning of 2012, 40 percent of the dog population in Daidd County had been vaccinated.'),
      p('In 2012, the number of DF cases was substantially lower than in any year other than 2010, with only 34 cases in dogs and 64 human cases, occurring over a period of 13 weeks:'),
      # IMAGE
      h3('2012'),
      img(src = 'df2012.png'),
      br(),
      p('Based on this information, opinion among Health Department employees was divided on the success of the vaccine. Some employees argued that the vaccine had effectively reduced the number of DF cases, and that the reduction in canine cases due to the vaccine might have indirectly protected people from contracting the disease as well. Other employees pointed out that there were more cases in 2012 than in 2010 (before the vaccine had been approved), and argued that vaccination of only 40 percent of dogs could not explain a 20-fold reduction in the number of canine cases (from an average of ~780 cases in previous years with outbreaks). This group concluded that the relatively small number of cases in 2012 could be unrelated to adoption of the vaccine.'),
      p('Not liking to rock the boat, the Health Department\'s Director ultimately decided to continue the vaccine information campaign for another year and then reassess the situation. Veterinarians and pet owners were encouraged to vaccinate dogs and reminded that the vaccine needs to be renewed annually. The community\'s response to the vaccine was generally positive, with anecdotal evidence suggesting no vaccinated dogs had gotten sick in 2012, and by the beginning of 2013, 50 percent of the dog population in Daidd County had been vaccinated.'),
      p('As it turned out, the 2013 data did not clarify things for the Director, and the debate within the department only grew more heated, with each side claiming that the 2013 data supported their argument:'),
      # IMAGE
      h3('2013'),
      img(src = 'df2013.png'),
      br(),
      p('In the first 17 weeks of 2013, 109 dogs and 213 people contracted DF.'),
      p(strong('What arguments might the groups on each side of the debate make about these data? What additional information (other than more years of data) would be useful to help determine to what extent the vaccine is responsible for the differences in the outbreaks observed before and after its introduction?')),
      br(),
      h1('Part 3: Introduction of a human vaccine'),
      p('Luckily for the Director of the Health Department, the approval of a human vaccine against DF in mid-2013 meant that he did not have to take a strong stance on the debate over dog vaccination. Instead, he simply redirected funds from the earlier information campaign on dog vaccination to promote human vaccination. By the beginning of 2014, 50 percent of the people in Daidd County had been vaccinated, though dog vaccine uptake fell to 20 percent.'),
      p('Although the 2014 season got off to a slow start, the Director was in for a surprise when the outbreak picked up later in the season. In the end, the 2014 outbreak was the worst one for years, with 480 canine cases and 315 human cases over a period of 29 weeks:'),
      # IMAGE
      h3('2014'),
      img(src = 'df2014.png'),
      br(),
      br(),
      p('By mid-June, the Director had had enough and decided step down in order to spend more time with his family and their dogs. His former Deputy was quickly appointed to be the Health Department\'s Acting Director, and she worked doggedly over the next few months to ensure that as many people in the community were vaccinated in time for the 2015 season as possible. By the beginning of the year, 80 percent of the people in Daidd County had been vaccinated. In the meantime, a contamination scare temporarily disrupted the availablility of the dog vaccine. The Acting Director had never been convinced that the dog vaccine had caused the reduction in dog or human DF cases following its introduction, believing instead that a new, less transmissible strain had probably been introduced around the same time as the canine vaccine. She was therefore surprised to see so many canine cases in Daidd County in 2015:'),
      # IMAGE
      h3('2015'),
      img(src = 'df2015.png'),
      br(),
      br(),
      p('In fact, there were more cases of DF in dogs in 2015 than in any year on record, with 812 canine cases, though there were only 149 human cases. As a dog lover, the AD is distraught, and it is at this point that she decides she needs outside expertise and brings you in as a consultant.'),
      p(strong('Having reviewed all of the data available to you, what potential DF transmission patterns could explain all of the observed data? What would you advise the AD to do in order to prepare for the 2016 DF season?')),
      br(),
      h1('Part 4: Moving forward'),
      p('Decide on target levels of vaccination for dogs and people in 2016, keeping in mind that it is unlikely that you will be able to acheive 100 percent vaccination of either population. Enter these values below, each as a number between 0 and 100.'),
      ## INPUT
      #sidebarPanel(
      sliderInput('VaxPct.Dogs',
                  'Target vaccination level for DOG population:',
                  min = 0,
                  max = 100,
                  value = 0,
                  width='350px'),
      sliderInput('VaxPct.Humans',
                  'Target vaccination level for HUMAN population:',
                  min = 0,
                  max = 100,
                  value = 0,
                  width='350px'),
      #),
      p('We\'ll now run the model once to see an example of what might happen if these levels of vaccination were acheived in 2016. Scroll down to see what happened...'),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      ## OUTPUT
      plotOutput('targetPlot'),
      br(),
      p(strong('Is this what you expected to happen? You can reload the page as many times as you like to get a feeling for whether the outcome above is typical of what would be expected when these levels of vaccination are achieved.')),
      p('Now let\'s run the simulation 1000 times with the target vaccination levels. This may take a while.'),
      p('These results can now be plotted to give you a better feeling for the variation in outcomes under an intervention acheiving the targeted levels of vaccination in each population:'),
      ## OUTPUT
      plotOutput('distPlot'),
      br(),
      p(strong('Do these plots for your chosen target vaccination levels give you any additional insight into the processes underlying DF transmission? If not, try lowering your target vaccination levels for at least one of the populations and repeating this section. What is each of these plots showing, and do the results surprise you?')),
      br(),
      p(em('Scroll down when you are ready continue to the final section.')),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      h1('Part 5: Vaccination outcomes'),
      p('The following plots show what happens when only one population (dogs or people) is vaccinated, with different levels of coverage. The red dots represent the mean number of cases over 100 runs of the simulation, and the boxes show the middle 50 percent of runs. The thick black horizontal line is the median outbreak size for each intervention.'),
      # IMAGE
      #h3('2015'),
      img(src = 'dfScenarios.png'),
      br(),
      br(),
      p(strong('Why does vaccinating 50 percent of dogs appear to eliminate cases in dogs when vaccinating 50 percent of people only reduces the number of human cases by about 50 percent? What do you think vaccinating 50 percent of dogs would do to the number of human cases, on average? What about the effect of vaccinating 50 percent of people on the number of dog cases?')),
      br()
    )
  )
))
