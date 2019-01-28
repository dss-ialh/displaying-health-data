![col-logo][col-logo]
Colloquium organized by the Institute on Aging and Lifelong Health at the University of Victoria
[digital poster][digicaster] - [print poster][poster]

# Recorded Videos
 - [Day 1][day-1-recording] 
 - [Day 2][day-2-recording]
 - Day 3 (pending

[day-1-recording]:https://www.youtube.com/watch?v=oMkACvpE5jg&t=3330s
[day-2-recording]:https://www.youtube.com/watch?v=XfPLIsMvVaY

# Day 1 : Health Data

[Day 1 recording][day-1-recording] 

## 1A
[Transactional data of Island Health: How patients vote with their feet][talk1a]
Dr. Ken Moselle (Island Health) and Dr. Andriy Koval ([BC Observatory](http://www.bccdc.ca/our-services/programs/bc-observatory-for-pop-public-health), [UCF](https://www.ucf.edu/))
 - [Clinical Context Coding Scheme][cccs_handout] handout

## 1B
[Visualizing logistic regression with the “coloring book” technique: A study in ggplot2][talk1b]
Dr. Andriy Koval ([BC Observatory](http://www.bccdc.ca/our-services/programs/bc-observatory-for-pop-public-health), [UCF](https://www.ucf.edu/))


# Day 2 : Substance Use

[Day 2][day-2-recording]

## 2A
[Nuances of information sharing and data display in a mobile application for students with substance use disorder][talk2a]
Dr. Barbara (Basia) Andraka-Christou ([University of Central Florida]((https://www.ucf.edu/)), Department of [Health Management and Informatics][hmi])
[hmi]:https://ccie.ucf.edu/hmi/welcome/

## 2B
[Optimizing public health surveillance through reproducible reporting: Response to opioid crisis on Vancouver Island][talk2b]
Shannon Tracey ([University of Victoria](https://www.uvic.ca/)) and Maritia Gully ([Island Health](https://www.islandhealth.ca/))

# Day 3 : Dashboard & Pipelines

## 3A
[Building pipelines and dashboards for practitioners: Mobilizing knowledge with reproducible reporting][talk3a]
Dr. Will Beasley ([University of Oklahoma Health Sciences Center](https://ouhsc.edu/bbmc/team/))

- [presentation slides][talk3b] 
- [Live Demo](https://raw.githack.com/dss-ialh/displaying-health-data/master/analysis/dashboard-1/dashboard-1.html)

## 3B
[Constructing workflows for reproducible analytics: Suppressing small counts for provincial chronic disease dashboard][talk3a]
Dr. Andriy Koval ([BC Observatory](http://www.bccdc.ca/our-services/programs/bc-observatory-for-pop-public-health), [UCF](https://www.ucf.edu/)) and Anthony Leamon ([Island Health](https://www.islandhealth.ca/))

- [presentation slides][talk3b] 
- [suppress-for-release][suppress-for-release] project

[talk1a]:https://drive.google.com/open?id=14swb2d7UKwFQuN6CRQM22jRbQcD80RSa   
[cccs_handout]:https://drive.google.com/open?id=1pQNX-dcLOZHrUxYve6ewKnZ8UgbrZllA
[talk1b]:https://drive.google.com/open?id=1ALz8dc-bTNSMwxEwMDwe-l5xlmaAtme2
[talk2a]:https://drive.google.com/file/d/1NSuSFeYBCJyUxA_JHWYutsmK9dQZwwNg/view?usp=sharing
[talk2b]:https://drive.google.com/open?id=1AeMVV47FPcJEl_kJSuqmWm2kzpfeRWj5
[talk3a]:https://raw.githack.com/dss-ialh/displaying-health-data/master/documentation/products/beasley/dhd-2018-uvic-3-a-beasley-2018-11-29.pdf
[talk3b]:https://drive.google.com/open?id=189-CqQO_MklrEBtowV4GfjQV_emvDkME
[suppress-for-release]:https://github.com/IHACRU/suppress-for-release

# Shared Documents
- master copy of the [detailed schedule][schedule] for both lectures and live coding session
- google doc with [notes and sketches for colloquium talks][notes_talks]
- google doc with [notes and sketches for live coding sessions][notes_live]

[notes_talks]:https://docs.google.com/document/d/15SYHa7mftXQk8qyGJ9aT26kQOjbCqtzrg6IS68Foflc/edit?usp=sharing
[notes_live]:https://docs.google.com/document/d/1ARRecAQWkWZ80dedC5Qcv7_fHOAny_sE1fHipssauJU/edit?usp=sharing
[schedule]:https://docs.google.com/document/d/1emVSoSsf7Sh1fSXrl_S_kC3ng_-lCD8t5EtIb1AHea8/edit?usp=sharing




# Live coding
- Theme 1: Longitudinal perspectives
- Theme 2: Alluvial (sankey) plots
- Theme 3: Venn Diagrams

[col-logo]:libs/images/colloquium-logo-head.jpg
[digicaster]:https://drive.google.com/open?id=1UmU7yrm4pon8Ilh1rhlGuKdxeLmhuay4
[poster]:https://drive.google.com/open?id=1ko083Jix6jp6urFq-ghMhvlFs0P_5VQd
[schedule-summary]:https://drive.google.com/open?id=1h5qDcoRTJNLSteSOc7AaJ4T7yiBHfANy


### Installation and Documentation

Please execute the following steps:
1. **[R](http://cran.r-project.org/)** is the centerpiece of the analysis. Every few months, you'll need to download the most recent version.  {added Sept 2012}

2. **[RStudio Desktop](http://www.rstudio.com/ide/download/desktop)** is the IDE (integrated design interface) that you'll use to interact with R, GitHub, Markdown, and LaTeX. Updates can be checked easily through the menus `Help` -> `Check for updates`.   {added Sept 2012}

3. Install packages needed for this project by executing the following lines:
```r
utils::install.packages("devtools")
devtools::source_gist("2c5e7459b88ec28b9e8fa0c695b15ee3", filename="package-janitor-bbmc.R")
package_janitor_remote("https://raw.githubusercontent.com/OuhscBbmc/RedcapExamplesAndPatterns/master/utility/package-dependency-list.csv")
install.packages("remotes") # Run this line if the 'remotes' package isn't installed already.
remotes::install_github(repo="dss-ialh/displaying-health-data")
 ```
