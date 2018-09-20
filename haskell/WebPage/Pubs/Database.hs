{-# LANGUAGE PatternGuards, TupleSections #-}

module WebPage.Pubs.Database where

import WebPage.Pubs.Paper


--
-- * Defintions
--

-- ** Authors
abbott       = author "Keeley" "Abbott"
adelsberger  = author "Stephan" "Adelsberger"
apel         = author "Sven" "Apel"
ataei        = author "Parisa" "Ataei"
berger       = author "Thorsten" "Berger"
bodden       = author "Eric" "Bodden"
bogart       = author "Christopher" "Bogart"
campora      = Author "John" (Just "Peter") "Campora" (Just "III")
chen         = author "Sheng" "Chen"
erwig        = author "Martin" "Erwig"
gopinath     = author "Rahul" "Gopinath"
hubbard      = author "Spencer" "Hubbard"
kaestner     = author "Christian" "Kästner"
le           = author "Duc" "Le"
meinicke     = author "Jens" "Meinicke"
meng         = author "Meng" "Meng"
ostermann    = author "Klaus" "Ostermann"
rendel       = author "Tillmann" "Rendel"
setzer       = author "Anton" "Setzer"
stanciulescu = author "Ștefan" "Stănciulescu"
termehchy    = author "Arash" "Termehchy"
walkingshaw  = author "Eric" "Walkingshaw"
wasowski     = author "Andrzej" "Wąsowski"
wong         = author "Chu-Pan" "Wong"
young        = author "Jeffrey" "Young"

-- ** Institutions
osu = "Oregon State University"

-- ** Journals
jfp    = short "JFP" "Journal of Functional Programming"
jvlc   = short "JVLC" "Journal of Visual Languages and Computing"
toplas = short "TOPLAS" "ACM Trans. on Programming Languages and Systems"
tosem  = short "TOSEM" "ACM Trans. on Software Engineering and Methodology"
pacmpl = short "PACMPL" "Proc. of the ACM on Programming Languages"
        
-- ** Proceedings in Journals
popl   = pacmpl `setIssue` "ACM SIGPLAN Symp. on Principles of Programming Languages (POPL)"
icfp   = pacmpl `setIssue` "ACM SIGPLAN Int. Conf. on Functional Programming (ICFP)"

-- ** Conferences
lncs   = ("LNCS",)
lipics = ("LIPIcs",)

dsl    = short "DSL" "IFIP Working Conf. on Domain-Specific Languages"
ecoop  = short "ECOOP" "European Conf. on Object-Oriented Programming"
gpce   = short "GPCE" "ACM SIGPLAN Int. Conf. on Generative Programming and Component Engineering"
icfpc  = short "ICFP" "ACM SIGPLAN Int. Conf. on Functional Programming"
icsme  = short "ICSME" "IEEE Int. Conf. on Software Maintenance and Evolution"
idetc  = short "IDETC" "ASME Int. Design Engineering Technical Conf. & Computers and Information in Engineering Conf."
onward = short "Onward!" "ACM SIGPLAN Symp. on New Ideas in Programming and Reflections on Software"
ppdp   = short "PPDP" "ACM SIGPLAN Int. Symp. on Principles and Practice of Declarative Programming"
setta  = short "SETTA" "Int. Symp. on Dependable Software Engineering: Theories, Tools, and Applications"
sigcse = short "SIGCSE" "ACM SIGCSE Technical Symp. on Computer Science Education"
sle    = short "SLE" "ACM SIGPLAN Int. Conf. on Software Language Engineering"
vlhcc  = short "VL/HCC" "IEEE Int. Symp. on Visual Languages and Human-Centric Computing"
vlhccdc | Just s <- _shortName vlhcc = short s ("Doctoral Consortium at " ++ _longName vlhcc)

-- ** Workshops
dbpl     = short "DBPL" "Int. Symp. on Database Programming Languages"
fosd     = short "FOSD" "Int. Workshop on Feature-Oriented Software Development"
foser    = short "FoSER" "ACM SIGSOFT Workshop on the Future of Software Engineering Research"
mutation = short "Mutation" "Int. Workshop on Mutation Analysis"
vamos    = short "VaMoS" "Int. Workshop on Variability Modelling of Software-Intensive Systems"


--
-- * Papers
--

-- ** Lists of papers in chronological order.

drafts = []
y18 = [icfp18,ppdp18,setta18,poly18,sigcse18,popl18]
y17 = [dbpl17,mutation17,vamos17]
y16 = [fosd16,icsme16,ecoop16]
y15 = [vlhcc15]
y14 = [onward14,gpce14,toplas14]
y13 = [fosd13,gttse11,phdthesis,jvlc13,vamos13]
y12 = [gpce12,icfp12,chapter12]
y11 = [msthesis,vlhcc11,dsl11,sle11,tosem11]
y10 = [foser10,vlhcc10,vlhcc10dc,qual]
y09 = [vlhcc09,idetc09,jfp09,dsl09a,dsl09b]
y08 = [vlhcc08,vlhcc08dc]

allPubs = concat [y18,y17,y16,y15,y14,y13,y12,y11,y10,y09,y08]


-- ** 2018

icfp18 = journal
  "icfp18-casts-and-costs"
  [campora,chen,walkingshaw]
  "Casts and Costs: Harmonizing Safety and Performance in Gradual Typing"
  2018
  `setCodeLink` "https://bitbucket.org/PeterCampora/castsandcostsartifact"
  `onPages` PagesIn 98 1 30
  @@ icfp `setVolume` 2

ppdp18 = accepted Conference
  "ppdp18-declarative-guis"
  [adelsberger,setzer,walkingshaw]
  "Declarative GUIs: Simple, Consistent, and Verified"
  2018
  `setCodeLink` "https://github.com/stephanadelsb/PPDP18"
  `onPages` PagesIn 4 1 15
  @@ ppdp

setta18 = accepted Conference
  "setta18-verified-guis"
  [adelsberger,setzer,walkingshaw]
  "Developing GUI Applications in a Verified Setting"
  2018
  `setCodeLink` "https://github.com/stephanpaper/SETTA18"
  @@ setta `setPublisher` "Springer"

poly18 = accepted Workshop
  "poly18-heterogeneous-dbs-spls"
  [ataei,termehchy,walkingshaw]
  "Managing Structurally Heterogeneous Databases in Software Product Lines"
  2018
  @@ short "Poly" "VLDB Workshop: Polystores and Other Systems for Heterogeneous Data"
  `setPublisher` "Springer"

sigcse18 = conference
  "sigcse18-algorithm-explanations"
  [young,walkingshaw]
  "A Domain Analysis of Data Structure and Algorithm Explanations in the Wild"
  2018
  `setDataLink` "https://github.com/lambda-land/XOP-Algorithms-Data"
  `onPages` Pages 870 875
  @@ sigcse

popl18 = journal
  "popl18-migrating-gradual-types"
  [campora,chen,erwig,walkingshaw]
  "Migrating Gradual Types"
  2018
  `setCodeLink` "https://bitbucket.org/plcacs/popl18aec"
  `onPages` PagesIn 15 1 29
  @@ popl `setVolume` 2 


-- ** 2017

dbpl17 = workshop
  "dbpl17-variational-databases"
  [ataei,termehchy,walkingshaw]
  "Variational Databases"
  2017
  `onPages` PagesIn 11 1 4
  @@ dbpl `setPublisher` "ACM"

mutation17 = workshop
  "mutation17-how-good-are-your-types"
  [gopinath,walkingshaw]
  "How Good are Your Types? Using Mutation Analysis to Evaluate the Effectiveness of Type Annotations"
  2017
  `onPages` Pages 122 127
  `withNote` "Best presentation"
  @@ mutation `setPublisher` "IEEE"

vamos17 = workshop
  "vamos17-variational-stacks"
  [meng,meinicke,wong,walkingshaw,kaestner]
  "A Choice of Variational Stacks: Exploring Variational Data Structures"
  2017
  `onPages` Pages 28 35
  @@ vamos `setPublisher` "ACM"


-- ** 2016

fosd16 = workshop
  "fosd16-formula-choice-calculus"
  [hubbard,walkingshaw]
  "Formula Choice Calculus"
  2016
  `setCodeLink` "https://github.com/lambda-land/FCC-Coq"
  `onPages` Pages 49 57
  @@ fosd `setPublisher` "ACM"

icsme16 = conference
  "icsme16-variation-control-system"
  [stanciulescu,berger,walkingshaw,wasowski]
  "Concepts, Operations, and Feasibility of a Projection-Based Variation Control System"
  2016
  `setBothLink` "https://bitbucket.org/modelsteam/2016-vcs-marlin"
  `onPages` Pages 323 333
  @@ icsme

ecoop16 = conference
  "ecoop16-variational-programming-calculus"
  [chen,erwig,walkingshaw]
  "A Calculus for Variational Programming"
  2016
  `onPages` PagesIn 6 1 28
  @@ ecoop `setSeries` lipics 56



-- ** 2015

vlhcc15 = conference
  "vlhcc15-programs-for-people"
  [abbott,bogart,walkingshaw]
  "Programs for People: What We Can Learn from Lab Protocols"
  2015
  `setDataLink` "https://github.com/lambda-land/ProtocolStudy-Data"
  `onPages` Pages 203 211
  @@ vlhcc


-- ** 2014

onward14 = conference
  "onward14-variational-data"
  [walkingshaw,kaestner,erwig,apel,bodden]
  "Variational Data Structures: Exploring Trade-Offs in Computing With Variability"
  2014
  `onPages` Pages 213 226
  @@ onward

gpce14 = conference
  "gpce14-projectional-editing"
  [walkingshaw,ostermann]
  "Projectional Editing of Variational Software"
  2014
  `onPages` Pages 29 38
  `withNote` "Best paper"
  @@ gpce

toplas14 = journal
  "toplas14-variational-typing"
  [chen,erwig,walkingshaw]
  "Extending Type Inference to Variational Programs"
  2014
  `onPages` PagesIn 1 1 54
  @@ toplas `setVolume` 36 `setNumber` 1


-- ** 2013

fosd13 = workshop
  "fosd13-variational-graphs"
  [erwig,walkingshaw,chen]
  "An Abstract Representation of Variational Graphs"
  2013
  `onPages` Pages 25 32
  @@ fosd `setPublisher` "ACM"

phdthesis = thesis
  "thesis-choice-calculus"
  [walkingshaw]
  "The Choice Calculus: A Formal Language of Variation"
  2013
  `atURL` "http://hdl.handle.net/1957/40652"
  @@ venue osu `setVenueKind` "PhD thesis"

jvlc13 = journal
  "jvlc13-probula"
  [erwig,walkingshaw]
  "A Visual Language for Explaining Probabilistic Reasoning"
  2013
  `onPages` Pages 88 109
  @@ jvlc `setVolume` 24 `setNumber` 2

vamos13 = workshop
  "vamos13-cc-select"
  [erwig,ostermann,rendel,walkingshaw]
  "Adding Configuration to the Choice Calculus"
  2013
  `onPages` PagesIn 13 1 8
  @@ vamos `setPublisher` "ACM"


-- ** 2012

gpce12 = conference
  "gpce12-compositional-cc"
  [walkingshaw,erwig]
  "A Calculus for Modeling and Implementing Variation"
  2012
  `onPages` Pages 132 140
  @@ gpce

icfp12 = conference
  "icfp12-variational-type-errors"
  [chen,erwig,walkingshaw]
  "An Error-Tolerant Type System for Variational Lambda Calculus"
  2012
  `onPages` Pages 29 40
  @@ icfpc

chapter12 = appeared Chapter
  "semantics-driven-dsl-design"
  [erwig,walkingshaw]
  "Semantics-Driven DSL Design"
  2012
  `onPages` Pages 56 80
  @@ venue "Formal and Practical Aspects of Domain-Specific Languages: Recent Developments"
     `setEditor` [author "Marjan" "Mernik"]
     `setPublisher` "IGI Global"


-- ** 2011

msthesis = thesis
  "msthesis-game-theory-dsls"
  [walkingshaw]
  "Domain-Specific Language Support for Experimental Game Theory"
  2011
  `atURL` "http://hdl.handle.net/1957/26757"
  `setCodeLink` "https://github.com/walkie/Hagl"
  @@ venue osu `setVenueKind` "MS thesis"

gttse11 = appeared Chapter
  "gttse11-variation-programming"
  [erwig,walkingshaw]
  "Variation Programming with the Choice Calculus"
  2013
  `onPages` Pages 55 100
  `setCodeLink` "https://github.com/walkie/CC-GTTSE"
  @@ venue "Generative and Transformational Techniques in Software Engineering IV (GTTSE 2011), Revised and Extended Papers"
     `setSeries` lncs 7680
     `setPublisher` "Springer"

vlhcc11 = conference
  "vlhcc11-ifdef-confirmed-harmful"
  [le,walkingshaw,erwig]
  "#ifdef Confirmed Harmful: Promoting Understandable Software Variation"
  2011
  `onPages` Pages 143 150
  @@ vlhcc

dsl11 = conference
  "dsl11-causation-dsl"
  [walkingshaw,erwig]
  "A DSEL for Studying and Explaining Causation"
  2011
  `onPages` Pages 143 167
  `setCodeLink` "https://github.com/walkie/NeuronDiagram"
  @@ dsl

sle11 = conference
  "sle11-semantics-first"
  [erwig,walkingshaw]
  "Semantics First! Rethinking the Language Design Process"
  2011
  `onPages` Pages 243 262
  @@ sle `setSeries` lncs 6940
     `setPublisher` "Springer"

tosem11 = journal
  "tosem11-choice-calculus"
  [erwig,walkingshaw]
  "The Choice Calculus: A Representation for Software Variation"
  2011
  `onPages` PagesIn 6 1 27
  @@ tosem `setVolume` 21 `setNumber` 1


-- ** 2010

foser10 = workshop
  "foser10-program-fields"
  [erwig,walkingshaw]
  "Program Fields for Continuous Software"
  2010
  `onPages` Pages 105 108
  @@ foser

vlhcc10 = conference
  "vlhcc10-neuron-diagrams"
  [erwig,walkingshaw]
  "Causal Reasoning with Neuron Diagrams"
  2010
  `onPages` Pages 101 108
  @@ vlhcc

vlhcc10dc = appeared Consortium
  "vlhcc10dc-variational-explanations"
  [walkingshaw]
  "Managing Variation in Explanation-Oriented Languages"
  2010
  `onPages` Pages 247 248
  @@ vlhccdc

qual = thesis
  "qualifier-variation-survey"
  [walkingshaw]
  "Features and Feature Models: A Survey of Variation Representations"
  2010
  `atURL` "http://hdl.handle.net/1957/19243"
  @@ venue ("Compendium of Computer Science Doctoral Qualifying Exams, " ++ osu)


-- ** 2009

vlhcc09 = conference
  "vlhcc09-visual-explanations-probability"
  [erwig,walkingshaw]
  "Visual Explanations of Probabilistic Reasoning"
  2009
  `onPages` Pages 23 27
  @@ vlhcc

idetc09 = conference
  "idetc09-software-hardware-design"
  [walkingshaw, author "Paul" "Strauss", erwig, author "John" "Mueller", author "Irem" "Tumer"]
  "A Formal Representation of Software-Hardware System Design"
  2009
  `onPages` Pages 1387 1398
  @@ idetc

jfp09 = journal
  "jfp09-hagl"
  [walkingshaw,erwig]
  "A Domain-Specific Language for Experimental Game Theory"
  2009
  `onPages` Pages 645 661
  `setCodeLink` "https://github.com/walkie/Hagl-JFP"
  @@ jfp `setVolume` 19
  
dsl09a = conference
  "dsl09-explaining-probability"
  [erwig,walkingshaw]
  "A DSL for Explaining Probabilistic Reasoning"
  2009
  `onPages` Pages 335 359
  `withNote` "Best paper"
  @@ dsl `setSeries` lncs 5658
     `setPublisher` "Springer"

dsl09b = conference
  "dsl09-hagl-expressiveness"
  [walkingshaw,erwig]
  "Varying Domain Representations in Hagl – Extending the Expressiveness of a DSL for Experimental Game Theory"
  2009
  `onPages` Pages 310 334
  `setCodeLink` "https://github.com/walkie/Hagl-WCDSL"
  @@ dsl `setSeries` lncs 5658
     `setPublisher` "Springer"


-- ** 2008

vlhcc08 = conference
  "vlhcc08-explaining-strategies"
  [erwig,walkingshaw]
  "A Visual Language for Representing and Explaining Strategies in Game Theory"
  2008
  `onPages` Pages 101 108
  @@ vlhcc

vlhcc08dc = appeared Consortium
  "vlhcc09dc-explanation-oriented-languages"
  [walkingshaw]
  "Designing Explanation-Oriented Languages"
  2008
  `onPages` Pages 274 275
  @@ vlhccdc
