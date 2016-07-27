{-# LANGUAGE PatternGuards, TupleSections #-}

module WebPage.Pubs.Database where

import WebPage.Pubs.Paper


--
-- * Defintions
--

-- ** Authors
abbott       = Author "Keeley" "Abbott"
apel         = Author "Sven" "Apel"
berger       = Author "Thorsten" "Berger"
bodden       = Author "Eric" "Bodden"
bogart       = Author "Christopher" "Bogart"
chen         = Author "Sheng" "Chen"
erwig        = Author "Martin" "Erwig"
kaestner     = Author "Christian" "Kästner"
le           = Author "Duc" "Le"
ostermann    = Author "Klaus" "Ostermann"
rendel       = Author "Tillmann" "Rendel"
stanciulescu = Author "Ștefan" "Stănciulescu"
walkingshaw  = Author "Eric" "Walkingshaw"
wasowski     = Author "Andrzej" "Wąsowski"

-- ** Institutions
osu = "Oregon State University"

-- ** Journals
jfp    = short "JFP" "Journal of Functional Programming"
jvlc   = short "JVLC" "Journal of Visual Languages and Computing"
toplas = short "TOPLAS" "ACM Trans. on Programming Languages and Systems"
tosem  = short "TOSEM" "ACM Trans. on Software Engineering and Methodology"

-- ** Conferences
lncs = ("LNCS",)

dsl    = short "DSL" "IFIP Working Conf. on Domain-Specific Languages"
ecoop  = short "ECOOP" "European Conf. on Object-Oriented Programming"
gpce   = short "GPCE" "ACM SIGPLAN Int. Conf. on Generative Programming and Component Engineering"
gttse  = short "GTTSE" "Generative and Transformational Techniques in Software Engineering"
icfp   = short "ICFP" "ACM SIGPLAN Int. Conf. on Functional Programming"
icsme  = short "ICSME" "IEEE Int. Conf. on Software Maintenance and Evolution"
idetc  = short "IDETC" "ASME Int. Design Engineering Technical Conf. & Computers and Information in Engineering Conf."
onward = short "Onward!" "ACM SIGPLAN Symp. on New Ideas in Programming and Reflections on Software"
sle    = short "SLE" "Int. Conf. on Software Language Engineering"
vlhcc  = short "VL/HCC" "IEEE Int. Symp. on Visual Languages and Human-Centric Computing"
vlhccdc | Just s <- _shortName vlhcc = short s ("Doctoral Consortium at " ++ _longName vlhcc)

-- ** Workshops
fosd  = short "FOSD" "Int. Workshop on Feature-Oriented Software Development"
foser = short "FoSER" "ACM SIGSOFT Workshop on the Future of Software Engineering Research"
vamos = short "VaMoS" "Int. Workshop on Variability Modelling of Software-Intensive Systems"


--
-- * Papers
--

-- ** Lists of papers in chronological order.

drafts = []
y16 = [icsme16,ecoop16]
y15 = [vlhcc15]
y14 = [onward14,gpce14,toplas14]
y13 = [fosd13,phdthesis,jvlc13,vamos13]
y12 = [gpce12,icfp12,chapter12]
y11 = [msthesis,gttse11,vlhcc11,dsl11,sle11,tosem11]
y10 = [foser10,vlhcc10,vlhcc10dc,qual]
y09 = [vlhcc09,idetc09,jfp09,dsl09a,dsl09b]
y08 = [vlhcc08,vlhcc08dc]

allPubs = concat [drafts,y16,y15,y14,y13,y12,y11,y10,y09,y08]


-- ** 2016

icsme16 = accepted Conference
  "icsme16-variation-control-system"
  [stanciulescu,berger,walkingshaw,wasowski]
  "Concepts, Operations, and Feasibility of a Projection-Based Variation Control System"
  2016
  @@ icsme

ecoop16 = conference
  "ecoop16-variational-programming-calculus"
  [chen,erwig,walkingshaw]
  "A Calculus for Variational Programming"
  2016
  `onPages` PagesIn 6 1 28
  @@ ecoop



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
  @@ fosd

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
  @@ vamos


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
  @@ icfp

chapter12 = appeared Chapter
  "semantics-driven-dsl-design"
  [erwig,walkingshaw]
  "Semantics-Driven DSL Design"
  2012
  `onPages` Pages 56 80
  @@ venue "Formal and Practical Aspects of Domain-Specific Languages: Recent Developments"
     `setEditor` [Author "Marjan" "Mernik"]
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
  2012
  `onPages` Pages 55 99
  `setCodeLink` "https://github.com/walkie/CC-GTTSE"
  @@ gttse

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
  [walkingshaw, Author "Paul" "Strauss", erwig, Author "John" "Mueller", Author "Irem" "Tumer"]
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

dsl09b = conference
  "dsl09-hagl-expressiveness"
  [walkingshaw,erwig]
  "Varying Domain Representations in Hagl – Extending the Expressiveness of a DSL for Experimental Game Theory"
  2009
  `onPages` Pages 310 334
  `setCodeLink` "https://github.com/walkie/Hagl-WCDSL"
  @@ dsl `setSeries` lncs 5658


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
