{-# LANGUAGE TupleSections #-}

module CV.PaperDB where

import CV.Paper


--
-- * Defintions
--

-- ** Authors
me       = Author "Eric" "Walkingshaw"
martin   = Author "Martin" "Erwig"
klaus    = Author "Klaus" "Ostermann"
sheng    = Author "Sheng" "Chen"
tillmann = Author "Tillmann" "Rendel"
duc      = Author "Duc" "Le"

-- ** Institutions
osu = "Oregon State University"

-- ** Journals
journal abbr name vol num = Venue name (Journal abbr vol num)

jfp    = journal "JFP" "Journal of Functional Programming"
jvlc   = journal "JVLC" "Journal of Visual Languages and Computing"
toplas = journal "TOPLAS" "ACM Trans. on Programming Languages and Systems"
tosem  = journal "TOSEM" "ACM Trans. on Software Engineering and Methodology"

-- ** Conferences
conf  abbr name = Venue name (Conference abbr Nothing)
confS abbr name series = Venue name (Conference abbr (Just series))
lncs = ("LNCS",)

dsl   = Venue "DSL" . Conference "IFIP Working Conf. on Domain-Specific Languages"
detc  = conf "DETC" "ASME Int. Design Engineering Technical Conf. & Computers and Information in Engineering Conf."
gpce  = conf "GPCE" "Int. Conf. on Generative Programming and Component Engineering"
gttse = conf "GTTSE" "Generative and Transformational Techniques in Software Engineering"
icfp  = conf "ICFP" "ACM SIGPLAN Int. Conf. on Functional Programming"
sle   = confS "SLE" "Int. Conf. on Software Language Engineering"
vlhcc = conf "VL/HCC" "IEEE Int. Symp. on Visual Languages and Human-Centric Computing"
vlhccdc = conf (abbr ++ "-DC") ("Doctoral Consortium at " ++ name)
  where Venue name (Conference abbr _) = vlhcc

-- ** Workshops
foser = conf "FoSER" "ACM SIGSOFT Workshop on the Future of Software Engineering Research"
vamos = conf "VaMoS" "Int. Workshop on Variability Modelling of Software-Intensive Systems"


--
-- * Papers
--

-- ** Under Review

gpce13 = submitted
  "gpce13-variational-graphs"
  [martin,me,sheng]
  "An Abstract Representation of Variational Graphs"
  2013
  gpce


-- ** 2013

phdthesis = appearedNoPages
  "thesis-choice-calculus"
  [me]
  "The Choice Calculus: A Formal Language of Variation"
  2013
  (Venue osu PhdThesis)
  `atURL` "http://hdl.handle.net/1957/40652"

toplas13 = accepted
  "toplas13-variational-typing"
  [sheng,martin,me]
  "Extending Type Inference to Variational Programs"
  2013
  (toplas Nothing Nothing)

jvlc13 = appeared
  "jvlc13-probula"
  [martin,me]
  "A Visual Language for Explaining Probabilistic Reasoning"
  2013
  (jvlc (Just 24) (Just 2))
  (Pages 88 109)

vamos13 = appeared
  "vamos13-cc-select"
  [martin,klaus,tillmann,me]
  "Adding Configuration to the Choice Calculus"
  2013
  vamos
  (PagesIn 13 1 8)


-- ** 2012

gpce12 = appeared
  "gpce12-compositional-cc"
  [me,martin]
  "A Calculus for Modeling and Implementing Variation"
  2012
  gpce
  (Pages 132 140)

icfp12 = appeared
  "icfp12-variational-type-errors"
  [sheng,martin,me]
  "An Error-Tolerant Type System for Variational Lambda Calculus"
  2012
  icfp
  (Pages 29 40)

chapter12 = appeared
  "semantics-driven-dsl-design"
  [martin,me]
  "Semantics-Driven DSL Design"
  2012
  (Venue "Formal and Practical Aspects of Domain-Specific Languages: Recent Developments"
    (Chapter [Author "Marjan" "Mernik"] "IGI Global"))
  (Pages 56 80)


-- ** 2011

msthesis = appearedNoPages
  "msthesis-game-theory-dsls"
  [me]
  "Domain-Specific Language Support for Experimental Game Theory"
  2011
  (Venue osu PhdThesis)
  `atURL` "http://hdl.handle.net/1957/26757"

gttse11 = appeared
  "gttse11-choice-calculus-tutorial"
  [martin,me]
  "Variation Programming with the Choice Calculus"
  2012
  gttse
  (Pages 55 99)

vlhcc11 = appeared
  "vlhcc11-ifdef-confirmed-harmful"
  [duc,me,martin]
  "#ifdef Confirmed Harmful: Promoting Understandable Software Variation"
  2011
  vlhcc
  (Pages 143 150)

dsl11 = appeared
  "dsl11-causation-dsl"
  [me,martin]
  "A DSEL for Studying and Explaining Causation"
  2011
  (dsl Nothing)
  (Pages 143 167)

sle11 = appeared
  "sle11-semantics-first"
  [martin,me]
  "Semantics First! Rethinking the Language Design Process"
  2011
  (sle (lncs 6940))
  (Pages 243 262)

tosem11 = appeared
  "tosem11-choice-calculus"
  [martin,me]
  "The Choice Calculus: A Representation for Software Variation"
  2011
  (tosem (Just 21) (Just 1))
  (PagesIn 6 1 27)


-- ** 2010

foser10 = appeared
  "foser10-program-fields"
  [martin,me]
  "Program Fields for Continuous Software"
  2010
  foser
  (Pages 105 108)

vlhcc10 = appeared
  "vlhcc10-neuron-diagrams"
  [martin,me]
  "Causal Reasoning with Neuron Diagrams"
  2010
  vlhcc
  (Pages 101 108)

vlhcc10dc = appeared
  "vlhcc10dc-variational-explanations"
  [me]
  "Managing Variation in Explanation-Oriented Languages"
  2010
  vlhccdc
  (Pages 247 248)

qual = appearedNoPages
  "qualifier-variation-survey"
  [me]
  "Features and Feature Models: A Survey of Variation Representations"
  2010
  (Venue "Compendium of Computer Science Doctoral Qualifying Exams"
    (Chapter [] osu))
  `atURL` "http://hdl.handle.net/1957/19243"


-- ** 2009

vlhcc09 = appeared
  "vlhcc09-visual-explanations-probability"
  [martin,me]
  "Visual Explanations of Probabilistic Reasoning"
  2009
  vlhcc
  (Pages 23 27)

detc09 = appeared
  "detc09-software-hardware-design"
  [me, Author "Paul" "Strauss", martin, Author "John" "Mueller", Author "Irem" "Tumer"]
  "A Formal Representation of Software-Hardware System Design"
  2009
  detc
  (Pages 1387 1398)

jfp09 = appeared
  "jfp09-hagl"
  [me,martin]
  "A Domain-Specific Language for Experimental Game Theory"
  2009
  (jfp (Just 19) Nothing)
  (Pages 645 661)
  
dsl09a = appeared
  "dsl09-explaining-probability"
  [martin,me]
  "A DSL for Explaining Probabilistic Reasoning"
  2009
  (dsl (Just (lncs 5658)))
  (Pages 335 359)
  `withNote` "Best paper"

dsl09b = appeared
  "dsl09-hagl-expressiveness"
  [me,martin]
  "Varying Domain Representations in Hagl -- Extending the Expressiveness of a DSL for Experimental Game Theory"
  2009
  (dsl (Just (lncs 5658)))
  (Pages 310 334)


-- ** 2008

vlhcc08 = appeared
  "vlhcc08-explaining-strategies"
  [martin,me]
  "A Visual Language for Representing and Explaining Strategies in Game Theory"
  2008
  vlhcc
  (Pages 101 108)

vlhcc08dc = appeared
  "vlhcc09dc-explanation-oriented-languages"
  [me]
  "Designing Explanation-Oriented Languages"
  2008
  vlhccdc
  (Pages 274 275)
