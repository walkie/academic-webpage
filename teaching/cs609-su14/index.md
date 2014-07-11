---
title: Modular Extensibility
---
<style>
td {
  padding: 0;
  padding-right: 2ex;
  vertical-align: top;
}
</style>

# CS 609 Diskussionsseminar: Modular Extensibility

<div class="no-print">

## Quick Links

 *  [Logistics]
 *  [Motivation]
 *  [Structure of Course]
 *  [Schedule and Reading Assignments]

</div>

## Logistics

----------------  -------------------------------------------------------------
Instructor:       <span class="my-name">Eric Walkingshaw</span>
                  <span class="marburg-email"></span>
Weekly meeting:   Mondays, 16:30 -- 17:45
                  Seminarraum V D5
Course web page:  <http://www.staff.uni-marburg.de/~walkings/teaching/cs609-su14/>
                  (This document, which will be updated throughout the course.)
----------------  -------------------------------------------------------------


## Motivation

A property of good software is the ability to add new features in a modular
way. That is, it should be possible to (1) add new features without making
invasive changes to the existing code, and (2) keep the code that implements
each new feature together. Modular extensibility makes software more
maintainable by keeping separate concerns separated in the code; it increases
flexibility by making software easier to extend in new and unforeseen ways; and
it supports customizability by making it easier to add and remove features
according to the needs of different users.

Many programming languages provide good support for some kinds of modular
extensions but not others. Most significantly, object-oriented languages and
functional languages support modular extensibility in orthogonal dimensions.
The challenge of supporting all kinds of modular extensions has been called the
"[expression problem](expression-problem.html)" or the "tyranny of the dominant
decomposition". 

In this seminar we will study modular extensibility, focusing especially on the
design patterns and language features that address the expression problem. Each
week we will meet to read and discuss one paper. Our working language will be
English.

In addition to the general goals of a discussion seminar (exposure to research,
practice reading and discussing papers, etc.) the specific goals of this
seminar are to:

 * Understand the specific concept of *modular extensibility*, how it can be
   achieved, and its significance in language design and software engineering.
 * Develop some working definitions of more general concepts like *modularity*
   and *expressiveness* that arise in discussions about languages and software.
 * Be able to recognize and evaluate the tradeoffs between different solutions
   to the expression problem.
 * Practice using one of these solutions in your own work, in order to develop
   a deeper understanding of the benefits and limitations of the approach.
   

## Structure of Course

Throughout the semester, the seminar will be conducted as a reading group.
Additionally, students taking the seminar for credit will use one of the
techniques or languages discussed in the seminar in a small programming project
of their choosing. Each student will write a short paper and give a
presentation of their project.


### Phase 1: Reading Group

Each week I will assign a paper and name a *discussion leader* for our next
meeting. All participants should read the assigned reading each week.

If you are *not* the discussion leader, you should submit 2-3 discussion topics
or questions to the discussion leader and to me. This is intended to encourage
you to think critically about the paper before you arrive at the meeting, and
so will hopefully lead to more interesting discussions.

If you are the discussion leader, you should use the submitted questions to
guide your preparation. Most importantly, you should *dig deeper* in order to
answer questions or provide additional insights. This will usually require
looking into related work.

During the discussion, the discussion leader will briefly summarize the main
points of each section, pose topics for discussion, attempt to answer
questions, offer additional insights, and keep the discussion on track.
Everyone else should come prepared to discuss the paper! Not only will this be
more lively and fun, it will make the discussion leader's job much easier.

To summarize, your responsibilities during this phase of the seminar are to:

 1. Read the assigned reading each week.
 2. Submit discussion topics/questions to me and the discussion leader 24 hours
    before each meeting.
 3. Attend and participate in the discussions.
 4. Prepare for the discussions that you will lead.


### Phase 2: Final Project

The final project is an opportunity to either (1) explore in more depth (and
practice using) a language or design pattern from the seminar that you found
interesting, or (2) learn independently about some techniques that we didn't
have time to discuss. You will submit a short paper and give a short
presentation to share your insights with the rest of the group.

**[Final Project Details](project.html)**


## Schedule and Reading Assignments

<table class="schedule">
<tr class="schedule-row">
  <td class="schedule-date">28 Apr</td>
  <td class="schedule-info">
  <span class="schedule-topic">
  Parnas, *On the Criteria to be Used in Decomposing Systems into Modules*,
  CACM 1972
  </span>
  
  Argues for "information hiding" as the most important role of modules.
  Expresses motivations for what would eventually become object-oriented
  programming.
  
  (Discussion leader: Eric)
  </td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">5 May</td>
  <td class="schedule-info">
  <span class="schedule-topic">
  [Introduction to the Expression Problem](expression-problem.html)
  </span>
  
  Motivates the expression problem from a programmer's perspective.
</tr>
<tr>
  <td> </td>
  <td class="schedule-info">
  <span class="schedule-topic">
  Wadler, *The Expression Problem*, email 1998
  </span>
  
  Coins the name of the expression problem and establishes some criteria
  for solutions.
  
  (Discussion leader: Eric)
  </td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">12 May</td>
  <td class="schedule-info">
  <span class="schedule-topic">
  Palsberg and Jay, *The Essence of the Visitor Pattern*, COMPSAC 1998
  </span>

  Discusses the "visitor pattern", which inverts the expression problem for
  object-oriented programs, making it easy to add new operations but
  difficult to add new cases. Introduces the "Walkabout" class as a reusable
  solution that is extensible in both dimensions but relies on reflection and
  is not type safe.
  
  (Discussion leader: Bastian)
  </td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">19 May</td>
  <td class="schedule-info">
  <span class="schedule-topic">
  Zenger and Odersky, *Extensible Algebraic Datatypes with Defaults*, ICFP
  2001
  </span>

  Describes the "extensible visitor pattern" as a solution to the expression
  problem. For background, you may be interested in the original extensible
  visitor pattern paper by Krishnamurthi, Felleisen, and Friedman:
  *Synthesizing Object-Oriented and Functional Design to Promote Re-Use*, ECOOP
  1998.

  (Discussion leader: Felix)
  </td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">26 May</td>
  <td class="schedule-info">
  <span class="schedule-topic">
  Janzen and De Volder, *Programming with Crosscutting Effective Views*, ECOOP
  2004
  </span>

  Describes a tool that presents two different, editable views of the same
  program, to support extending with new cases or new operations. Shows how
  good tool support can provide a partial solution to the expression problem.

  (Discussion leader: Elena)
  </td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">2 Jun</td>
  <td class="schedule-info">
  <span class="schedule-topic">
  Clifton et al., *MultiJava: Modular Open Classes and Symmetric Multiple
  Dispatch for Java*, OOPSLA 2000
  </span>
  
  Extends Java with open classes and
  [multi-methods](https://en.wikipedia.org/wiki/Multi-methods). Open classes
  support extending existing classes with new methods, while multi-methods
  provide a solution to the "binary method problem".

  (Discussion leader: Eric)
  </td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">9 Jun</td>
  <td class="schedule-info">
  <span class="schedule-topic">(Holiday)</span>
  </td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">16 Jun</td>
  <td class="schedule-info">
  <span class="schedule-topic">
  Kästner, Apel, and Ostermann, *The Road to Feature Modularity?*, SPLC 2011.
  </span>
  
  Discusses modularity and extensibility from the perspective of
  [feature-oriented software development](http://www.jot.fm/issues/issue_2009_07/column5/index). 
  A unique challenge in this context is that it must be possible not only to
  *add* new features to a program, but to selectively *enable and disable*
  them.

  (Discussion leader: Iratxe)
  </td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">23 Jun</td>
  <td class="schedule-info">
  <span class="schedule-topic">
  Oliveira and Cook, *Extensibility for the Masses: Practical Extensibility
  with Object Algebras*, ECOOP 2012
  </span>
  
  Describes an elegant design pattern for solving the expression problem in OO
  languages with generics.

  (Discussion leader: Jona)
  </td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">30 Jun</td>
  <td class="schedule-info">
  <span class="schedule-topic">
  Lämmel and Ostermann, *Software Extension and Integration with Type
  Classes*, GPCE 2006.
  </span>
  
  Shows how Haskell's type classes can be used to (partially) solve many of the
  modularity challenges we have discussed during this seminar, including the
  expression problem and the binary method problem.

  (Discussion leader: Eric)
  </td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">7 Jul</td>
  <td class="schedule-info">
  <span class="schedule-topic">
  Löh and Hinze, *Open Data Types and Open Functions*, PPDP 2006.
  </span>
  
  Discusses an extension to Haskell that allows data types and functions to be
  declared *open*, allowing them to be modularly extended with new cases later.
  This is analogous in many ways to the work on MultiJava, but starting from
  the functional perspective.

  (Discussion leader: Yi)
  </td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">21 Jul</td>
  <td class="schedule-info">
  <span class="schedule-topic">
  Final project presentations
  </span>
  
  --------  -----------------------------------------------------
   Iratxe:  The meaning of "independently extensible"
    Felix:  Modular extensibility in Java with [AspectJ](https://eclipse.org/aspectj/)
    Elena:  Comparing solutions to the expression problem in Scala and Ruby
  Bastian:  Comparing different solutions to the expression problem in Haskell
  --------  -----------------------------------------------------
  </td>
</tr>
</table>


## Further Reading

A list of other interesting and relevant papers is provided below.

### More Solutions to the Expression Problem

 * Apel, Kästner, Lengauer, *Feature Featherweight Java: A Calculus for
   Feature-Oriented Programming and Stepwise Refinement*, GPCE 2008

 * Carette, Kiselyov, and Shan, *Finally Tagless, Partially Evaluated: Tagless
   Staged Interpreters for Simpler Typed Languages*, JFP 2009
 
 * Swiestra, *Data Types à la Carte*, JFP 2008.
 
 * Zenger and Odersky, *Independently Extensible Solutions to the Expression
   Problem*, FOOL 2005
 
### Rethinking Modularity

 * Cook, *On Understanding Data Abstraction, Revisited*, OOPSLA 2009

 * Ostermann et al., *Revisiting Information Hiding: Reflections on Classical
   and Nonclassical Modularity*, ECOOP 2011
