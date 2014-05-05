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


### Phase 2: Programming Project

The programming project is an opportunity to explore in more depth (and
practice using) a language or technique from the seminar that you found
interesting. You will submit a short paper describing your project and give a
15 minute presentation to the rest of the group, in order to share your project
and your insights on your chosen language/technique.

More details on the requirements of the programming project, paper, and
presentation will be given partway through the seminar.


## Schedule and Reading Assignments

### Foundation

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
</table>

### Expression Problem Solutions: Libraries and Design Patterns

<table class="schedule">
<tr class="schedule-row">
  <td class="schedule-date">12 May</td>
  <td class="schedule-info">
  <span class="schedule-topic">
  Palsberg and Jay, *The Essence of the Visitor Pattern*, COMPSAC 1998
  </span>

  Discusses the "visitor pattern", which inverts the expression problem for
  object-oriented programs, making it easy to add new operations, but
  difficult to add new cases. Introduces the "Walkabout" class as a reusable
  solution that is extensible in both dimensions but relies on reflection and
  is not type safe.
  
  (Discussion leader: ???)
  </td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">19 May</td>
  <td class="schedule-info">
  <span class="schedule-topic">
  Zenger and Odersky, *Extensible Algebraic Datatypes with Defaults*, ICFP
  2001
  </span>

  Adds default cases to the "extensible visitor pattern" to make it easier to
  use and more type safe. For background, you may be interested in the
  original extensible visitor pattern paper by Krishnamurthi, Felleisen, and
  Friedman: *Synthesizing Object-Oriented and Functional Design to Promote
  Re-Use*, ECOOP 1998.

  (Discussion leader: ???)
  </td>
</tr>
</table>


## Potential Future Papers

A list of papers we may discuss is provided below. If you would like to discuss
a particular paper not on this list, please send it my way!

### Expression Problem Solutions: Libraries and Design Patterns

 * Zenger and Odersky, *Independently Extensible Solutions to the Expression
   Problem*, FOOL 2005.

 * Swiestra, *Data Types à la Carte*, JFP 2008.
 
 * Lämmel and Ostermann, *Software Extension and Integration with Type
   Classes*, GPCE 2006.
 
 * Carette, Kiselyov, and Shan, *Finally Tagless, Partially Evaluated: Tagless
   Staged Interpreters for Simpler Typed Languages*, JFP 2009.

 * Oliveira and Cook, *Extensibility for the Masses: Practical Extensibility
   with Object Algebras*, ECOOP 2012.


### Expression Problem Solutions: Language Extensions
 
 * Clifton et al., *MultiJava: Modular Open Classes and Symmetric Multiple
   Dispatch for Java*, OOPSLA 2000.
 
 * Löh and Hinze, *Open Data Types and Open Functions*, PPDP 2006.

 * Apel, Kästner, Lengauer, *Feature Featherweight Java: A Calculus for
   Feature-Oriented Programming and Stepwise Refinement*, GPCE 2008.


### Rethinking Modularity

 * Cook, *On Understanding Data Abstraction, Revisited*, OOPSLA 2009.

 * Ostermann et al., *Revisiting Information Hiding: Reflections on Classical
   and Nonclassical Modularity*, ECOOP 2011.
