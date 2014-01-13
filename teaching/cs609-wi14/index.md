---
title: Human Factors in Programming Languages
---
<style>
td {
  padding: 0;
  padding-right: 2ex;
  vertical-align: top;
}
</style>

# CS 609 Diskussionsseminar: Human Factors in Programming Languages

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
Weekly meeting:   Mondays, 16:00 -- 17:30
                  Seminarraum XII C3
Course web page:  <http://www.staff.uni-marburg.de/~walkings/teaching/cs609-wi14/>
                  (This document, which will be updated throughout the course.)
----------------  -------------------------------------------------------------


## Motivation

Most programming language research is focused on formal qualities like type
safety and correctness, or else on machine-centric qualities like efficiency.
However, a programming language is ultimately intended to be used by a human.
The languages we use shape the way we approach, understand, and attempt to
solve problems. Good languages can not only help professional programmers be
more productive and solve harder problems, they can empower non-traditional
programmers by making computing more accessible.

In this seminar, we will focus on human aspects of programming language design.
We will discuss theories from cognitive science related to programming and how
these theories can be applied to improve the design of programming languages.
We will discuss human-centric language design methodologies and theoretical
frameworks for analyzing the usability of programming languages. Finally, we
will study the unique challenges of designing a language for the huge and
rapidly growing class of non-traditional, end-user programmers.

In addition to the general goals of a discussion seminar (exposure to research,
practice reading and discussing papers, etc.) the specific goals of this
seminar are to:

 * Motivate end-user programming and user-focused language design.
 * Develop a vocabulary for discussing language and interface design issues
   related to usability.
 * Develop a "toolbox" of theories and strategies that you can apply to your
   own design problems.
 * Practice applying these tools to a specific problem of your choice.
   

## Structure of Course

The seminar will be divided into two phases. During the first and longer phase,
we will function as a reading group. During the second and much shorter phase,
you will apply your new knowledge to a language or interface design problem,
and share your design with the group.


### Phase 1: Reading Group

This phase will last for the first 14 weeks of the semester.

Each week I will assign a paper, book chapter, or other reading for our next
meeting. I will also provide a brief motivation for why I selected the reading
in order to help you understand the paper in the context of the seminar. All
participants will read the assigned reading each week.

One participant will lead the discussion at each meeting. 

If you are *not* the discussion leader, you must submit 2-3 discussion topics
or questions to the discussion leader and to me. This is intended to encourage
you to think critically about the paper before you arrive at the meeting, and
so will hopefully lead to more interesting discussions.

If you *are* the discussion leader, you should use the submitted questions to
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
 4. Prepare for the discussions that you will lead (dig deeper).


### Phase 2: Application to a Design Problem

This phase will last for the final two weeks of the semester, although you should
start working on it earlier.

In this phase of the seminar you will apply the knowledge you have gained
throughout the semester to a small design project. You will design a language
or tool that enables a specific user to perform some specific programming tasks
that would be difficult or impossible for them to accomplish otherwise. You
will use at least two of the intellectual tools we have discussed this semester
during the design process.

**[Link to Project Specification](project.html)**



## Schedule and Reading Assignments

<table class="schedule">
<tr class="schedule-row">
  <td class="schedule-date">15 Oct</td>
  <td class="schedule-info">Course introduction, schedule a weekly meeting time.</td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">21 Oct</td>
  <td class="schedule-info">
  Blackwell, *First Steps in Programming: A Rationale for Attention Investment
  Models*, HCC 2002.
  
  This paper serves two roles. First, it provides definitions of "programmer"
  and "programming" that are somewhat broader than you might expect. This will
  help us to define the scope of the seminar. In particular, we will consider
  many aspects of what might traditionally be called "user interface design" to
  be within our purview as language designers. Second, it introduces the
  *attention investment* model, which can help to understand when a user will
  invest effort in learning a new language or feature, and suggests strategies
  for encouraging this investment.

  (Discussion leader: Jan)
  </td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">28 Oct</td>
  <td class="schedule-info">
  Green and Petre, *Usability Analysis of Visual Programming Environments: A 
  ‘Cognitive Dimensions’ Framework*, JVLC 1996.

  The *cognitive dimensions* are the most well-known and widely used analytical
  framework for the usability of languages and interfaces. Perhaps their most
  valuable contribution to the field is providing a shared vocabulary for
  notational qualities that impact usability, something that will also be
  useful for us in this seminar. Besides this vocabulary, the framework
  emphasizes the trade-offs between dimensions.
  
  (Discussion leader: Tobias)
  </td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">4 Nov</td>
  <td class="schedule-info">
  Peyton Jones, Blackwell, and Burnett, *A User-Centered Approach to Functions
  in Excel*, ICFP 2003.

  This paper applies the two intellectual tools we have seen so far--the
  Attention Investment model and the Cognitive Dimensions--to the design of
  language extensions to Excel spreadsheets. It provides a nice illustration of
  how these tools can support a systematic, analytical approach to user-focused
  design.

  (Discussion leader: Daniel)
  </td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">11 Nov</td>
  <td class="schedule-info">
  Wilson et al., *Harnessing Curiosity to Increase Correctness in End-User
  Programming*, CHI 2003.

  This paper introduces the surprise-explain-reward strategy in the context of
  encouraging end-user programmers to test their programs. Attention investment
  provides a theory about how users decide where to spend their attention based
  on cost, risk, and reward. Surprise-explain-reward provides a strategy for
  altering this equation. Specifically, it attempts to lower the (perceived and
  actual) costs associated with learning a new feature, while making the reward
  more immediate and clear.

  (Discussion leader: Michael)
  </td>
</tr>
<tr class="schedule-row schedule-important">
  <td class="schedule-date">18 Nov</td>
  <td class="schedule-info">No meeting this week.</td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">25 Nov</td>
  <td class="schedule-info">
  Moody, *The "Physics" of Notations: Towards a Scientific Basis for
  Constructing Visual Notations in Software Engineering*, TSE 2009.

  The *physics of notations* is a theory for designing usable visual languages.
  It is founded on a synthesis and organization of a large amount of existing
  research on human perception and cognition. Compared to the cognitive
  dimensions, it is intended to be prescriptive, more objective, and
  falsifiable. However, it focuses mostly on picking effective graphical
  symbols, and so is less generally applicable than the cognitive dimensions
  and the other theories we have discussed.

  (Discussion leader: Jonathan)
  </td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">2 Dec</td>
  <td class="schedule-info">
  Continue discussion of the *Physics of Notation*.
  </td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">9 Dec</td>
  <td class="schedule-info">
  Burnett et al., *Gender Differences and Programming Environments: Across
  Programming Populations*, ESEM 2010.

  The field of [Gender HCI](http://en.wikipedia.org/wiki/Gender_HCI) is
  concerned with how males and females interact differently with computers (on
  average), and how to create software that takes these differences into
  account, thereby supporting a broader range of users and problem-solving
  strategies. I think this is an especially important issue for programming
  languages and tools since these are the keys to computing empowerment. This
  paper looks specifically at the intersection of Gender HCI and programming
  environments. The paper is also highly empirical, complementing the mostly
  theoretical papers we have read so far in this seminar.

  (Discussion leader: Björn)
  </td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">16 Dec</td>
  <td class="schedule-info">
  Pane and Myers, *More Natural Programming Languages and Environments*, in
  *End User Development*, Springer, 2006.

  A language can be called "natural" if it has a high closeness of mapping with
  the (possibly informal) way that (some group of) people think about and
  express solutions to programming-like problems. The term can be a red flag in
  human-centric research, since it is usually applied post hoc and without
  evidence. The [Natural Programming Project](http://www.cs.cmu.edu/~NatProg/),
  however, proposes a design methodology where designers first identify what is
  "natural" for users in a particular domain, then iteratively design and
  evaluate a language tailored to the way these potential users already think.
  While their focus is on non-programmers, the methodology can also be applied
  in other contexts. In contrast to the other design tools we have discussed
  this semester, the natural programming methodology is fundamentally
  user-centric and bottom-up.
  
  (Discussion leader: Stefan)
  </td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">13 Jan</td>
  <td class="schedule-info">
  Ko and Myers, *Designing the Whyline: A Debugging Interface for Asking
  Questions about Program Behavior*, CHI 2004.
  
  As part of the Natural Programming Project, Ko and Myers found that, when
  faced with unexpected results, both programmers and non-programmers formulate
  "why did ..." and "why didn't ..." questions about the program's behavior. With
  traditional debuggers, attempting to answer these questions is difficult and
  time-consuming, perhaps because the interaction with the system is in quite
  different terms (e.g. breakpoints, log files, code-stepping). The Whyline
  allows users to ask these kinds of questions directly through a process called
  *interrogative debugging*. This paper also illustrates how the cognitive
  dimensions can be used in *interaction design*, in addition to interface and
  language design.

  (Discussion leader: Eric)
  </td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">20 Jan</td>
  <td class="schedule-info">
  Coughlan and Johnson, *Interaction in Creative Tasks: Ideation,
  Representation and Evaluation in Composition*, CHI 2006.

  (Discussion leader: ???)
  </td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">27 Jan</td>
  <td class="schedule-info">
  Meyerovich and Rabkin, *Empirical Analysis of Programming Language Adoption*,
  OOPSLA 2013.
  
  (Discussion leader: ???)
  </td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">3 Feb</td>
  <td class="schedule-info">
  Design project presentations.
  </td>
</tr>
<tr class="schedule-row">
  <td class="schedule-date">10 Feb</td>
  <td class="schedule-info">
  Design project presentations.
  </td>
</tr>
</table>

