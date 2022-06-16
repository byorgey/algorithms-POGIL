POGIL Activities for Algorithms
===============================

This is a collection of [POGIL](https://pogil.org/) activities
developed for [Algorithms (CSCI
382)](https://hendrix-cs.github.io/csci382/) at [Hendrix
College](https://www.hendrix.edu/).  Most were developed by [Brent
Yorgey](http://ozark.hendrix.edu/~yorgey/), with some contributions
also by [Gabe
Ferrer](https://www.hendrix.edu/mathcs/profile.aspx?id=70718).

Background
----------

At Hendrix, CSCI 382 is a required course for the Computer Science
major, taken mostly by juniors, with a handful of sophomores and
seniors.  Before taking the course, students must have as
prerequisites a Data Structures course (which itself has a CS1 course
as a prerequisite) and a Discrete Math course.  In general, these
materials assume that students already:

- are reasonably good programmers, with a good working grasp of
  things like functions, conditionals, loops, and recursion
- understand data structures like arrays, lists, stacks, queues,
  binary trees, dictionaries, and hash tables
- understand basic sorting and searching algorithms
- know basic logic and set theory
- have some experience writing formal mathematical proofs, including
  proofs by induction

Copyright
---------

<a rel="license"
href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative
Commons License" style="border-width:0"
src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br
/>All the materials in this repository are licensed under a <a rel="license"
href="http://creativecommons.org/licenses/by/4.0/">Creative Commons
Attribution 4.0 International License</a>.

Basically, this means you are free to use, modify, and/or republish
anything you find here for any purpose, as long as you attribute the
source (preferably with a link to this repository).

If you contribute to this repository (which you are welcome and
encouraged to do!) you agree for your contributions to be available
under the same license.

Sources
-------

XXX LaTeX, diagrams

List of activities
------------------

The eventual goal is for these activities to develop into an
officially endorsed collection.  In the meantime, they are in varying
states of development, ranging from some that are quite refined and
battle-tested, to some that were written hastily the day before class
and only used once.

The below table contains all the activities, listed in the order I
typically use them during a semester.  Each one lists
  - Other activities which are prerequisites
  - Whether the activity contains embedded code for diagrams.  This
    makes it much more difficult to build the LaTeX sources and hence
    difficult to modify.  Eventually I plan to revise all the
    activities to use standalone diagram generation code.
  - Whether a PDF of the activity has been checked into the repository.
  - Whether an answer key is available for the activity.
  - The status of the activity.  Statuses used include:
      - Unknown --- use at your own risk!
      - Needs revision --- not up to my own quality standards.
      - Revised --- has been recently revised and is at least up to my
        own quality standards, but has not necessarily gone through a
        review process.
      - Under review --- has been submitted for review through the
        [Pogil Activity
        Clearinghouse](http://pac.chem.pitt.edu/index.php/pac)
      - Endorsed --- officially endorsed by The POGIL Project.

| Activity                                                                                                               | Prerequisites                | Diagrams? | PDF?                                                                               | Key?                                                                                       | Status       |
|------------------------------------------------------------------------------------------------------------------------|------------------------------|-----------|------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------|--------------|
| [Intro to POGIL and CSCI 382](https://github.com/byorgey/algorithms-POGIL/tree/main/POGIL-intro)                       |                              |           |                                                                                    |                                                                                            | Unknown      |
| [Brute force algorithms](https://github.com/byorgey/algorithms-POGIL/tree/main/brute-force)                            |                              |           |                                                                                    |                                                                                            | Unknown      |
| [Reasons for POGIL](https://github.com/byorgey/algorithms-POGIL/tree/main/why-POGIL)                                   | Intro to POGIL               |           |                                                                                    |                                                                                            | Unknown      |
| [GCD analysis](https://github.com/byorgey/algorithms-POGIL/tree/main/GCD-analysis)                                     |                              |           |                                                                                    |                                                                                            | Unknown      |
| [Intro to asymptotic analysis](https://github.com/byorgey/algorithms-POGIL/tree/main/AA-intro)                         |                              |           | [PDF](https://github.com/byorgey/algorithms-POGIL/blob/main/AA-intro/AA-intro.pdf) | [Key](https://github.com/byorgey/algorithms-POGIL/blob/main/AA-intro/AA-intro-answers.pdf) | Under review |
| [Asymptotic analysis definitions](https://github.com/byorgey/algorithms-POGIL/tree/main/AA-definitions)                | Intro to AA                  |           |                                                                                    |                                                                                            | Unknown      |
| [Asmyptotic analysis limit theorems](https://github.com/byorgey/algorithms-POGIL/tree/main/AA-limits)                  | AA definitions               |           |                                                                                    |                                                                                            | Unknown      |
| [Three proofs](https://github.com/byorgey/algorithms-POGIL/tree/main/three-proofs)                                     | AA definitions               |           |                                                                                    |                                                                                            | Unknown      |
| [Intro to graphs](https://github.com/byorgey/algorithms-POGIL/tree/main/graphs)                                        |                              |           |                                                                                    |                                                                                            | Unknown      |
| [BFS applications and directed graphs](https://github.com/byorgey/algorithms-POGIL/tree/main/BFS-applications)         | Intro to graphs              |           |                                                                                    |                                                                                            | Unknown      |
| [Introduction to Dijkstra's Algorithm](https://github.com/byorgey/algorithms-POGIL/tree/main/Dijkstra-intro)           | Intro to graphs              |           |                                                                                    |                                                                                            | Unknown      |
| [Minimum Spanning Trees](https://github.com/byorgey/algorithms-POGIL/tree/main/MST)                                    | Intro to graphs              |           |                                                                                    |                                                                                            | Unknown      |
| [Kruskal's algorithm](https://github.com/byorgey/algorithms-POGIL/tree/main/Kruskal)                                   | MSTs                         |           |                                                                                    |                                                                                            | Unknown      |
| [Introduction to Divide & Conquer](https://github.com/byorgey/algorithms-POGIL/tree/main/divide-and-conquer-intro)     |                              |           |                                                                                    |                                                                                            | Unknown      |
| [Divide & Conquer Arithmetic](https://github.com/byorgey/algorithms-POGIL/tree/main/divide-and-conquer-arithmetic)     | Intro to D&C                 |           |                                                                                    |                                                                                            | Unknown      |
| [Selection](https://github.com/byorgey/algorithms-POGIL/tree/main/selection)                                           | Intro to D&C                 |           |                                                                                    |                                                                                            | Unknown      |
| [Introduction to Dynamic Programming](https://github.com/byorgey/algorithms-POGIL/tree/main/dynamic-programming-intro) |                              |           |                                                                                    |                                                                                            | Unknown      |
| [2D dynamic programming (subset sum)](https://github.com/byorgey/algorithms-POGIL/tree/main/2D-dynamic-programming)    | Intro to DP                  |           |                                                                                    |                                                                                            | Unknown      |
| [Floyd-Warshall](https://github.com/byorgey/algorithms-POGIL/tree/main/floyd-warshall)                                 | 2D dynamic programming       |           |                                                                                    |                                                                                            | Unknown      |
| [Introduction to flow networks](https://github.com/byorgey/algorithms-POGIL/tree/main/flow-intro)                      |                              |           |                                                                                    |                                                                                            | Unknown      |
| [Max flow](https://github.com/byorgey/algorithms-POGIL/tree/main/max-flow)                                             | Intro to flow networks       |           |                                                                                    |                                                                                            | Unknown      |
| [Introduction to amortized analysis](https://github.com/byorgey/algorithms-POGIL/tree/main/amortized-intro)            |                              |           |                                                                                    |                                                                                            | Unknown      |
| [Amortized analysis of arrays](https://github.com/byorgey/algorithms-POGIL/tree/main/amortized-array)                  | Intro to amortized analysis  |           |                                                                                    |                                                                                            | Unknown      |
| [Binomial heaps](https://github.com/byorgey/algorithms-POGIL/tree/main/amortized-binomial-heap)                        | Amortized analysis of arrays |           |                                                                                    |                                                                                            | Unknown      |
| [Introduction to reductions](https://github.com/byorgey/algorithms-POGIL/tree/main/reductions)                         |                              |           |                                                                                    |                                                                                            | Unknown      |
| [SAT and 3-SAT](https://github.com/byorgey/algorithms-POGIL/tree/main/SAT)                                             | Intro to reductions          |           |                                                                                    |                                                                                            | Unknown      |
| [Linear sorting](https://github.com/byorgey/algorithms-POGIL/tree/main/linear-sorting)                                 |                              |           |                                                                                    |                                                                                            | Unknown      |

