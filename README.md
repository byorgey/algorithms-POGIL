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

Using and modifying activites
-----------------------------

For activities which have a PDF checked into the repository, you are
welcome to simply use the PDF as-is.  I have tried to ensure that all
such PDFs do not contain details specific to my course (such as
dates, the course number, etc.) so you should be able to use it in
your own course.

If a PDF is not available, or you wish to modify the activity for your
own needs, you are welcome to download the provided LaTeX source and
modify/build it yourself.  Note that you will also need the
[`algo-activity.sty`
style file](https://github.com/byorgey/algorithms-POGIL/blob/main/algo-activity.sty)
in order to build the sources.

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
      - `?` --- unknown, use at your own risk!
      - Needs revision --- not up to my own quality standards.
      - Revised --- has been recently revised and is at least up to my
        own quality standards, but has not necessarily gone through a
        review process.
      - Under review --- has been submitted for review through the
        [POGIL Activity
        Clearinghouse](http://pac.chem.pitt.edu/index.php/pac)
      - Endorsed --- officially endorsed by The POGIL Project.

| Activity                                                                                                               | Prerequisites                | Diagrams? | PDF?                                                                               | Key?                                                                                       | Status         |
|------------------------------------------------------------------------------------------------------------------------|------------------------------|-----------|------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------|----------------|
| [Intro to POGIL and CSCI 382](https://github.com/byorgey/algorithms-POGIL/tree/main/POGIL-intro)                       |                              |           |                                                                                    |                                                                                            | ?              |
| [Brute force algorithms](https://github.com/byorgey/algorithms-POGIL/tree/main/brute-force)                            |                              |           |                                                                                    |                                                                                            | ?              |
| [Reasons for POGIL](https://github.com/byorgey/algorithms-POGIL/tree/main/why-POGIL)                                   | Intro to POGIL               |           |                                                                                    |                                                                                            | ?              |
| [GCD analysis](https://github.com/byorgey/algorithms-POGIL/tree/main/GCD-analysis)                                     |                              |           |                                                                                    |                                                                                            | ?              |
| [Intro to asymptotic analysis](https://github.com/byorgey/algorithms-POGIL/tree/main/AA-intro)                         |                              |           | [PDF](https://github.com/byorgey/algorithms-POGIL/blob/main/AA-intro/AA-intro.pdf) | [Key](https://github.com/byorgey/algorithms-POGIL/blob/main/AA-intro/AA-intro-answers.pdf) | Under review   |
| [Asymptotic analysis definitions](https://github.com/byorgey/algorithms-POGIL/tree/main/AA-definitions)                | Intro to AA                  |           |                                                                                    |                                                                                            | ?              |
| [Asmyptotic analysis limit theorems](https://github.com/byorgey/algorithms-POGIL/tree/main/AA-limits)                  | AA definitions               |           |                                                                                    |                                                                                            | ?              |
| [Three proofs](https://github.com/byorgey/algorithms-POGIL/tree/main/three-proofs)                                     | AA definitions               | embedded  |                                                                                    |                                                                                            | ?              |
| [Intro to graphs](https://github.com/byorgey/algorithms-POGIL/tree/main/graphs)                                        |                              | embedded  |                                                                                    |                                                                                            | ?              |
| [BFS applications and directed graphs](https://github.com/byorgey/algorithms-POGIL/tree/main/BFS-applications)         | Intro to graphs              | embedded  |                                                                                    |                                                                                            | ?              |
| [Introduction to Dijkstra's Algorithm](https://github.com/byorgey/algorithms-POGIL/tree/main/Dijkstra-intro)           | Intro to graphs              | embedded  |                                                                                    |                                                                                            | ?              |
| [Minimum Spanning Trees](https://github.com/byorgey/algorithms-POGIL/tree/main/MST)                                    | Intro to graphs              | embedded  |                                                                                    |                                                                                            | ?              |
| [Kruskal's algorithm](https://github.com/byorgey/algorithms-POGIL/tree/main/Kruskal)                                   | MSTs                         | embedded  |                                                                                    |                                                                                            | ?              |
| [Introduction to Divide & Conquer](https://github.com/byorgey/algorithms-POGIL/tree/main/divide-and-conquer-intro)     |                              | embedded  |                                                                                    |                                                                                            | ?              |
| [Divide & Conquer Arithmetic](https://github.com/byorgey/algorithms-POGIL/tree/main/divide-and-conquer-arithmetic)     | Intro to D&C                 |           |                                                                                    |                                                                                            | ?              |
| [Selection](https://github.com/byorgey/algorithms-POGIL/tree/main/selection)                                           | Intro to D&C                 |           |                                                                                    |                                                                                            | ?              |
| [Introduction to Dynamic Programming](https://github.com/byorgey/algorithms-POGIL/tree/main/dynamic-programming-intro) |                              |           |                                                                                    |                                                                                            | Needs revision |
| [2D dynamic programming (subset sum)](https://github.com/byorgey/algorithms-POGIL/tree/main/2D-dynamic-programming)    | Intro to DP                  |           |                                                                                    |                                                                                            | ?              |
| [Floyd-Warshall](https://github.com/byorgey/algorithms-POGIL/tree/main/floyd-warshall)                                 | 2D dynamic programming       |           |                                                                                    |                                                                                            | ?              |
| [Introduction to flow networks](https://github.com/byorgey/algorithms-POGIL/tree/main/flow-intro)                      |                              | embedded  |                                                                                    |                                                                                            | ?              |
| [Max flow](https://github.com/byorgey/algorithms-POGIL/tree/main/max-flow)                                             | Intro to flow networks       |           |                                                                                    |                                                                                            | ?              |
| [Introduction to amortized analysis](https://github.com/byorgey/algorithms-POGIL/tree/main/amortized-intro)            |                              | embedded  |                                                                                    |                                                                                            | Needs revision |
| [Amortized analysis of arrays](https://github.com/byorgey/algorithms-POGIL/tree/main/amortized-array)                  | Intro to amortized analysis  |           |                                                                                    |                                                                                            | ?              |
| [Binomial heaps](https://github.com/byorgey/algorithms-POGIL/tree/main/amortized-binomial-heap)                        | Amortized analysis of arrays |           |                                                                                    |                                                                                            | ?              |
| [Introduction to reductions](https://github.com/byorgey/algorithms-POGIL/tree/main/reductions)                         |                              | embedded  |                                                                                    |                                                                                            | ?              |
| [SAT and 3-SAT](https://github.com/byorgey/algorithms-POGIL/tree/main/SAT)                                             | Intro to reductions          | embedded  |                                                                                    |                                                                                            | ?              |
| [Linear sorting](https://github.com/byorgey/algorithms-POGIL/tree/main/linear-sorting)                                 |                              | embedded  |                                                                                    |                                                                                            | ?              |

