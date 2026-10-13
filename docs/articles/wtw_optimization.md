# Optimization

## Introduction

This article is intended for users already familiar with the basic
functionality of the *Where to Work* tool. It is recommended to first
read the [official
manual](https://ncc-cnc.github.io/wheretowork/articles/wheretowork.md)
before proceeding.

The goal of this article is to provide an understanding of the
optimization procedures that the *Where to Work* tool uses to generate
prioritizations. Specifically, the *Where to Work* tool uses the
[prioritizr R package](https://prioritizr.net/) to generate
prioritizations with exact algorithm solvers (i.e.,
[Gurobi](https://www.gurobi.com/) and
[CBC](https://github.com/coin-or/Cbc). Briefly, this article will (i)
provide an overview of exact algorithms, (ii) discuss the advantages and
disadvantageous of exact algorithms, (iii) provide an explanation for
how one exact algorithm works (i.e., the branch and bound algorithm),
and (iv) provide resources for learning more about these exact
algorithms. Although it is beyond the scope of this article to provide a
detailed understanding of the inner workings of all exact algorithms,
this article intends to provide enough information so that users of the
*Where to Work* tool can feel comfortable using these algorithms.

## Overview of exact algorithms

Exact algorithms are a class of algorithms that guarantee optimality (A.
S. L. Rodrigues & Gaston, 2002; Underhill, 1994). There are many
different algorithms that are classified as exact algorithms, including
the branch and bound algorithm (Land & Doig, 1960), branch and cut
algorithm (Padberg & Giovanni, 1991), simplex algorithm (Dantzig, 1951),
and interior point algorithm (Dikin, 1967). Broadly speaking, exact
algorithms can solve various types of optimization problems. For
example, the simplex algorithm can solve linear optimization problems
that contain only continuous decision variables (termed “linear
programming problems”). Additionally, the branch and bound algorithm can
solve linear optimization problems that contain only integer decision
variables (termed “integer programming problems”) and those that contain
both integer and continuous decision variables (termed “mixed integer
programming problems”). These algorithms have been applied to many
different tasks, such as facility location (Canel & Khumawala, 1996),
job scheduling (Beaumont, 1997), and route optimization (Schouwenaars et
al., 2001). They are also becoming increasingly popular in conservation
planning (Hanson et al., 2019; A. S. Rodrigues et al., 2000; Schuster et
al., 2020).

The *Where to Work* tool uses exact algorithms to generate
prioritizations. To achieve this, it formulates conservation planning
problems using the minimum set (A. S. Rodrigues et al., 2000) or minimum
shortfall (Jung et al., 2021) reserve selection problems (depending on
whether a budget is specified or not) and then interfaces with external
optimization software to solve problems (termed “exact algorithm
solvers”). Currently, the *Where to Work* tool can use the
[Gurobi](https://www.gurobi.com/) and
[CBC](https://github.com/coin-or/Cbc) exact algorithm solvers (note that
Gurobi is not publicly available and only available with advanced user
access). Broadly speaking, these solvers often support multiple
different algorithms (e.g., they might support both the simplex
algorithm and branch and bound algorithm) so they can be applied to a
wide variety of optimization problems. For example, the Gurobi software
supports many different algorithms, including the branch and bound
algorithm, barrier algorithm, interior point algorithm and various
simplex algorithms (see <https://www.gurobi.com/>). Additionally, exact
algorithm solvers may also use heuristic algorithms – a class of
iterative algorithms that do not guarantee optimality – in conjunction
with other algorithms to augment the optimization process (e.g.,
Achterberg & Berthold, 2007). They may also apply presolve algorithms –
a class of algorithms designed to simplify and reduce the size of
optimization problems – to speedup the subsequent optimization process
(Achterberg et al., 2020). Although it is beyond the scope of this
article to describe exactly which algorithms a particular solver will
apply to a given optimization problem, this has hopefully provided an
overview of the algorithms that they employ.

## Advantages and disadvantages of exact algorithm solvers

There are numerous advantages with using exact algorithm solvers. A key
advantage is that exact algorithms provide guarantees on solution
quality. For example, if required, they can solve problems to
optimality. In other words, they can identify the best possible solution
to a mathematical optimization problem. In real conservation decisions,
this can sometimes make a large difference in outcome (Schuster et al.,
2020). Solving to optimality can also reassure decision makers that
proposed solutions provide the best possible outcomes, given the data
and objectives they have entered into the system. Also, if strict
optimality is not required, they can solve problems to within a
pre-specified optimality gap (e.g., identify a solution that is within
10% of optimality). This can be useful solvers are often able to
generate near optimal solutions much more quickly than strictly optimal
solutions, whilst still providing guarantees on solution quality.
Although other types of algorithms can be applied to conservation
problems (e.g., heuristic or meta-heuristic algorithms; J. B.
Kirkpatrick, 1983; Ciarleglio et al., 2008; S. Kirkpatrick et al., 1983;
Nicholls & Margules, 1993), they do not provide such guarantees and, as
such, may require re-running the optimization process tens of thousands
of times to verify solution quality (Ardron et al., 2010). Additionally,
exact algorithm solvers can be readily applied to a wide variety of
problems. Because the *Where to Work* tool uses exact algorithm solvers,
it can easily accommodate a variety of different conservation planning
problems. For instance, the *Where to Work* tool can be used to generate
(i) prioritizations that minimize cost whilst ensuring all goals are met
(i.e., a “minimum set reserve selection problem”) and (ii)
prioritizations that aim to reach as many goals as possible, whilst
ensuring that the total selected area does not exceed a pre-specified
budget (i.e., a “budget-limited reserve selection problem”).
Furthermore, another advantage of using exact algorithm solvers is that
they are steadily increasing in performance over time (Achterberg &
Wunderling, 2013). By interfacing with exact algorithm solvers, this
means that the *Where to Work* tool leverages such improvements as newer
versions of the solvers become available over time.

There are also several disadvantages with using exact algorithm solvers.
A key disadvantage is that the best performing exact algorithm solvers
are commercial software (e.g., [Gurobi](https://www.gurobi.com/) and
[IBM
CPLEX](https://www.ibm.com/products/ilog-cplex-optimization-studio)).
Although open source exact algorithm solvers are available (e.g.,
[CBC](https://github.com/coin-or/Cbc), [HiGHS](https://highs.dev/), and
[SYMPHONY](https://github.com/coin-or/SYMPHONY)), they generally cannot
solve optimization problems as quickly as the commercial solvers and
they might not be able to solve particularly large problems (Schuster et
al., 2020). Commercial solvers often provide special free licenses for
academic use (e.g.,
[Gurobi](https://www.gurobi.com/academia/academic-program-and-licenses/)
and [IBM
CPLEX](https://www.ibm.com/support/pages/ibm-ilog-optimization-academic-initiative));
however, commercial licenses may be required for use by governmental,
commercial, and non-governmental organizations. As such, organizations
may need to carefully consider the potential benefits and costs
associated with using commercial solvers. Another disadvantage is that
exact algorithms can be much slower than other types of algorithms. For
example, some heuristic algorithms – while they cannot guarantee
solution quality – can generate near-optimal solutions to optimization
problems faster than exact algorithms (Pressey et al., 1997; Zanakis &
Evans, 1981). Indeed, the Zonation decision support tool – which is
powered by iterative algorithms (Moilanen et al., 2022) – can likely
solve budget-limited reserve selection problems faster than the *Where
to Work* tool. Finally, it can be challenging to solve large-scale
non-linear problems with exact algorithms \[e.g., problems that include
complex metrics for connectivity; Keeley et al. (2021)\]. Although this
means that they might not be able to solve particularly complex
mathematical optimization problems (Moilanen, 2008), exact algorithm
solvers can be applied to many conservation planning problems (e.g.,
Hanson et al., 2019; A. S. Rodrigues et al., 2000).

## Explanation of how an exact algorithm works

Here we will explain how the branch and bound algorithm – one of the
earliest exact algorithms – can be used to solve optimization problems
(Land & Doig, 1960). The main aim of this section is to explain how it
is possible for an algorithm to guarantee optimality, without iterating
over every single possible solution. Since this article is intended for
an audience that does not have a strong mathematical background, we will
only provide basic understanding of the algorithm.

For ease of interpretation, we will begin with an intuitive description.
The branch and bound algorithm involves iteratively subdividing an
optimization problem into a hierarchical tree of sub-problems. Here, the
base (or stump) is the original optimization problem, each branch is a
different sub-problem, and the tips of the branches are different
solutions to the original problem. By exploiting the hierarchical
structure of the tree, the algorithm can effectively search for an
optimal solution without having to examine each and every possible
solution (i.e., tip). Indeed, the algorithm can examine particular
branches and determine if an optimal solution will not be found in any
of the tips connected to the branch. The algorithm terminates once it
has explored enough of the tree to identify an optimal solution to the
original problem. Hopefully, this intuitive description is useful.
However, if further details are required, we will provide a more
thorough description below.

We will define some terminology to aid in describing the algorithm. An
*objective value* is a measure that quantifies the quality of a
candidate solution (e.g., the cost of a set of a prioritization, or how
well a prioritization safeguards a variety of species). Also, an
*optimal solution* is a solution which has the best possible objective
value to a particular problem, whilst meeting all the constraints (e.g.,
the cost for a prioritization must be below a particular budget
threshold) and variable restrictions (e.g., particular variables such as
“purchase” or “do not purchase” must be integer or binary) defined for
the problem. Additionally, an *incumbent solution* is the best solution
found so far during the course of an optimization process, which is
sometimes initially estimated using a heuristic algorithm. Finally, a
*bound for the optimal solution* is the best estimate for the objective
value of the optimal solution. During the course of an optimization
process, the *bound for the optimal solution* will become more accurate.
Also, note that the *bound for the optimal solution* is mathematically
defined to be equal to, or better than, the objective value of the
optimal solution (e.g., in a maximization problem, it will be greater
than the objective value of the optimal solution). After defining these
terms, we can begin describing the branch and bound algorithm in more
detail.

The branch and bound algorithm starts by solving a relaxed version of
the original optimization problem. In particular, a problem that
contains integer (or binary) variables can be relaxed to create a new
problem by substituting the integer (or binary) variables for continuous
variables. For example, if we had a problem that contained an integer
variable ranging between 1 and 5, the relaxed version of this problem
could contain a new continuous variable ranging between 1.0 and 5.0.
Since problems that contain only continuous variables are much easier to
solve, the relaxed problem can be solved relatively quickly. If the
solution to the relaxed problem meets the integer (or binary)
restrictions to the original problem, then the *optimal solution* to the
original problem has been found. However, this is rarely the case.

The solution to the relaxed problem will likely contain non-integer (or
non-binary) values for the integer (or binary) variables. As such, the
solution to the relaxed problem is not considered *feasible* (because it
contains non-integer values for “purchase” or “do not purchase”
variables that must contain integer or binary values), and it is not the
*optimal solution* to the original problem. However, this does not mean
that the solution is useless. Because the *objective value* for the
solution to the relaxed problem will always be better than (or equal to)
the *objective value* for the *optimal solution* to the original
solution, the *objective value* for the solution to the relaxed problem
can be used to set an initial estimate for the *bound for the optimal
solution*. As we will see in subsequent procedures, the *bound for the
optimal solution* will be updated and refined during the course of the
optimization process.

The solution to the relaxed problem can be used to subdivide the relaxed
problem into a series of sub-problems. These sub-problems can be viewed
as hierarchically subordinate “children” to the “parent” problem. For
example, if the solution to the relaxed problem has a value of 4.3 for
an integer variable, then this variable could be used to create two
sub-problems by (i) adding a constraint specifying this variable must be
$`<= 4`$ and (ii) adding a constraint specifying this variable must be
$`>= 5`$. If the solution to the relaxed problem contains many values
that do not meet the integer (or binary) restrictions for the variables,
there might be a very large number of sub-problems. The algorithm will
then decide on a particular sub-problem to explore further. Indeed, the
methods used to determine which sub-problem to explore further have an
enormous impact on run time, and fast exact algorithm solvers will have
algorithms dedicated to help speed up this process.

The algorithm will then solve the chosen sub-problem. Although the
sub-problem has one (or potentially multiple) additional constraint(s),
it is still a relaxed version of the original problem and can be solved
relatively quickly. Depending on the solution to the sub-problem, one of
the three following procedures will be applied. First, if (i) the
solution to the sub-problem contains values that meet all the integer
(or binary) restrictions for the variables and (ii) the objective value
for the solution is better than the *objective value for the incumbent
solution*, then the solution is earmarked as the new *incumbent
solution*. Second, if (i) the solution to the sub-problem contains
values that do not meet all the integer (or binary) restrictions for the
variables and (ii) the objective value for the solution does not provide
a better *bound for the optimal solution*, then the sub-problem is
abandoned and another sub-problem is selected as a chosen sub-problem.
Third, if (i) the solution to the sub-problem contains values that do
not meet all the integer (or binary) restrictions for the variables and
(ii) the objective value for the solution provides a better *bound for
the optimal solution*, then the objective value for the solution is used
to update the *bound for the optimal solution* and the sub-problem is
subdivided into further sub-sub-problems. One of these sub-sub-problems
is then selected as the new chosen sub-problem, and the procedures
previously outlined in this paragraph are applied, recursively, to the
new chosen sub-problem.

During the course of the optimization process, the *bound for the
optimal solution* and the *objective value* for the *incumbent solution*
will become more similar to each other. Indeed, as sub-problems are
solved to obtain better estimates of the *bound for the optimal
solution*, the *bound for the optimal solution* will get closer to the
*objective value* for the *incumbent solution*. Additionally, as
sub-problems are solved to obtain better quality *feasible* solutions,
the *objective value* for the *incumbent solution* will improve. For
example, in a maximization problem, the *bound for the optimal solution*
will typically start with a relatively high value (i.e., reflecting an
overly high initial estimate of the optimal *objective value*) and
decrease as sub-problems are explored and the bound is refined. Whereas,
the *objective value* for the *incumbent solution* will start with a low
value (i.e., reflecting a poor quality solution to the original
optimization problem) and increase as solving sub-problems yields better
quality *feasible* solutions.

The algorithm terminates when the *bound for the optimal solution* and
the *objective value* for the *incumbent solution* both have very
similar values. In particular, the algorithm will terminate when the
relative difference expressed as a percentage (i.e., optimality gap)
between the *bound for the optimal solution* and the *objective value*
for the *incumbent solution* meets a pre-specified threshold. For
example, if a 0% optimality gap is specified, then the algorithm
terminates when the *bound for the optimal solution* and the *objective
value for the incumbent solution* are equal to each other. The *Where to
Work* tool applies a two stage optimization process, where it uses an
optimality gap of 10% in the first stage where it focuses on maximizing
species’ representation or minimizing cost (if using a budget or not,
receptively) and an optimality gap of 15% to subsequently minimize
spatial fragmentation (if specified). When the algorithm terminates, it
will output the *incumbent solution*. This is because the *incumbent
solution* is the best solution found that meets the integer (or binary)
restrictions for the variables.

## Resources for learning more about exact algorithms

Here we will provide resources for learning more about exact algorithms.
In particular, Gurobi Optimization – the developer of the [Gurobi exact
algorithm solver](https://www.gurobi.com/) – has created numerous online
resources to help its users understand exact algorithms. These resources
include [online guides](https://www.gurobi.com/resource-center/) and
[videos](https://www.youtube.com/@GurobiVideos). They have been created
by experts in mathematical optimization and provide detailed
explanations. Among these resources is a [webinar series on using exact
algorithms to solve mixed integer programming problems (i.e., those that
contain both continuous and integer (or binary
variables)](https://www.youtube.com/watch?v=mUjkCeLx6_4&list=PLHiHZENG6W8Dt7ub1H1RsF4xXB_YtxXv-&pp=iAQB)
and a [webinar series on using exact algorithms to solve linear
programming problems (i.e., those that contain only continuous
variables)](https://www.youtube.com/watch?v=U-2CdTe-j2Q&list=PLHiHZENG6W8BeAfJfZ3myo5dsSQjEV5pJ&pp=iAQB).
Although watching both of these webinar series will be useful for better
understanding the algorithms that underpin the *Where to Work* tool,
readers that are interested in an illustrative example of the branch and
bound algorithm (discussed in the previous section) may find the [video
on this
algorithm](https://youtu.be/CdfZ4XOpSho?list=PLHiHZENG6W8Dt7ub1H1RsF4xXB_YtxXv-)
of particular interest.

## References

Achterberg, T., & Berthold, T. (2007). Improving the feasibility pump.
*Discrete Optimization*, *4*(1), 77–86.

Achterberg, T., Bixby, R. E., Gu, Z., Rothberg, E., & Weninger, D.
(2020). Presolve reductions in mixed integer programming. *INFORMS
Journal on Computing*, *32*(2), 473–506.

Achterberg, T., & Wunderling, R. (2013). Mixed Integer Programming:
Analyzing 12 Years of Progress. In M. Jünger & G. Reinelt (Eds.),
*Facets of combinatorial optimization: Festschrift for martin grötschel*
(pp. 449–481). Springer.

Ardron, J. A., Possingham, H. P., & Klein, C. J. (2010). *Marxan Good
Practices Handbook* ((2nd version)). Pacific Marine Analysis; Research
Association.

Beaumont, N. (1997). Scheduling staff using mixed integer programming.
*European Journal of Operational Research*, *98*(3), 473–484.

Canel, C., & Khumawala, B. M. (1996). A mixed-integer programming
approach for the international facilities location problem.
*International Journal of Operations and Production Management*,
*16*(4), 49–68.

Ciarleglio, M., Barnes, J. W., & Sarkar, S. (2008). ConsNet—A tabu
search approach to the spatially coherent conservation area network
design problem. *Journal of Heuristics*, *16*(4), 537–557.

Dantzig, G. B. (1951). Maximization of a linear function of variables
subject to linear inequalities. In *Activity Analysis of Production and
Allocation—Proceedings of a Conference* (Vol. 13, pp. 339–334). Wiley.

Dikin, I. I. (1967). Iterative solution of problems of linear and
quadratic programming. *Doklady Akademii Nauk*, *174*(4), 747–748.

Hanson, J. O., Schuster, R., Strimas‐Mackey, M., & Bennett, J. R.
(2019). Optimality in prioritizing conservation projects. *Methods in
Ecology and Evolution*, *10*(10), 1655–1663.

Jung, M., Arnell, A., Lamo, X. de, García-Rangel, S., Lewis, M., Mark,
J., Merow, C., Miles, L., Ondo, I., Pironon, S., Ravilious, C., Rivers,
M., Schepaschenko, D., Tallowin, O., Soesbergen, A. van, Govaerts, R.,
Boyle, B. L., Enquist, B. J., Feng, X., … Visconti, P. (2021). Areas of
global importance for conserving terrestrial biodiversity, carbon and
water. *Nature Ecology &Amp; Evolution*, *5*(11), 1499–1509.

Keeley, A. T. H., Beier, P., & Jenness, J. S. (2021). Connectivity
metrics for conservation planning and monitoring. *Biological
Conservation*, *255*, 109008.
<http://dx.doi.org/10.1016/j.biocon.2021.109008>

Kirkpatrick, J. B. (1983). An iterative method for establishing
priorities for the selection of nature reserves: An example from
tasmania. *Biological Conservation*, *25*(2), 127–134.

Kirkpatrick, S., Gelatt, C. D., & Vecchi, M. P. (1983). Optimization by
simulated annealing. *Science*, *220*(4598), 671–680.

Land, A. H., & Doig, A. G. (1960). An automatic method of solving
discrete programming problems. *Econometrica*, *28*(3), 497–520.

Moilanen, A. (2008). Two paths to a suboptimal solution – once more
about optimality in reserve selection. *Biological Conservation*,
*141*(7), 1919–1923.

Moilanen, A., Lehtinen, P., Kohonen, I., Jalkanen, J., Virtanen, E. A.,
& Kujala, H. (2022). Novel methods for spatial prioritization with
applications in conservation, land use planning and ecological impact
avoidance. *Methods in Ecology and Evolution*, *13*(5), 1062–1072.

Nicholls, A. O., & Margules, C. R. (1993). An upgraded reserve selection
algorithm. *Biological Conservation*, *64*(2), 165–169.

Padberg, M., & Giovanni, R. (1991). A branch-and-cut algorithm for the
resolution of large-scale symmetric traveling salesman problems. *SIAM
Review*, *33*(1), 497–520.

Pressey, R. L., Possingham, H. P., & Day, J. R. (1997). Effectiveness of
alternative heuristic algorithms for identifying indicative minimum
requirements for conservation reserves. *Biological Conservation*,
*80*(2), 207–219.

Rodrigues, A. S. L., & Gaston, K. J. (2002). Optimisation in reserve
selection procedures—why not? *Biological Conservation*, *107*(1),
123–129.

Rodrigues, A. S., Orestes Cerdeira, J., & Gaston, K. J. (2000).
Flexibility, efficiency, and accountability: Adapting reserve selection
algorithms to more complex conservation problems. *Ecography*, *23*(5),
565–574.

Schouwenaars, T., De Moor, B., Feron, E., & How, J. (2001). Mixed
integer programming for multi-vehicle path planning. In *2001 European
Control Conference (ECC)* (pp. 2603–2608). Porto, Portugal.
<https://doi.org/10.23919/ECC.2001.7076321>

Schuster, R., Hanson, J. O., Strimas-Mackey, M., & Bennett, J. R.
(2020). Exact integer linear programming solvers outperform simulated
annealing for solving conservation planning problems. *PeerJ*, *8*,
e9258.

Underhill, L. G. (1994). Optimal and suboptimal reserve selection
algorithms. *Biological Conservation*, *70*(1), 85–87.

Zanakis, S. H., & Evans, J. R. (1981). Heuristic "optimization", why,
when, and how to use it. *Interfaces*, *11*(5), 84–91.
