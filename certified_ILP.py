from sage.all import *
from sage.numerical.mip import MixedIntegerLinearProgram
from sage.graphs.graph_generators import graphs
from sage.graphs.graph_plot import DEFAULT_PLOT_OPTIONS

DEFAULT_PLOT_OPTIONS['figsize'] = [10,100]
def certified_ILP(graph):
    n = graph.order()

    p = MixedIntegerLinearProgram(maximization=False, solver="GLPK")
    x = p.new_variable(binary=True)
    a = p.new_variable(binary=True)  # Auxiliary variable for zero neighbors condition

    # Objective Function: Minimize the cardinality of D
    p.set_objective(sum(x[i] for i in graph.vertices()))

    # Constraint 1: Dominating Set
    for i in graph.vertices():
        p.add_constraint(x[i] + sum(x[j] for j in graph.neighbors(i)) >= 1)

    # Constraint 2: Certified Condition
    M = n  # A sufficiently large constant
    for i in graph.vertices():
        # Zero Neighbors Condition
        p.add_constraint(sum(1 - x[j] for j in graph.neighbors(i)) + M * a[i] >= 2 * x[i])

        # At Least Two Neighbors Condition
        p.add_constraint(sum(1 - x[j] for j in graph.neighbors(i)) + M * (a[i] - 1) + 2 <= 2 * x[i])

    # Solve the LP
    p.solve()
    D = [i for i in graph.vertices() if p.get_values(x[i]) == 1]
    return D

# # Create a sample graph
# # G = Graph([(1, 2), (2, 3), (3, 4)])
# G = graphs.RandomTree(500)
# # G = graphs.RandomGNP(500, 0.5)
# # Output the minimum certified dominating set
#G = Graph([(1,2),(1,3),(3,8),(8,18),(18,24),(3,7),(7,17),(7,16),(16,23),(16,22),(2,6),(6,15),(15,21),(21,27),(21,26),(27,29),(2,5),(5,14),(5,13),(5,12),(2,4),(4,11),(4,10),(4,9),(9,19),(10,20),(20,25),(25,28),(28,30)])
#certified_dominating_set = certified_ILP(G)
#colors = {'#FF0000': certified_dominating_set}
#G.plot(vertex_labels=True, vertex_colors=colors,layout='tree', tree_root=1).save("tree_mip2.png")
