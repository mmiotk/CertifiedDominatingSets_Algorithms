from sage.all import *
from sage.graphs.graph_generators import graphs
from sage.graphs.graph_plot import DEFAULT_PLOT_OPTIONS
import random
from collections import defaultdict

DEFAULT_PLOT_OPTIONS['figsize'] = [10,100]

global f
def random_non_leaf_vertex(T):
    non_leaf_vertices = [v for v in T.vertices() if T.degree(v) > 1]
    return random.choice(non_leaf_vertices)

def is_leaf(T, vertex):
    return T.degree(vertex) == 1

def weak_supports(T):
    return {v for v in T.vertices() if sum(is_leaf(T, u) for u in T.neighbors(v)) == 1}
def support_vertices(T):
    return {neighbor for vertex in T.vertices() if T.degree(vertex) == 1 for neighbor in T.neighbors(vertex)}

def relabel_vertices(T, non_leaf):
    T_relabel = T.copy()
    T_dfs_order = T.breadth_first_search(non_leaf)
    T_dfs_order = list(T_dfs_order)
    dict = {}
    for i in range(len(T_dfs_order)):
        if T_dfs_order[i] == non_leaf:
            new_leaf = i + 1
        dict[T_dfs_order[i]] = i + 1
    T_relabel.relabel(dict, inplace=True)
    return (T_relabel,new_leaf)

def get_children(T,x):
    return [u for u in T.neighbors(x) if u > x]

def get_parent(T,x):
    try:
        if x != 1:
            return [u for u in T.neighbors(x) if u < x][0]
    except:
        print(f'x={x}')
        print(f'T={T}')


from collections import defaultdict

def phase0(T):

    F = defaultdict(lambda: {"leaf": 0, "add": None, "n_0": 0, "n_1": 0, "n_2": 0 ,"n_22":0, "sw": 0, "sta": ""})

    nodes = list(T.vertices())
    for v in nodes:
        F[v]["leaf"] = 0
        F[v]["add"] = None
        F[v]["n_0"] = 0
        F[v]["n_1"] = 0
        F[v]["n_2"] = 0
        F[v]["n_22"] = 0
        F[v]["sw"] = 0
        F[v]["sta"] = ""

    for v in reversed(nodes):
            degree = T.degree(v)
            if degree == 1:
                F[v]['sta'] = 'L'
                F[get_parent(T,v)]["leaf"] += 1
                F[get_parent(T,v)]["add"] = v
            if F[v]["leaf"] == 1:
                F[v]['sta'] = 'SW'
            if F[v]["leaf"] > 1:
                F[v]['sta'] = 'S'
    return F

def phase1(T, G):
    F = G.copy()
    nodes = list(T.vertices())
    leaves = [v for v in F if F[v]['sta'] == 'L']
    root = nodes[0]
    for v in reversed([node for node in nodes if node not in leaves]):
        # if v is not root:
            if F[v]["leaf"] > 0:
                if F[v]["n_0"] + F[v]["n_1"] + F[v]["n_22"] == 0 and F[v]["leaf"] == 1:
                    F[get_parent(T, v)]["sw"] += 1
                else:
                    F[v]['sta'] = 'S'
                F[get_parent(T, v)]["n_2"] += 1
            else:
                if F[v]["n_0"] == 0:
                    if F[v]["n_2"] > 0:
                      F[v]['sta'] = 1
                      F[get_parent(T,v)]["n_1"] += 1
                    else:
                      F[v]['sta'] = 0
                      F[get_parent(T, v)]["n_0"] += 1
                else:
                    if F[v]["n_0"] == 1 and F[v]["n_1"] + F[v]['sw'] == 0:
                      F[v]['sta'] = 22
                      F[get_parent(T,v)]["n_2"] += 1
                      F[get_parent(T,v)]["n_22"] += 1
                    else:
                      if F[get_parent(T,v)]['sta'] == 'SW':
                         f = 1
                      else:
                         f = 0
                      if F[v]["n_0"] > (F[v]["sw"] + f):
                        F[v]['sta'] = 2
                        F[get_parent(T,v)]["n_2"] += 1
                      else:
                        F[v]['sta'] = 11
                        F[get_parent(T,v)]["n_1"] += 1

    if F[root]["n_2"] == 0:
        F[root]['sta'] = 2

    return F

def phase2(T, G):

    F = G.copy()
    nodes = list(T.vertices())
    leaves = [v for v in F if F[v]['sta'] == 'L']

    for v in reversed([node for node in nodes]):
         if v != nodes[0]:  # not the root
                if F[v]['sta'] == 'SW' and F[get_parent(T,v)]['sta'] in {2, 'SW', 'S'}:
                    child = F[v]['add']
                    F[child]['sta'] = 2
                    F[v]['n_2'] += 1
                if F[v]['sta'] == 0 and F[get_parent(T,v)]['sta'] == 11:
                    F[v]['sta'] = 2
                    F[get_parent(T, v)]["n_2"] += 1
                if F[v]['sta'] == 0 and F[get_parent(T,v)]['sta'] == 22:
                    if F[get_parent(T,get_parent(T,v))]['sta'] in {2,22, 'SW', 'S'}: 
                      F[v]['sta'] = 2
                      F[get_parent(T,v)]['sta'] = 1
                      F[get_parent(T, v)]["n_2"] += 1
                      F[get_parent(T, get_parent(T,v))]["n_1"] += 1
                      F[get_parent(T, get_parent(T, v))]["n_2"] -= 1
                      if F[get_parent(T,get_parent(T,v))]['sta'] == 'SW':
                          F[get_parent(T, get_parent(T, v))]['sta'] = 'S'
                if F[v]['sta'] == 11:
                    F[v]['sta'] = 1
                if F[v]['sta'] == 22:
                    F[v]['sta'] = 2

    root = nodes[0]
    if F[root]['sta'] == 'SW' and F[root]['n_0'] + F[root]['n_1'] == 0:
        child = F[root]['add']
        F[child]['sta'] = 2
    D = [v for v in F if F[v]['sta'] in {2,22, 'SW','S'}]
    return D

def print_statuses(dictionary):
    for key in dictionary.keys():
        sta = dictionary[key]['sta']
        print(f'Vertices: {key} -> Status: {sta}')
    print("--------------------------------------")
    
def linear_trees(tree):
    status = phase0(tree)
    status1 = phase1(tree, status)
    result = phase2(tree, status1)
    return result

# Test the function with a sample tree
#tree = Graph([(1,2),(1,3),(3,8),(8,18),(18,24),(3,7),(7,17),(7,16),(16,23),(16,22),(2,6),(6,15),(15,21),(21,27),(21,26),(27,29),(2,5),(5,14),(5,13),(5,12),(2,4),(4,11),(4,10),(4,9),(9,19),(10,20),(20,25),(25,28),(28,30)])
# non_leaf = random_non_leaf_vertex(tree)
# tree, new_leaf = relabel_vertices(tree, non_leaf)
#result = linear_trees(tree)
#colors = {'#FF0000': result}
#tree.plot(vertex_labels=True, vertex_colors=colors, layout='tree', tree_root=non_leaf).save("tree.png")
