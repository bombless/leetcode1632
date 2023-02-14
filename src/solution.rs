use std::collections::{HashMap, HashSet};
use std::hash::Hash;

struct UnionFind<E>(HashMap<E, E>);

impl<E: Hash + PartialEq + Eq + Clone> UnionFind<E> {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
    pub fn find(&mut self, e: &E) -> E {
        let p = self.0.get(e).unwrap();
        if p == e {
            return e.clone();
        }
        let r = self.0.get(p).unwrap().clone();
        self.0.insert(p.clone(), r.clone());
        r
    }
    pub fn union(&mut self, lhs: E, rhs: E) {
        if !self.0.contains_key(&lhs) {
            self.0.insert(lhs.clone(), lhs.clone());
        }
        if !self.0.contains_key(&rhs) {
            self.0.insert(rhs.clone(), rhs.clone());
        }
        let pl = self.find(&lhs);
        let pr = self.find(&rhs);
        if pl != pr {
            self.0.insert(pl.clone(), pr.clone());
        }
    }
    pub fn groups<'a>(&'a mut self) -> HashMap<E, Vec<E>> {
        let mut ret = HashMap::new();
        let keys = self.0.keys().cloned().collect::<Vec<_>>();
        for v in keys {
            let k = self.find(&v);
            if !ret.contains_key(&k) {
                ret.insert(k.clone(), Vec::new());
            }
            ret.get_mut(&k).unwrap().push(v);
        }
        ret
    }
}


struct Graph<T> {
    nodes: HashSet<T>,
    edges: HashSet<(T, T)>,
}

impl<T: Hash + Clone + PartialEq + Eq> Graph<T> {
    pub fn new() -> Self {
        Self {
            nodes: HashSet::new(),
            edges: HashSet::new(),
        }
    }
    pub fn edge_count(&self) -> usize {
        self.edges.len()
    }
    pub fn neighbors<'a>(&'a self, id: &'a T) -> impl Iterator<Item=&'a T> {
        self.edges.iter().filter_map(move |(l, r)| {
            if l == id {
                return Some(r)
            }
            if r == id {
                return Some(l)
            }
            None
        })
    }
    pub fn nodes<'a>(&'a self) -> impl Iterator<Item=&'a T> {
        self.nodes.iter()
    }
    pub fn edges<'a>(&'a self) -> impl Iterator<Item=(&'a T, &'a T)> {
        self.edges.iter().map(|&(ref l, ref r)| (l, r))
    }
    pub fn add_node(&mut self, v: T) {
        self.nodes.insert(v);
    }
    pub fn remove_node(&mut self, id: &T) {
        let mut edges = Vec::new();
        for (ref l, ref r) in &self.edges {
            if l == id {
                edges.push((l.clone(), r.clone()));
            } else if r == id {
                edges.push((l.clone(), r.clone()));
            }
        }
        for edge in &edges {
            self.edges.remove(edge);
        }
        self.nodes.remove(id);
    }
    pub fn add_edge(&mut self, node1: T, node2: T) {
        self.edges.insert((node1, node2));
    }
}

impl Solution {
    pub fn matrix_rank_transform(mut matrix: Vec<Vec<i32>>) -> Vec<Vec<i32>> {
        let mut uf = Self::union_find(&matrix);
        let mut graph = Self::graph(&matrix, &mut uf);
        let groups = uf.groups();
        let ranking = Self::rank_groups(&mut graph);

        for (id, positions) in groups {
            let rank = *ranking.get(&id).unwrap();
            for (i, j) in positions {
                matrix[i][j] = rank;
            }
        }
        matrix
    }
    fn union_find(matrix: &Vec<Vec<i32>>) -> UnionFind<(usize, usize)> {
        let m = matrix.len();
        let n = matrix[0].len();

        let mut uf = UnionFind::new();

        for i in 0 .. m {
            for j in 0 .. n {
                let val = matrix[i][j];
                for k in 0 .. m {
                    if matrix[k][j] == val {
                        uf.union((i, j), (k, j))
                    }
                }
                for k in 0 .. n {
                    if matrix[i][k] == val {
                        uf.union((i, j), (i, k))
                    }
                }
            }
        }

        uf
    }
    fn graph(matrix: &Vec<Vec<i32>>, uf: &mut UnionFind<(usize, usize)>) -> Graph<(usize, usize)> {
        let m = matrix.len();
        let n = matrix[0].len();

        let mut ret = Graph::new();

        for i in 0 .. m {
            for j in 0 .. n {
                let val = matrix[i][j];
                let lhs = uf.find(&(i, j));
                ret.add_node(lhs);
                for k in (i + 1) .. m {
                    if matrix[k][j] < val {
                        let rhs = uf.find(&(k, j));
                        ret.add_node(rhs);
                        ret.add_edge(rhs, lhs);
                    } else if val < matrix[k][j] {
                        let rhs = uf.find(&(k, j));
                        ret.add_node(rhs);
                        ret.add_edge(lhs, rhs);
                    }
                }
                for k in (j + 1) .. n {
                    if matrix[i][k] < val {
                        let rhs = uf.find(&(i, k));
                        ret.add_node(rhs);
                        ret.add_edge(rhs, lhs);
                    } else if val < matrix[i][k] {
                        let rhs = uf.find(&(i, k));
                        ret.add_node(rhs);
                        ret.add_edge(lhs, rhs);
                    }
                }
            }
        }

        ret
    }
    fn rank_groups(graph: &mut Graph<(usize, usize)>) -> HashMap<(usize, usize), i32> {
        fn find_minimal(graph: &Graph<(usize, usize)>) -> (usize, usize) {
            let mut nodes = graph.nodes().collect::<HashSet<_>>();
            for (_, r) in graph.edges() {
                nodes.remove(&r);
            }
            for v in nodes {
                return *v;
            }
            unreachable!()
        }

        let mut ret = HashMap::new();

        for &id in graph.nodes() {
            ret.insert(id, 1);
        }
        while graph.edge_count() > 0 {
            let node = find_minimal(graph);
            for neighbor in graph.neighbors(&node) {
                ret.insert(*neighbor, i32::max(*ret.get(neighbor).unwrap(), *ret.get(&node).unwrap() + 1));
            }
            graph.remove_node(&node);
        }
        ret
    }
}

pub struct Solution;
