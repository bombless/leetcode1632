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

struct Solution;

fn main() {
    println!("{:?}", Solution::matrix_rank_transform(get_data()));
}

fn from_array<const M: usize, const N: usize>(arr: [[i32; N]; M]) -> Vec<Vec<i32>> {
    IntoIterator::into_iter(arr).map(Vec::from).collect()
}

fn get_data() -> Vec<Vec<i32>> {
    // from_array([[1,2],[3,4]])
    // from_array([[7,7],[7,7]])
    // from_array([[20,-21,14],[-19,4,19],[22,-47,24],[-19,4,19]])
    // from_array([[-32,15,38,17,-44,43,42,-47,-44,-41],[34,-43,-24,7,-10,-43,36,-5,-22,37],[4,-13,-38,49,16,-21,30,13,-20,47],[2,-35,32,11,26,-31,40,31,-46,-7],[4,19,18,-27,16,43,-10,-11,44,39],[18,9,48,-29,30,5,8,-13,-42,-43],[48,47,30,29,24,-29,22,-31,12,-37],[38,-23,44,-13,-46,37,-12,31,14,-31],[-28,23,-50,-23,12,23,18,-11,-44,31],[-10,37,16,11,-18,17,40,-41,26,-31]])
    from_array([[25,8,31,42,-39,8,31,-10,33,-44,7,-30,9,44,15,26],[-3,-48,-17,-18,9,-12,-21,10,1,44,-17,14,-27,48,-21,-6],[49,28,27,-18,-31,4,-13,34,49,48,47,-18,33,40,15,38],[5,-28,-49,-38,1,32,-25,-50,29,-32,35,-46,-43,48,-49,-6],[-27,-24,23,-14,-47,-12,7,6,25,-16,47,-26,13,-12,-33,-18],[45,-48,3,-26,-23,-36,-17,38,17,12,15,46,37,40,47,26],[-19,-24,-21,-2,-7,-48,47,30,5,-8,23,-46,21,-32,-33,-26],[-27,32,27,-26,21,-32,-49,-10,5,20,-29,46,-43,-44,39,22],[-43,48,27,26,-27,12,-1,-10,-27,12,-29,-34,41,-28,-25,-30],[25,-36,35,-26,37,-20,31,14,-19,-40,-29,-2,-39,-28,11,46],[49,-32,-29,-6,-47,32,-17,-18,-23,24,23,22,-47,-44,27,14],[37,-44,-33,-18,-47,24,-17,-46,-43,-32,15,-46,-27,-8,-25,46],[41,-40,31,-30,13,-24,-29,22,-15,-16,47,2,-39,4,-25,-42],[-3,12,7,14,-7,8,-37,-34,-7,-12,39,-38,1,44,27,-34],[-47,4,7,-2,-43,-32,27,2,-43,-8,-33,14,49,-48,-5,30],[-15,8,-33,-26,-23,-32,-25,22,13,-20,-9,26,29,4,-1,2]])
}
