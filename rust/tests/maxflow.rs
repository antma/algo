use algo::maxflow::dinic::DinicMaxFlow;
use algo::maxflow::graph::Graph;
use algo::maxflow::push_relabel::PushRelabelMaxFlow;
use algo::random::KnuthRandom;

type Edges = Vec<(usize, usize, i32)>;

fn build_graph(edges: &Edges) -> Graph<i32> {
  let n = edges.iter().fold(0, |acc, p| acc.max(p.0.max(p.1)));
  let mut g = Graph::new(n + 1);
  for p in edges {
    g.add_edge(p.0, p.1, p.2, 0);
  }
  g
}

fn random_bipartite_graph(seed: i32, n: usize, v: i32) -> Edges {
  let mut rnd = KnuthRandom::new(seed);
  let mut edges = Vec::new();
  for i in 1..=n {
    edges.push((0, i, rnd.randrange(1..v + 1)));
    edges.push((n + i, n + n + 1, rnd.randrange(1..v + 1)));
    for j in 1..=n {
      edges.push((i, j, rnd.randrange(1..v + 1)));
    }
  }
  edges
}

fn random_full_graph(seed: i32, n: usize, v: i32) -> Edges {
  let mut rnd = KnuthRandom::new(seed);
  let mut edges = Vec::new();
  for j in 1..n {
    for i in 0..j {
      edges.push((i, j, rnd.randrange(1..v + 1)));
    }
  }
  edges
}

fn run(e: &Edges) {
  assert_eq!(
    PushRelabelMaxFlow::new(build_graph(&e)).max_flow(),
    DinicMaxFlow(build_graph(&e)).max_flow(0x7fff_ffff)
  );
}

#[test]
fn maxflow_random_tests() {
  for n in (10..100).step_by(5) {
    run(&random_full_graph(n as i32, n, n.pow(3) as i32));
    run(&random_bipartite_graph(777 + n as i32, n, n.pow(3) as i32));
  }
}

#[test]
fn maxflow_hand_tests() {
  let mut g = Graph::new(3);
  g.add_edge(0, 1, 1, 1);
  g.add_edge(0, 2, 1, 1);
  g.add_edge(1, 2, 1, 1);
  assert_eq!(PushRelabelMaxFlow::new(g).max_flow(), 2);
  let mut g = Graph::new(3);
  g.add_edge(0, 1, 1, 1);
  g.add_edge(0, 2, 1, 1);
  g.add_edge(1, 2, 1, 1);
  assert_eq!(DinicMaxFlow(g).max_flow(1_000), 2);
}
