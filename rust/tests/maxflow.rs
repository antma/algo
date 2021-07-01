use algo::maxflow::graph::Graph;
use algo::maxflow::dinic::DinicMaxFlow;
use algo::maxflow::push_relabel::PushRelabelMaxFlow;

#[test]
fn hand1() {
  let mut g = Graph::new(3);
  g.add_edge(0, 1, 1, 1);
  g.add_edge(0, 2, 1, 1);
  g.add_edge(1, 2, 1, 1);
  assert_eq!(PushRelabelMaxFlow::new(g).max_flow(), 2);
  let mut g = Graph::new(3);
  g.add_edge(0, 1, 1, 1);
  g.add_edge(0, 2, 1, 1);
  g.add_edge(1, 2, 1, 1);
  assert_eq!(DinicMaxFlow::new(g).max_flow(1_000), 2);
}
