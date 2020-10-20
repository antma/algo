use algo::maxflow::PushRelabelMaxFlowGraph;

#[test]
fn hand1() {
  let mut g = PushRelabelMaxFlowGraph::new(3);
  g.add_edge(0, 1, 1, 1);
  g.add_edge(0, 2, 1, 1);
  g.add_edge(1, 2, 1, 1);
  assert_eq!(g.max_flow(), 2);
}
