use crate::geometry::plane::{Circle, Point};
use std::io::Write;

const METAPOST: u8 = 0;
pub struct MetapostWriter {
  o: Option<std::io::LineWriter<std::fs::File>>,
  center: Point<f64>,
}

impl MetapostWriter {
  pub fn new(center: Point<f64>) -> std::io::Result<Self> {
    Ok(Self {
      o: if METAPOST > 0 {
        Some(std::io::LineWriter::new(std::fs::File::create("out.mp")?))
      } else {
        None
      },
      center,
    })
  }
  pub fn prologue(&mut self) -> std::io::Result<()> {
    if let Some(u) = self.o.as_mut() {
      writeln!(u, "prologues := 3;")?;
      writeln!(u, "beginfig(1);")?;
      writeln!(u, "u=8mm;")?;
    }
    Ok(())
  }
  pub fn epilogue(&mut self) -> std::io::Result<()> {
    if let Some(u) = self.o.as_mut() {
      writeln!(u, "endfig;\nend.")?;
    }
    Ok(())
  }
  fn point(&self, p: &Point<f64>) -> String {
    let q = *p + self.center;
    format!("({:.3}u, {:.3}u)", q.x, q.y)
  }
  pub fn draw_circle(&mut self, c: &Circle<f64>) -> std::io::Result<()> {
    if self.o.is_none() {
      return Ok(());
    }
    let p = self.point(&c.c);
    if let Some(u) = self.o.as_mut() {
      writeln!(u, "draw fullcircle scaled {:.3}u shifted {};", 2.0 * c.r, p)?;
    }
    Ok(())
  }
  pub fn draw_arrow(&mut self, p1: &Point<f64>, p2: &Point<f64>) -> std::io::Result<()> {
    if self.o.is_none() {
      return Ok(());
    }
    let p1 = self.point(&p1);
    let p2 = self.point(&p2);
    if let Some(u) = self.o.as_mut() {
      writeln!(u, "drawarrow {}--{};", p1, p2)?;
    }
    Ok(())
  }
}
