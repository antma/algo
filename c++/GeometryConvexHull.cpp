#include <iostream>
#include <vector>

using std::ostream;
using std::vector;

template<typename T> struct Point {
  using P = Point<T>;
  T x, y;
  bool operator== (const P& rhs) const & { return x == rhs.x && y == rhs.y; }
  bool operator< (const P& rhs) const & {
    if (y < rhs.y) return true;
    if (y > rhs.y) return false;
    return x < rhs.x;
  }
  P& operator+= (const P& rhs) & {
    x += rhs.x;
    y += rhs.y;
    return *this;
  }
  P& operator-= (const P& rhs) & {
    x -= rhs.x;
    y -= rhs.y;
    return *this;
  }
  T dotProduct (const P& rhs) const & {
    return x * rhs.x + y * rhs.y;
  }
  bool zeroCrossProduct (const P& rhs) const & {
    return x * rhs.y == y * rhs.x;
  }
  T crossProduct (const P& rhs) const & {
    return x * rhs.y - y * rhs.x;
  }
  bool cmpAngle (const P& rhs) const & {
    auto cp = crossProduct (rhs);
    if (cp > 0) return true;
    if (cp < 0) return false;
    return dotProduct (*this) > rhs.dotProduct (rhs);
  }
  bool left (const P& b, const P& c) const & {
    return (c.x - x) * (b.y - y) < (b.x - x) * (c.y - y);
  }
  Point<T> (T _x = 0, T _y = 0) : x (_x), y (_y) {}
  Point<T> (const Point<T>& rhs) : x (rhs.x), y (rhs.y) {}
};

template<typename T> ostream& operator<<(ostream &o, const Point<T> &p) {
  return o << "(" << p.x << ", " << p.y << ")";
}

template<typename T> vector<Point<T> > convexHull (const vector<Point<T> > &points) {
  vector<Point<T> > h;
  const int n = (int) points.size ();
  if (!n) return h;
  vector<Point<T> > x (points.cbegin (), points.cend ());
  auto it = min_element (x.cbegin (), x.cend ());
  const auto me = *it;
  for (auto& p : x) p -= me;
  x.erase (remove (x.begin (), x.end (), Point<T>()), x.end ());
  sort (x.begin (), x.end (), [] (const auto& u, const auto &v) {
    return u.cmpAngle (v);
  });
  x.erase (unique (x.begin (), x.end (), [] (const auto &u, const auto &v) {
    return u.zeroCrossProduct (v);
  }), x.end ());
  h.emplace_back (0, 0);
  for (auto i = 0U; i < 2U && i < x.size (); ++i) {
    h.emplace_back (x[i]);
  }
  auto m = h.size ();
  for (auto i = 2U; i < x.size (); ++i) {
    while (!(h[m-2].left (h[m - 1], x[i]))) {
      --m;
    }
    if (m < h.size ()) {
      h[m] = x[i];
    } else {
      h.emplace_back (x[i]);
    }
    ++m;
  }
  h.erase (h.begin () + m, h.end ());
  for (auto &p : h) p += me;
  return h;
}
