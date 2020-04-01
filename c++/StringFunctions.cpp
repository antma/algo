//KMP
#include <vector>
#include <string>

using namespace std;

vector<int> computePrefixFunction (const string& s) {
  const auto n = s.length ();
  vector<int> p (n);
  p[0] = 0;
  int k = 0;
  for (string::size_type q = 1; q < n; ++q) {
    while (k > 0 && s[k] != s[q]) {
      k = p[k-1];
    }
    if (s[k] == s[q]) {
      k++;
    }
    p[q] = k;
  }
  return p;
}

string::size_type kmpFind (const string &t, const string &p, const vector<int> &pi, string::size_type start = 0) {
  const auto m = p.length ();
  string::size_type q = 0;
  for (auto i = start; i < t.length (); ++i) {
    const char c = t[i];
    while (q > 0 && c != p[q]) {
      q = pi[q-1];
    }
    if (c == p[q]) {
      ++q;
    }
    if (q == m) {
      return i - (m - 1);
    }
  }
  return string::npos;
}
