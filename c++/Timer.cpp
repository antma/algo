#include <sys/time.h>

class Timer {
private:
  double deadline;
  double start;
  static inline double elapsedSecondsSinceEpoch() {
    struct timeval t;
    gettimeofday (&t,0);
    return t.tv_sec + 0.000001 * t.tv_usec;
  }
public:
  Timer (double time_limit) {
    start = elapsedSecondsSinceEpoch();
    deadline = start + time_limit;
  }
  inline bool timeoff() const { return (elapsedSecondsSinceEpoch() >= deadline); }
  double elapsed() const {return elapsedSecondsSinceEpoch() - start;}
};
