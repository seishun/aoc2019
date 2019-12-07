#include <algorithm>
#include <string>

namespace {
  using namespace std;

  auto passwords = [](int from, int to, auto criterion) {
    int count = 0;
    for (int password = from; password < to + 1; password++) {
      string digits = to_string(password);
      if (!is_sorted(begin(digits), end(digits)))
        continue;
      if (!criterion(move(digits)))
        continue;
      count++;
    }
    return count;
  };

  int part1(int from, int to) {
    return passwords(from, to, [](string digits) {
      return adjacent_find(begin(digits), end(digits)) != end(digits);
    });
  }

  int part2(int from, int to) {
    return passwords(from, to, [](string digits) {
      auto it = begin(digits);
      while ((it = adjacent_find(it, end(digits))) != end(digits)) {
        if (it + 2 == end(digits) || *it != *(it + 2))
          return true;
        it = upper_bound(it, end(digits), *it);
      }
      return false;
    });
  }
}
