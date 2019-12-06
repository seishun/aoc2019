#include <fstream>
#include <map>
#include <set>

namespace {
  using namespace std;
  
  using Vector = pair<int, int>;

  auto trace = [](ifstream& input, auto f) {
    Vector point = { 0, 0 };
    int steps = 0;
    do {
      char direction;
      int count;
      input >> direction >> count;
      while (count--) {
        if (direction == 'U')
          point.first--;
        else if (direction == 'L')
          point.second--;
        else if (direction == 'D')
          point.first++;
        else if (direction == 'R')
          point.second++;
        f(point, ++steps);
      }
    } while (input.peek() == ',' && input.ignore());
  };

  int part1(ifstream input) {
    set<Vector> first;
    trace(input, [&](Vector point, int) {
      first.insert(point);
    });
    int minimum = INT_MAX;
    trace(input, [&](Vector point, int) {
      if (first.count(point)) {
        int distance = abs(point.first) + abs(point.second);
        if (distance < minimum)
          minimum = distance;
      }
    });
    return minimum;
  }

  int part2(ifstream input) {
    map<Vector, int> first;
    trace(input, [&](Vector point, int steps) {
      first[point] = steps;
    });
    int minimum = INT_MAX;
    trace(input, [&](Vector point, int steps) {
      if (auto it = first.find(point); it != first.end()) {
        int combined = it->second + steps;
        if (combined < minimum)
          minimum = combined;
      }
    });
    return minimum;
  }
}
