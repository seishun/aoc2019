#include <algorithm>
#include <fstream>
#include <string>
#include <unordered_map>
#include <vector>

namespace {
  using namespace std;

  auto parse(ifstream input) {
    vector<pair<int, int>> asteroids;
    string line;
    for (int y = 0; getline(input, line); y++) {
      for (int x = 0; x < line.length(); x++) {
        if (line[x] == '#')
          asteroids.push_back({ x, y });
      }
    }
    return asteroids;
  }

  auto best(vector<pair<int, int>> asteroids) {
    pair<pair<int, int>, unordered_map<double, pair<int, int>>> best;
    for (auto [x1, y1] : asteroids) {
      unordered_map<double, pair<int, int>> angles;
      for (auto [x2, y2] : asteroids) {
        if (tie(x2, y2) == tie(x1, y1))
          continue;
        angles.insert({ -atan2(x2 - x1, y2 - y1), { x2 - x1, y2 - y1 } });
      }
      if (angles.size() > best.second.size())
        best = { {x1, y1}, move(angles) };
    }
    return best;
  }

  int part1(ifstream input) {
    auto location = best(parse(move(input)));
    return location.second.size();
  }

  int part2(ifstream input) {
    auto location = best(parse(move(input)));
    vector<pair<double, pair<int, int>>> angles(begin(location.second), end(location.second));
    nth_element(begin(angles), begin(angles) + 199, end(angles));
    auto [x1, y1] = location.first;
    auto [x2, y2] = angles[199].second;
    return (x1 + x2) * 100 + y1 + y2;
  }
}
