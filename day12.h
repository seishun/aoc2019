#include <array>
#include <fstream>
#include <numeric>
#include <regex>
#include <vector>

namespace {
  using namespace std;

  auto parse(ifstream input) {
    array<vector<pair<int, int>>, 3> axes;
    string line;
    while (getline(input, line)) {
      smatch match;
      regex_match(line, match, regex(R"(<x=(-?\d+), y=(-?\d+), z=(-?\d+)>)"));
      axes[0].push_back({ stoi(match[1]), 0 });
      axes[1].push_back({ stoi(match[2]), 0 });
      axes[2].push_back({ stoi(match[3]), 0 });
    }
    return axes;
  }

  void step(vector<pair<int, int>>& moons) {
    for (auto& [p1, v1] : moons) {
      for (auto&& [p2, v2] : moons) {
        if (p2 > p1)
          v1++;
        if (p2 < p1)
          v1--;
      }
    }
    for (auto& [p, v] : moons)
      p += v;
  }

  auto part1(ifstream input) {
    auto axes = parse(move(input));
    for (int i = 0; i < 1000; i++)
      for (auto& axis : axes)
        step(axis);
    int energy = 0;
    for (int i = 0; i < axes[0].size(); i++) {
      int potential = 0;
      int kinetic = 0;
      for (auto&& axis : axes) {
        auto&& [p, v] = axis[i];
        potential += abs(p);
        kinetic += abs(v);
      }
      energy += potential * kinetic;
    }
    return energy;
  }

  auto part2(ifstream input) {
    auto axes = parse(move(input));
    long long total = 1;
    for (auto&& first : axes) {
      auto axis = first;
      int steps = 0;
      do {
        step(axis);
        steps++;
      } while (axis != first);
      total = lcm(total, steps);
    }
    return total;
  }
}
