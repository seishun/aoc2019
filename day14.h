#include <fstream>
#include <regex>
#include <string>
#include <unordered_map>
#include <vector>

namespace {
  using namespace std;

  using Reactions = unordered_map<string, vector<tuple<int, string, int>>>;

  Reactions parse(ifstream input) {
    Reactions reactions;
    string line;
    while (getline(input, line)) {
      smatch match;
      regex_match(line, match, regex(R"((?:\d+ \w+, )*\d+ \w+ => (\d+) (\w+))"));
      auto ou = stoi(match[1]);
      auto oc = match[2];
      regex input(R"((\d+) (\w+)[, ])");
      for_each(sregex_iterator(begin(line), end(line), input), {}, [&](auto&& match) {
        reactions[match[2]].push_back({ stoi(match[1]), oc, ou });
      });
    }
    return reactions;
  }

  long long require(const Reactions& reactions, unordered_map<string, long long>& m, const string& ic) {
    if (m.count(ic))
      return m[ic];
    long long u = 0;
    for (auto&& [iu, oc, ou] : reactions.at(ic)) {
      auto ceil = (require(reactions, m, oc) + ou - 1) / ou;
      u += iu * ceil;
    }
    m.insert({ ic, u });
    return u;
  }

  auto ore(int fuel, const Reactions& reactions) {
    unordered_map<string, long long> m;
    m["FUEL"] = fuel;
    return require(reactions, m, "ORE");
  }

  auto part1(ifstream input) {
    auto reactions = parse(move(input));
    return ore(1, reactions);
  }

  auto part2(ifstream input) {
    auto reactions = parse(move(input));
    int lo = 1;
    while (ore(lo * 2, reactions) <= 1000000000000)
      lo *= 2;
    auto hi = lo * 2;
    while (hi - lo > 1) {
      auto fuel = (lo + hi) / 2;
      if (ore(fuel, reactions) > 1000000000000)
        hi = fuel;
      else
        lo = fuel;
    }
    return lo;
  }
}
