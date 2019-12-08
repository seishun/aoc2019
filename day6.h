#include <algorithm>
#include <fstream>
#include <iterator>
#include <string>
#include <unordered_map>
#include <vector>

namespace {
  using namespace std;

  auto parse(ifstream input) {
    unordered_map<string, string> map;
    string a;
    while (getline(input, a, ')')) {
      string b;
      getline(input, b);
      map.insert({ b, a });
    }
    return map;
  }

  int part1(ifstream input) {
    auto map = parse(move(input));
    int total = 0;
    for (auto [a, b] : map) {
      total++;
      for (; b != "COM"; b = map[b])
        total++;
    }
    return total;
  }

  int part2(ifstream input) {
    auto map = parse(move(input));
    vector<string> you, san;
    for (auto b = map["YOU"]; b != "COM"; b = map[b])
      you.push_back(b);
    for (auto b = map["SAN"]; b != "COM"; b = map[b])
      san.push_back(b);
    return you.size() + san.size() - 2 * distance(rbegin(you), mismatch(rbegin(you), rend(you), rbegin(san)).first);
  }
}
