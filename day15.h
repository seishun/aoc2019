#include <fstream>
#include <optional>
#include <queue>
#include <set>
#include <unordered_map>

namespace {
  using namespace std;

  using Location = pair<int, int>;
  using State = tuple<int, int, unordered_map<int, long long>>;

  unordered_map<int, long long> parse(ifstream input) {
    unordered_map<int, long long> program;
    long long value;
    for (int pos = 0; input >> value; pos++) {
      program[pos] = value;
      input.ignore(1, ',');
    }
    return program;
  }

  int run(State& state, optional<int> input) {
    auto& [pos, base, program] = state;
    while (program[pos] != 99) {
      int op = program[pos] % 100;
      auto param = [&, mode = program[pos++] / 10]() mutable -> long long& {
        mode /= 10;
        switch (mode % 10) {
        case 0:
          return program[program[pos++]];
        case 1:
          return program[pos++];
        case 2:
          return program[base + program[pos++]];
        }
      };
      if (op == 1) {
        param() = param() + param();
      } else if (op == 2) {
        param() = param() * param();
      } else if (op == 3) {
        param() = *input;
        input.reset();
      } else if (op == 4) {
        return param();
      } else if (op == 5) {
        pos = param() != 0 ? param() : pos + 1;
      } else if (op == 6) {
        pos = param() == 0 ? param() : pos + 1;
      } else if (op == 7) {
        auto left = param();
        param() = left < param() ? 1 : 0;
      } else if (op == 8) {
        param() = param() == param() ? 1 : 0;
      } else if (op == 9) {
        base += param();
      }
    }
  }

  pair<int, pair<Location, State>> bfs(Location pos, State state) {
    set<Location> visited;
    queue<pair<int, pair<Location, State>>> unvisited;
    unvisited.push({ 0, { pos, state } });
    int steps;
    pair<Location, State> ls;
    while (!unvisited.empty()) {
      tie(steps, ls) = move(unvisited.front());
      auto [x, y] = ls.first;
      auto adjacent = {
        tuple{ 1, 2, pair{ x, y - 1 } },
        tuple{ 2, 1, pair{ x, y + 1 } },
        tuple{ 3, 4, pair{ x - 1, y } },
        tuple{ 4, 3, pair{ x + 1, y } },
      };
      int viable = 0;
      for (auto [forth, back, pos] : adjacent) {
        if (visited.count(pos))
          continue;
        auto& state = ls.second;
        auto status = run(state, forth);
        if (status == 0) {
          visited.insert(pos);
          continue;
        }
        if (status == 1) {
          run(state, back);
          viable++;
        }
        if (status == 2)
          return { steps + 1, { pos, state } };
      }
      for (auto [forth, back, pos] : adjacent) {
        if (visited.count(pos))
          continue;
        [&](auto state) {
          run(state, forth);
          unvisited.push({ steps + 1, { pos, move(state) } });
        }(!--viable ? move(ls.second) : ls.second);
      }
      visited.insert({ x, y });
      unvisited.pop();
    }
    return { steps, {} };
  }

  int part1(ifstream input) {
    auto program = parse(move(input));
    return bfs({ 0, 0 }, { 0, 0, move(program) }).first;
  }

  int part2(ifstream input) {
    auto program = parse(move(input));
    auto [pos, state] = bfs({ 0, 0 }, { 0, 0, move(program) }).second;
    return bfs(pos, move(state)).first;
  }
}
