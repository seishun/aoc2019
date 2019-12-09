#include <algorithm>
#include <fstream>
#include <optional>
#include <vector>

namespace {
  using namespace std;

  vector<int> parse(ifstream input) {
    vector<int> program;
    int value;
    while (input >> value) {
      program.push_back(value);
      input.ignore(1, ',');
    }
    return program;
  }

  pair<optional<int>, bool> run(int& pos, vector<int>& program, optional<int> input) {
    optional<int> output;
    for (int op; (op = program[pos] % 100) != 3 || input;) {
      auto param = [&, mode = program[pos++] / 10]() mutable -> int& {
        mode /= 10;
        if (mode % 10 == 0)
          return program[program[pos++]];
        if (mode % 10 == 1)
          return program[pos++];
      };
      if (op == 1) {
        param() = param() + param();
      } else if (op == 2) {
        param() = param() * param();
      } else if (op == 3) {
        param() = *input;
        input.reset();
      } else if (op == 4) {
        output = param();
      } else if (op == 5) {
        pos = param() != 0 ? param() : pos + 1;
      } else if (op == 6) {
        pos = param() == 0 ? param() : pos + 1;
      } else if (op == 7) {
        int left = param();
        param() = left < param() ? 1 : 0;
      } else if (op == 8) {
        param() = param() == param() ? 1 : 0;
      } else if (op == 99) {
        return { output, true };
      }
    }
    return { output, false };
  };

  int loop(const vector<int>& program, const vector<int>& phases) {
    bool halted = false;
    vector<pair<int, vector<int>>> states;
    for (int phase : phases) {
      auto& state = states.emplace_back(0, program);
      tie(ignore, halted) = run(state.first, state.second, phase);
    }
    optional<int> output = 0;
    while (!halted) {
      for (auto& [pos, program] : states) {
        tie(output, halted) = run(pos, program, output);
      }
    }
    return *output;
  }

  int part1(ifstream input) {
    auto program = parse(move(input));
    vector<int> phases { 0, 1, 2, 3, 4 };
    int maximum = 0;
    do {
      maximum = max(maximum, loop(program, phases));
    } while (next_permutation(begin(phases), end(phases)));
    return maximum;
  }

  int part2(ifstream input) {
    auto program = parse(move(input));
    vector<int> phases { 5, 6, 7, 8, 9 };
    int maximum = 0;
    do {
      maximum = max(maximum, loop(program, phases));
    } while (next_permutation(begin(phases), end(phases)));
    return maximum;
  }
}
