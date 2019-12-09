#include <fstream>
#include <unordered_map>

namespace {
  using namespace std;

  unordered_map<int, long long> parse(ifstream input) {
    unordered_map<int, long long> program;
    int value;
    for (int pos = 0; input >> value; pos++) {
      program[pos] = value;
      input.ignore(1, ',');
    }
    return program;
  }

  long long run(unordered_map<int, long long> program, int input) {
    long long output;
    int pos = 0;
    int base = 0;
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
        param() = input;
      } else if (op == 4) {
        output = param();
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
    return output;
  }

  long long part1(ifstream input) {
    return run(parse(move(input)), 1);
  }

  long long part2(ifstream input) {
    return run(parse(move(input)), 2);
  }
}
