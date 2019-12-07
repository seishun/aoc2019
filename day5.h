#include <fstream>
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

  int run(vector<int> program, int input) {
    int output;
    int pos = 0;
    while (program[pos] != 99) {
      int op = program[pos] % 100;
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
        param() = input;
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
      }
    }
    return output;
  }

  int part1(ifstream input) {
    return run(parse(move(input)), 1);
  }

  int part2(ifstream input) {
    return run(parse(move(input)), 5);
  }
}
