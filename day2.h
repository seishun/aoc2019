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

  int run(vector<int> program, int noun, int verb) {
    program[1] = noun;
    program[2] = verb;
    int pos = 0;
    while (program[pos] != 99) {
      int op = program[pos];
      int a = program[pos + 1];
      int b = program[pos + 2];
      int c = program[pos + 3];
      if (op == 1)
        program[c] = program[a] + program[b];
      else if (op == 2)
        program[c] = program[a] * program[b];
      pos += 4;
    }
    return program[0];
  }

  int part1(ifstream input) {
    return run(parse(move(input)), 12, 2);
  }

  int part2(ifstream input) {
    auto program = parse(move(input));
    for (int noun = 0; noun < 100; noun++) {
      for (int verb = 0; verb < 100; verb++) {
        int output = run(program, noun, verb);
        if (output == 19690720)
          return 100 * noun + verb;
      }
    }
  }
}
