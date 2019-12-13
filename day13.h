#include <fstream>
#include <map>
#include <optional>
#include <set>
#include <unordered_map>

namespace {
  using namespace std;

  using IntcodeState = tuple<int, int, unordered_map<int, long long>>;
  struct GameState {
    set<pair<int, int>> blocks;
    int paddle;
    int ball;
    int score;
  };

  unordered_map<int, long long> parse(ifstream input) {
    unordered_map<int, long long> program;
    long long value;
    for (int pos = 0; input >> value; pos++) {
      program[pos] = value;
      input.ignore(1, ',');
    }
    return program;
  }

  optional<int> run(IntcodeState& state, optional<int> input) {
    auto& [pos, base, program] = state;
    for (int op; (op = program[pos] % 100) != 3 || input;) {
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
      } else if (op == 99) {
        return {};
      }
    }
    return {};
  }

  void draw(GameState& gs, IntcodeState& is, optional<int> input) {
    auto x = run(is, input);
    do {
      auto y = run(is, {});
      auto tile = run(is, {});
      if (x == -1 && y == 0)
        gs.score = *tile;
      else if (tile == 2)
        gs.blocks.insert({ *x, *y });
      else if (tile == 3)
        gs.paddle = *x;
      else if (tile == 4)
        gs.ball = *x;
      else if (tile == 0)
        gs.blocks.erase({ *x, *y });
    } while (x = run(is, {}));
  }

  int play(IntcodeState is) {
    GameState gs;
    draw(gs, is, {});
    while (!gs.blocks.empty()) {
      int joystick = gs.ball < gs.paddle ? -1 : gs.ball > gs.paddle ? 1 : 0;
      draw(gs, is, joystick);
    }
    return gs.score;
  }

  auto part1(ifstream input) {
    IntcodeState is{ 0, 0, parse(move(input)) };
    GameState gs;
    draw(gs, is, {});
    return gs.blocks.size();
  }

  auto part2(ifstream input) {
    auto program = parse(move(input));
    program[0] = 2;
    return play({ 0, 0, program });
  }
}
