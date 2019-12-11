#include <fstream>
#include <map>
#include <optional>
#include <unordered_map>

namespace {
  using namespace std;

  using State = tuple<int, int, unordered_map<int, long long>>;
  using Panels = map<pair<int, int>, int>;

  enum class Direction {
    Up, Right, Down, Left
  };

  Direction turn(Direction facing, int direction) {
    if (direction == 0) {
      switch (facing) {
      case Direction::Up:
        return Direction::Left;
      case Direction::Right:
        return Direction::Up;
      case Direction::Down:
        return Direction::Right;
      case Direction::Left:
        return Direction::Down;
      }
    }
    if (direction == 1) {
      switch (facing) {
      case Direction::Up:
        return Direction::Right;
      case Direction::Right:
        return Direction::Down;
      case Direction::Down:
        return Direction::Left;
      case Direction::Left:
        return Direction::Up;
      }
    }
  }

  pair<int, int> move(pair<int, int> pos, Direction facing) {
    auto [x, y] = pos;
    switch (facing) {
    case Direction::Up:
      return { x, y + 1 };
    case Direction::Right:
      return { x + 1, y };
    case Direction::Down:
      return { x, y - 1 };
    case Direction::Left:
      return { x - 1, y };
    }
  }

  unordered_map<int, long long> parse(ifstream input) {
    unordered_map<int, long long> program;
    long long value;
    for (int pos = 0; input >> value; pos++) {
      program[pos] = value;
      input.ignore(1, ',');
    }
    return program;
  }

  optional<int> run(State& state, optional<int> input) {
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
    return {};
  }

  void paint(unordered_map<int, long long> program, Panels& panels) {
    State state{ 0, 0, move(program) };
    pair<int, int> pos;
    auto facing = Direction::Up;
    while (auto color = run(state, panels[pos])) {
      auto direction = run(state, {});
      panels[pos] = *color;
      facing = turn(facing, *direction);
      pos = move(pos, facing);
    }
  }

  string draw(const Panels& panels) {
    string id;
    int line = 0;
    for (auto&& [panel, color] : panels) {
      if (panel.first != line)
        id += '\n';
      id += ".#"[color];
      line = panel.first;
    }
    return id;
  }

  auto part1(ifstream input) {
    Panels panels;
    paint(parse(move(input)), panels);
    return panels.size();
  }

  string part2(ifstream input) {
    Panels panels{ { {0, 0}, 1 } };
    paint(parse(move(input)), panels);
    return draw(panels);
  }
}
