#include <algorithm>
#include <fstream>
#include <iomanip>
#include <numeric>
#include <sstream>
#include <string>
#include <vector>

namespace {
  using namespace std;

  constexpr int width = 25;
  constexpr int height = 6;

  vector<string> parse(ifstream input) {
    vector<string> layers;
    string layer;
    while (input >> setw(width * height) >> layer)
      layers.push_back(layer);
    return layers;
  }

  string draw(string image) {
    stringstream ss(image);
    image.clear();
    string row;
    while (ss >> setw(width) >> row) {
      transform(begin(row), end(row), back_inserter(image), [](char p) {
        if (p == '0') return ' ';
        if (p == '1') return '#';
      });
      image.push_back('\n');
    }
    return image;
  }

  auto part1(ifstream input) {
    auto layers = parse(move(input));
    auto layer = min_element(begin(layers), end(layers), [](auto&& a, auto&& b) {
      return count(begin(a), end(a), '0') < count(begin(b), end(b), '0');
    });
    return count(begin(*layer), end(*layer), '1') * count(begin(*layer), end(*layer), '2');
  }

  string part2(ifstream input) {
    auto layers = parse(move(input));
    return draw(accumulate(next(begin(layers)), end(layers), layers[0], [](auto a, auto&& b) {
      transform(begin(a), end(a), begin(b), begin(a), [](char a, char b) {
        return a == '2' ? b : a;
      });
      return a;
    }));
  }
}
