#include <fstream>
#include <iterator>
#include <numeric>

namespace {
  using namespace std;

  int fuel(int mass) {
    return mass / 3 - 2;
  }

  int part1(ifstream input) {
    return transform_reduce(istream_iterator<int>(input), {}, 0, plus<>(), fuel);
  }

  int part2(ifstream input) {
    return transform_reduce(istream_iterator<int>(input), {}, 0, plus<>(), [](int mass) {
      int module = 0;
      while ((mass = fuel(mass)) > 0)
        module += mass;
      return module;
    });
  }
}
