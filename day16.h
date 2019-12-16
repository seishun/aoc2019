#include <algorithm>
#include <fstream>
#include <numeric>
#include <sstream>
#include <string>
#include <vector>

namespace {
  using namespace std;

  string part1(ifstream input) {
    string signal;
    input >> signal;
    for (int phase = 0; phase < 100; phase++)
      for (auto i = 0; i < signal.size(); i++) {
        int output = 0;
        for (auto j = 0; j < signal.size(); j++) {
          int pattern[]{ 0, 1, 0, -1 };
          output += pattern[(j + 1) / (i + 1) % size(pattern)] * (signal[j] - '0');
        }
        signal[i] = (abs(output) % 10) + '0';
      }
    return signal.substr(0, 8);
  }

  string part2(ifstream input) {
    string signal;
    input >> signal;
    vector<int> slice;
    for (auto offset = stoi(signal.substr(0, 7)); offset < signal.length() * 10000; offset++)
      slice.push_back(signal[offset % signal.length()]);
    for (int phase = 0; phase < 100; phase++)
      inclusive_scan(rbegin(slice), rend(slice), rbegin(slice), [](auto a, auto b) {
        return abs(a + b) % 10;
      });
    ostringstream message;
    for_each(begin(slice), begin(slice) + 8, [&](auto digit) {
      message << digit;
    });
    return message.str();
  }
}
