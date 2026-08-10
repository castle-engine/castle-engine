#include "random_utils.h"
#include <random>
#include <sstream>

std::string generateRandomVectorToString() {
    std::vector<int> randomNumbers;
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> dis(1, 100);

    for (int i = 0; i < 5; ++i) {
        randomNumbers.push_back(dis(gen));
    }

    std::stringstream ss;
    for (const auto& num : randomNumbers) {
        ss << num << " ";
    }

    return ss.str();
}

