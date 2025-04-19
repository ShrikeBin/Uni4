#include "select.hpp"
#include "sorts.hpp"
#include <algorithm>

// na pewno da się szybciej to wszystko zrobić
int RandomSelect(std::vector<int>& arr, int place) 
{
    std::function<int(std::vector<int>&, int)> randomSelect = [&](std::vector<int>& arr, int place) -> int {
        if (arr.size() == 1) return arr[0];

        int pivot = arr[std::rand() % arr.size()];
        std::vector<int> left, right;
        int equalCount = 0;

        for (int val : arr) {
            if (val < pivot) left.push_back(val);
            else if (val > pivot) right.push_back(val);
            else equalCount++;
        }

        if (place < left.size()) {
            return randomSelect(left, place);
        } else if (place < left.size() + equalCount) {
            return pivot;
        } else {
            return randomSelect(right, place - left.size() - equalCount);
        }
    };

    return randomSelect(arr, place);
}

int Select(std::vector<int>& arr, int place) {
    const int base = 5;

    std::function<int(std::vector<int>&)> getMedian = [&](std::vector<int>& group) -> int {
        std::sort(group.begin(), group.end());
        return group[group.size() / 2];
    };

    std::function<int(std::vector<int>&)> medianOfMedians = [&](std::vector<int>& arr) -> int {
        if (arr.size() <= base) return getMedian(arr);

        std::vector<int> medians;
        for (size_t i = 0; i < arr.size(); i += base) {
            std::vector<int> group;
            for (size_t j = i; j < i + base && j < arr.size(); ++j)
                group.push_back(arr[j]);
            medians.push_back(getMedian(group));
        }

        return Select(medians, medians.size() / 2);
    };

    std::function<int(std::vector<int>&, int)> deterministicSelect = [&](std::vector<int>& arr, int place) -> int {
        if (arr.size() == 1) return arr[0];

        int pivot = medianOfMedians(arr);
        std::vector<int> left, right;
        int equalCount = 0;

        for (int val : arr) {
            if (val < pivot) left.push_back(val);
            else if (val > pivot) right.push_back(val);
            else equalCount++;
        }

        if (place < left.size()) {
            return deterministicSelect(left, place);
        } else if (place < left.size() + equalCount) {
            return pivot;
        } else {
            return deterministicSelect(right, place - left.size() - equalCount);
        }
    };

    return deterministicSelect(arr, place);
}

int ParametrizedSelect(std::vector<int>& arr, int place, int parameter) {
    std::function<int(std::vector<int>&)> getMedian = [&](std::vector<int>& group) -> int {
        std::sort(group.begin(), group.end());
        return group[group.size() / 2];
    };

    std::function<int(std::vector<int>&, int)> medianOfMedians = [&](std::vector<int>& arr, int parameter) -> int {
        if (arr.size() <= parameter) return getMedian(arr);

        std::vector<int> medians;
        for (size_t i = 0; i < arr.size(); i += parameter) {
            std::vector<int> group;
            for (size_t j = i; j < i + parameter && j < arr.size(); ++j)
                group.push_back(arr[j]);
            medians.push_back(getMedian(group));
        }

        return ParametrizedSelect(medians, medians.size() / 2, parameter);
    };

    std::function<int(std::vector<int>&, int, int)> selectParam = [&](std::vector<int>& arr, int place, int parameter) -> int {
        if (arr.size() == 1) return arr[0];

        int pivot = medianOfMedians(arr, parameter);
        std::vector<int> left, right;
        int equalCount = 0;

        for (int val : arr) {
            if (val < pivot) left.push_back(val);
            else if (val > pivot) right.push_back(val);
            else equalCount++;
        }

        if (place < left.size()) {
            return selectParam(left, place, parameter);
        } else if (place < left.size() + equalCount) {
            return pivot;
        } else {
            return selectParam(right, place - left.size() - equalCount, parameter);
        }
    };

    return selectParam(arr, place, parameter);
}