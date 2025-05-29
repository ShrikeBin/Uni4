#ifndef UTILS_HPP
#define UTILS_HPP

#include <string>
// if ever needed to log precise location of error
#define LOC() std::string(__FILE__) + ":" + std::to_string(__LINE__) + " [ " + std::string(__func__) + " ] "

#endif