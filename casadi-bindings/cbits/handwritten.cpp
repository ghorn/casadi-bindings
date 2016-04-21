#include "math.h"

extern "C" double c_fmod(double x, double y);
double c_fmod(double x, double y) {
  return fmod(x, y);
}

extern "C" float c_fmodf(float x, float y);
float c_fmodf(float x, float y) {
  return fmodf(x, y);
}
