#include <cmath>
#include "Unit.h"


namespace wm {
    size_t Percent(size_t value, size_t total) {
        return (size_t) ceil(float(value) / 100 * total);
    }
}
