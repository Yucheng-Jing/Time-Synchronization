#pragma once


namespace wm {
    typedef size_t (*Unit)(size_t value, size_t total);
    
    size_t Percent(size_t value, size_t total);
}
