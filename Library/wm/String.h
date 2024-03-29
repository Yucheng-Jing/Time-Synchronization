#pragma once


#include <string>
#include "Object.h"


#define S(string) \
    (::wm::String(TEXT(string)))


namespace wm {
    class String: public Object, public std::basic_string<TCHAR> {
    public:
        static String format(const TCHAR* spec, ...);


    public:
        String(): std::basic_string<TCHAR>(TEXT("")) {
        }


        String(std::basic_string<TCHAR> string):
            std::basic_string<TCHAR>(string)
        {
        }


        String(const TCHAR* string):
            std::basic_string<TCHAR>(string)
        {
        }


#ifdef UNICODE
        String(const char* string);
#endif
        virtual char* toCharArray();
    };
}
