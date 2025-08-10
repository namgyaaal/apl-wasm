#include "Common.hh"

void UnicodeCharTraits::assign(char_type& c1, char_type c2) noexcept
{
    c1 = c2;
}

UnicodeCharTraits::char_type* UnicodeCharTraits::assign(char_type* s, std::size_t n, char_type c)
{
    return reinterpret_cast<UnicodeCharTraits::char_type*>(
        std::char_traits<char32_t>::assign(reinterpret_cast<char32_t*>(s), n, static_cast<char32_t>(c)));
}


bool UnicodeCharTraits::eq(char_type c1, char_type c2) noexcept
{
    return c1 == c2;
}


bool UnicodeCharTraits::lt(char_type c1, char_type c2) noexcept
{
    return c1 < c2;
}


UnicodeCharTraits::char_type* UnicodeCharTraits::move(char_type* s1, const char_type* s2, std::size_t n)
{

    std::memmove(s1, s2, n * sizeof(char_type));
    return s1;
}


UnicodeCharTraits::char_type* UnicodeCharTraits::copy(char_type* s1, const char_type* s2, std::size_t n)
{
    std::memcpy(s1, s2, n * sizeof(char_type));
    return s1;
}


int UnicodeCharTraits::compare(const char_type* s1, const char_type* s2, std::size_t n)
{
    return std::memcmp(s1, s2, n * sizeof(char_type));
}


std::size_t UnicodeCharTraits::length(const char_type* s)
{
    std::size_t len = 0;
    while(s[len] != Unicode_0) {
        len++;
    }
    return len;
}


const UnicodeCharTraits::char_type* UnicodeCharTraits::find(const char_type* s, std::size_t n, const char_type& c)
{
   for (std::size_t i = 0; i < n; ++i) {
        if (s[i] == c) {
            return &s[i];
        }
    }
}


UnicodeCharTraits::char_type UnicodeCharTraits::to_char_type(int_type i) noexcept
{   
    return static_cast<UnicodeCharTraits::char_type>(i);
}


UnicodeCharTraits::int_type UnicodeCharTraits::to_int_type(char_type c) noexcept
{
    return static_cast<int>(c);
}


bool UnicodeCharTraits::eq_int_type(int_type i1, int_type i2) noexcept
{
    return i1 == i2;
}


UnicodeCharTraits::int_type UnicodeCharTraits::eof() noexcept
{
    return std::char_traits<char32_t>::eof();
}


UnicodeCharTraits::int_type UnicodeCharTraits::not_eof(int_type i) noexcept
{
    return std::char_traits<char32_t>::not_eof(i);
}
