#include "Unicode.hh"

void UnicodeCharTraits::assign(char_type& c1, char_type c2) noexcept
{
    c1 = c2;
}

UnicodeCharTraits::char_type* UnicodeCharTraits::assign(char_type* s, std::size_t n, char_type c)
{
    return reinterpret_cast<UnicodeCharTraits::char_type*>(
        std::char_traits<char>::assign(reinterpret_cast<char*>(s), n, static_cast<char>(c)));
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
    std::memmove(s1, s2, n);
    return s1;
}


UnicodeCharTraits::char_type* UnicodeCharTraits::copy(char_type* s1, const char_type* s2, std::size_t n)
{
    std::memcpy(s1, s2, n);
    return s1;
}


int UnicodeCharTraits::compare(const char_type* s1, const char_type* s2, std::size_t n)
{
    return std::memcmp(s1, s2, n);
}


std::size_t UnicodeCharTraits::length(const char_type* s)
{
    return std::strlen(reinterpret_cast<const char*>(s));
}


const UnicodeCharTraits::char_type* UnicodeCharTraits::find(const char_type* s, std::size_t n, const char_type& c)
{
    return reinterpret_cast<const UnicodeCharTraits::char_type*>(
        std::char_traits<char>::find(reinterpret_cast<const char*>(s), n, static_cast<char>(c)));
}


UnicodeCharTraits::char_type UnicodeCharTraits::to_char_type(int_type i) noexcept
{
    return static_cast<UnicodeCharTraits::char_type>(std::char_traits<char>::to_char_type(i));
}


UnicodeCharTraits::int_type UnicodeCharTraits::to_int_type(char_type c) noexcept
{
    return std::char_traits<char>::to_int_type(static_cast<char>(c));
}


bool UnicodeCharTraits::eq_int_type(int_type i1, int_type i2) noexcept
{
    return i1 == i2;
}


UnicodeCharTraits::int_type UnicodeCharTraits::eof() noexcept
{
    return std::char_traits<char>::eof();
}


UnicodeCharTraits::int_type UnicodeCharTraits::not_eof(int_type i) noexcept
{
    return std::char_traits<char>::not_eof(i);
}
