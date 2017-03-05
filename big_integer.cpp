#include <iostream>
#include <cstdio>
#include <algorithm>
#include <vector>
#include <cassert>
#include <cstdlib>
#include <utility>
#include <limits>
#include "big_integer.h"

big_integer::big_integer()
    :a(1)
    ,sign(1)
{}

big_integer::big_integer(big_integer const& x)
    :a(x.a)
    ,sign(x.sign)
{}

big_integer::big_integer(int x)
{
    if (x == std::numeric_limits<int32_t>::min()) {
        a.push_back(0);
        a.push_back(1);
        sign = -1;
        return;
    }
    a.push_back(abs(x));
    if (x != 0) {
        sign = x / abs(x);
    }
    else {
        sign = 1;
    }
}

big_integer::big_integer(std::string const& str)
    :sign(1)
{
    int32_t start = 0;
    if (str[0] == '-') {
        start = 1;
    }
    for (size_t i = start; i < str.size(); i++) {
        *this *= big_integer(10);
        *this += (str[i] - '0');
    }
    if (str[0] == '-') {
        sign = -1;
    }
}

big_integer::~big_integer(){}

big_integer& big_integer::operator=(big_integer const& x)
{
    this->a = x.a;
    this->sign = x.sign;
    return *this;
}


int big_integer::cmp(std::vector<int64_t> const& a, std::vector<int64_t> const& b) {
    bool larger;
    bool notzero = false;
    if (a.size() != b.size()) {
        larger = a.size() > b.size();
        notzero = true;
    }
    else {
        for (size_t i = a.size(); (i > 0) && !notzero; i--) {
            if (a[i - 1] != b[i - 1]) {
                notzero = true;
                if (a[i - 1] > b[i - 1]) {
                    larger = true;
                }
                else {
                    larger = false;
                }
            }
        }
    }
    if (!notzero) {
        return 0;
    }
    if (larger) {
        return 1;
    }
    else {
        return -1;
    }

}

big_integer& big_integer::operator+=(big_integer const& rhs)
{
    if (this->sign != rhs.sign) {
        *this -= -rhs;
        return *this;
    }
    uint32_t sizemax = std::max(this->a.size(), rhs.a.size());
    uint32_t sizemin = std::min(this->a.size(), rhs.a.size());
    size_t prevsize = this->a.size();
    this->a.resize(sizemax + 1);
    uint64_t carry = 0;
    uint64_t tmp;
    for (size_t i = 0; i < sizemin; i++) {
        tmp = this->a[i] + rhs.a[i] + carry;
        this->a[i] = tmp % (uint64_t)BASE;
        carry = tmp / (uint64_t)BASE;
    }
    for (size_t i = prevsize; i < rhs.a.size(); i++) {
        tmp = rhs.a[i] + carry;
        this->a[i] = tmp % (uint64_t)BASE;
        carry = tmp / (uint64_t)BASE;
    }
    for (size_t i = rhs.a.size(); i < prevsize; i++) {
        tmp = this->a[i] + carry;
        this->a[i] = tmp % (uint64_t)BASE;
        carry = tmp / (uint64_t)BASE;
    }
    this->a[sizemax] = carry;
    this->delete_zeroes();
    return *this;
}
big_integer& big_integer::operator-=(big_integer const& rhs)
{
    if (this->sign != rhs.sign) {
        *this += -rhs;
        return *this;
    }

    int compare = cmp(this->a, rhs.a);

    if (compare == 0) {
        *this = 0;
        return *this;
    }
    const std::vector<int64_t>* f = &this->a;
    const std::vector<int64_t>* s = &rhs.a;
    if (compare < 0) {
        this->sign *= -1;
        std::swap(f, s);
    }
    this->a.resize(f->size());
    int64_t carry = 0;
    int64_t tmp;
    for (size_t i = 0; i < s->size(); i++) {
        tmp = carry + f->at(i) - s->at(i);
        if (tmp < 0) {
            carry = -1;
            tmp += BASE;
        }
        else {
            carry = 0;
        }
        this->a[i] = tmp;
    }
    for (size_t i = s->size(); i < f->size(); i++) {
        tmp = carry + f->at(i);
        if (tmp < 0) {
            carry = -1;
            tmp += BASE;
        }
        else {
            carry = 0;
        }
        this->a[i] = tmp;
    }

    this->delete_zeroes();
    return *this;
}

big_integer big_integer::multiply(int32_t const& rhs) const  {
	int64_t r = 0;
	big_integer ans;
	ans.a.clear();                      //ÓÁÐÀÒÜ ÏÓØÁÝÊ ÑÄÊËÀÒÜ ÐÅÑÀÉÉÇ        ÅÙÅ Â ÑÄÂÈÃÅ
	ans.a.resize(this->a.size());
	ans.sign = this->sign;
	for (size_t i = 0; i < this->a.size(); i++) {
		r = static_cast<int64_t>(this->a[i]) * rhs + r;
		ans.a[i] = r % BASE;
		r /= BASE;
	}
	if (r) {
		ans.a.push_back(static_cast<int64_t>(r));
	}
	return ans;
}

big_integer & big_integer::operator*=(big_integer const & t) {
	int8_t sign = t.sign * this->sign;
	this->sign = 1;
	big_integer rhs = t;
	rhs.sign = 1;
	big_integer tmp = 0;
	tmp.a.clear();
	for (size_t i = rhs.a.size(); i > 0; i--) {
        if (i != rhs.a.size()) {
            tmp.a.push_back(0);
            for (size_t j = tmp.a.size() - 1; j > 0; j--) {
                std::swap(tmp.a[j], tmp.a[j - 1]);
            }
        }
		tmp += this->multiply(rhs.a[i - 1]);
	}
	this->a = tmp.a;
	this->sign = sign;
	return *this;
}

big_integer& big_integer::operator/=(big_integer const& rhs)
{
	int8_t si = this->sign * rhs.sign;
    if (((*this < rhs) && (this->sign == 1)) || ((*this > rhs) && (this->sign == -1))) {
		*this = big_integer(0);
		return *this;
	}
	if (rhs.a.size() == 1) {
		int64_t r = 0, q = rhs.a.back();
		for (size_t j = this->a.size(); j > 0; j--) {
			int64_t tmp = this->a[j - 1] + r * BASE;
			this->a[j - 1] = tmp / q;
			r = tmp % q;
		}
		this->normalize();
		this->sign = si;
		return *this;
	}
	int32_t normal;
	if (rhs.a[0] == 0 && rhs.a.size() == 1) {
		normal = 0;
	} else {
		normal = 1;
		while (rhs.a.back() * normal < (BASE / 2)) {
			normal *= 2;
		}
	}
	big_integer q = this->multiply(normal), b = rhs.multiply(normal);
	q.sign = 1;
	b.sign = 1;
	int32_t n = b.a.size();
	int32_t m = q.a.size() - n;
	b <<= static_cast<int>(m * POW);
	std::vector<int64_t> res(m);
	if (((q >= b) && (q.sign == 1)) || ((q <= b) && (q.sign == -1))) {
		res.push_back(1);
		q -= b;
	}
	for (size_t i = m; i > 0; i--) {
		if (q.a[0] == 0 && q.a.size() == 1) {
			break;
		}
		b >>= POW;
        res[i - 1] = std::min(((q.a[n + i - 1] * BASE + q.a[n + i - 2]) / static_cast<int64_t>(b.a.back())), BASE - static_cast<int64_t>(1));		q -= b.multiply(res[i - 1]);
        while (q < 0)  {
        	res[i - 1]--;
			q += b;
		}
	}
	this->a = res;
	this->sign = si;
	return *this;
}

big_integer& big_integer::operator%=(big_integer const& rhs)
{
    this->operator=(*this - (*this / rhs) * rhs);
    return *this;
}

big_integer& big_integer::operator&=(big_integer const& rhs)
{
    for (size_t i = 0; i < std::min(this->a.size(), rhs.a.size()); i++) {
        this->a[i] = (this->a[i]*this->sign) & (rhs.a[i] * rhs.sign);
    }
    for (size_t i = rhs.a.size(); i < this->a.size(); i++) {
        this->a[i] = 0;
    }
    this->normalize();
    return *this;
}

big_integer& big_integer::operator|=(big_integer const& rhs)
{
    for (size_t i = 0; i < std::min(this->a.size(), rhs.a.size()); i++) {
        this->a[i] = (this->a[i]*this->sign) | (rhs.a[i]*rhs.sign);
    }
    for (size_t i = this->a.size(); i < rhs.a.size(); i++) {
        this->a.push_back(rhs.a[i]);
    }
    this->normalize();
    return *this;
}

big_integer& big_integer::operator^=(big_integer const& rhs)
{
    for (size_t i = 0; i < std::min(this->a.size(), rhs.a.size()); i++) {
        this->a[i] = (this->a[i] * this->sign) ^ (rhs.a[i] * rhs.sign);
    }
    for (size_t i = this->a.size(); i < rhs.a.size(); i++) {
        this->a.push_back(rhs.a[i]);
    }
    this->normalize();
    return *this;
}

big_integer& big_integer::operator<<=(int rhs)
{
    if (rhs == 0) {
        return *this;
    }
    if (rhs < 0) {
        this->operator>>=(-1*rhs);
        return *this;
    }
    uint32_t shift = rhs / POW;
    uint32_t mul = rhs % POW;
    big_integer ans;
    ans.a.clear();
    ans.a.resize(this->a.size() + shift);
    ans.sign = this->sign;
    for (size_t i = 0; i < shift; i++) {
        ans.a[i] = 0;
    }
    for (size_t i = 0; i < this->a.size(); i++) {
        ans.a[i + shift] = this->a[i];
    }
    ans *= (1 << mul);
    this->operator=(ans);
    return *this;
}

big_integer& big_integer::operator>>=(int rhs)
{
    if (rhs == 0) {
        return *this;
    }
    if (rhs < 0) {
        this->operator<<=(-1*rhs);
        return *this;
    }
    uint32_t shift = rhs / POW;
    uint32_t mul = rhs % POW;
    big_integer ans;
    ans.a.clear();
    ans.a.resize(this->a.size() - shift);
    ans.sign = this->sign;
    for (size_t i = shift; i < this->a.size(); i++) {
        ans.a[i - shift] = this->sign * ((this->sign * this->a[i]) >> mul);
        ans.a[i - shift] += (i + 1 < this->a.size()) ? ((a[i + 1] & uint32_t((1 << mul) - 1)) << (POW - mul)) : 0;
    }
    this->operator=(ans);
    return *this;
}

big_integer big_integer::operator+() const
{
    return *this;
}

big_integer big_integer::operator-() const
{
    big_integer r = *this;
    if (r != 0) {
        r.sign *= -1;
    }
	return r;
}

big_integer big_integer::operator~() const
{
	big_integer tmp(*this);
	tmp += 1;
	tmp.sign = -1 * tmp.sign;
	return tmp;
}

big_integer& big_integer::operator++()
{
    this->operator+=(1);
    return *this;
}

big_integer big_integer::operator++(int)
{
    big_integer r = *this;
    ++*this;
    return r;
}

big_integer& big_integer::operator--()
{
    this->operator-=(1);
    return *this;
}

big_integer big_integer::operator--(int)
{
    big_integer r = *this;
    --*this;
    return r;
}

big_integer operator+(big_integer a, big_integer const& b)
{
    return a += b;
}

big_integer operator-(big_integer a, big_integer const& b)
{
    return a -= b;
}

big_integer operator*(big_integer a, big_integer const& b)
{
    return a *= b;
}

big_integer operator/(big_integer a, big_integer const& b)
{
    return a /= b;
}

big_integer operator%(big_integer a, big_integer const& b)
{
    return a %= b;
}

big_integer operator&(big_integer a, big_integer const& b)
{
    return a &= b;
}

big_integer operator|(big_integer a, big_integer const& b)
{
    return a |= b;
}

big_integer operator^(big_integer a, big_integer const& b)
{
    return a ^= b;
}

big_integer operator<<(big_integer a, int b)
{
    return a <<= b;
}

big_integer operator>>(big_integer a, int b)
{
    return a >>= b;
}

bool operator==(big_integer const& x, big_integer const& y)
{
    return x.sign == y.sign && x.a == y.a;
}

bool operator!=(big_integer const& x, big_integer const& y)
{
    return !(x == y);
}

bool operator<(big_integer const& x, big_integer const& y)
{
    if (x.sign != y.sign) {
        return x.sign < y.sign;
    }
    if (x.a.size() != y.a.size()) {
        return (x.sign * x.a.size()) < (y.sign * y.a.size());
    }
    for (size_t i = x.a.size(); i > 0; i--) {
        if (x.a[i - 1] != y.a[i - 1]) {
            return (x.sign * x.a[i - 1]) < (y.sign * y.a[i - 1]);
        }
    }
    return false;
}

bool operator>(big_integer const& x, big_integer const& y)
{
    return !((x < y) || (x == y));
}

bool operator<=(big_integer const& x, big_integer const& y)
{
    return (x < y) || (x == y);
}

bool operator>=(big_integer const& x, big_integer const& y)
{
    return !(x < y);
}

std::string to_string(big_integer const& x)
{
    int32_t wt[4] = {6, 8, 4, 2};
    big_integer x1 = x;
    x1.sign = 1;
    std::string s = "";
    if (x1 == 0) {
        s += "0";
        return s;
    }
    while (x1 > 0) {
        int32_t cur = x1.a[0] % 10;
        for (size_t i = 1; i < x1.a.size(); i++) {
            cur += (x1.a[i] % 10) * wt[i % 4];
            cur %= 10;
        }
        s += ('0' + cur);
        x1 /= 10;
    }
    for (size_t i = 0; i < s.size() / 2; i++) {
        std::swap(s[i], s[s.size() - 1 - i]);
    }
    if (x.sign == -1) {
        s = "-" + s;
    }
    return s;
}

std::ostream& operator<<(std::ostream& s, big_integer const& a)
{
    return s << to_string(a);
}

inline void big_integer::normalize() {
    big_integer tmp = *this;
    tmp.a.push_back(0);
    for (size_t i = 0; i < tmp.a.size() - 1; i++) {
        if (tmp.a[i] < 0) {
            tmp.a[i + 1]--;
            tmp.a[i] += BASE;
        }
        else {
            if (tmp.a[i] >= BASE) {
                tmp.a[i + 1] += tmp.a[i] / BASE;
                tmp.a[i] %= BASE;
            }
        }
    }
    if (tmp.a[tmp.a.size() - 1] < 0) {
        tmp = *this;
        tmp.a.push_back(0);
        tmp.sign *= -1;
        for (size_t i = 0; i < tmp.a.size(); i++) {
            tmp.a[i] *= -1;
        }
        for (size_t i = 0; i < tmp.a.size() - 1; i++) {
            if (tmp.a[i] < 0) {
                tmp.a[i + 1]--;
                tmp.a[i] += BASE;
            }
            else {
                if (tmp.a[i] >= BASE) {
                    tmp.a[i + 1] += tmp.a[i] / BASE;
                    tmp.a[i] %= BASE;
                }
            }
        }
    }
    int32_t last = -1;
    this->a.clear();
    this->sign = tmp.sign;
    for (uint32_t i = 0; i < tmp.a.size(); i++) {
        if (tmp.a[i] != 0) {
            last = i;
        }
    }
    if (last == -1) {
        this->sign = 1;
        last = 0;
    }
    this->a.resize(last + 1);
    for (size_t i = 0; i <= (uint32_t)last; i++) {
        this->a[i] = tmp.a[i];
    }
}

void big_integer::delete_zeroes() {
	while (!this->a.empty() && !this->a.back()) {
		this->a.pop_back();
	}
	if (this->a.empty()) {
		this->sign = 1;
		this->a.push_back(0);
	}
}
