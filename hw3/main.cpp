#include <cstdio>
#include <algorithm>
#include <exception>
#include <iostream>

using namespace std;

struct signed_tag {};
struct unsigned_tag {};

struct overflow_exception : std::exception {
    char *what() throw() {
        return "overflow";
    }
};

template<typename T>
class checked {
private:
    T value;
public:
    checked () {};
    checked (T x) {value = x;};
    T get_value() {return value;};
};

template<typename T>
checked<T> sum(checked<T> a1, checked<T> b1, unsigned_tag) {
    T a = a1.get_value(), b = b1.get_value();
    T MAX = std::numeric_limits<T>::max();
    if (MAX - a < b) {
        throw overflow_exception();
    } else {
        return a + b;
    }
}

template<typename T>
checked<T> sum(checked<T> a1, checked<T> b1, signed_tag) {
    T a = a1.get_value(), b = b1.get_value();
    T MAX = std::numeric_limits<T>::max();
    T MIN = std::numeric_limits<T>::min();
    if (((b > 0) && (a > (MAX - b))) || ((b < 0) && (a < (MIN - b)))) {
        throw overflow_exception();
    } else {
        return a + b;
    }
}

template<typename T>
checked<T> sub(checked<T> a1, checked<T> b1, unsigned_tag) {
    T a = a1.get_value(), b = b1.get_value();
    if (a < b) {
        throw overflow_exception();
    }
    else {
        return a - b;
    }
}

template<typename T>
checked<T> sub(checked<T> a1, checked<T> b1, signed_tag) {
    T a = a1.get_value(), b = b1.get_value();
    T MAX = std::numeric_limits<T>::max();
    T MIN = std::numeric_limits<T>::min();
    if ((b > 0 && a < MIN + b) || (b < 0 && a > MAX + b)) {
        throw overflow_exception();
    } else {
        return a - b;
    }
}


template<typename T>
checked<T> mul(checked<T> a1, checked<T> b1, unsigned_tag) {
    T a = a1.get_value(), b = b1.get_value();
    if (b <= 1) {
        return a * b;
    }
    T MAX = std::numeric_limits<T>::max();
    T MIN = std::numeric_limits<T>::min();
    if (MAX / b < a) {
        throw overflow_exception();
    }
    else {
        return a * b;
    }
}

template<typename T>
checked<T> mul(checked<T> a1, checked<T> b1, signed_tag) {
    T a = a1.get_value(), b = b1.get_value();
    T MAX = std::numeric_limits<T>::max();
    T MIN = std::numeric_limits<T>::min();
    double bad1 = (double)MAX / (double)MIN;
    double bad2 = (double)MIN / (double)MAX;
    if ((b <= 1)&&((double)b >= bad1)) {
        return a * b;
    }
    if (((double)b >= bad2) && ((double)b <= bad1)) {
        if (a < -1 * MAX) {
            throw overflow_exception();
        }
        else {
            return a * b;
        }
    }
    if ((MAX / b < a) || (MIN / b > a)) {
        throw overflow_exception();
    }
    else {
        return a * b;
    }
}

template<typename T>
checked<T> div(checked<T> a1, checked<T> b1, unsigned_tag) {
    T a = a1.get_value(), b = b1.get_value();
    T MAX = std::numeric_limits<T>::max();
    if (b == 0) {
        throw overflow_exception();
    }
    if ((b < 1) && (a > MAX * b)) {
        throw overflow_exception();
    }
    else {
        return a / b;
    }
}

template<typename T>
checked<T> div(checked<T> a1, checked<T> b1, signed_tag) {
    T a = a1.get_value(), b = b1.get_value();
    T MAX = std::numeric_limits<T>::max();
    T MIN = std::numeric_limits<T>::min();
    double bad1 = (double)MAX / (double)MIN;
    double bad2 = (double)MIN / (double)MAX;
    if (b == 0) {
        throw overflow_exception();
    }
    if ((b >= 1) || ((double)b <= bad2)){
        return a / b;
    }
    if (((double)b >= bad2) && ((double)b <= bad1)) {
        if (a < -1 * MAX) {
            throw overflow_exception();
        }
        else {
            return a / b;
        }
    }

    if ((a > MAX * b) || (a < MIN * b)) {
        throw overflow_exception();
    }
    else {
        return a / b;
    }
}


template<typename T>
checked<T> neg(checked<T> a1, unsigned_tag) {
    T a = a1.get_value();
    if (a == 0) {
        return 0;
    }
    else {
        throw overflow_exception();
    }
}

template<typename T>
checked<T> neg(checked<T> a1, signed_tag) {
    T a = a1.get_value();
    T MAX = std::numeric_limits<T>::max();
    if (a + MAX < 0 ) {
        throw overflow_exception();
    }
    else {
        return a * -1;
    }
}

template<typename T>
checked<T> operator+ (const checked<T>& a, const checked<T>& b) {
    if (is_unsigned<T>::value) {
        return sum(a, b, unsigned_tag());
    }
    else {
        return sum(a, b, signed_tag());
    }
}

template<typename T>
checked<T> operator- (const checked<T>& a, const checked<T>& b) {
    if (is_unsigned<T>::value) {
        return sub(a, b, unsigned_tag());
    }
    else {
        return sub(a, b, signed_tag());
    }
}

template<typename T>
checked<T> operator* (const checked<T>& a, const checked<T>& b) {
    if (is_unsigned<T>::value) {
        return mul(a, b, unsigned_tag());
    }
    else {
        return mul(a, b, signed_tag());
    }
}

template<typename T>
checked<T> operator/ (const checked<T>& a, const checked<T>& b) {
    if (is_unsigned<T>::value) {
        return div(a, b, unsigned_tag());
    }
    else {
        return div(a, b, signed_tag());
    }
}

template<typename T>
checked<T> operator- (const checked<T>& a) {
    if (is_unsigned<T>::value) {
        return neg(a, unsigned_tag());
    }
    else {
        return neg(a, signed_tag());
    }
}
