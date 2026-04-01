#include "big_integer.h"
#include <algorithm>
#include <cctype>
#include <climits>
#include <stdexcept>
#include <string>

static void remove_leading_zeros(std::vector<int>& digits) {
    while (digits.size() > 1 && digits.back() == 0) {
        digits.pop_back();
    }
}

static int compare_abs(const std::vector<int>& a, const std::vector<int>& b) {
    if (a.size() != b.size()) {
        return (a.size() < b.size()) ? -1 : 1;
    }
    for (int i = static_cast<int>(a.size()) - 1; i >= 0; --i) {
        if (a[i] != b[i]) {
            return (a[i] < b[i]) ? -1 : 1;
        }
    }
    return 0;
}

static std::vector<int> add_abs(const std::vector<int>& a, const std::vector<int>& b) {
    std::vector<int> result;
    int carry = 0;
    size_t max_len = std::max(a.size(), b.size());

    for (size_t i = 0; i < max_len || carry != 0; ++i) {
        int sum = carry;
        if (i < a.size()) sum += a[i];
        if (i < b.size()) sum += b[i];

        result.push_back(sum % 10);
        carry = sum / 10;
    }
    return result;
}

static std::vector<int> subtract_abs(const std::vector<int>& a, const std::vector<int>& b) {
    std::vector<int> result;
    int borrow = 0;

    for (size_t i = 0; i < a.size(); ++i) {
        int diff = a[i] - borrow;
        if (i < b.size()) diff -= b[i];

        if (diff < 0) {
            diff += 10;
            borrow = 1;
        } else {
            borrow = 0;
        }
        result.push_back(diff);
    }

    remove_leading_zeros(result);
    return result;
}

BigInteger::BigInteger() {
    digits_ = {0};
    negative_ = false;
}

BigInteger::BigInteger(int value) {
    if (value == INT_MIN) {
        negative_ = true;
        long long temp = -(static_cast<long long>(value));
        while (temp > 0) {
            digits_.push_back(static_cast<int>(temp % 10));
            temp /= 10;
        }
        return;
    }

    negative_ = (value < 0);
    if (value < 0) value = -value;

    if (value == 0) {
        digits_ = {0};
        return;
    }

    while (value > 0) {
        digits_.push_back(value % 10);
        value /= 10;
    }
}

BigInteger::BigInteger(long long value) {
    unsigned long long abs_value;

    if (value < 0) {
        negative_ = true;
        if (value == LLONG_MIN) {
            abs_value = static_cast<unsigned long long>(LLONG_MAX) + 1;
        } else {
            abs_value = static_cast<unsigned long long>(-value);
        }
    } else {
        negative_ = false;
        abs_value = static_cast<unsigned long long>(value);
    }

    if (abs_value == 0) {
        digits_ = {0};
        negative_ = false;
        return;
    }

    while (abs_value > 0) {
        digits_.push_back(static_cast<int>(abs_value % 10));
        abs_value /= 10;
    }
}

BigInteger::BigInteger(const std::string& str) {
    if (str.empty()) {
        digits_ = {0};
        negative_ = false;
        return;
    }

    size_t start = 0;
    negative_ = false;

    if (str[0] == '-') {
        negative_ = true;
        start = 1;
    } else if (str[0] == '+') {
        start = 1;
    }

    if (start == str.length()) {
        digits_ = {0};
        negative_ = false;
        return;
    }

    for (int i = static_cast<int>(str.length()) - 1; i >= static_cast<int>(start); --i) {
        char c = str[i];
        if (!std::isdigit(static_cast<unsigned char>(c))) {
            digits_ = {0};
            negative_ = false;
            return;
        }
        digits_.push_back(c - '0');
    }

    remove_leading_zeros(digits_);

    if (digits_.size() == 1 && digits_[0] == 0) {
        negative_ = false;
    }
}

bool BigInteger::operator==(const BigInteger& other) const {
    return negative_ == other.negative_ && digits_ == other.digits_;
}

bool BigInteger::operator!=(const BigInteger& other) const {
    return !(*this == other);
}

bool BigInteger::operator<(const BigInteger& other) const {
    if (negative_ != other.negative_) {
        return negative_;
    }

    int cmp = compare_abs(digits_, other.digits_);
    if (!negative_) {
        return cmp < 0;
    }
    return cmp > 0;
}

bool BigInteger::operator<=(const BigInteger& other) const {
    return (*this < other) || (*this == other);
}

bool BigInteger::operator>(const BigInteger& other) const {
    return !(*this <= other);
}

bool BigInteger::operator>=(const BigInteger& other) const {
    return !(*this < other);
}

BigInteger BigInteger::operator+(const BigInteger& other) const {
    BigInteger result;

    if (negative_ == other.negative_) {
        result.digits_ = add_abs(digits_, other.digits_);
        result.negative_ = negative_;
    } else {
        int cmp = compare_abs(digits_, other.digits_);
        if (cmp == 0) {
            return BigInteger(0);
        } else if (cmp > 0) {
            result.digits_ = subtract_abs(digits_, other.digits_);
            result.negative_ = negative_;
        } else {
            result.digits_ = subtract_abs(other.digits_, digits_);
            result.negative_ = other.negative_;
        }
    }

    if (result.is_zero()) {
        result.negative_ = false;
    }
    return result;
}

BigInteger BigInteger::operator-(const BigInteger& other) const {
    return *this + (-other);
}

BigInteger BigInteger::operator*(const BigInteger& other) const {
    std::vector<int> result(digits_.size() + other.digits_.size(), 0);

    for (size_t i = 0; i < digits_.size(); ++i) {
        int carry = 0;
        for (size_t j = 0; j < other.digits_.size() || carry != 0; ++j) {
            long long sum = result[i + j] + carry;
            if (j < other.digits_.size()) {
                sum += static_cast<long long>(digits_[i]) * other.digits_[j];
            }
            result[i + j] = static_cast<int>(sum % 10);
            carry = static_cast<int>(sum / 10);
        }
    }

    remove_leading_zeros(result);

    BigInteger res;
    res.digits_ = result;
    res.negative_ = (negative_ != other.negative_) && !res.is_zero();
    return res;
}

BigInteger BigInteger::operator/(const BigInteger& other) const {
    if (other.is_zero()) {
        throw std::runtime_error("Division by zero");
    }

    if (compare_abs(digits_, other.digits_) < 0) {
        return BigInteger(0);
    }

    if (*this == other) {
        return BigInteger(negative_ == other.negative_ ? 1 : -1);
    }

    bool result_sign = (negative_ != other.negative_);

    BigInteger dividend = *this;
    BigInteger divisor = other;
    dividend.negative_ = false;
    divisor.negative_ = false;

    std::vector<int> quotient;
    BigInteger current;

    for (int i = static_cast<int>(dividend.digits_.size()) - 1; i >= 0; --i) {
        if (current.digits_.size() == 1 && current.digits_[0] == 0) {
            current.digits_[0] = dividend.digits_[i];
        } else {
            current.digits_.insert(current.digits_.begin(), dividend.digits_[i]);
        }
        remove_leading_zeros(current.digits_);

        int digit = 0;
        while (compare_abs(current.digits_, divisor.digits_) >= 0) {
            current.digits_ = subtract_abs(current.digits_, divisor.digits_);
            ++digit;
        }

        quotient.insert(quotient.begin(), digit);
    }

    remove_leading_zeros(quotient);

    BigInteger result;
    result.digits_ = quotient;
    result.negative_ = result_sign;

    if (result.is_zero()) {
        result.negative_ = false;
    }
    return result;
}

BigInteger BigInteger::operator%(const BigInteger& other) const {
    BigInteger quotient = *this / other;
    return *this - (quotient * other);
}

BigInteger& BigInteger::operator+=(const BigInteger& other) {
    *this = *this + other;
    return *this;
}

BigInteger& BigInteger::operator-=(const BigInteger& other) {
    *this = *this - other;
    return *this;
}

BigInteger& BigInteger::operator*=(const BigInteger& other) {
    *this = *this * other;
    return *this;
}

BigInteger& BigInteger::operator/=(const BigInteger& other) {
    *this = *this / other;
    return *this;
}

BigInteger& BigInteger::operator%=(const BigInteger& other) {
    *this = *this % other;
    return *this;
}

BigInteger BigInteger::operator-() const {
    BigInteger result = *this;
    if (!result.is_zero()) {
        result.negative_ = !result.negative_;
    }
    return result;
}

BigInteger& BigInteger::operator++() {
    *this += 1;
    return *this;
}

BigInteger BigInteger::operator++(int) {
    BigInteger old = *this;
    ++(*this);
    return old;
}

BigInteger& BigInteger::operator--() {
    *this -= 1;
    return *this;
}

BigInteger BigInteger::operator--(int) {
    BigInteger old = *this;
    --(*this);
    return old;
}

std::string BigInteger::to_string() const {
    if (is_zero()) {
        return "0";
    }

    std::string result;
    if (negative_) {
        result += "-";
    }

    for (int i = static_cast<int>(digits_.size()) - 1; i >= 0; --i) {
        result += static_cast<char>('0' + digits_[i]);
    }
    return result;
}

bool BigInteger::is_zero() const {
    return digits_.size() == 1 && digits_[0] == 0;
}

bool BigInteger::is_negative() const {
    return negative_;
}

BigInteger::operator bool() const {
    return !is_zero();
}

std::ostream& operator<<(std::ostream& os, const BigInteger& value) {
    os << value.to_string();
    return os;
}

std::istream& operator>>(std::istream& is, BigInteger& value) {
    std::string str;
    is >> str;
    value = BigInteger(str);
    return is;
}