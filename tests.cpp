#include <gtest/gtest.h>
#include <sstream>
#include <climits>
#include "big_integer.h"

// =============================================================================
//  1. Constructors and basic properties
// =============================================================================

TEST(Constructor, Default) {
    BigInteger a;
    EXPECT_EQ(a.to_string(), "0");
    EXPECT_TRUE(a.is_zero());
    EXPECT_FALSE(a.is_negative());
}

TEST(Constructor, FromInt) {
    EXPECT_EQ(BigInteger(0).to_string(),   "0");
    EXPECT_EQ(BigInteger(42).to_string(),  "42");
    EXPECT_EQ(BigInteger(-42).to_string(), "-42");
    EXPECT_EQ(BigInteger(INT_MAX).to_string(), std::to_string(INT_MAX));
    EXPECT_EQ(BigInteger(INT_MIN).to_string(), std::to_string(INT_MIN));
}

TEST(Constructor, FromLongLong) {
    long long big = 9'000'000'000'000'000'000LL;
    EXPECT_EQ(BigInteger(big).to_string(), std::to_string(big));

    long long neg = -9'000'000'000'000'000'000LL;
    EXPECT_EQ(BigInteger(neg).to_string(), std::to_string(neg));
}

TEST(Constructor, FromString) {
    EXPECT_EQ(BigInteger("0").to_string(), "0");
    EXPECT_EQ(BigInteger("12345678901234567890").to_string(), "12345678901234567890");
    EXPECT_EQ(BigInteger("-99999999999999999999").to_string(), "-99999999999999999999");
    EXPECT_EQ(BigInteger("00042").to_string(), "42");     // leading zeros
    EXPECT_EQ(BigInteger("-00042").to_string(), "-42");
    EXPECT_EQ(BigInteger("-0").to_string(), "0");          // negative zero = zero
}

TEST(Constructor, Copy) {
    BigInteger a("123456789012345678901234567890");
    BigInteger b(a);
    EXPECT_EQ(a.to_string(), b.to_string());
}

// =============================================================================
//  2. Comparison
// =============================================================================

TEST(Comparison, Equal) {
    EXPECT_TRUE(BigInteger(0) == BigInteger(0));
    EXPECT_TRUE(BigInteger(42) == BigInteger(42));
    EXPECT_TRUE(BigInteger(-42) == BigInteger(-42));
    EXPECT_TRUE(BigInteger("999999999999999999999") == BigInteger("999999999999999999999"));
    EXPECT_FALSE(BigInteger(1) == BigInteger(-1));
}

TEST(Comparison, NotEqual) {
    EXPECT_TRUE(BigInteger(1) != BigInteger(2));
    EXPECT_FALSE(BigInteger(42) != BigInteger(42));
}

TEST(Comparison, LessThan) {
    EXPECT_TRUE(BigInteger(-1) < BigInteger(0));
    EXPECT_TRUE(BigInteger(0) < BigInteger(1));
    EXPECT_TRUE(BigInteger(99) < BigInteger(100));
    EXPECT_TRUE(BigInteger(-100) < BigInteger(-99));
    EXPECT_TRUE(BigInteger("999999999999999999998") < BigInteger("999999999999999999999"));
    EXPECT_FALSE(BigInteger(5) < BigInteger(5));
    EXPECT_FALSE(BigInteger(10) < BigInteger(3));
}

TEST(Comparison, GreaterThan) {
    EXPECT_TRUE(BigInteger(1) > BigInteger(0));
    EXPECT_TRUE(BigInteger(0) > BigInteger(-1));
    EXPECT_TRUE(BigInteger(-99) > BigInteger(-100));
    EXPECT_FALSE(BigInteger(5) > BigInteger(5));
}

TEST(Comparison, LessOrEqual) {
    EXPECT_TRUE(BigInteger(5) <= BigInteger(5));
    EXPECT_TRUE(BigInteger(4) <= BigInteger(5));
    EXPECT_FALSE(BigInteger(6) <= BigInteger(5));
}

TEST(Comparison, GreaterOrEqual) {
    EXPECT_TRUE(BigInteger(5) >= BigInteger(5));
    EXPECT_TRUE(BigInteger(6) >= BigInteger(5));
    EXPECT_FALSE(BigInteger(4) >= BigInteger(5));
}

TEST(Comparison, DifferentLengths) {
    EXPECT_TRUE(BigInteger(9) < BigInteger(10));
    EXPECT_TRUE(BigInteger(999) < BigInteger(1000));
    EXPECT_TRUE(BigInteger(-1000) < BigInteger(-999));
}

// =============================================================================
//  3. Addition
// =============================================================================

TEST(Addition, BasicPositive) {
    EXPECT_EQ((BigInteger(2) + BigInteger(3)).to_string(), "5");
    EXPECT_EQ((BigInteger(0) + BigInteger(0)).to_string(), "0");
    EXPECT_EQ((BigInteger(999) + BigInteger(1)).to_string(), "1000");
}

TEST(Addition, WithNegatives) {
    EXPECT_EQ((BigInteger(-2) + BigInteger(-3)).to_string(), "-5");
    EXPECT_EQ((BigInteger(-5) + BigInteger(3)).to_string(), "-2");
    EXPECT_EQ((BigInteger(5) + BigInteger(-3)).to_string(), "2");
    EXPECT_EQ((BigInteger(3) + BigInteger(-5)).to_string(), "-2");
    EXPECT_EQ((BigInteger(5) + BigInteger(-5)).to_string(), "0");
}

TEST(Addition, Large) {
    BigInteger a("99999999999999999999999999999");
    BigInteger b("1");
    EXPECT_EQ((a + b).to_string(), "100000000000000000000000000000");
}

TEST(Addition, CompoundAssignment) {
    BigInteger a(10);
    a += BigInteger(20);
    EXPECT_EQ(a.to_string(), "30");

    a += BigInteger(-50);
    EXPECT_EQ(a.to_string(), "-20");
}

// =============================================================================
//  4. Subtraction
// =============================================================================

TEST(Subtraction, Basic) {
    EXPECT_EQ((BigInteger(5) - BigInteger(3)).to_string(), "2");
    EXPECT_EQ((BigInteger(3) - BigInteger(5)).to_string(), "-2");
    EXPECT_EQ((BigInteger(5) - BigInteger(5)).to_string(), "0");
}

TEST(Subtraction, WithNegatives) {
    EXPECT_EQ((BigInteger(-3) - BigInteger(-5)).to_string(), "2");
    EXPECT_EQ((BigInteger(-5) - BigInteger(-3)).to_string(), "-2");
    EXPECT_EQ((BigInteger(-5) - BigInteger(3)).to_string(), "-8");
    EXPECT_EQ((BigInteger(5) - BigInteger(-3)).to_string(), "8");
}

TEST(Subtraction, Large) {
    BigInteger a("100000000000000000000000000000");
    BigInteger b("1");
    EXPECT_EQ((a - b).to_string(), "99999999999999999999999999999");
}

TEST(Subtraction, CompoundAssignment) {
    BigInteger a(10);
    a -= BigInteger(3);
    EXPECT_EQ(a.to_string(), "7");
}

// =============================================================================
//  5. Multiplication
// =============================================================================

TEST(Multiplication, Basic) {
    EXPECT_EQ((BigInteger(6) * BigInteger(7)).to_string(), "42");
    EXPECT_EQ((BigInteger(0) * BigInteger(12345)).to_string(), "0");
    EXPECT_EQ((BigInteger(-3) * BigInteger(4)).to_string(), "-12");
    EXPECT_EQ((BigInteger(-3) * BigInteger(-4)).to_string(), "12");
}

TEST(Multiplication, Large) {
    BigInteger a("123456789012345678901234567890");
    BigInteger b("987654321098765432109876543210");
    // Verified with Python: 123456789012345678901234567890 * 987654321098765432109876543210
    EXPECT_EQ((a * b).to_string(),
              "121932631137021795226185032733866767003075587392818230219019050100");
}

TEST(Multiplication, CompoundAssignment) {
    BigInteger a(100);
    a *= BigInteger(-5);
    EXPECT_EQ(a.to_string(), "-500");
}

TEST(Multiplication, ByZero) {
    BigInteger a("99999999999999999999");
    EXPECT_EQ((a * BigInteger(0)).to_string(), "0");
    EXPECT_TRUE((a * BigInteger(0)).is_zero());
}

// =============================================================================
//  6. Division and modulo
// =============================================================================

TEST(Division, Basic) {
    EXPECT_EQ((BigInteger(10) / BigInteger(3)).to_string(), "3");
    EXPECT_EQ((BigInteger(10) / BigInteger(-3)).to_string(), "-3");
    EXPECT_EQ((BigInteger(-10) / BigInteger(3)).to_string(), "-3");
    EXPECT_EQ((BigInteger(-10) / BigInteger(-3)).to_string(), "3");
    EXPECT_EQ((BigInteger(0) / BigInteger(42)).to_string(), "0");
}

TEST(Division, Exact) {
    EXPECT_EQ((BigInteger(100) / BigInteger(10)).to_string(), "10");
    EXPECT_EQ((BigInteger(999) / BigInteger(3)).to_string(), "333");
}

TEST(Division, Large) {
    BigInteger a("100000000000000000000000000000");
    BigInteger b("100000000000000");
    EXPECT_EQ((a / b).to_string(), "1000000000000000");
}

TEST(Division, ByOne) {
    BigInteger a("12345678901234567890");
    EXPECT_EQ((a / BigInteger(1)).to_string(), "12345678901234567890");
    EXPECT_EQ((a / BigInteger(-1)).to_string(), "-12345678901234567890");
}

TEST(Division, CompoundAssignment) {
    BigInteger a(100);
    a /= BigInteger(7);
    EXPECT_EQ(a.to_string(), "14");
}

TEST(Modulo, Basic) {
    EXPECT_EQ((BigInteger(10) % BigInteger(3)).to_string(), "1");
    EXPECT_EQ((BigInteger(-10) % BigInteger(3)).to_string(), "-1");
    EXPECT_EQ((BigInteger(10) % BigInteger(-3)).to_string(), "1");
    EXPECT_EQ((BigInteger(0) % BigInteger(42)).to_string(), "0");
}

TEST(Modulo, DivisionConsistency) {
    // Verify: a == (a / b) * b + (a % b)
    BigInteger a("123456789012345678901234567890");
    BigInteger b("9876543210");
    BigInteger q = a / b;
    BigInteger r = a % b;
    EXPECT_EQ((q * b + r).to_string(), a.to_string());
}

TEST(Modulo, CompoundAssignment) {
    BigInteger a(17);
    a %= BigInteger(5);
    EXPECT_EQ(a.to_string(), "2");
}

// =============================================================================
//  7. Unary minus
// =============================================================================

TEST(UnaryMinus, Basic) {
    EXPECT_EQ((-BigInteger(42)).to_string(), "-42");
    EXPECT_EQ((-BigInteger(-42)).to_string(), "42");
    EXPECT_EQ((-BigInteger(0)).to_string(), "0");
}

TEST(UnaryMinus, Large) {
    BigInteger a("99999999999999999999999999999");
    EXPECT_EQ((-a).to_string(), "-99999999999999999999999999999");
}

// =============================================================================
//  8. Increment and decrement
// =============================================================================

TEST(Increment, Prefix) {
    BigInteger a(41);
    BigInteger& ref = ++a;
    EXPECT_EQ(a.to_string(), "42");
    EXPECT_EQ(&ref, &a);  // must return reference to itself
}

TEST(Increment, Postfix) {
    BigInteger a(41);
    BigInteger old = a++;
    EXPECT_EQ(old.to_string(), "41");
    EXPECT_EQ(a.to_string(), "42");
}

TEST(Increment, AroundZero) {
    BigInteger a(-1);
    ++a;
    EXPECT_EQ(a.to_string(), "0");
    ++a;
    EXPECT_EQ(a.to_string(), "1");
}

TEST(Increment, Carry) {
    BigInteger a(999);
    ++a;
    EXPECT_EQ(a.to_string(), "1000");
}

TEST(Decrement, Prefix) {
    BigInteger a(43);
    BigInteger& ref = --a;
    EXPECT_EQ(a.to_string(), "42");
    EXPECT_EQ(&ref, &a);
}

TEST(Decrement, Postfix) {
    BigInteger a(43);
    BigInteger old = a--;
    EXPECT_EQ(old.to_string(), "43");
    EXPECT_EQ(a.to_string(), "42");
}

TEST(Decrement, AroundZero) {
    BigInteger a(1);
    --a;
    EXPECT_EQ(a.to_string(), "0");
    --a;
    EXPECT_EQ(a.to_string(), "-1");
}

TEST(Decrement, Borrow) {
    BigInteger a(1000);
    --a;
    EXPECT_EQ(a.to_string(), "999");
}

// =============================================================================
//  9. I/O
// =============================================================================

TEST(IO, Output) {
    std::ostringstream oss;
    oss << BigInteger(42);
    EXPECT_EQ(oss.str(), "42");

    oss.str("");
    oss << BigInteger(-12345);
    EXPECT_EQ(oss.str(), "-12345");

    oss.str("");
    oss << BigInteger(0);
    EXPECT_EQ(oss.str(), "0");
}

TEST(IO, Input) {
    std::istringstream iss("42");
    BigInteger a;
    iss >> a;
    EXPECT_EQ(a.to_string(), "42");

    iss.clear();
    iss.str("-9876543210");
    iss >> a;
    EXPECT_EQ(a.to_string(), "-9876543210");
}

TEST(IO, InputLarge) {
    std::istringstream iss("123456789012345678901234567890");
    BigInteger a;
    iss >> a;
    EXPECT_EQ(a.to_string(), "123456789012345678901234567890");
}

TEST(IO, RoundTrip) {
    BigInteger original("-99999999999999999999999999999");
    std::ostringstream oss;
    oss << original;

    std::istringstream iss(oss.str());
    BigInteger restored;
    iss >> restored;
    EXPECT_EQ(original, restored);
}

// =============================================================================
//  10. Operator bool
// =============================================================================

TEST(BoolConversion, Basic) {
    EXPECT_FALSE(static_cast<bool>(BigInteger(0)));
    EXPECT_TRUE(static_cast<bool>(BigInteger(1)));
    EXPECT_TRUE(static_cast<bool>(BigInteger(-1)));
    EXPECT_TRUE(static_cast<bool>(BigInteger("999999999999999999999")));
}

// =============================================================================
//  11. Edge cases
// =============================================================================

TEST(EdgeCases, SelfAssignment) {
    BigInteger a("12345");
    a = a;
    EXPECT_EQ(a.to_string(), "12345");
}

TEST(EdgeCases, SelfAddition) {
    BigInteger a("50000000000000000000");
    a += a;
    EXPECT_EQ(a.to_string(), "100000000000000000000");
}

TEST(EdgeCases, SelfMultiplication) {
    BigInteger a(12345);
    a *= a;
    EXPECT_EQ(a.to_string(), "152399025");
}

TEST(EdgeCases, SelfSubtraction) {
    BigInteger a("99999999999999999999");
    a -= a;
    EXPECT_EQ(a.to_string(), "0");
    EXPECT_TRUE(a.is_zero());
}

TEST(EdgeCases, ChainedOperations) {
    BigInteger a(10);
    BigInteger b(20);
    BigInteger c(30);
    BigInteger result = a + b * c - BigInteger(5);
    // 10 + 20*30 - 5 = 10 + 600 - 5 = 605
    EXPECT_EQ(result.to_string(), "605");
}

TEST(EdgeCases, VeryLargeNumber) {
    // 100-digit number
    std::string big(100, '9');
    BigInteger a(big);
    EXPECT_EQ(a.to_string(), big);
    EXPECT_EQ((a + BigInteger(1)).to_string(), "1" + std::string(100, '0'));
}
