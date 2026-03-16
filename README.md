# Lab Assignment: BigInteger

## Student Info

| Field     | Value               |
|-----------|---------------------|
| **Name**  | _John Doe_          |
| **Group** | _CS-101_            |

> **Replace the data above with your own!**

---

## Description

Implementation of the `BigInteger` class -- an arbitrary-precision signed integer supporting arithmetic operations, comparisons, and stream-based I/O.

## How to Work with This Repo

### 1. Fork the repository

Click the **Fork** button in the top-right corner of this page.

### 2. Clone your fork

```bash
git clone https://github.com/<your-username>/biginteger-lab.git
cd biginteger-lab
```

### 3. Enable GitHub Actions

Go to the **Actions** tab in your fork and click
**"I understand my workflows, go ahead and enable them"**.

### 4. Implement `big_integer.cpp`

Create the file `big_integer.cpp` and implement all methods declared in `big_integer.h`.

**Rules:**
- Modifying `big_integer.h` (public interface) is **forbidden**.
- Modifying the private section is **allowed** (internal representation is your choice).
- Modifying `tests.cpp` is **forbidden**.
- You may add helper files, but you should not need to change `CMakeLists.txt`.

### 5. Build and test locally

```bash
cmake -B build
cmake --build build -j$(nproc)
cd build && ctest --output-on-failure
```

### 6. Push and check CI

```bash
git add .
git commit -m "Implement BigInteger"
git push
```

Go to the **Actions** tab of your fork -- a test run will appear there.
Green checkmark -- all tests passed. Red cross -- there are errors.

### 7. Fill in the README

Enter your name and group in the table above.

---

## Build Details

The project is compiled with strict flags:

```
-Wall -Wextra -Wpedantic -Werror
-fsanitize=address,undefined
```

This means **all warnings are treated as compilation errors**, and AddressSanitizer / UndefinedBehaviorSanitizer are enabled at runtime. Make sure your code is warning-free and has no memory errors.
