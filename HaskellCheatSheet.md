| Operator(s) | Precedence | Associativity | Description | Example | Explanation |
|------------|------------|--------------|-------------|---------|-------------|
| `.`        | 9          | right        | Function composition | `(negate . sum) [1,2,3]` → `-6` | `f . g` means applying `g` first, then `f`. |
| `!!`       | 9          | left         | List indexing | `[10,20,30] !! 1` → `20` | Retrieves the element at the given index (zero-based). |
| `^`, `^^`, `**` | 8     | right        | Exponentiation | `2 ^ 3` → `8`, `2 ** 3` → `8.0` | `^` (integer exponentiation), `^^` (fractional exponentiation), `**` (floating-point). |
| `*`, `/`   | 7          | left         | Multiplication, division | `6 * 3 / 2` → `9.0` | Standard arithmetic; `*` is multiplication, `/` is floating-point division. |
| `+`, `-`   | 6          | left         | Addition, subtraction | `5 + 3 - 2` → `6` | Standard addition and subtraction. |
| `:`        | 5          | right        | List construction (*cons*) | `1 : [2,3]` → `[1,2,3]` | Prepends an element to a list. |
| `++`       | 5          | right        | List concatenation | `[1,2] ++ [3,4]` → `[1,2,3,4]` | Joins two lists together. |
| `` `elem` ``, `` `notElem` `` | 4 | left | List membership | `3 \`elem\` [1,2,3]` → `True` | Checks if an element is in a list. |
| `==`, `/=`, `<`, `<=`, `>=`, `>` | 4 | left | Equality and comparison | `5 > 3` → `True`, `5 /= 3` → `True` | Standard comparison operators. |
| `&&`       | 3          | right        | Logical AND | `True && False` → `False` | Both must be `True` for the result to be `True`. |
| `\|\`       | 2          | right        | Logical OR | `True || False` → `True` | Either operand can be `True` for the result to be `True`. |
| `>>`, `>>=` | 1         | left         | Monadic bind (ignore return or pass value) | `Just 5 >>= (\x -> Just (x + 1))` → `Just 6` | Used in monadic expressions for sequencing computations. |
| `=<<`      | 1         | right        | Reverse monadic bind | `(\x -> Just (x + 1)) =<< Just 5` → `Just 6` | Same as `>>=`, but arguments reversed. |
| `$`        | 0          | right        | Function application | `sum $ map (*2) [1,2,3]` → `12` | `f $ x` is the same as `f x`, but allows omitting parentheses. |
