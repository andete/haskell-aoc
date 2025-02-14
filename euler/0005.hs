-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

--        1   2   3   2*2 5  2*3 7   2*2*2 3*3  2*5
--        1   2   3   4   5  6   7   8     9    10
example = 1 * 2 * 3 * 2 * 5    * 7 * 2   * 3

-- any number that cannot be written as a product of the previous prime factors, the new prime factor is added

--       1   2   3   2*2 5   2*3 7   2*2*2 3*3 2*5 11 2*2*3 13  2*7 3*5 2*2*2*2 17 2*3*3 19 2*2*5
--       1   2   3   4   5   6   7   8     9   10  11 12    13  14  15  16      17 18    19 20  
result = 1 * 2 * 3 * 2 * 5     * 7 * 2   * 3     * 11     * 13        * 2     * 17     * 19

expected = result == 232792560