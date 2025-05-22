# CIS194 Homework 1: Validating Credit Card Numbers

## Learning Objectives
- Get comfortable with Haskell syntax and basic types
- Learn about pattern matching and guards
- Understand basic list operations
- Work with recursion in Haskell

## Problem Description
In this homework, you will implement functions to validate credit card numbers. The validation process involves several steps:

1. Double every second digit from the right
2. Sum all the digits
3. Check if the sum is divisible by 10

## Tasks
1. Implement `toDigits` to convert a number to a list of digits
2. Implement `doubleEveryOther` to double every other digit
3. Implement `sumDigits` to sum all digits
4. Implement `validate` to check if a credit card number is valid

## Getting Started
1. Open `Homework1.hs` in your editor
2. Implement the required functions
3. Test your implementation using `stack test`

## Notes for C Developers
- Haskell uses pattern matching instead of if/else statements
- Lists are immutable and recursive
- Functions are pure and don't have side effects
- Type signatures are important and help catch errors early 