# Othello

>"Language-oriented development allows us to shape our tools around the unique linguistics of our problems, making our code more intuitive and expressive. When combined with test-balanced development, we focus not just on covering every inch, but on ensuring that our software's core narrative remains unbroken and robust. It's a harmonious dance between the poetry of our language and the rhythm of our expressions."
>
>Alistair Haldane, Practical Functional Programming, 2023


## Table of Contents

- [Philosophy](#philosophy)
- [License](#license)


## Philosophy

The Othello library aims to provide an expressive set of functional programming tools to the Emacs Lisp. The philosophy that underlies its design and functionality can be summarized as follows:

1. **First-Class Functions**: In functional programming, functions are treated as first-class citizens, meaning they can be assigned to variables, passed as arguments, and returned as values. Othello follows this paradigm rigorously, with a focus on composing and manipulating functions.

2. **Immutable Data**: Othello emphasizes immutability. Rather than changing data in place, operations create and return new values, leaving the original data untouched. This approach reduces side effects and makes code more predictable.

3. **Elegance and Simplicity**: One of the major draws of functional programming is its elegance. Othello provides abstractions that aim to simplify complex operations, making code concise, expressive, and easier to reason about.

4. **Higher-Order Functions**: Functions that operate on other functions, either by taking them as arguments or by returning them, are central to Othello. Constructs such as `map`, `filter`, and `fold` demonstrate this concept.

5. **Recursion Over Iteration**: The library leans towards recursive patterns over iterative ones. While this may be slightly foreign to those accustomed to traditional looping mechanisms, recursion is a fundamental aspect of functional programming.

6. **Code Clarity**: The library aims to present solutions in a way that they read as close to English as possible. For example, the `evenp` function checks if a number is even, and `sum` calculates the sum of a list.

7. **Declarative Over Imperative**: Othello favors a declarative approach. Instead of specifying _how_ to achieve something step-by-step, you describe _what_ you want to achieve, and the library takes care of the details.

8. **Extensibility**: As the Lisp environment is inherently extensible, so too is Othello. The library provides basic building blocks that can be combined and extended to suit specific needs.

By providing these functional programming primitives and tools, Othello hopes to facilitate a functional programming paradigm within the Emacs ecosystem, allowing developers to harness the power and elegance of functional programming in their Emacs Lisp projects.


## License
Othello is licensed under the MIT License. See LICENSE for more information.


>"Threading, for data-centric development – such that the developer thinks foremost about the transformation of data through the compositional application of pure functions, is one of the most profound functional programming concepts in terms of setting the mindset of the developer."
