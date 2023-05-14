# WARNING: WIP. DO NOT USE

# 🔥 Flama

Flama is a domain-specific programming language designed for the [Diamondfire](https://mcdiamondfire.com) Minecraft server. Flama compiles your code into Diamondfire templates.

### Quick Start:

If you have [Rust](https://www.rust-lang.org/tools/install) installed, you can use the `cargo install flama` command.

There is also a downloadable executable in the [releases](https://www.github.com/fallow64/flama/releases) page on GitHub.

### Features:

- Lexical Scoping
- Type Checking and Inference
- Classes
- First-class(ish) Functions

### Cons

- Not suitable for minigame development
- Resulting code will be harder to read than hand-placing
- The language is subject to change
- The CodeUtils support is... iffy.
- Items are a huge pain.

### TODO:

- [ ] Compiler
- [ ] Solidify Grammar
- [ ] Clean up `src/check/type_checker.rs`
- [ ] Seperate modules into different crates (?)
- [ ] Design system for lexically-scoped variables with recursion
- [ ] Classes
- [ ] Exponential Syntax
- [ ] Lambdas / Closures

<sup>it wouldn't be a programming language without a fire emoji in the readme</sup>
