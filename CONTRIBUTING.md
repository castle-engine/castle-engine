# Contributing to Castle Game Engine development

Thank you for contributing to our engine!

## Coding conventions and guidlines

We have [coding conventions](https://castle-engine.io/coding_conventions) page that describes in full everything. Here's a short summary:

- **Mundane stuff**: Indent by 2 spaces, never use tabs, matching `begin` and `end` should usually have the same indentation, never use `with`.

- **Write a documentation** for everything in the unit interface, simply by placing a comment before each identifier. Use [PasDoc](https://pasdoc.github.io/) tags.

- In general, **changes should be backward-compatible**. Use `deprecated` to keep old API available.

- Put **extra care into the code quality**. Figure out the simplest solution to the given problem, that is reliable (will work in a predictable way in various cases) and easy to maintain.

- **Do not make low-level code optimizations blindly**, they are often not worth the work and they lower code quality. In particular, never use assembler in CGE. **Think of high-level speed optimizations (e.g. doing some work on the GPU)**.

- **Fix all compiler warnings**.

- Contribute by GitHub **pull requests**.

- **Fix all problems reported by the GitHub Actions** (you will get an email from GitHub when your commit has issues).

## Let's talk

If you have any questions, join us on [forum](https://forum.castle-engine.io/) or [Discord or other channels](https://castle-engine.io/talk.php). We're friendly :)

Tell us what you want to do, how, why. Tell us about your success stories (or the other ones :) ) when trying to use the engine. We're eager to hear them all!
