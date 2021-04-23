Please read the following document to make collaborating on the project easier for both sides.

# Reporting bugs
If you've encountered a bug, do the following:

- Check if the documentation has information about the problem you have.
  Maybe this isn't a bug, but a desired behavior.
- Check past and current issues, maybe someone had reported your problem already.
  If there's no issue, describing your problem, or there is, but it is closed, please create new issue, and link all closed issues that relate to this problem, if any.
- Tag issue with a `BUG:` at the beginning of the issue name.

# Suggesting features and/or changes
Before suggesting a feature, please check if this feature wasn't requested before.
You can do that in the issues, by filtering issues by `FEATURE:`.
If no feature found, please file new issue, and tag it with a `FEATURE:` at the beginning of the issue name.

# Contributing changes
Please do.

When deciding to contribute a large amount of changes, first consider opening a `DISCUSSION:` type issue, so we could first decide if such dramatic changes are in the scope of the project.
This will save your time, in case such changes are out of the project's scope.

If you're contributing a bugfix, please open an `BUG:` issue first, unless someone already did that.
All bug related merge requests must have a linked issues with a meaningful explanation and steps of reproducing a bug.
Small fixes are also welcome, and doesn't require filing an issue, although we may ask you to do so.

## Writing code
When writing code, consider following the existing style without applying dramatic changes to formatting unless really necessary.
For this particular project, please follow rules as described in [Clojure Style Guide](https://github.com/bbatsov/clojure-style-guide).
If you see any inconsistencies with the style guide in the code, feel free to change these in a non-breaking way.

If you've added new functions, each one must be covered with a set of tests.

When changing existing functions make sure that all tests pass.
If some tests do not pass, make sure that these tests are written to test this function.
If not, then, perhaps, you've broke something horribly.

Before comitting changes you must run tests with `make test`, and all of the tests must pass without errors.
Consider checking test coverage with `make luacov` and rendering it with your preferred reporter.
Makefile also has `luacov-console` target, which can be used to see coverage of lua code directly in the terminal.

## Writing documentation
If you've added new code, make sure it is covered not only by tests but also with documentation.
This is better done by writing documentation strings directly in the code, by using docstring feature of the language.
This way this documentation can be exported to markdown later on.

Documentation files uses Markdown format, as it is widely supported and can be read without any special software.
Please make sure to follow existing style of documentation, which can be shortly describing as:

-   One sentence per line.
    This makes easier to see changes while browsing history.
-   No indentation of text after headings.
    This makes little sense with one sentence per line approach anyway.
-   No empty lines after headings.
-   Amount of empty lines in text should be:
    -   Single empty lines between paragraphs.
    -   Double empty lines before top level headings.
    -   Single empty lines before other headings.
-   Consider using spell checking.
    If you find a word not known by the dictionary, please add it to the `LocalWords` section at the bottom of the document.

## Working with Git

Check out new branch from project's main development branch.
If you've cloned this project some time ago, consider checking if your branch has all recent changes from upstream.

Each commit must have a type, which is one of `feat` (or `feature`), `fix`, followed by optional scope, and a must have description after `:` colon.
For example:

    fix(parsing): fix #42
    feat(tests): add more strict tests

-   `feat` must be used when adding new code.
-   `fix` must be used when fixing existing code.

For a more comprehensive explanation please check [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/).

When creating merge request consider squashing your commits at merge.
You may do this manually, or use Gitlab's "Squash commits" button.

<!-- LocalWords:  bugfix docstring comitting VSCode SublimeText Gitlab's LocalWords
 -->
