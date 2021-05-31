# Staged_regex-Haskell
A project which uses Template Haskell and its ability to generate code at compile time, to make regex matching faster.

More concretely, a DFA is constructed for a given regex and based on that DFA, a singular lambda function is generated which takes a string and returns a boolean, reflecting if the string was accepted by the DFA.
Because this function doesn't need extra function calls at runtime, it saves a significant amount of time when matching a string.

The regex you want to match has to be known at compile time, so a DFA can be constructed for that regex.

This is mainly meant as a way to show the code; it is not really suited for personal use. If you really want to use this, you can change the test1 regex in Regex.hs, and generate the code by compiling.
This project does have a couple of dependencies, but Template Haskell is the main one you'll need. The regex-tdfa, regex-pcre and Criterion dependencies are only used for testing, and you can eliminate the need for these by commenting out Main.hs completely.

Additionally, this repository will not be actively maintained, but small fixes or optimisations are a possibility.