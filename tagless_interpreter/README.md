## sbt project compiled with Dotty

### Usage

This is a normal sbt project, you can compile code with `sbt compile` and run it
with `sbt run`, `sbt console` will start a Dotty REPL.

For more information on the sbt-dotty plugin, see the
[dotty-example-project](https://github.com/lampepfl/dotty-example-project/blob/master/README.md).

Note: The actual implementation of staging doesn't allow the 'fix' method to work since it firstly generates the code and then evaluates it which explains the StackOverflowError when trying to do some recursion.
