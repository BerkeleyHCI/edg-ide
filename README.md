## IntelliJ Configuration

sbt-idea-plugin really doesn't seem to know what it's doing and IntelliJ doesn't seem to recognize the plugin be default.
Try this instead:

1. In IntelliJ, make sure the official (JetBrains) Scala plugin is installed
1. To import the project, open the .sbt file
   - This should properly set up the project to index the generated proto code
1. Set up a sbt Task run configuration, with the command `compile`
1. Set up another sbt Task run configuration, with the command `runIDE`, and with a dependency on the `compile` configuration


## Resources

- [sbt-idea-plugin repo](https://github.com/JetBrains/sbt-idea-plugin): SBT build configurations
- [sbt-idea-example](https://github.com/JetBrains/sbt-idea-example): SBT IDEA plugin very basic example, and very outdated
- [intellj-sdk-code-samples](https://github.com/JetBrains/intellij-sdk-code-samples): code samples for plugins