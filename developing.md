# Developing Setup and Notes

This IDE is an IntelliJ plugin written in Scala.

Note that this includes the Polymorphic Blocks HDL as a submodule, and its compiler is compiled into the IDE.
See [Polymorphic Blocks' developing.md](https://github.com/BerkeleyHCI/PolymorphicBlocks/blob/master/developing.md) for details on working with the core HDL and compiler independently. 


## Build and run from command line

_If you only need to build from source and do not plan on doing heavy development, you can just use command-line sbt, which can be [downloaded here](https://www.scala-sbt.org/download.html). sbt is a build tool for Scala-based projects._

1. Run `sbt` in the repository root directory, to start the sbt interactive console.
2. In the sbt interactive console, run `runIDE` to start IntelliJ with the plugin installed.
   - sbt will automatically fetch dependencies, including the base IntelliJ installation.
   - This will use a dedicated IntelliJ installation, separate from any that may already be installed on your system.
3. In this dedicated IntelliJ installation, you may disable all plugins (for efficiency) except for:
   - IDE Settings / Settings Repository
   - Other Tools / IntelliLang
   - Other Tools / Python Community Edition
   - and of course, this one
   - You may find some other plugins useful, for example Mypy, Markdown, GitHub, Terminal 


### Notes

- `runIDE` may fail to re-compile dependencies. `clean; runIDE` is safe(r).
- Per [sbt-idea-plugin](https://github.com/JetBrains/sbt-idea-plugin), `packageArtifactZip` generates a .zip file which can be installed into another IntelliJ installation.
  The file is dumped in the `target/` folder.


## IntelliJ setup

_If you plan to do significant development, setting up an IDE is highly recommended. We used IntelliJ Community Edition, which can be [downloaded here](https://www.jetbrains.com/idea/download)._

sbt-idea-plugin really doesn't seem to know what it's doing and IntelliJ doesn't seem to recognize the plugin be default.
Try this instead:

1. In IntelliJ, make sure the official (JetBrains) Scala plugin is installed
2. To import the project, open the .sbt file
    - This should properly set up the project to index the generated proto code
    - If IntelliJ gives you the option to update the version of SBT, click yes.
3. Set up a sbt Task run configuration, with the command `compile`
4. Set up another sbt Task run configuration, with the command `runIDE`, and with a dependency on the `compile` configuration
5. You may also use the sbt shell tab (next to the Terminal tab) to run these commands directly.


## Linking PyCharm sources

While the plugin downs and indexes the IntelliJ platform sources automatically, it does not do the same for PyCharm.
This is only needed if you want to reference PyCharm sources / APIs (eg, PSI structure) in a readable (with comments) instead of decompiled format.

1. `git clone https://github.com/JetBrains/intellij-community.git`
   (this may take a while)
2. In the newly cloned repo: `git checkout pycharm/202.7660.27`
   (the version should match the PyCharm plugin version, which can be found in the File > Settings > Plugins within the IDE)
    - You may need to `checkout -f` to force.
3. Select any PyCharm reference, navigate to it (Ctrl+B).
   It should give you a decompiled source listing, and an option to choose sources.
   Click that, and point it to your cloned `intellij-community\python` folder.
4. It should detect and list the sub-projects in that folder.
   Accept that.
5. The source should update to the non-decompiled version shortly.


## Resources

- [sbt-idea-plugin repo](https://github.com/JetBrains/sbt-idea-plugin): SBT build configurations
- [sbt-idea-example](https://github.com/JetBrains/sbt-idea-example): SBT IDEA plugin very basic example, and very outdated
- [intellj-sdk-code-samples](https://github.com/JetBrains/intellij-sdk-code-samples): code samples for plugins
