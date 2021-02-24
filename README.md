## IntelliJ Configuration

sbt-idea-plugin really doesn't seem to know what it's doing and IntelliJ doesn't seem to recognize the plugin be default.
Try this instead:

1. In IntelliJ, make sure the official (JetBrains) Scala plugin is installed
1. To import the project, open the .sbt file
   - This should properly set up the project to index the generated proto code
1. Set up a sbt Task run configuration, with the command `compile`
1. Set up another sbt Task run configuration, with the command `runIDE`, and with a dependency on the `compile` configuration
1. You may also use the sbt shell tab (next to the Terminal tab) to run these commands directly. 


### Version issues

When running the IDE, you may get this bug:
```
com.intellij.diagnostic.PluginException: 
While loading class [class]:
[class] has been compiled by a more recent version of the Java Runtime (class file version 59.0), this version of the Java Runtime only recognizes class file versions up to 55.0
```

This means the project is being compiled to target a more recent Java binary format than IntelliJ is running.
To solve (probably):
1. In IntelliJ, go to menu > File > Project Structure.
1. Under Project Settings > Project, set the Project SDK to be Java 11.
1. Clean and recompile until it works. Hopefully.


## Command line SBT notes

- `runIDE` may fail to re-compile dependencies. `clean; runIDE` is safe(r).
- Per [sbt-idea-plugin](https://github.com/JetBrains/sbt-idea-plugin), `packageArtifactZip` generates a .zip file which can be installed into IntelliJ.
  The file is dumped in the `target/` folder.


## Linking PyCharm sources

While the plugin downs and indexes the IntelliJ platform sources automatically, it does not do the same for PyCharm.
This is only needed if you want to reference PyCharm sources / APIs (eg, PSI structure) in a readable (with comments) instead of decompiled format.

1. `git clone https://github.com/JetBrains/intellij-community.git`
   (this may take a while)
1. In the newly cloned repo: `git checkout pycharm/202.7660.27`
   (the version should match the `PythonCore` plugin in build.sbt)
   - You may need to `checkout -f` to force.
1. Select any PyCharm reference, navigate to it (Ctrl+B).
   It should give you a decompiled source listing, and an option to choose sources.
   Click that, and point it to your cloned `intellij-community\python` folder.
1. It should detect and list the sub-projects in that folder.
   Accept that.
1. The source should update to the non-decompiled version shortly. 
   

## Resources

- [sbt-idea-plugin repo](https://github.com/JetBrains/sbt-idea-plugin): SBT build configurations
- [sbt-idea-example](https://github.com/JetBrains/sbt-idea-example): SBT IDEA plugin very basic example, and very outdated
- [intellj-sdk-code-samples](https://github.com/JetBrains/intellij-sdk-code-samples): code samples for plugins