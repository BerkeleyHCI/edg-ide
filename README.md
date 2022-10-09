# edg-ide

An IDE for the [Polymorphic Blocks board-level HDL](https://github.com/BerkeleyHCI/PolymorphicBlocks) that adds a schematic-like graphical interface (with block diagram visualization) side-by-side with the text editor.
Schematic-like actions on the graphical interface (like insert-block or connect-ports) generate into lines of code on the HDL.
Direct textual edits to the HDL can also be made at any time, and the block diagram can be updated through a fast incremental recompilation.

![IDE Screenshot](PolymorphicBlocks/docs/ide/overview.png)

Once you have [the IDE set up](#setup), follow [the getting started tutorial in PolymorphicBlocks](PolymorphicBlocks/getting-started.md), which introduces the HDL and describes graphical actions.

For a slightly deeper technical overview, check out our [UIST'21 paper and recorded talks](https://doi.org/10.1145/3472749.3474804).

**This is alpha software, and is a continuing work-in-progress. It is definitely rough around the edges. But feel free to report bugs and suggestions on as issues on this repository!** 


## Setup

**TODO**: we aren't yet at the point of releasing pre-built files, so this must be compiled from source for now.
Follow the [command-line build instructions](developing.md#build-and-run-from-command-line) (tl;dr: install [sbt](https://www.scala-sbt.org/download.html), then run `sbt runIDE` in the repository root directory).

**This project uses submodules**, and may depend on a synchronized version of the `PolymorphicBlocks` submodule.
If you're getting compile errors, try updating submodules with `git submodule update`. 


## Developing

See [developing.md](developing.md) for getting set up to build and run the IDE from this repository.
