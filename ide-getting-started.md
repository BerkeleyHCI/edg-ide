# IDE Getting Started

## Core Concepts
The core abstraction is the hierarchical block diagram, which we will explain using an example design of a microcontroller driving an LED.

In conventional schematic tools, such a design could be a flat schematic, consisting of the microcontroller module, LED, and resistor:
![Blinky Hierarchy Block Diagram](PolymorphicBlocks/docs/blinky_model_flat.png)

Many modern tools have the concept of hierarchy blocks, where a block could be a subcircuit:
![Blinky Hierarchy Block Diagram](PolymorphicBlocks/docs/blinky_model_hierarchy1.png)
In this example, the LED-resistor subcircuit is contained within a block, which can be manipulated as a unit, and exposes ports (circles on the diagram) while encapsulating internal pins.
(note: in tools with this feature, the subcircuit is usually presented in its own sheet, instead of having its contents displayed in the block)

Generalizing this model, components are blocks too, and component pins are also block ports:
![Blinky Hierarchy Block Diagram](PolymorphicBlocks/docs/blinky_model_hierarchy2.png)

The main concepts our model extends on top of the simple hierarchy blocks above are **parameters**, **links**, and **generators**.

**Parameters** are variables that can be attached to blocks and ports.
For example, a digital IO, like `digital[0]` in the example above, would have parameters like input voltage tolerance, output voltage range, and logic thresholds.
This allows for a more powerful design correctness check (think ERC++), and provides a foundation for generators.

**Links** are connections between ports, which defines how parameters propagate between those ports and any constraints on them.
Continuing the digital IO example, the link would check the output thresholds against the input thresholds, and provide the worst-case voltage levels given all connected drivers.
These could be viewed as a block-like object (diamonds on the diagram) instead of direct wire connections:
![Blinky Hierarchy Block Diagram](docs/blinky_model_full.png)

Finally, **generators** allow a block's internal contents to be automatically and dynamically constructed, possibly based on parameters on it and its ports.
For example, the `IndicatorLed` block might automatically size the resistor based on the input voltage on the `sig` pin, or a DC-DC converter block might automatically size inductors and capacitors based on the expected output voltage and current.

### Hardware Description Language (HDL)
To support user-defined computation of parameters and generator blocks, the design system is implemented as a _hardware description language_ (HDL).
That is, blocks are "placed" or instantiated, and their ports are connected, through lines in code instead of GUI actions in a graphical schematic.

There are a couple of basic operations, which you'll get to try in the tutorial:
- **Block Instantiation**: creates a sub-block in the current block
  - For example, `self.led = self.Block(IndicatorLed())` instantiates a `IndicatorLed` block and names it `led` in the current block
- **Port Instantiation**: creates an exterior port in the current block, used for building library blocks.
  - For example, `self.vdd = self.Port(ElectricalSink(voltage_limits=(2.3, 5.5)*Volt, current_draw=(0, 15)*uAmp))` instantiates a port of type `ElectricalSink` (voltage input) with defined voltage limits and current draw ranges, and names it `vdd`.
- **Connect**: connects two ports
  - For example, `self.connect(self.mcu.digital[0], self.led.signal)` connects a digital IO port on `mcu` to the signal port of `led`

### Graphical Editor and Integrated Development Environment (IDE)
While an HDL is needed to support parameter computation and programmatic construction, some operations (like building a top-level design with an LED connected to a microcontroller) may not require the full power provided by an HDL and may be more intuitive or familiar within a graphical environment.
However, because this design makes use of generator blocks (the LED), and because blocks may also take parameters (such as the target output voltage of a DC-DC converter), we have kept the HDL as the primary design input. 

To help with these more basic operations and to support those more familiar with a graphical schematic capture flow, we provide is an IDE to help bridge the graphical schematic-like and HDL code representations. Specifically, it:
- provides a schematic-like visualization of the design
- allows inspection of solved / computed parameters in the design
- generates and inserts HDL code from schematic editor-like actions

In the rest of this tutorial, we'll cover IDE operations with an example project, then drop into HDL to build a custom part.


## IDE basics tutorial: Blinky
_In this example, we will create a circuit consisting of a LED and switch connected to a microcontroller._

Start by opening `blinky_skeleton.py`, which is pre-populated with this skeleton code:
```python
from edg import *


class BlinkyExample(Block):
  def contents(self) -> None:
    super().contents()
    # your implementation here
```

- `from edg import *` brings in the base classes for circuit construction, like `Block` and the library parts we'll use in the rest of this tutorial.
- `class BlinkyExample` contains the (top-level) circuit you're going to build, and it extends the hierarchical block base class `Block`.
  It's empty for now, but we'll fill it in the next section.

### Hello, world
Let's start by compiling this design.


### Creating the microcontroller and LED


### Adding power


### Adding and refining a power converter


### Kitchen sink: SPI LCD


### Advanced: arraying LEDs


## Advanced tutorial: making parts
_In this section, we build and add a digital magnetic field sensor (LF21215TMR) to our design._


## Sidenote: syntactic sugar


