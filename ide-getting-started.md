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
  - For example, `self.vdd = self.Port(VoltageSink(voltage_limits=(2.3, 5.5)*Volt, current_draw=(0, 15)*uAmp))` instantiates a port of type `VoltageSink` (voltage input) with defined voltage limits and current draw ranges, and names it `vdd`.
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


class BlinkyExample(SimpleBoardTop):
  def contents(self) -> None:
    super().contents()
    # your implementation here

    
if __name__ == "__main__":
  compile_board_inplace(BlinkyExample)
```

- `from edg import *` brings in the base classes for circuit construction, like `SimpleBoardTop` and the library parts we'll use in the rest of this tutorial.
- `class BlinkyExample` contains the (top-level) circuit you're going to build, and it extends the top-level board type `SimpleBoardTop`.
  It's empty for now, but we'll fill it in the next section.
- The stuff in `if __name__ == "__main__":` allows the design to compile (and generate netlists) by running the file.

Your IDE should look something like this (minus the red annotation text):

![Annotated IDE screen](docs/ide_start_annotated.png)

The major components are:
- **Block Diagram Visualization**: the compiled design is visualized as a block diagram here.
  - **Tree View**: shows the tree view block hierarchy of the compiled design.
- **Block Depth Selection**: selects the maximum depth of blocks shown in the Block Diagram Visualization. 1 means show only one level, 2 means show the top-level blocks and one level of internal blocks, and so on.  
- **Update Button**: click to re-compile and update the visualization. Also available through hotkey Ctrl + Alt + R.
- **Library Browser**: shows all the library blocks, ports, and links. The text box at the top allows filtering by keyword.
  - **Library Preview**: shows more information on the selected library block, including a docstring (if available)parameters, and block diagram preview.
  
We'll go over each in more detail as they are used.

### Hello, world
Let's start by compiling this (empty) design.
Select anywhere in the BlinkyExample class, then click on main menu > Tools > Set Visualizer Design.
Then, click on main menu > Tools > Refresh File (or Ctrl + Alt + R, or the update button on the Block Visualizer window) to compile.

As you might expect, you should get an empty design.

### Creating the microcontroller and LED
_In this section, you'll add and connect the microcontroller and LED blocks through GUI operations._

In the Library Browser search filter textbox, type in `microcontroller` to pre-filter the blocks.

![Libraries filtered by microcontroller](docs/ide_library_microcontroller.png)

The icons have these meanings:
- ![Folder](docs/intellij_icons/AllIcons.Nodes.Folder.svg) (category): this "block" is actually a category organizer and should not be instantiated.
- ![Abstract Type](docs/intellij_icons/AllIcons.Hierarchy.Subtypes.dark.svg) (abstract type): this block is an abstract type.
  In most cases, it can be instantiated, but will need a choice of refinement ("concrete" subtype) before a design is complete.
  This is typically used for "jellybean" parts, to indicate that many parts can be used in place, depending on design requirements - such as surface-mount vs. through-hole.
- ![Footprint](docs/intellij_icons/PlatformDebuggerImplIcons.MemoryView.Active.dark.svg) (footprint): this block is a PCB footprint directly (as opposed to, for example, an application circuit which contains PCB footprints indirectly).
  In general, footprints are low-level constructs are should not be used where a non-footprint application circuit is also defined.
  But, in some cases, as with the Nucleo_F303k8, both are one and the same.

In the code editor, click at the end of `super().contents()` to set the caret position to insert code for a new block.
Then, double-click the Lpc1549_48 block in the library panel, give it a name (let's say, `mcu`), hit enter, and the relevant line of code should pop up.
The block should also appear on the visualization:

![New block preview](docs/ide_visualizer_mcu_preview.png)

and the block instantiation line should appear in the code editor:

```python
self.mcu = self.Block(Lpc1549_48())
```

The hatched fill generally indicates that the block may be out-of-sync with the code until the next re-compile.
In this case, the Lpc1549_48 block is only a preview, while the enclosing BlinkyExample has been modified.

The red boxes indicate a missing required connection, in this example including the power and ground pair, and the SWD programming line.

Then, repeat the above with an IndicatorLed block, and name it `led`.

### Connecting the microcontroller and LED

To start a connection operation on a port, double-click the port.
For example, to connect the LED to the microcontroller, double click on `mcu`'s `digital[0]` port:

![Connect view](docs/ide_visualizer_connect_mcu.png)

Then, select the ports to connect by clicking on them (in this case, `led`'s `signal` port), and double-click anywhere to insert the connect code.
You can optionally give the new net a name, or leave it blank.
The new connection should show up on the visualizer:

![Connected blocks](docs/ide_visualizer_connected.png)

and the connect line should appear in the code editor:

```python
self.connect(self.mcu.digital[0], self.led.signal)
```

> Note that the connect statement is ordered with the starting item first, then the rest in order.
> Stylistically, we prefer sources before sinks, defined loosely (including dataflow or power flow notations).
> This convention is also used by the block diagram visualizer to define an order where it isn't apparent from the port types, for example if two bidirectional digital ports are connected together, as opposed to a digital source to a digital sink connection.

> Note that `mcu.digital` is an array-like port, and additional ports will appear as existing ones are connected.

> Explicit pin assignments are supported, but `SimpleBoardTop` forces auto-assignment for simplicity in this tutorial.

Repeat the same for the `gnd` port of both blocks.

If you recompile now, the hatched fill should go away, but you'll get a bunch of errors.
These mostly stem from the missing power source, which are indicated on the block visualizer with the red ports.

### Adding power and programming
_In this section, you'll add and connect a power source and programming connector to fix errors._

Repeat the add block flow with the `Pj_102a` barrel jack connector (you can search for `BarrelJack`), and the  `SwdCortexTargetHeader` (you can search for `swd` to find all the SWD connectors).

Note that you'll have to give the barrel jack a voltage specification.
Parameterization can only be done in the code editor.
We're going to arbitrarily pick 5 volts with a ±10% tolerance, so change the line of code to

```python
self.jack = self.Block(Pj_102a(voltage_out=5*Volt(tol=0.10)))
```

Repeat the connection flow with the `swd` and `pwr` lines (making sure to power both the microcontroller and SWD header).

To add additional ports to an existing connection (such as `gnd`), you must start the connect operation with a port in the existing connection.
In this case, if your caret is also over the prior connect statement, the new port is inserted to the prior connect.
Otherwise, a new connect statement is inserted.

Re-compile, and you should get a lot less errors now.

### Navigation and inspection
However, overvoltage errors remain in links in the microcontroller block.
You can navigate into the microcontroller block by double-clicking it.
Inside, there's a bunch of decoupling capacitors and resistors, as well as the chip block `Lpc1549_48_Device` itself.
If you mouse over the `vdd` pin connection, you should see the problem:

![Connected blocks](docs/ide_visualizer_overvolt.png)

The microcontroller is seeing a 5.0v ±10% voltage on a 3.3v device (technically 2.4-3.6v, rendered as 3.0v ±20%).

You can double-click on the root block to navigate out of the microcontroller block.

### Adding and refining a power converter
_In this section, you'll insert an abstract power converter to fix the prior error, and refine it with a specific part number._

Repeat the add block flow with the abstract `BuckConverter` block.
As this requires an output_voltage parameter, you'll again need to write this in code.
Let's ask for a 3.3v converter with a ±5% tolerance:
```python
self.buck = self.Block(BuckConverter(output_voltage=3.3*Volt(tol=0.05)))
```

To insert the converter between the barrel jack and low-voltage components, we'll need to disconnect the barrel jack.
Modifications like this need to be done in code, but you can right-click the port and navigate to connect statements it's involved in, then delete the port from the connection.
Then, refresh the visualization by recompiling, and you can hook up the buck converter via the GUI.

If you recompile, you'll still have a few errors.
The buck converter is still an abstract type (it has no implementation), so we must give it one.

Select the buck converter in the block diagram visualization.
Then, search for `Buck` in the library browser, and pick the `Tps561201` under the BuckConverter.
Right click on it, and select Refine Instance Selection to set a refinement for this particular block instance.
The associated refinement lines should appear in the top-level design in the code editor:

```python
def refinements(self) -> Refinements:
  return super().refinements() + Refinements(
    instance_refinements=[
      (['buck'], Tps561201),
    ]
  )
```

Recompile, and there should be no more errors.

> Abstract types are useful primarily in libraries to preserve alternatives, the refinement of which is left to the top-level designer.
> For example, SimpleBoardTop defines a default set of refinements for 0603 surface-mount components, but because libraries are written with (for example) abstract Resistor classes, you can select a resistor and override it with a through-hole part, such as `AxialResistor`.

### Advanced: arraying LEDs
_In this section, you'll modify the GUI-inserted code to programmatically create an array of LEDs._

> In general, code offers you a lot more flexibility and power than can be achieved through the GUI.
> For example, the buck converter automatically sizes the internal components (inductors, capacitors) based on standard design equations - you can even check out the buck converter code to see what's going on under the hood.
> This functionality is probably more useful in those library components, but can also come in handy in some cases in the top-level design. 

Since the circuit is "constructed" by executing Python code, we can actually write arbitrary Python to generate hardware.
Take your LED instantiation and connection code, and move the lines together:

```python
self.led = self.Block(IndicatorLed())
self.connect(self.mcu.digital[0], self.led.signal)
self.connect(self.mcu.gnd, self.led.gnd)
```

Declare `led` to be an array by adding:

```python
self.led = ElementDict()
```

and refactor the actual LED instantiation and connection by wrapping everything in a `for`-loop:

```python
self.led = ElementDict()
for i in range(4):
  self.led[i] = self.Block(IndicatorLed())
  self.connect(self.mcu.digital[i], self.led[i].signal)
  self.connect(self.mcu.gnd, self.led[i].gnd)
```

> As a general note, note that HDLs have different non-functional degrees of freedom compared to schematic.
> In HDL, the style (including ordering) of hardware construction statements is somewhat like the placement and arrangement of symbols and nets in a graphical schematic.
> While the block diagram visualization is automatically generated, the style of the code can impact readability and maintainability.
> 
> While GUI operations are useful in writing lines of code, it is up to you to determine where they should be placed to keep the HDL readable.
> Feel free to refactor or clean up the code if you'd like.


## Syntactic sugar
_In this section, we clean up the prior example by consolidating some repetitive connections through implicit scopes._

> Syntactic sugar refers to syntax within programming languages that makes things more usable.

> The IDE does not provide any special support or understanding for these operations, but will render the final outcome.

Because some connections (like power and ground) are very common, the HDL provides the idea of an implicit connection scope to automatically make them when a block is instantiated.
In our example, if we wanted to create a scope with an implicit power connection from the buck converter output, and an implicit ground connection from the barrel jack input, we can write:

```python
with self.implicit_connect(
    ImplicitConnect(self.buck.pwr_out, [Power]),
    ImplicitConnect(self.jack.gnd, [Common]),
) as imp:
  ...
```

If we move the microcontroller, SWD, and LED instantiations inside this scope, we no longer need to have connect statements for their power and ground ports.
_Note the use of `imp.Block(...)` instead of `self.Block(...)`!_

```python
  self.mcu = imp.Block(Lpc1549_48())
  self.swd = imp.Block(SwdCortexTargetHeader())
  
  self.led = imp.Block(IndicatorLed())
```

Note that we still have to make the connections for the SWD interface and the LED signal.
Those do not need to be placed in the implicit scope, but may be for stylistic purposes. 

There also exists a chain connect that allows a block instantiation and connection on one line, but as this tutorial focuses on the IDE, we'll skip that.
If you're interested, the HDL getting started doc has [a section on chain connects](PolymorphicBlocks/getting-started.md#chain-connects).


## Advanced tutorial: making parts
_In this section, we build and add a digital magnetic field sensor ([LF21215TMR](https://www.littelfuse.com/~/media/electronics/datasheets/magnetic_sensors_and_reed_switches/littelfuse_tmr_switch_lf21215tmr_datasheet.pdf.pdf)) to our design._
_We do this in two stages, first defining a FootprintBlock for the chip itself, then building the wrapper application circuit around it._

### Creating a part
We start off by defining an empty block.

```python
class Lf21215tmr_Device(FootprintBlock):
  def __init__(self) -> None:
    super().__init__()
    # block boundary (ports, parameters) definition here 

  def contents(self) -> None:
    super().contents()
    # block implementation (subblocks, internal connections, footprint) here
```

> While `Block`s are arbitrary hierarchy blocks that only have ports, inner blocks, and connections, `FootprintBlock` also allows up to one PCB footprint, and a mapping from the block ports to footprint pins.
> You can loosely think of `FootprintBlock` as analogous to a schematic symbol, while `Block` is closer to a hierarchy sheet.

> `__init__` is meant to define the interface of a block (all Ports and Parameters), while `contents` is meant to define the contents of a block (largely Blocks, connections, and constraints).
> This split is not enforced (and there are cases where it is desirable to mix them into just `__init__`), but the main benefits of this are performance (avoid elaborating the full design tree unnecessarily) and separation for readability.



### Creating the application circuit




