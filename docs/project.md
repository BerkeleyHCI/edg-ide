# Project Specification

In this project, you'll build a USB-powered ambient light sensor.
Your system should have:
- A USB device-side port, which will be used to power the board and provide communications.
  - You may want to add an ESD diode to protect the USB data lines.
- Some kind of user readout, such as some kind of display.
- The BH1620FVC ambient light sensor (described in the next section), which you'll need to write the part definition for.
- You may need additional parts to interface the above.
  All parts you need should be in the library, aside from the BH1620FVC.
  
Consider building the rest of the circuit first, and modeling and inserting the light sensor last.

You should create a new top-level block (subclass of `SimpleBoardTop`) for this project.
To set it as the visualizer's working design, click anywhere in the new class and, then go to main menu > Tools > Set Visualizer Design.

Feel free to use the tutorial design as a reference, but also feel free to try other parts in the library, like other power topologies or microcontrollers.

A [language reference section](#hdl-reference) is also at the bottom of this file. 


## Light Sensor: BH1620FVC

[BH1620FVC](https://rohmfs.rohm.com/en/products/databook/datasheet/ic/sensor/light/bh1620fvc-e.pdf) is an analog light sensor with a current output.
However, to generate a voltage output (to interface with a microcontroller ADC), we will use a load resistor as in the datasheet's Block Diagram / application circuit (page 4).
Other relevant pages are:
- Page 1: Recommended Operating Conditions.
- Page 2: Electrical Characteristics, including current draw and digital logic thresholds.
  - For simplicity, **use the more restrictive 2.0V input high threshold** instead of modeling the 2.4-3.6v and 3.6-5.5v cases.
- Page 4: application circuit, for this exercise **ignore the output capacitor**.
- Page 5: load resistor setting, **Vout = 0.0057e-6 * Ev * R1**, where Ev is the illuminance in lux and Vout is in volts. The load resistance should also be within the range of 1kΩ-1MΩ.
- Page 6: pin table (with numbers referring to the diagrams on page 7).

The device appears **compatible with the SOT-553 footprint (with pins numbered identically)**, while the WSON-6 1.5mm x 1.5mm (0.5mm pitch) footprint is too small.

Your application circuit should be **parameterized in terms of the input illuminance and output voltage, and calculate the resistance internally**.

You should model the digital ports on the chip as **DigitalSink**s, but for simplicity in the application block, you can hard-connect pull-up resistors to Vcc and assume it will run in low-gain mode (range of ~100,000 lux).

Although AnalogSource ports are modeled with impedances, and the impedance of this overall circuit should be well-defined (current source is zero impedance, so just the impedance of the load resistor), we'll leave it blank here for simplicity.
Unspecified impedances default to zero on the source side and infinity on the sink side.

As you implement the chip, this information may be helpful:
- A port "model" can be declared separately from where it is "instantiated" so it can be re-used.
  For example, one way to define two DigitalSink ports of the same type is:
  ```python
  dig_model = DigitalSink.from_supply(...)
  self.dig1 = self.Port(dig_model)
  self.dig2 = self.Port(dig_model)
  ```
- **AnalogSource** port: represents a pin that functions as an analog output.
  - `voltage_out` defines the output voltage range.
  - `current_limits` defines the maximum current out of the pin.
  - `impedance` defines the source impedance at the pin.
- `__init__` can also take arguments, but they must be one of the parameter types above, and must have a default (which can be an empty value like `RangeExpr()`).
  - If `__init__` takes arguments, it must be annotated with `@init_in_parent`, so parameters specified in the containing block are also set there.
  - The suggested constructor for the light sensor application circuit is:
    ```python
    class Bh1620fvc(Block):
      @init_in_parent
      def __init__(self, max_illuminance: FloatLike = FloatExpr(),
                   target_voltage: RangeLike = RangeExpr()) -> None:
        super().__init__()
    ```
    where it's parameterized by the maximum illuminance as a numeric value and the target voltage at that maximum illuminance as a range (to tolerance resistors).
    Yes, this syntax is magical, ugly, and non-intuitive. We're working on it.
  - You shouldn't need parameters to the constructor for the `FootprintBlock`, if you leave the AnalogSource default (zero impedance).
- There isn't a current-to-voltage load resistor class, but you can instantiate a **Resistor**, which has an `a` and `b` Passive port.
  Passive ports can be "cast" to another type, such as by using `.as_analog_sink()` (which optionally takes parameters, otherwise presents an "ideal" analog sink port with infinite impedance) or `.as_ground()`.

## HDL Reference

These can be used in a `Block`:
- **Block Instantiation**: creates a sub-block in the current block
  ```python
  self.led = self.Block(IndicatorLed())
  ```
- **Port Instantiation**: creates an exterior port in the current block, used for building library blocks.
  ```python
  self.vdd = self.Port(VoltageSink(voltage_limits=(2.3, 5.5)*Volt, current_draw=(0, 15)*uAmp))
  ```
- **Connect**: connects two (or more) ports.
  ```python
  self.connect(self.mcu.digital[0], self.led.signal)
  ```
- **Implicit Connect**: defines an implicit scope
  ```python
  with self.implicit_connect(
          ImplicitConnect(self.buck.pwr_out, [Power]),
          ImplicitConnect(self.jack.gnd, [Common]),
  ) as imp:
    self.mcu = imp.Block(Lpc1549_48())
  ```
  **Note that the usual `self.Block(...)` are replaced with `imp.Block(...)` so the newly created blocks have implicit-capable ports connected!**


These can be used in a `FootprintBlock`:
- **Associate Footprint**: associates a KiCad footprint with this block
  ```python
  self.footprint(
    'U', 'Package_TO_SOT_SMD:SOT-23',
    {
      '1': self.vcc,
      '2': self.vout,
      '3': self.gnd,
    },
    mfr='Littelfuse', part='LF21215TMR',
    datasheet='https://www.littelfuse.com/~/media/electronics/datasheets/magnetic_sensors_and_reed_switches/littelfuse_tmr_switch_lf21215tmr_datasheet.pdf.pdf'
  )
  ```
