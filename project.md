# Project Specification

In this project, you'll build a USB-powered ambient light sensor.
Your system should have:
- A USB device-side port, which will be used to power the board and provide communications.
- Some kind of user readout, such as a display or a couple of LEDs.
- The BH1620FVC ambient light sensor (described in the next section), which you'll need to write the part definition for.
- You may need additional parts to interface the above.
  All parts you need should be in the library, aside from the BH1620FVC.
  
Consider building the rest of the circuit first, and modeling and inserting the light sensor last.

## Light Sensor: BH1620FVC

[BH1620FVC](http://rohmfs.rohm.com/en/products/databook/datasheet/ic/sensor/light/bh1620fvc-e.pdf) is an analog light sensor with a current output.
However, to generate a voltage output (to interface with a microcontroller ADC), we will use a load resistor as in the datasheet's Block Diagram / application circuit (page 4).
Other relevant pages are:
- Page 1: Recommended Operating Conditions.
- Page 2: Electrical Characteristics, including current draw and digital logic thresholds.
  - For simplicity, use the more restrictive 2.0V high threshold instead of modeling the 2.4-3.6v and 3.6-5.5v cases.
- Page 5: load resistor setting, Vout = 0.0057e-6 * Ev * R1, where Ev is the illuminance in lux and Vout is in volts. The load resistance should also be within the range of 1kΩ-1MΩ.
- Page 6: pin table (with numbers referring to the diagrams on page 7).

The device appears compatible with the SOT-553 footprint (with pins numbered identically), while the WSON-6 1.5mm x 1.5mm (0.5mm pitch) footprint is too small.

Your application circuit should be parameterized in terms of the input illuminance and output voltage, and calculate the resistance internally.

You should model the digital ports on the chip as **DigitalSink**s, but for simplicity in the application block, you can hard-connect pull-up resistors to Vcc and assume it will run in low-gain mode (range of ~100,000 lux).

The overall output impedance of the application circuit will just be the impedance of the resistor (the current source is approximately zero impedance).
Because the electronics model works strictly in terms of analog sources and sinks, the recommendation is to set the output impedance of the chip's pin itself to the resistor impedance, then tack on a resistor with an AnalogSink with infinite input impedance.
See the point on Resistor and Passive ports below.

As you implement the chip, this information may be helpful:
- A port "model" can be declared separately from where it is "instantiated" so it can be re-used.
  For example, one way to define two DigitalSink ports of the same type is:
  ```python
  dig_model = DigitalSink.from_supply(...)
  self.dig1 = self.Port(dig_model)
  self.dig2 = self.Port(dig_model)
  ```
- **AnalogSource** port: represents a pin that functions as an analog input.
  - `voltage_out` defines the output voltage range.
  - `current_limits` defines the maximum current into the pin.
  - `impedance` defines the source impedance at the pin.
- `__init__` can also take arguments, but they must be one of the parameter types above, and must have a default (which can be an empty value like `RangeExpr()`).
  - If `__init__` takes arguments, it must be annotated with `@init_in_parent`, so parameters specified in the containing block are also set there.
  - For example, if you were passing a real, interval-valed `impedance` to `__init__`, the definition would look like:
    ```python
    @init_in_parent
    def __init__(self, impedance: RangeLike = RangeExpr()) -> None:
      ...
    ```
    `impedance` can them be used in the body like any other parameter.
    Yes, this syntax is ugly and non-intuitive. We're working on it.
- There isn't a current-to-voltage load resistor class, but you can instantiate a **Resistor**, which has an `a` and `b` Passive port.
  Passive ports can be "cast" to another type, such as by using `.as_analog_sink()` (which optionally takes parameters, otherwise presents an "ideal" analog sink port with infinite impedance).
- You can (but may not need to) define a parameter using `self.[name] = self.Parameter([parameter tyoe])`.
  - Values for `[parameter type]` include `FloatExpr()` (a real-valued number) and `RangeExpr()` (a real-valued interval type)
  