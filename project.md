# Project Specification

In this project, you'll build a USB-powered ambient light sensor.
Your system should have:
- A USB device-side port, which will be used to power the board and provide communications.
- Some kind of user readout, either a display or a couple of LEDs.
- The BH1620FVC ambient light sensor (described in the next section).
- You may need additional parts to interface the above.
  All parts you need should be in the library, aside from the BH1620FVC.

## Light Sensor: BH1620FVC

[BH1620FVC](http://rohmfs.rohm.com/en/products/databook/datasheet/ic/sensor/light/bh1620fvc-e.pdf) is an analog light sensor with a current output.
However, to generate a voltage output (to interface with a microcontroller ADC), we will use a load resistor as in the datasheet's Block Diagram / application circuit (page 4).
Other relevant pages are:
- Page 1: recommended Operating Conditions.
- Page 2: Electrical Characteristics, including current draw and digital logic thresholds.
- Page 5: load resistor setting, Vout = 0.0057e-4 * Ev * R1, where Ev is the illuminance in lux and Vout is in volts.
- Page 6: pin table (with numbers referring to the diagrams on page 7\).

You should model the digital ports on the chip as **DigitalSink**s, but for simplicity in the application block, you can hard-connect pull-up resistors to Vcc and assume it will run in low-gain mode (range of ~100,000 lux).

Your application block should take as input parameters the desired illuminance range (or just the maximum) and the desired output voltage at the maximum illuminance, and compute the resistor internally.
Your computations (and parameter definition) should produce a range for the resistor resistance, to allow for part tolerance.

The device appears compatible with the SOT-553 footprint (with pins numbered identically), while the WSON-6 1.5mm x 1.5mm (0.5mm pitch) footprint is too small.

As you implement the chip, this information may be helpful:
- A port "model" can be declared separately from where it is "instantiated" so it can be re-used.
  For example, one way to define two DigitalSink ports of the same type is:
  ```python
  dig_model = DigitalSink(...)
  self.dig1 = self.Port(dig_model)
  self.dig2 = self.Port(dig_model)
  ```
- **AnalogSink** port: represents a pin that functions as an analog input.
  - `voltage_limits` defines the recommended voltage limits. If left unspecified, it assumes no voltage limits.
  - `current_draw` defines the expected runtime current draw. If left unspecified, it defaults to zero.
  - `impedance` defines the impedance at the pin. If left unspecified, it defaults to infinite impedance.
    Assume the current source has zero impedance (so impedance should only be determined by the resistor).
- You can define a parameter using `self.[name] = self.Parameter([parameter tyoe])`.
  - Values for `[parameter type]` include `FloatExpr()` (a real-valued number) and `RangeExpr()` (a real-valued interval type)
- `__init__` can also take arguments, but they must be one of the parameter types above, and must have a default (which can be an empty value like `RangeExpr()`).
  - If `__init__` takes arguments, it must be annotated with `@init_in_parent`, so parameters specified in the containing block are also set there.
  