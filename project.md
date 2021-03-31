# Project Specification

## System Description

## Current Sensor: ZXCT1110 

Relevant parts of the [datasheet](https://www.diodes.com/assets/Datasheets/ZXCT1107_10.pdf) include:
- Page 1: pinmap
- Page 3: recommended operating conditions
- Page 10: application circuit
- Page 11: error calculation equation, the percentage error is ±(0.082/Vsense + 1.8)%, with Vsense given in volts.


When modeling the ZXCT1110, this information may be helpful:
- **AnalogSink** port: represents a pin that functions as an analog input.
  - `voltage_limits` defines the recommended voltage limits. If left unspecified, it assumes no voltage limits.
  - `current_draw` defines the expected runtime current draw. If left unspecified, it defaults to zero.
  - `impedance` defines the impedance at the pin. If left unspecified, it defaults to infinite impedance. **Because this is not specified in the datasheet, assume infinite impedance**.
- **CurrentSenseResistor** block: a resistor configured to sense current, with four ports:
  - `pwr_in`, `pwr_out`: power input and power output ports, modeled as **VoltageSink** and **VoltageSource**, respectively. 
  - `sense_in`, `sense_out`: sense input and sense output ports, both modeled as **AnalogSource**. Physically shares pins with `pwr_in` and `pwr_out`, respectively 
- You can define a parameter using `self.[name] = self.Parameter([parameter tyoe])`.
  - Values for `[parameter type]` include `FloatExpr()` (a real-valued number) and `RangeExpr()` (a real-valued interval type)
- `__init__` can also take arguments, but they must be one of the parameter types above, and must have a default (which can be an empty value like `RangeExpr()`).
  - If `__init__` takes arguments, it must be annotated with `@init_in_parent`, so parameters specified in the containing block are also set there.
