(module R_1210_3225Metric (layer F.Cu) (tedit 5F68FEEE)
        (descr "Resistor SMD 1210 (3225 Metric), square (rectangular) end terminal, IPC_7351 nominal, (Body size source: IPC-SM-782 page 72, https://www.pcb-3d.com/wordpress/wp-content/uploads/ipc-sm-782a_amendment_1_and_2.pdf), generated with kicad-footprint-generator")
        (tags resistor)
        (attr smd)
        (fp_text reference REF** (at 0 -2.28) (layer F.SilkS)
        (effects (font (size 1 1) (thickness 0.15)))
        )
        (fp_text value R_1210_3225Metric (at 0 2.28) (layer F.Fab)
        (effects (font (size 1 1) (thickness 0.15)))
        )
        (fp_line (start -1.6 1.245) (end -1.6 -1.245) (layer F.Fab) (width 0.1))
        (fp_line (start -1.6 -1.245) (end 1.6 -1.245) (layer F.Fab) (width 0.1))
        (fp_line (start 1.6 -1.245) (end 1.6 1.245) (layer F.Fab) (width 0.1))
        (fp_line (start 1.6 1.245) (end -1.6 1.245) (layer F.Fab) (width 0.1))
        (fp_line (start -0.723737 -1.355) (end 0.723737 -1.355) (layer F.SilkS) (width 0.12))
        (fp_line (start -0.723737 1.355) (end 0.723737 1.355) (layer F.SilkS) (width 0.12))
        (fp_line (start -2.28 1.58) (end -2.28 -1.58) (layer F.CrtYd) (width 0.05))
        (fp_line (start -2.28 -1.58) (end 2.28 -1.58) (layer F.CrtYd) (width 0.05))
        (fp_line (start 2.28 -1.58) (end 2.28 1.58) (layer F.CrtYd) (width 0.05))
        (fp_line (start 2.28 1.58) (end -2.28 1.58) (layer F.CrtYd) (width 0.05))
        (pad 1 smd roundrect (at -1.4625 0) (size 1.125 2.65) (layers F.Cu F.Mask F.Paste) (roundrect_rratio 0.222222))
        (pad 2 smd roundrect (at 1.4625 0) (size 1.125 2.65) (layers F.Cu F.Mask F.Paste) (roundrect_rratio 0.222222))
        (fp_text user %R (at 0 0) (layer F.Fab)
        (effects (font (size 0.8 0.8) (thickness 0.12)))
        )
        (model ${KISYS3DMOD}/Resistor_SMD.3dshapes/R_1210_3225Metric.wrl
        (at (xyz 0 0 0))
        (scale (xyz 1 1 1))
        (rotate (xyz 0 0 0))
        )
        )