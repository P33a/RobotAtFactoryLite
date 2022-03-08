module LR_bot_slider() {
  $fn = 32;
  sw = 4;
  sh = 44.5;
  difference() {
    linear_extrude(height = 26, center = true, convexity = 10, twist = 0)
    polygon(points = [[0, 0], 
                      [16, 0],
                      [16, sw],
                      [12, sw],
                      [12, sw + 4],
                      [sw, sw + 4],
                      [sw, sh],
                      [-26, sh],
                      [-26, sh - sw],
                      [0, sh - sw],
                      [0, 0]
                     ]);
    
    // center hole
    translate([-26/2, 0, 0])
    rotate([90, 0, 0])
    cylinder(d = 2.8, h = 200, center = true); 

    // body holes
    translate([8, 0, 15/2])
    rotate([90, 0, 0])
    cylinder(d = 2, h = 200, center = true); 

    translate([8, 0, -15/2])
    rotate([90, 0, 0])
    cylinder(d = 2, h = 200, center = true); 


  }
}

LR_bot_slider();