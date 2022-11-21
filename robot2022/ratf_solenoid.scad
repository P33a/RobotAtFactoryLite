module ratf_solenoid() {
  $fn = 32;
  sw = 4;
  sh = 16;
  disp_u = 4;
  difference() {
    union() {
      linear_extrude(height = 32, center = true, convexity = 10, twist = 0)
      polygon(points = [[0, 0], 
                        [24, 0],
                        [24, -20],
                        [24 + sw, -20],
                        [24 + sw, sw],
                        [24, sw],
                        [sw, sw],
                        [sw - disp_u, sh],
                        [-19 - disp_u, sh],
                        [-19 - disp_u, sh - sw],
                        [-disp_u, sh - sw],
                        [0, 0]
                       ]);
      
      // Switch Support
      translate([-11.5, 18.5, 13.5])
      difference() {
        cube([23, 5, 5], center = true);  
        
        hull() {
          translate([-1, 0, 0])
          rotate([0, 0, 90])
          cylinder(d = 1.6, h = 10, center = true);

          translate([-6, 0, 0])
          rotate([0, 0, 90])
          cylinder(d = 1.6, h = 10, center = true);
        }

        hull() {
          translate([9, 0, 0])
          rotate([0, 0, 90])
          cylinder(d = 1.6, h = 10, center = true);

          translate([4, 0, 0])
          rotate([0, 0, 90])
          cylinder(d = 1.6, h = 10, center = true);
        }
      }
    }    
    
    // Hinge holes
    translate([24 - 4, sw/2, 0])
    #cylinder(d = 1.8, h = 200, center = true);     

    translate([24 - 4 - 14, sw/2, 0])
    #cylinder(d = 1.8, h = 200, center = true);     
    
    // center hole
    translate([-26/2, 0, 0])
    rotate([90, 0, 0])
    cylinder(d = 4.2, h = 200, center = true); 

    // body holes
    hull() {
      translate([6, 0, 12/2])
      rotate([90, 0, 0])
      cylinder(d = 2.5, h = 200, center = true); 

      translate([16, 0, 12/2])
      rotate([90, 0, 0])
      cylinder(d = 2.5, h = 200, center = true); 
    }

    hull() {
      translate([6, 0, -12/2])
      rotate([90, 0, 0])
      cylinder(d = 2.5, h = 200, center = true); 

      translate([16, 0, -12/2])
      rotate([90, 0, 0])
      cylinder(d = 2.5, h = 200, center = true); 
    }

  }
}

rotate([180, 0, 0])
ratf_solenoid();