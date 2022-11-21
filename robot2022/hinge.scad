module front_guide() {
  $fn = 32;
  eps = 0.2;
  wall = 1;
  di = 2 + 2 + 2;
  difference() {
    hull() {
      translate([di, 0, 0])
      cylinder(d = 6, h = 3, center = true);

      translate([-di, 0, ])
      cylinder(d = 6, h = 3, center = true);
    }
    
    translate([di, 0, 0])
    cylinder(d = 2.4, h = 20, center = true);

    translate([-di, 0, ])
    cylinder(d = 2.4, h = 20, center = true);
  }
}

disp = 8;
front_guide();

translate([0, disp, 0])
front_guide();

translate([0, 2*disp, 0])
front_guide();

translate([0, 3*disp, 0])
front_guide();