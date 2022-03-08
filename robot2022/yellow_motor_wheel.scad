
$fa=12;
$fs=0.5;
screw_M2 = 1.7;
screw_M25 = 2.1;
screw_M3 = 2.5;

slack = 0.3;
wheel_width = 12;
wheel_diameter = 66;
rim_xr = 0.5;
rim_w = 0.5;
rim_b = 9;

hole_diameter = 0;
hole_chanfer = 60;
chanfer_depth = 8;
top_chanfer_diameter = 50;
hub_diameter = 18;
hub_height = 3;

//     hole_diameter
// <-->  top chanfer_diameter
// <---->  hole_chanfer
// <----->__________________________
//   I __/         ^                | 
//   ^             |               /___
//   |            wheel_width     |    |
//  hole_depth     |              |  __| rim_b
//                 |               \
//     ____________v________________| rim_w
// <-                          -><->
//  wheel_diameter/2              rim_xr

screw_radius = screw_M3 / 2;


module flat_shaft_axis(fsa_d = 3, flat_d = 2.5, fsa_h = 10, slack = 0.1){
  $fn = 64;
  difference() {
    cylinder(d = fsa_d + slack, h = fsa_h, center = true);  
    
    translate([flat_d, 0, 0])
    cube([fsa_d, fsa_d + 1, fsa_h + 1], center = true);

  }  
}


module yellow_mottor_axis(eps = 0.3) {
  intersection() {
    cylinder(d = 5.4 + eps, h = 100, center = true, $fn = 32);
    cube([10, 3.7 + eps, 110], center = true);
  }
}


module yellow_motor_wheel(slack = 0.4){
  $fn = 64;
  difference() {
    union() {
      difference() {
        rotate_extrude(convexity = 10, $fn = 128)
        polygon( points=[[0, 0], 
                         [wheel_diameter/2 + rim_xr, 0], 
                         [wheel_diameter/2 + rim_xr, rim_w], 
                         [wheel_diameter/2, wheel_width/2 - rim_b/2], 
                         [wheel_diameter/2, wheel_width/2 + rim_b/2], 
                         [wheel_diameter/2 + rim_xr, wheel_width - rim_w], 
                         [wheel_diameter/2 + rim_xr, wheel_width], 
                         [hole_chanfer/2, wheel_width],
                         [top_chanfer_diameter/2, wheel_width - chanfer_depth],
                         [hub_diameter/2, wheel_width - chanfer_depth],
                         [hub_diameter/2, wheel_width + hub_height],
                         [0, wheel_width + hub_height]] );

        union(){
          translate([0, 0, wheel_width]){
            cylinder(h = 2 * (wheel_width - chanfer_depth), d = hole_diameter + slack, center = true);            
            //flat_shaft_axis(fsa_h = 2 * (wheel_width - chanfer_depth));
            yellow_mottor_axis();
            
            translate([0, 0, hub_height - 3]) 
            #rotate([0, 90, 90])
            cylinder(h = 2 * hub_diameter, d = 2.5, center = true);
          }
          // Holes
          for(a = [0 :90: 270]){
            rotate([0, 0, 45 + a])
            translate([0, wheel_diameter/3.3, -1e-3]) {
              cylinder(h = 100, d = wheel_diameter/4,  center = true);
              cylinder(h = 1, d1 = wheel_diameter/4 + 2, d2 = wheel_diameter/4,  center = false);
            }

            rotate([0, 0, a])
            translate([0, wheel_diameter/2.7, -1e-3]) {
              cylinder(h = 100, d = wheel_diameter/6,  center = true);
              cylinder(h = 1, d1 = wheel_diameter/6 + 2, d2 = wheel_diameter/6,  center = false);
            }
          }
        }
      }
    }
  }
}


yellow_motor_wheel();
