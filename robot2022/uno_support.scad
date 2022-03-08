$fn = 32;

arduino_holes = [
  [0, 0],
  [1.1, 48.4],
  [-51, 15.3],
  [-51, 43.3]
];

module place_things(pos_list) {
   for (t = pos_list) {  
     translate(t)
     //children(0);
     children([0:1:$children-1]);
   }
 }


module uno_support(db = 6, cx1 = -20, cx2 = -30, cy = 27) {

  difference() {
    union() {
      place_things(arduino_holes)
      cylinder(d = 5, h = 4);
      
      linear_extrude(height = 1.2, center = true, convexity = 10, twist = 0) {
        hull() {
          translate(arduino_holes[0])
          circle(d = db); 
          
          translate([cx1, cy, 0])
          circle(d = db); 
        }

        hull() {
          translate(arduino_holes[1])
          circle(d = db); 
          
          translate([cx1, cy, 0])
          circle(d = db); 
        }

        hull() {
          translate(arduino_holes[3])
          circle(d = db); 
          
          translate([cx2, cy, 0])
          circle(d = db); 
        }
        
        hull() {
          translate(arduino_holes[2])
          circle(d = db); 
          
          translate([cx2, cy, 0])
          circle(d = db); 
        }

        hull() {
          translate([cx1, cy, 0])
          circle(d = db + 3); 
          
          translate([cx2, cy, 0])
          circle(d = db + 3); 
        }
      }
    }
    place_things(arduino_holes)
    cylinder(d = 3, h = 10, center = true);    
    
    translate([cx1, cy, 0])
    cylinder(d = 2.5, h = 10, center = true);    

    translate([cx2, cy, 0])
    cylinder(d = 2.5, h = 10, center = true);    

  }
}


uno_support();