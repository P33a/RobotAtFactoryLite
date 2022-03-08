//use <solenoid_support.scad>

base_h = 4;
base_len = 150;
base_width = 85;

corner_r = 5;

front = base_len/2;
back = -base_len/2;

left = base_width/2;
right = -base_width/2;

module place_things(pos_list) {
   for (t = pos_list) {  
     translate(t)
     children([0:1:$children-1]);
   }
}

pillar_holes = [
  [front - corner_r, left - corner_r], 
  [front - corner_r, right + corner_r], 
  [back + corner_r, left - corner_r], 
  [back + corner_r - 2, 0], 
  [back + corner_r, right + corner_r]

];

center_arduino_x = 16.0;
center_arduino_y = -24.18;

arduino_holes = [
  [center_arduino_x + 0, center_arduino_y + 0],
  [center_arduino_x + 1.1, center_arduino_y + 48.4],
  [center_arduino_x -51, center_arduino_y + 15.3],
  [center_arduino_x -51, center_arduino_y + 43.3]
];


ball_holes = [
  [0 ,-16/2],
  [0 , 0],
  [0,  16/2]
];

module round_base(bl, bw, bh, cr) {
  c_x = bl / 2 - cr;
  c_y = bw / 2 - cr;
  corners = [
  [  c_x,  c_y],
  [  c_x, -c_y],
  [ -c_x,  c_y],
  [ -c_x, -c_y]
  ];  
  
  linear_extrude(height = bh, center = true, convexity = 10, $fn = 8) 
  hull() {
    place_things(corners)   
      circle(r = cr, $fn = 64); 
    }

}

extra_x = front - 8;
extra_y = 22;
extra_sensor_holes = [
  [ extra_x, -2 * extra_y/2],
  [ extra_x, 0],
  [ extra_x,  2 * extra_y/2]
];

motor_h = 22.5;
motor_wall_width = 5;
motor_wall_len = 40;

module motor_wall() {
  $fn = 32;
  difference() {
    union() {
      cube([motor_wall_len, motor_wall_width, motor_h + 2], center = true);

      translate([(motor_wall_len + motor_wall_width)/2, -7/2, 0]) 
      cube([motor_wall_width, 7 + motor_wall_width, motor_h + 2], center = true);
      
    }

    // motor support holes
    translate([motor_wall_len/2 - 31.8, 0, 17.5/2])    
    rotate([90, 0, 0])
    cylinder(d = 3, h = 20, center = true);

    translate([motor_wall_len/2 - 31.8, 0, -17.5/2])    
    rotate([90, 0, 0])
    cylinder(d = 3, h = 20, center = true);

    // motor shaft hole
    translate([motor_wall_len/2 - 11.2, 0, 0])    
    rotate([90, 0, 0])
    cylinder(d = 8, h = 20, center = true);

    // motor bump hole
    translate([motor_wall_len/2 - 22.5, 0, 0])    
    rotate([90, 0, 0])
    cylinder(d = 5, h = 20, center = true);

    // motor top hole
    translate([(motor_wall_len + motor_wall_width)/2, 0, 0])    
    rotate([90, 0, 0])
    cylinder(d = 2, h = 30, center = true);
  }
}

module motor_support() {
  motor_base_h = 3;

  difference() {
    union() {
      translate([0, 0, motor_base_h])
      motor_wall();
      
      // Motor base
      translate([motor_wall_width/2, -19/2 + 2/2, -motor_h/2 + motor_base_h/2])
      cube([motor_wall_len + motor_wall_width, motor_wall_width + 19 - 2, motor_base_h], center = true); 
    }
    // remove 0.5 from floor and front wall
    translate([0, -19/2 - motor_wall_width/2, motor_base_h])
    cube([motor_wall_len + 1, 19, motor_h + 1], center = true); 
  }
}

module arduino() {
  color("SteelBlue", 1)
  import("arduino_solid_holes.stl", convexity = 10);
}


module LR_bot(){
  IR_z = 25;
  $fn = 64;
  IR_wall = 5;
  IR_wall_width = 32;
  
  difference() {
    union() {
      round_base(base_len, base_width, base_h, corner_r);
      //#cube([base_len, base_width, base_h], center = true);
      
      translate([front - (30 + motor_wall_len/2), right + motor_wall_width/2 + 19, motor_h/2 + base_h/2])
      motor_support();
      
      mirror([0, 1, 0])
      translate([front - (30 + motor_wall_len/2), right + motor_wall_width/2 + 19, motor_h/2 + base_h/2])
      motor_support();
      
      //translate([10, 0, -5])
      //rotate([0, 0, 180])
      //arduino();
      

      // IR Support
      translate([front - IR_wall/2, 26/2, IR_z])
      rotate([0, -30, 0])
      cylinder(d = IR_wall, h = 5, center = false);
      // IR Support
      translate([front - IR_wall/2, -26/2, IR_z])
      rotate([0, -30, 0])
      cylinder(d = IR_wall, h = 5, center = false);
      
      translate([front - IR_wall/2, 0, IR_z/2 + base_h/2])
      cube([IR_wall, IR_wall_width, IR_z], center = true);

      // Extra support      
      #translate([front - IR_wall/2, IR_wall_width/2 - 4/2, base_h/2])
      rotate([90, 0, 180])
      linear_extrude(height = 4, center = true, convexity = 10, twist = 0)
      polygon(points = [[0,0],[20,0],[0,IR_z]]);

      translate([front - IR_wall/2, -(IR_wall_width/2 - 4/2), base_h/2])
      rotate([90, 0, 180])
      linear_extrude(height = 4, center = true, convexity = 10, twist = 0)
      polygon(points = [[0,0],[20,0],[0,IR_z]]);
      
      // Switch Support
      //translate([front + 5/2 + 16, -IR_wall_width/2 + 5/2, base_h/2 + IR_z/2 - base_h/2 + 16])
      //cube([5, 5, IR_z + base_h], center = true);    

      //translate([front - IR_wall/2 + 1, 0, motor_h/2 + base_h/2 + 11.5 + 5 - 26/2])  
      //rotate([0, 0, 90])
      //solenoid_support();
      
      // Solenoid
      //translate([16, 0, 16])
      //translate([front + 15/2, 0, 18])
      //rotate([0, 90, 0])
      //cylinder(d = 1, h = 15, center = true);
      
      //translate([front, 0, 21])
      //rotate([0, 90, -90])
      //ratf_solenoid();

      // pedestral bateria 42 x 76 x 6
      //translate([-45, 0, base_h/2 + 6/2])
      //cube([42, 76, 6], center = true);
    }
    
    
    //translate([0, 0, -5])   
    //rotate([0, 0, 180])
    //arduino();
    
    translate([front - IR_wall/2, 0, 28])
    rotate([0, -30, 0])
    cube([30, 21, 10], center = true);

    //translate([front - IR_wall/2, -10, 10])
    //rotate([0, 90, 0])
    //cylinder(d = 2.8, h = 20, center = true);

    //translate([front - IR_wall/2, 10, 10])
    //rotate([0, 90, 0])
    //cylinder(d = 2.8, h = 20, center = true);
    
    // Solenoid holes
    translate([front - IR_wall/2, 6, 10])
    rotate([0, 90, 0])
    cylinder(d = 2, h = 10, center = true);

    translate([front - IR_wall/2, -6, 10])
    rotate([0, 90, 0])
    cylinder(d = 2, h = 10, center = true);
    
    // Switch holes
    //translate([front + 5/2 + 16, -IR_wall_width/2 + 5/2, base_h/2 + IR_z/2 + 5 + 16])
    //rotate([90, 0, 0])
    //cylinder(d = 2, h = 10, center = true);

    //#translate([front + 5/2 + 16, -IR_wall_width/2 + 5/2, base_h/2 + IR_z/2 - 5 + 16])
    //rotate([90, 0, 0])
    //cylinder(d = 2, h = 10, center = true);

    
    place_things(pillar_holes)   
    cylinder(r = 3.5/2, h = 2 * base_h, center = true);
    
    translate([back + 35, 0, 0])
    rotate([180, 0, 0])
    rotate([0, 0, 180])
    #place_things(arduino_holes)   
    cylinder(d = 2.2, h = 2 * base_h, center = true);    
    
    
    place_things(extra_sensor_holes)
    cylinder(d = 2, h = 2 * base_h, center = true); 
    
    //place_things([[40 ,0], [0, 0], [-40, -30], [-40, 30]])
    //cylinder(d = 25, h = 2 * base_h, center = true);   
    
    translate([front - IR_wall/2, 26/2, IR_z])
    rotate([0, -30, 0])
    cylinder(d = 2, h = 20, center = true);

    // IR Support hole
    translate([front - IR_wall/2, -26/2, IR_z])
    rotate([0, -30, 0])
    cylinder(d = 2, h = 20, center = true);    
    
    //translate([back + 20, 0, 0])  
    //place_things(ball_holes)
    //cylinder(d = 2.5, h = 2 * base_h, center = true);
    
    // slider support
    slid_x = -37.5;
    slid_y = -15;
    
    slid_holes = [
      [ slid_x/2, -slid_y/2],
      [ slid_x/2,  slid_y/2]
      //[ slid_x/2 - 4,  0]
    ];
    
    place_things(slid_holes)
    cylinder(d = 3.5, h = 2 * base_h, center = true); 
    
    bat_x = 19.3;
    bat_y = 55.3;
    
    bat_holes = [
//      [  bat_x/2, -bat_y/2],
//      [  bat_x/2,  bat_y/2],
      [  bat_x/2,  0],
      [ -bat_x/2,  0]
//      [ -bat_x/2, -bat_y/2],
//      [ -bat_x/2,  bat_y/2] 
    ];
    
    translate([back + 27, 0, 0])
    place_things(bat_holes)
    cylinder(d = 2.2, h = 5 * base_h, center = true);   
  

    // Switch hole
    translate([front - 15, base_width/2 - 14, 0])
    cylinder(d = 6.2, h = 2 * base_h, center = true); 

    // Central hole
    translate([front - 40, 0, 0])  
    round_base(30, 25, 100, corner_r);  
  }
  
  
}



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
                        [20, sw],
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

LR_bot();
//rotate([180, 0, 0])
//ratf_solenoid();