//----------------------------------------------------------------------------
//	Filename:	flat_springV1_3.scad
//	Author:		R.Pritz ( r@pritz.me )
//	Date:		Sunday, 10 Jan 2016
//
// A parametric spring with end mounting stubs.
//----------------------------------------------------------------------------

/* [Basic] */

//width from opposing coil radius
width = 12;
//number of coils
spring_count = 6;
// how tall the spring will be
depth = 18;

/* [Advanced] */

// length of support stub
stub_length = 0;
// radius of turn in each coil
radius = 1;
// width of spring coils 
thickness = 1;

/* [Hidden] */
$fn = 32;

module spring_maker(radius = 5, thickness = 5, depth = 20, width = 40, spring_count = 21, end_len = 30){

    
    module arc_maker(radius, thickness, depth){
        difference(){
            linear_extrude( depth, center=true)
                circle( radius+(thickness/2) );
            linear_extrude( depth*1.2, center=true )
                circle( radius-(thickness/2) );
            translate([0,-radius-(thickness/2),0])
                linear_extrude( depth*1.2, center=true )
                    square( radius*3 );
        }
    }
    union(){
        
        translate([(width/2),-((end_len/2)+(radius/2)),0])
            rotate([0,0,90])
            linear_extrude( depth, center=true )
                square( [end_len, thickness], center=true );
        translate([(width/4),-radius,0])
            linear_extrude( depth, center=true )
                square( [width/2, thickness], center=true );
        arc_maker(radius, thickness, depth);
        
        for (i = [1:spring_count]){
            mod = i%2;
            rotate([0,mod*180,0])
                translate([mod*-width,(radius*2*i),0])
                    arc_maker(radius, thickness, depth);
            translate([width/2,i*(radius*2)-radius,0])
                linear_extrude( depth, center=true )
                    square( [width, thickness], center=true );        
        }
        
        translate([width/4+(spring_count%2*(width/2)),(spring_count+1) *(radius*2)-radius,0])
                linear_extrude( depth, center=true )
                    square( [(width/2), thickness], center=true );
        translate([(width/2),((end_len/2)+(radius/2))+(radius*2*spring_count),0])
            rotate([0,0,90])
            linear_extrude( depth, center=true )
                square( [end_len, thickness], center=true );
    }
}

module front_guide() {
  $fn = 32;
  eps = 0.0;
  wall = 0;
  difference() {
    cube([3, depth, 24], center = true);
        

        
    // body holes
    hull() {
      translate([0, 12/2, 6])
      rotate([0, 90, 0])
      cylinder(d = 2.5, h = 20, center = true); 

      translate([0, 12/2, -6])
      rotate([0, 90, 0])
      cylinder(d = 2.5, h = 20, center = true); 
    }    

    // body holes
    hull() {
      translate([0, -12/2, 6])
      rotate([0, 90, 0])
      cylinder(d = 2.5, h = 20, center = true); 

      translate([0, -12/2, -6])
      rotate([0, 90, 0])
      cylinder(d = 2.5, h = 20, center = true); 
    }    
  }
}

module back_guide() {
  $fn = 32;
  eps = 0.0;
  wall = 0;
  difference() {
    cube([3, depth, 24], center = true);
        

        
    // body holes
    translate([0, 12/2, -2])
    rotate([0, 90, 0])
    cylinder(d = 2, h = 20, center = true); 

    // body holes
    translate([0, -12/2, -2])
    rotate([0, 90, 0])
    cylinder(d = 2, h = 20, center = true); 
  }
}

module spring_adapter() {
  difference() {
    union() {
      translate([2, 0, 16])
      rotate([0, 90, -90])
      spring_maker(radius, thickness, depth, width, spring_count, stub_length);

      back_guide();
      
      translate([16, 0, 0])
      front_guide();

      translate([2, 0, -18])
      rotate([0, -90, -90])
      spring_maker(radius, thickness, depth, width, spring_count, stub_length);
    }  
  }
}


rotate([90, 0, 0])
spring_adapter();
