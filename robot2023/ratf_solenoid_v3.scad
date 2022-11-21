/* Copyright (c) 2022  Paulo Costa
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

   * Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in
     the documentation and/or other materials provided with the
     distribution.
   * Neither the name of the copyright holders nor the names of
     contributors may be used to endorse or promote products derived
     from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE. */

module switch_upport() {
  difference() {
    cube([23, 5, 4], center = true);  
    
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
                        //[24, -20],
                        //[24 + sw, -20],
                        [24 - sw, sw],
                        [24, sw],
                        [sw, sw],
                        [sw - disp_u, sh],
                        [-19 - disp_u + sw, sh],
                        [-19 - disp_u + sw, sh + 10],
                        [-19 - disp_u, sh + 10],
                        [-19 - disp_u, sh - sw - 10],
                        [-19 + sw - disp_u, sh - sw - 10],
                        [-19 + sw - disp_u, sh - sw],
                        [-disp_u, sh - sw],
                        [0, 0]
                       ]);
      
      // Switch Support
      translate([-11.5, 18.5, 14])
      switch_upport();
      
      
      translate([-11.5, 18.5, -14])
      switch_upport();
    }    
  
    
    // center hole
    hull() {
      translate([-26/2 - 3, 0, 0])
      rotate([90, 0, 0])
      cylinder(d = 4.2, h = 200, center = true);

      translate([-26/2 + 5, 0, 0])
      rotate([90, 0, 0])
      cylinder(d = 4.2, h = 200, center = true);
    }

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