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