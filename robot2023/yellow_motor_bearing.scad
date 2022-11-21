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

hh_dist = 20.6;
bear_w = 32;
wall = 2;
wall_h = 8;

dx = 38;
dy = 26;
dh = 3;

hdisp = 17.5;

eps = 0.2;

module yellow_motor_bearing() {
  $fn = 64;
  difference() {
    union() {
      translate([dx/2 - 6, 0, dh/2])
      cube([dx, dy, dh], center = true);
      
      translate([hh_dist, 0, 0])
      cylinder(d = bear_w + 2 * wall, h = wall_h, center = false);

    }

    translate([hh_dist, 0, dh])
    cylinder(d = bear_w - 2, h = wall_h, center = false);
    
    translate([hh_dist, 0, dh + 0.5])
    cylinder(d = bear_w + eps, h = wall_h, center = false);
     
    // fixing holes
    translate([0, hdisp/2, 0])
    cylinder(d = 4, h = 3 * dh, center = true);

    translate([0, -hdisp/2, 0])
    cylinder(d = 4, h = 3 * dh, center = true);

    // axis hole
    translate([hh_dist, 0, 0])
    cylinder(d = 24, h = 10 * dh, center = true);
    
    // bump hole
    translate([9, 0, 0])
    cylinder(d = 6, h = 10 * dh, center = true);


  }
}

yellow_motor_bearing();