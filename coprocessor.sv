`default_nettype none

module Adder
  #(parameter WIDTH=8)
  (input  logic [WIDTH-1:0] A, B,
   input  logic             Cin,
   output logic [WIDTH-1:0] S,
   output logic             Cout);

   assign {Cout, S} = A + B + Cin;

endmodule : Adder

module Multiplexer
  #(parameter WIDTH=8)
  (input  logic [WIDTH-1:0]         I,
   input  logic [$clog2(WIDTH)-1:0] S,
   output logic                     Y);

   assign Y = I[S];

endmodule : Multiplexer

module Counter
  #(parameter WIDTH=4)
  (input  logic             clk, en, clear, load, up,
   input  logic [WIDTH-1:0] D,
   output logic [WIDTH-1:0] Q);

  // Clear takes priority over load, which takes priority over counting.
  always_ff @(posedge clk)
    if (clear)
      Q <= 'd0;
    else if (load)
      Q <= D;
    else if (en & up)
      Q <= Q + 'd1;
    else if (en & ~up)
      Q <= Q - 'd1;

endmodule : Counter

module Register
  #(parameter WIDTH=8)
  (input  logic [WIDTH-1:0] D,
   input  logic             en, clear, clock,
   output logic [WIDTH-1:0] Q);

  always_ff @(posedge clock)
    if (en)
      Q <= D;
    else if (clear)
      Q <= 0;

endmodule : Register

module ShiftRegister
  #(parameter WIDTH = 4)
  (input  logic             clk, left, en, load,
   input  logic [WIDTH-1:0] D,
   output logic [WIDTH-1:0] Q);

  always_ff @(posedge clk)
    if (load)
      Q <= D;
    else if (en && left)
      Q <= {Q[WIDTH-2:0], 1'b0};
    else if (en && ~left)
      Q <= {1'b0, Q[WIDTH-1:1]};

endmodule : ShiftRegister

module arithShiftRegister
  (input  logic             clk, left, en, load,
   input  logic [7:0] D,
   output logic [15:0] Q);

    logic [15:0] mask1;
    assign mask1 = 16'hFF00;
    //assign mask2 = 16'h00FF;

  always_ff @(posedge clk)
    if (load) begin
      if (D[7])   
        Q <= {8'hFF,D};
      else
        Q <= {8'h00,D};
    end
    else if (en && left)
      Q <= {Q[14:0], 1'b0};
    else if (en && ~left)
      Q <= {1'b0, Q[15:1]};

endmodule : arithShiftRegister

module mult_coprocessor
    (input logic start,clock,reset_L,
     input logic [7:0] a,b,
     output logic [15:0] out,
     output logic done,
     output logic [3:0] con_code); 

/* 1. Clear the product (i.e. the result register).
   2. Determine the absolute value of B, Babs
   3. Check the LSB of Babs. If it's a 1, add A to the product.
   4. Shift Babs right by 1, and A left by one.
   5. Repeat Steps 3 and 4 until you have checked all 8 bits of Babs.
   6. If B was originally negative, negate the product. */
    
    logic [7:0] Babs;
    logic bIsNeg;
    assign bIsNeg = (b[7]==1);
    assign Babs = bIsNeg ? ~b + 1 : b; 

    logic counter_en,counter_ld,counter_up;
    logic [3:0] tmp;
    logic [3:0] count;
    assign tmp = start ? 4'b1111 : count;
    Counter #(4) c(clock,counter_en,0,counter_ld,1,tmp+1,count); 

    logic [15:0] acc;


    logic add;
    logic [15:0] newA;
    logic cin,cout;
    Adder #(8) ad(newA,acc,0,0,cout);
    
    logic clear;
    Register #(16) rr(acc+newA,add, clear,clock,acc);

    logic BabsRight;
    logic BabsLd;
    logic [7:0] newBabs;
    ShiftRegister #(8) sr(clock,0,BabsRight,BabsLd,Babs,newBabs);

    logic Aleft;
    logic A_load;
    arithShiftRegister sr1(clock,1,Aleft,A_load,a,newA);

    assign BabsLd = start;
    assign A_load = start;
    assign clear = start;
    assign counter_ld = start;   
 
    always_comb begin
        add = 0;
        done = 0;
        con_code = 4'b0000;
        if (count < 8) begin
            counter_en = 1;
            counter_up = 1;
            BabsRight = 1;
            Aleft = 1;
            if (newBabs[0]) add = 1;
        end
        else begin // count = 8
                done = 1;
                if (bIsNeg) out = ~acc +1;
                else out = acc;
                if (out==0) con_code = 4'b1000; // ZCNV
                else if (out[15]) con_code = 4'b0010; 
        end
    end

endmodule:mult_coprocessor
/*
module mult_test;

    logic start,done,reset_L,clock;
    logic [7:0] a,b;
    logic [15:0] out;
    logic [3:0] con_code;

    mult_coprocessor dut(.*);
    
    initial begin //  manage  your  reset  and  clock  here
            clock = 0;
            reset_L = 0;
            reset_L <= 1;
            forever #5 clock = ~clock;
    end

    //assign a = 8'h92;
    //assign a = 8'h0;
    assign a = 8'hFF; // -1
    //assign a = 8'h80; // -128

    assign b = 8'hFF; // 127
    //assign b = 8'h64;

//    assign b = 8'h80;

    initial begin
        $monitor($time,," bIsNeg = %d, a = %d, b = %h, out = %h, add = %b, acc = %b, count = %d, start = %b, done = %b, newBabs = %b",dut.bIsNeg, a,b,out,dut.add,dut.acc,dut.count,start,done,dut.newBabs);
        start <= 1; 
        @(posedge clock);
        start <= 0;
        @(posedge clock);
        while (!done) begin
            @(posedge clock);
        end
        #20
        $finish;
    end


endmodule:mult_test */
