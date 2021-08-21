module Top(
           input        CLK_32MHZ,
           input        RX,
           output       TX,
           output [3:0] SS_AN,
           output [6:0] SS_SEG,
           output       SS_DP
           );

   topEntity u_topEntity
     (.CLK(CLK_32MHZ),
      .RX(RX),
      .TX(TX),
      .SS_AN(SS_AN[3:0]),
      .SS_SEG(SS_SEG[6:0]),
      .SS_DP(SS_DP)
      );
   
endmodule
