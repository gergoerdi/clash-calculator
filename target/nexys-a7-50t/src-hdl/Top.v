module Top(
           input        CLK100MHZ,
           input        UART_TXD_IN,
           output       UART_RXD_OUT,
           output [7:0] AN,
           output [6:0] SEG,
           output       DP,
           inout [10:1] JA
           );

   assign AN[7:4] = 4'b1111;
   
   topEntity u_topEntity
     (.CLK(CLK100MHZ),
      .RX(UART_TXD_IN),
      .TX(UART_RXD_OUT),
      .SS_AN(AN[3:0]),
      .SS_SEG(SEG[6:0]),
      .SS_DP(DP),
      .ROWS(JA[10:7]),
      .COLS(JA[4:1])
      );
   
endmodule
