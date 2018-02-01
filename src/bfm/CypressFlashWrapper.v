module CypressFlashWrapper(SI,SO,SCK,CSNeg,WPNeg,RESETNeg,clk,rst);
inout SI;
inout SO;
input SCK;
input CSNeg;
inout WPNeg;
inout RESETNeg;
input clk;
input rst;

s25fs512s bfm(
        // Data Inputs/Outputs
        .SI(SI),
        .SO(SO),
        // Controls
        .SCK(SCK),
        .CSNeg(CSNeg),       //Not this?
        .WPNeg(WPNeg),       //Not this?
        .RESETNeg(RESETNeg)
);

endmodule

