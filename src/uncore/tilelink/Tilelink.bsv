package Tilelink;

import Tilelink_Types.bsv ::*;

interface Ifc_Master_link;

interface Put#(A_channel) xactor_request;
interface Get#(D_channel) xactor_response;

interface Get#(A_channel) fabric_side;
interface Put#(D_channel) fabric_side;

endinterface

module mkMasterXactor(Ifc_Master_link);

endmodule

endpackage
