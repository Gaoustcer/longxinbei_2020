module register_file
    #(parameter WIDTH=32)
    (
        input clk,
        input [4:0] raddr1,raddr2,//two write address
        input [4:0] waddr,//one write address
        input wd,we,//we is the chip enable signal and wd is the write enable signal
        input [WIDTH-1:0] data,//write in data
        output [WIDTH-1:0] read1,read2
    );
    initial Mem_file[0]=0;
    initial Mem_file[1]=0;
    initial Mem_file[2]=0;
    initial Mem_file[3]=0;
    initial Mem_file[4]=0;
    initial Mem_file[5]=0;
    initial Mem_file[6]=0;
    initial Mem_file[7]=0;
    initial Mem_file[8]=0;
    initial Mem_file[9]=0;
    initial Mem_file[10]=0;
    initial Mem_file[11]=0;
    initial Mem_file[12]=0;
    initial Mem_file[13]=0;
    initial Mem_file[14]=0;
    initial Mem_file[15]=0;
    initial Mem_file[16]=0;
    initial Mem_file[17]=0;
    initial Mem_file[18]=0;
    initial Mem_file[19]=0;
    initial Mem_file[20]=0;
    initial Mem_file[21]=0;
    initial Mem_file[22]=0;
    initial Mem_file[23]=0;
    initial Mem_file[24]=0;
    initial Mem_file[25]=0;
    initial Mem_file[26]=0;
    initial Mem_file[27]=0;
    initial Mem_file[28]=0;
    initial Mem_file[29]=0;
    initial Mem_file[30]=0;
    initial Mem_file[31]=0;
    reg [WIDTH-1:0] Mem_file [WIDTH-1:0];
    assign read1=we?Mem_file[raddr1]:0;//if the we is 1,then read the read2
    assign read2=we?Mem_file[raddr2]:0;//if the we is 1,then read the read1
    always@(posedge clk)
    begin
        if(we&&wd)//when the we and wd is both 1,the data can be writen into the register file
            Mem_file[waddr]=data;//The storage unit whose Index is waddr will be writen with the data
    end
endmodule

module ALU_32
    #(parameter WIDTH=32,
    ADD=3'b000,
    MIN=3'b001,
    AND=3'b010,
    OR=3'b011,
    DUFF=3'b100
    )
    (input [31:0] a,b,
    input [2:0] alu_op,
    output zf,cf,of,
    output [31:0] result
    );
    reg cf_reg,of_reg;
    reg [WIDTH-1:0] reg_result;
    assign zf = (result==0)?1:0 ;
    assign cf = cf_reg ;
    assign of = of_reg ;
    assign result = reg_result ;
    always@(*)begin
        case (alu_op)
        ADD:begin
            {cf_reg,reg_result}=a+b;
            of_reg=(~a[WIDTH-1]&~b[WIDTH-1]&result[WIDTH-1])|(a[WIDTH-1]&b[WIDTH-1]&~result[WIDTH-1]);
        end
        MIN:begin
            {cf_reg,reg_result}=a-b;
            of_reg=(~a[WIDTH-1]&b[WIDTH-1]&result[WIDTH-1])|(a[WIDTH-1]&~b[WIDTH-1]&~result[WIDTH-1]);
        end
        AND:begin
            reg_result=a&b;
            cf_reg=0;
            of_reg=0;
        end
        OR:begin
            reg_result=a|b;
            cf_reg=0;
            of_reg=0;
        end
        DUFF:begin
            reg_result=a^b;
            cf_reg=0;
            of_reg=0;
        end
        default:begin
            reg_result=8'b11111111;
            cf_reg=0;
            of_reg=0;
        end
        endcase
    end
endmodule

module Mux2_1
    #(parameter WIDTH=32)
    (
        input [WIDTH-1:0] A,B,//0 is A and 1 is B
        input sel,
        output [WIDTH-1:0] result
    );
    assign result=sel?:B:A;//if sel=0,then select A,oR select B
endmodule

module en_reg
    #(parameter WIREWIDTH=32)
    (
        input [WIREWIDTH-1:0] data,
        input enable,
        input clk,
        input rst,
        output reg [WIREWIDTH-1:0] result
    );
    //initial result=0;
    always@(posedge clk or posedge rst)
        if(rst) result=0;
        else 
        if (enable)
        result=data;
        else ;
        
endmodule

module equivent
    #(parameter WIDTH=32)
(
    input [WIDTH-1:0] a,b,
    output Eqid
);
    assign Eqid=(a==b)?1:0;
endmodule // equivent

//Branch:beq=1 else =0
//Regwrite:write in Register File 1,else 0
//ALUSrc:Immdiate number is 1,else is 0
//MemtoReg:Memory data is 1,else is 0
//RegDst:wirte in Rt(eg:lw) is 1,write in Rd is 0
//jump:j is 1,else is 0
//Memwrite:write in Memory is 1,else is 0
//ALUOP:R_type is 2'b00,
module control(
    input [5:0] aluop,
    output reg Branch,Regwrite,ALUSrc,MemtoReg,RegDst,jump,Memwrite
    output reg [1:0] ALUOP
);
    always@(aluop)
    case (aluop)
        6'b000000:{Branch,Regwrite,ALUSrc,ALUOP,MemtoReg,RegDst,jump,Memrite}=9'b010000100;//add
        6'b001000:{Branch,Regwrite,ALUSrc,ALUOP,MemtoReg,RegDst,jump,Memwrite}=9'b01000000;//addi
        6'b100011:{Branch,Regwrite,ALUSrc,ALUOP,MemtoReg,RegDst,jump,Memwrite}=9'b011011000;//lw
        6'b101011:{Branch,Regwrite,ALUSrc,ALUOP,MemtoReg,RegDst,jump,Memwrite}=9'b001010001;//sw
        6'b000100:{Branch,Regwrite,ALUSrc,ALUOP,MemtoReg,RegDst,jump,Memwrite}=9'b100100000;//beq
        6'b000010:{Branch,Regwrite,ALUSrc,ALUOP,MemtoReg,RegDst,jump,Memwrite}=9'b000000010;//j
        default: {Branch,Regwrite,ALUSrc,ALUOP,MemtoReg,RegDst,jump,Memwrite}=9'b000000000;
    endcase

endmodule // control

module ALU_control(
    input [1:0] aluop,
    input [5:0] funct,
    output [2:0] alucontrol
);
    alway@(*)
    begin
        case (aluop)
            2'b00:alucontrol=3'b000;
            2'b01:alucontrol=3'b000;
            2'b10:alucontrol=3'b001;
            default:alucontrol=3'b111;
        endcase
    end

endmodule // ALU_control

module REG_always_enable
    #(parameter WIDTH=32)  
    (
        input clk,
        input rst,
        input [WIDTH-1:0] data,
        output [WIDTH-1:0] read
    );
    reg [WIDTH-1:0] Read_tmp;
    assign read=Read_tmp;
    always@(posedge clk or posedge rst)
    if(rst) Read_tmp<=0;
    else Read_tmp=data;
endmodule

module signal_deal
    (
        input clk,
        input data,
        input rst,
        output signal
    );
    reg [1:0] state,next_state;
    assign signal=(state==2'b01);
    always@(posedge clk or posedge rst)
    begin
        if(rst) next_state=2'b00;
        else state=next_state;
    end
    always@(*)
    begin
        case (state)
        1'b00:
        begin
            if(data) next_state=2'b01;
            else next_state=2'b00;
        end
        2'b01:
        begin
            if(data) next_state=2'b11;
            else next_state=2'b00;
        end
        2'b11:
        begin
            if(data) next_state=2'b11;
            else next_state=2'b00;
        end
        default:state=0;
        endcase
    end
endmodule

module sign_extend
    #(parameter IN=16
    OUT=32)
    (
        input [IN-1:0] in_Instruction,
        output [OUT-1:0] out_extend
    );
    assign out_extend=in_Instruction[IN-1:0];
    assign out_extend[OUT-1:IN]=in_Instruction[IN-1]?4'hFFFF:4'F0000;
endmodule

module Mux4_1
    #(parameter WIDTH=32)
(
    input [WIDTH-1:0] A,B,C,D,
    input [1:0] sel4_1,
    output [WIDTH-1:0] result

);
    reg [WIDTH-1:0] R;
    assign result=R;
    always@(*)
    case (sel)
        2'b00:R=A;
        2'b01:R=B;
        2'b10:R=C;
        2'b11:R=D; 
        default:R=32'hFFFF;
    endcase

endmodule // Mux4_1


module Instruction_memory(
    input [7:0] a,
    output [31:0] d
);

endmodule // Instruction_memory
module CPU_multicycle
    #(parameter WIDTH=32)
(
    input clk,rst
);
    wire [WIDTH-1:0] PCin,PCout;
    wire [WIDTH-1:0] IF_ID_NPCin,IF_ID_NPCout;
    wire IF_IDwrite;
    wire PCwrite;
    wire [WIDTH-1:0] Instructionin,Instructionout;//Rs [25:21],Rt[20:16],Rd[15:11]
    wire RegWriteD,MemtoRegD,MemWriteD,
    ALUSrcD,RegDstD,Branch,jump;//the first decode signal,which exist in the decode 
    wire [1:0] aluopcode;
    wire StallF,StallD;
    wire PCSrc;
    wire [WIDTH-1:0] PCSrcout_2;
    wire [WIDTH-1:0] Jumpaddr;
    wire [WIDTH-1:0] RFread1,RFread2;
    wire [WIDTH-1:0] RFwriteindata;
    wire [WIDTH-1:0] extendImm;
    wire PCSrcD;
    wire equivent;//if A==B then the value is 1,else is 0 Decode
    wire FlushE;//This is the start of the EX process signal
    wire RegwriteE;
    wire MemtoRegE;
    wire MemwriteE;
    wire ALUSrcE;
    wire RegDstE;
    wire [1:0] ALUOPE;//end of EX process signal
    wire [4:0] RsE,RdE,RtE;//EX_Decode signal
    //Instruction Fetch(from the input of PC register)
    en_reg regPC(.data(PCin),.enable(!StallF),.clk(clk),.rst(rst),.result(PCout));
    assign IF_ID_NPCin=PCout+4;
    en_reg IF_ID_NPC(.data(IF_ID_NPCin),.enable(!StallD),.clk(clk),.rst(rst|PCSrc),.result(IF_ID_NPCout));
    Instruction_memory Instruction_memory_example(.a(PCout[9:2]),.d(Instructionin));
    en_reg IF_ID_Instruction(.data(Instructionin),.enable(!StallD),
    .clk(clk),.rst(rst|PCSrc),.result(Instructionout));//
    Mux2_1 PCSrc_1(.sel(PCSrc),.A(IF_ID_NPCin),.B(PCSrcout_2),.result(PCin));
    Mux2_1 PCSrc_2(.sel(jump),.A(IF_ID_NPCout),.B(Jumpaddr),.result(PCSrcout_2));
    //Instrcution Decode
    assign PCSrc=jump|PCSrcD;
    assign PCSrcD=Branch&equivent;
    assign equivent=(RFread1==RFread2)?1:0;
    control pipelinecontrol(.aluop(Instructionout[31:25]),.Branch(Branch),.Regwrite(RegWriteD),
    .ALUSrc(ALUSrcD),.ALUOP(aluopcode)
    ,.MemtoReg(MemtoRegD),.RegDst(RegDstD),.jump(jump),.Memwrite(MemwriteD));
    register_file register_file_pipeline(.clk(clk),.raddr1(Instructionout[25:21]),.raddr2(Instructionout[20:16]),
    .waddr(),.wd(RFwd),.we(RFwe),.data(RFwriteindata),.read1(RFread1),.read2(RFread2));
    sign_extend extend(.in_Instruction(Instructionout[15:0]),.out_extend(extendImm));
    //The following is the ID_EX_Register
    REG_always_enable #(1)ID_EX_Regwrite(.clk(clk),.rst(rst|FlushE),.data(RegWriteD),.read(RegwriteE));
    REG_always_enable #(1)ID_EX_MemtoReg(.clk(clk),.rst(rst|FlushE),.data(MemtoRegD),.read(MemtoRegE));
    REG_always_enable #(1)ID_EX_Memwrite(.clk(clk),.rst(rst|FlushE),.data(MemwriteD),.read(MemwriteE));
    REG_always_enable #(1)ID_EX_ALUSrc(.clk(clk),.rst(rst|FlushE),.data(ALUSrcD),.read(ALUSrcE));
    REG_always_enable #(1)ID_EX_RegDst(.clk(clk),.rst(rst|FlushE),.data(RegDstD),.read(RegDstE));
    REG_always_enable #(2)ID_EX_aluop(.clk(clk),.rst(rst|FlushE),.data(aluopcode),.read(ALUOPE));
    REG_always_enable #(5)ID_EX_Rs(.clk(clk),.rst(rst|FlushE),.data(Instructionout[25:21]),.read(RsE));
    REG_always_enable #(5)ID_EX_Rt(.clk(clk),.rst(rst|FlushE),.data(Instructionout[20:16]),.read(RtE));
    REG_always_enable #(5)ID_EX_Rd(.clk(clk),.rst(rst|FlushE),.data(Instructionout[15:11]),.read(RdE));
    //The following is the EX process


endmodule // CPU_multicycle