module StereoVolumenControl(
    input wire [2:0] volume_ctrl,
    input wire signed [15:0] audio_left_in,
    input wire signed [15:0] audio_right_in,
    output wire signed [15:0] audio_left_out,
    output wire signed [15:0] audio_right_out
);

    reg signed [15:0] volumen_maximo = 16'h7FFF;  // Volumen máximo 
    reg signed [15:0] audio_left_atenuado;
    reg signed [15:0] audio_right_atenuado;

    always @* begin
        case (volume_ctrl)
            3'b111: begin  // Volumen mínimo
                audio_left_atenuado = 0;
                audio_right_atenuado = 0;
            end
            3'b110: begin  
                audio_left_atenuado = audio_left_in >>> 6;
                audio_right_atenuado = audio_right_in >>> 6;
            end
            3'b101: begin  
                audio_left_atenuado = audio_left_in >>> 5;
                audio_right_atenuado = audio_right_in >>> 5;
            end
            3'b100: begin  
                audio_left_atenuado = audio_left_in >>> 4;
                audio_right_atenuado = audio_right_in >>> 4;
            end
            3'b011: begin  
                audio_left_atenuado = audio_left_in >>> 3;
                audio_right_atenuado = audio_right_in >>> 3;
            end
            3'b010: begin  
                audio_left_atenuado = audio_left_in >>> 2;
                audio_right_atenuado = audio_right_in >>> 2;
            end
            3'b001: begin  
                audio_left_atenuado = audio_left_in >>> 1;
                audio_right_atenuado = audio_right_in >>> 1;
            end


            3'b000: begin  // Volumen máximo
                audio_left_atenuado = audio_left_in;
                audio_right_atenuado = audio_right_in;
            end
            default: begin  // Valor por defecto (volumen mínimo)
                audio_left_atenuado = 0;
                audio_right_atenuado = 0;
            end
        endcase
		  // Limitador de volumen
        if (audio_left_atenuado > volumen_maximo) begin
            audio_left_atenuado = volumen_maximo;
        end
        if (audio_right_atenuado > volumen_maximo) begin
            audio_right_atenuado = volumen_maximo;
        end
    end

    assign audio_left_out = audio_left_atenuado;
    assign audio_right_out = audio_right_atenuado;

endmodule