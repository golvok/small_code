`timescale 1 ns / 100 ps


module tester;
	reg [2:0] in;
	wire [2:0] out;

	initial begin
		$display("=== Begin Simulation ===");
		in = 3'd0;
	end

	// count up, testing all bit patterns
	always begin
		#1
		if (in == ~out) begin
			$display("%b is handled correctly", in);
		end else begin
			$display("for %b did not get %b, got %b instead", in, ~in, out);
		end
		in = in + 1;
	end

	initial begin
		#9
		$display("=== End Simulation ===");
		$finish;
	end

	invert3with2notgates under_test(
		.in(in), .out(out)
	);

endmodule // tester


module invert3with2notgates (
	input [2:0] in,
	output [2:0] out
);

	wire a_and_b;
	wire b_and_c;
	wire c_and_a;
	wire a_or_b;
	wire b_or_c;
	wire c_or_a;

	assign a_and_b = in[0] & in[1];
	assign b_and_c = in[1] & in[2];
	assign c_and_a = in[2] & in[0];
	assign a_or_b = in[0] | in[1];
	assign b_or_c = in[1] | in[2];
	assign c_or_a = in[2] | in[0];

	wire is_two_or_more;
	wire is_one_or_more;
	wire is_three;
	wire is_two;
	wire is_one_or_two;
	wire is_one_or_none;
	wire is_one;
	wire is_none;
	assign is_two_or_more = a_and_b | b_and_c | c_and_a;
	assign is_one_or_more = in[0] | in[1] | in[2];
	assign is_three = in[0] & in[1] & in[2];
	assign is_two = is_two_or_more & !is_three; // === inverter ===
	assign is_one_or_two = is_one_or_more & !is_three; // === inverter ==
	assign is_one_or_none = !is_two_or_more; // === inverter ===
	assign is_one = is_one_or_more & !is_two_or_more; // === inverter ===
	assign is_none = !is_one_or_more; // === inverter ===

	wire [2:0] is_two_and_the_others_are_high;
	assign is_two_and_the_others_are_high[0] = is_two & b_and_c;
	assign is_two_and_the_others_are_high[1] = is_two & c_and_a;
	assign is_two_and_the_others_are_high[2] = is_two & a_and_b;

	wire [2:0] is_two_or_more_and_the_others_are_high;
	assign is_two_or_more_and_the_others_are_high[0] = is_two_or_more & b_and_c;
	assign is_two_or_more_and_the_others_are_high[1] = is_two_or_more & c_and_a;
	assign is_two_or_more_and_the_others_are_high[2] = is_two_or_more & a_and_b;

	wire [2:0] is_one_or_two_and_not_me;
	assign is_one_or_two_and_not_me[0] = (is_one & b_or_c) | (is_two & b_and_c);
	assign is_one_or_two_and_not_me[1] = (is_one & c_or_a) | (is_two & c_and_a);
	assign is_one_or_two_and_not_me[2] = (is_one & a_or_b) | (is_two & a_and_b);

	wire [2:0] is_two_or_more_incl_me;
	assign is_two_or_more_incl_me = {3{is_two_or_more}} & in;

	wire [2:0] is_one_and_not_me;
	assign is_one_and_not_me[0] = is_one_or_none & b_or_c;
	assign is_one_and_not_me[1] = is_one_or_none & c_or_a;
	assign is_one_and_not_me[2] = is_one_or_none & a_or_b;

	wire [2:0] pull_high;
	wire [2:0] allow_signal_high;

	assign pull_high = is_two_and_the_others_are_high | is_one_and_not_me; // | {3{is_none}};
	// assign allow_signal_high = {3{!is_three}}; // === inverter ===
	// assign allow_signal_high = is_two_and_the_others_are_high;
	assign allow_signal_high = 3'b111;

	assign out = allow_signal_high & pull_high;

endmodule

