import Vector :: * ;

module problem1();
	 Vector #(1000, Integer) my_vector = genVector(); //tweak to UInt or Int to synthesize
	 
	 function Integer f(Integer n); //using this weird thing instead of filter so we get synthesizable hardware
			if(n % 3 == 0 || n % 5 == 0)
				 return n;
			else
				 return 0;
	 endfunction

	 let my_sum = fold (\+ , map(f,my_vector));
	 
	 rule show;
			$display(my_sum);
			$finish;
	 endrule
endmodule:problem1
