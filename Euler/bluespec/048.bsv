import Vector::*;

//tweak Integer to UInt to make synthesizable
module problem48();
	 Vector #(1001,Integer) my_vector = genVector();
	 
	 function Integer f(Integer n); //could do this much more efficiently by truncating leading bits
			return n**n;
	 endfunction	 
	 
	 let my_sum = fold (\+ , map(f,my_vector)) - 1; //drop 1 because we included 0**0
	 rule show;
			$display(my_sum);
			$finish;
	 endrule
endmodule:problem48
	 


