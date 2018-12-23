/*bani 3 456 3 -- deleted 2 lines to get line numbers == PC value
eqri 3 72 3
addr 3 4 4
seti 0 0 4 -- end bani test
seti 0 5 3          R3 = 0
bori 3 65536 2      R2 = R3 | 0x10000
seti 7637914 8 3    R3 = Big number
bani 2 255 1        R1 = lowest byte of R2
addr 3 1 3          R3 = Big number + byte
bani 3 16777215 3   R3 = Lowest 3 bytes of R3
muli 3 65899 3      R3 = R3 x big number 2
bani 3 16777215 3   R3 = Lowest 3 bytes of R3
gtir 256 2 1        If R2 < 256, go to #28, else #17
addr 1 4 4          
addi 4 1 4          (go to #17)
seti 27 1 4         (go to #28)
seti 0 7 1          R1 = 0
addi 1 1 5          R5 = R1 + 1
muli 5 256 5        R5 = R5 * 256 (shift left 1 byte)
gtrr 5 2 5          If R5 > R2, set R2 = R1 and go to #8, else increment R1, go to #18
addr 5 4 4
addi 4 1 4          (go to #24)
seti 25 3 4         (go to #26)
addi 1 1 1          R1 = R1 + 1
seti 17 0 4         Go to #18
setr 1 8 2          R2 = R1
seti 7 7 4          Go to #8
eqrr 3 0 1
addr 1 4 4
seti 5 5 4          Go to #6
 */
package day21;

import java.util.HashSet;
import java.util.Set;

/**
 * Inspection of the program shows that lines 17-27 are the most time-consuming part of the program,
 * and are doing a 8-bit right shift in the least efficient way imaginable.
 * I rewrote the algorithm in Java below, and then looked for the first value being compared to R0
 * that had already been seen. The value in the iteration prior to that is the answer to part 2 -
 * it's the value that will cause the program to run longest while still terminating.
 */
public class Part2 {
	private static int l17(int r2) {
		return r2 >> 8;
//		int r1 = 0;
//		
//		while ((r1 + 1) * 256 <= r2) {
//			r1++;
//		}
//		return r1; // assign to r2;
	}
	private static void algo() {
		int r0 = 0;
		int r3 = 0;
		
		boolean done = false;
		int count = 0;
		final Set<Integer> seen = new HashSet<>();
		do {			
			int r2 = r3 | 0x10000;
//			System.out.println("r2 init: " + r2);
			r3 = 7637914;
			
			do {
//			for (; r2 >= 256; r2 >>= 8) {
				int r1 = r2 & 0xff; // 8
				r3 += r1;
				r3 &= 0xffffff;
				r3 *= 65899;
				r3 &= 0xffffff;
//				System.out.println("r3 iter: " + r3);
				r2 >>= 8;
			} while (r2 >= 1);
			System.out.println(r3);
			count++;
			done = !seen.add(r3);
		} while (r3 != r0 && !done);
		System.out.println("Saw " + r3 + " after " + count);
	}
	
	public static void main(String[] args) {
		algo();
	}
}
