"""
Test suite for BitOps module
Run with: include("test_bitops.jl")
"""

using Test
include("BitOps.jl")
using .BitOps

@testset "BitOps Test Suite" begin
    
    @testset "Basic Bit Operations" begin
        @testset "setbit" begin
            @test setbit(UInt8(0b00000000), 0) == 0b00000001
            @test setbit(UInt8(0b00000000), 7) == 0b10000000
            @test setbit(UInt16(0), 15) == 0x8000
            @test setbit(Int8(0), 3) == Int8(8)
            @test setbit(UInt64(0), 63) == typemax(Int64) + 1
        end
        
        @testset "setbits" begin
            @test setbits(UInt8(0), [0, 2, 4, 6]) == 0b01010101
            @test setbits(UInt16(0), [0, 15]) == 0x8001
            @test setbits(UInt32(0x1000), [0, 1, 2]) == 0x1007
        end
        
        @testset "clearbit" begin
            @test clearbit(UInt8(0b11111111), 0) == 0b11111110
            @test clearbit(UInt8(0b11111111), 7) == 0b01111111
            @test clearbit(UInt16(0xFFFF), 15) == 0x7FFF
            @test clearbit(Int8(-1), 3) == Int8(-9)
        end
        
        @testset "clearbits" begin
            @test clearbits(UInt8(0xFF), [0, 2, 4, 6]) == 0b10101010
            @test clearbits(UInt16(0xFFFF), [0, 15]) == 0x7FFE
        end
        
        @testset "togglebit" begin
            @test togglebit(UInt8(0b10101010), 0) == 0b10101011
            @test togglebit(UInt8(0b10101010), 1) == 0b10101000
            @test togglebit(UInt32(0), 31) == 0x80000000
            @test togglebit(togglebit(UInt16(0x1234), 5), 5) == 0x1234
        end
        
        @testset "testbit" begin
            @test testbit(UInt8(0b10101010), 1) == true
            @test testbit(UInt8(0b10101010), 0) == false
            @test testbit(UInt64(1) << 63, 63) == true
            @test testbit(Int8(-1), 7) == true
            @test all(i -> testbit(UInt8(0xFF), i), 0:7)
            @test !any(i -> testbit(UInt8(0x00), i), 0:7)
        end
    end
    
    @testset "Bit Counting" begin
        @testset "popcount/countones" begin
            @test popcount(UInt8(0)) == 0
            @test popcount(UInt8(0xFF)) == 8
            @test popcount(UInt8(0b10101010)) == 4
            @test countones(UInt32(0x12345678)) == 13
            @test popcount(typemax(UInt64)) == 64
            @test popcount(UInt16(0x8000)) == 1
        end
        
        @testset "countzeros" begin
            @test countzeros(UInt8(0)) == 8
            @test countzeros(UInt8(0xFF)) == 0
            @test countzeros(UInt8(0b10101010)) == 4
            @test countzeros(UInt32(0x12345678)) == 19
        end
        
        @testset "leadingzeros" begin
            @test leadingzeros(UInt8(0)) == 8
            @test leadingzeros(UInt8(0xFF)) == 0
            @test leadingzeros(UInt8(0b00001111)) == 4
            @test leadingzeros(UInt32(1)) == 31
            @test leadingzeros(UInt64(1) << 63) == 0
        end
        
        @testset "trailingzeros" begin
            @test trailingzeros(UInt8(0)) == 8
            @test trailingzeros(UInt8(0xFF)) == 0
            @test trailingzeros(UInt8(0b11110000)) == 4
            @test trailingzeros(UInt32(0x80000000)) == 31
            @test trailingzeros(UInt64(1) << 63) == 63
        end
    end
    
    @testset "Bit Extraction and Manipulation" begin
        @testset "extractbits" begin
            @test extractbits(UInt8(0b11001100), 2, 4) == 0b0011
            @test extractbits(UInt16(0x1234), 4, 8) == 0x23
            @test extractbits(UInt32(0xABCDEF00), 8, 16) == 0xCDEF
            @test extractbits(UInt64(typemax(UInt64)), 0, 64) == typemax(UInt64)
        end
        
        @testset "insertbits" begin
            @test insertbits(UInt8(0b11110000), UInt8(0b1010), 2, 4) == 0b11101000
            @test insertbits(UInt16(0x0000), UInt16(0xFF), 4, 8) == 0x0FF0
            @test insertbits(UInt32(0), UInt32(0xABCD), 16, 16) == 0xABCD0000
        end
        
        @testset "reversebits" begin
            @test reversebits(UInt8(0b10110000)) == 0b00001101
            @test reversebits(UInt8(0b11111111)) == 0b11111111
            @test reversebits(UInt8(0b00000000)) == 0b00000000
            @test reversebits(UInt16(0x1234)) == 0x2c48
            @test reversebits(reversebits(UInt32(0x12345678))) == 0x12345678
        end
        
        @testset "swapbits" begin
            @test swapbits(UInt8(0b10000001), 0, 7) == 0b00000011
            @test swapbits(UInt8(0b10000001), 1, 6) == 0b11000000
            @test swapbits(UInt16(0x8001), 0, 15) == 0x0003
            @test swapbits(UInt8(0xFF), 3, 4) == 0xFF  # No change when both bits are same
        end
    end
    
    @testset "Power of 2 Operations" begin
        @testset "ispowerof2" begin
            @test ispowerof2(UInt8(1)) == true
            @test ispowerof2(UInt8(2)) == true
            @test ispowerof2(UInt8(4)) == true
            @test ispowerof2(UInt8(128)) == true
            @test ispowerof2(UInt8(0)) == false
            @test ispowerof2(UInt8(3)) == false
            @test ispowerof2(UInt8(127)) == false
            @test ispowerof2(UInt64(1) << 63) == true
        end
        
        @testset "nextpowerof2" begin
            @test nextpowerof2(UInt8(0)) == 1
            @test nextpowerof2(UInt8(1)) == 1
            @test nextpowerof2(UInt8(2)) == 2
            @test nextpowerof2(UInt8(3)) == 4
            @test nextpowerof2(UInt8(127)) == 128
            @test nextpowerof2(UInt32(1000)) == 1024
        end
        
        @testset "prevpowerof2" begin
            @test prevpowerof2(UInt8(0)) == 0
            @test prevpowerof2(UInt8(1)) == 1
            @test prevpowerof2(UInt8(2)) == 2
            @test prevpowerof2(UInt8(3)) == 2
            @test prevpowerof2(UInt8(127)) == 64
            @test prevpowerof2(UInt8(128)) == 128
            @test prevpowerof2(UInt32(1000)) == 512
        end
        
        @testset "roundtopowerof2" begin
            @test roundtopowerof2(UInt8(0)) == 0
            @test roundtopowerof2(UInt8(1)) == 1
            @test roundtopowerof2(UInt8(3)) == 4  # Closer to 4 than 2
            @test roundtopowerof2(UInt8(5)) == 4  # Closer to 4 than 8
            @test roundtopowerof2(UInt8(120)) == 128
        end
    end
    
    @testset "Bit Masks" begin
        @testset "lowmask" begin
            @test lowmask(UInt8, 0) == 0b00000000
            @test lowmask(UInt8, 4) == 0b00001111
            @test lowmask(UInt8, 8) == 0b11111111
            @test lowmask(UInt16, 12) == 0x0FFF
            @test lowmask(UInt32, 16) == 0x0000FFFF
        end
        
        @testset "highmask" begin
            @test highmask(UInt8, 0) == 0b00000000
            @test highmask(UInt8, 4) == 0b11110000
            @test highmask(UInt8, 8) == 0b11111111
            @test highmask(UInt16, 12) == 0xFFF0
            @test highmask(UInt32, 16) == 0xFFFF0000
        end
        
        @testset "rangemask" begin
            @test rangemask(UInt8, 2, 5) == 0b00111100
            @test rangemask(UInt8, 0, 7) == 0b11111111
            @test rangemask(UInt16, 4, 11) == 0x0FF0
            @test rangemask(UInt32, 8, 23) == 0x00FFFF00
        end
    end
    
    @testset "Parity and Gray Code" begin
        @testset "parity" begin
            @test parity(UInt8(0)) == 0
            @test parity(UInt8(1)) == 1
            @test parity(UInt8(0b11)) == 0
            @test parity(UInt8(0b111)) == 1
            @test parity(UInt8(0b11111111)) == 0
        end
        
        @testset "Gray code conversion" begin
            # Test round-trip conversion
            for i in UInt8(0):UInt8(255)
                gray = togray(i)
                back = fromgray(gray)
                @test back == i
            end
            
            # Specific values
            @test togray(UInt8(0)) == 0
            @test togray(UInt8(1)) == 1
            @test togray(UInt8(2)) == 3
            @test togray(UInt8(3)) == 2
            
            @test fromgray(UInt8(0)) == 0
            @test fromgray(UInt8(1)) == 1
            @test fromgray(UInt8(3)) == 2
            @test fromgray(UInt8(2)) == 3
        end
    end
    
    @testset "Bit Interleaving (Morton Encoding)" begin
        @testset "interleave2/deinterleave2" begin
            # Test round-trip
            for x in UInt32[0, 1, 15, 255, 1000, typemax(UInt16)]
                for y in UInt32[0, 1, 15, 255, 1000, typemax(UInt16)]
                    z = interleave2(x, y)
                    x2, y2 = deinterleave2(z)
                    @test x2 == x
                    @test y2 == y
                end
            end
            
            # Specific patterns
            @test interleave2(UInt32(0), UInt32(0)) == 0
            @test interleave2(UInt32(0xFF), UInt32(0)) == 0x5555
            @test interleave2(UInt32(0), UInt32(0xFF)) == 0xAAAA
        end
    end
    
    @testset "Sign Operations" begin
        @testset "signextend" begin
            @test signextend(Int8(0b00001111), 4) == Int8(-1)  # 0x0F with sign bit set
            @test signextend(Int16(0x007F), 7) == Int16(127)
            @test signextend(Int16(0x0080), 8) == Int16(-128)
            @test signextend(Int32(0x00000FFF), 12) == Int32(-1)
        end
        
        @testset "zerofill" begin
            @test zerofill(UInt8(0xFF), 4) == 0x0F
            @test zerofill(UInt16(0xFFFF), 8) == 0x00FF
            @test zerofill(UInt32(0xFFFFFFFF), 16) == 0x0000FFFF
        end
    end
    
    @testset "Bit Rotation" begin
        @testset "rotateleft" begin
            @test rotateleft(UInt8(0b10000001), 1) == 0b00000011
            @test rotateleft(UInt8(0b11110000), 4) == 0b00001111
            @test rotateleft(UInt16(0x1234), 8) == 0x3412
            @test rotateleft(UInt8(0xAB), 0) == 0xAB  # No rotation
            @test rotateleft(UInt8(0xAB), 8) == 0xAB  # Full rotation
        end
        
        @testset "rotateright" begin
            @test rotateright(UInt8(0b10000001), 1) == 0b11000000
            @test rotateright(UInt8(0b00001111), 4) == 0b11110000
            @test rotateright(UInt16(0x1234), 8) == 0x3412
            @test rotateright(UInt8(0xAB), 0) == 0xAB  # No rotation
            @test rotateright(UInt8(0xAB), 8) == 0xAB  # Full rotation
        end
        
        @testset "rotation round-trip" begin
            for val in [UInt8(0x5A), UInt16(0x1234), UInt32(0x12345678)]
                for n in 1:5
                    @test rotateleft(rotateright(val, n), n) == val
                    @test rotateright(rotateleft(val, n), n) == val
                end
            end
        end
    end
    
    @testset "Parallel Bit Operations" begin
        @testset "pdep" begin
            @test pdep(UInt8(0b0011), UInt8(0b10101010)) == 0b00001010
            @test pdep(UInt8(0xFF), UInt8(0b11110000)) == 0b11110000
            @test pdep(UInt8(0x00), UInt8(0xFF)) == 0x00
            @test pdep(UInt16(0b111), UInt16(0b1010100)) == 0b0010100
        end
        
        @testset "pext" begin
            @test pext(UInt8(0b10101010), UInt8(0b11110000)) == 0b00001010
            @test pext(UInt8(0xFF), UInt8(0b00001111)) == 0b00001111
            @test pext(UInt8(0x00), UInt8(0xFF)) == 0x00
            @test pext(UInt16(0b1010100), UInt16(0b1111100)) == 0b00010101
        end
        
        @testset "pdep/pext round-trip" begin
            for mask in [UInt8(0b10101010), UInt8(0b11110000), UInt8(0b00111100)]
                for val in UInt8(0):UInt8(15)  # Test with values that fit in mask
                    deposited = pdep(val, mask)
                    extracted = pext(deposited, mask)
                    @test extracted == val & lowmask(UInt8, count_ones(mask))
                end
            end
        end
    end
    
    @testset "Bit Manipulation Hacks" begin
        @testset "isolate_rightmost_set" begin
            @test isolate_rightmost_set(UInt8(0b10101000)) == 0b00001000
            @test isolate_rightmost_set(UInt8(0b11111111)) == 0b00000001
            @test isolate_rightmost_set(UInt8(0)) == 0
            @test isolate_rightmost_set(UInt16(0x8000)) == 0x8000
        end
        
        @testset "isolate_rightmost_unset" begin
            @test isolate_rightmost_unset(UInt8(0b10100111)) == 0b00001000
            @test isolate_rightmost_unset(UInt8(0b11111111)) == 0  # No unset bits
            @test isolate_rightmost_unset(UInt8(0)) == 1
        end
        
        @testset "turnoff_rightmost_set" begin
            @test turnoff_rightmost_set(UInt8(0b10101000)) == 0b10100000
            @test turnoff_rightmost_set(UInt8(0b11111111)) == 0b11111110
            @test turnoff_rightmost_set(UInt8(1)) == 0
            @test turnoff_rightmost_set(UInt8(0)) == 0
        end
        
        @testset "turnon_rightmost_unset" begin
            @test turnon_rightmost_unset(UInt8(0b10100111)) == 0b10101111
            @test turnon_rightmost_unset(UInt8(0)) == 1
            # Note: all bits set will overflow, which is expected behavior
        end
        
        @testset "isolate_trailing_zeros" begin
            @test isolate_trailing_zeros(UInt8(0b10101000)) == 0b00000111
            @test isolate_trailing_zeros(UInt8(0b11111111)) == 0
            @test isolate_trailing_zeros(UInt8(0b10000000)) == 0b01111111
        end
        
        @testset "propagate_rightmost_set" begin
            @test propagate_rightmost_set(UInt8(0b10101000)) == 0b10101111
            @test propagate_rightmost_set(UInt8(0b10000000)) == 0b11111111
            @test propagate_rightmost_set(UInt8(0)) == 0xFF  # Underflow wraps
        end
    end
    
    @testset "Swap Operations" begin
        @testset "swap_nibbles" begin
            @test swap_nibbles(UInt8(0x12)) == 0x21
            @test swap_nibbles(UInt8(0xAB)) == 0xBA
            @test swap_nibbles(UInt16(0x1234)) == 0x2143
        end
        
        @testset "swap_bytes" begin
            @test swap_bytes(UInt16(0x1234)) == 0x3412
            @test swap_bytes(UInt32(0x12345678)) == 0x34127856
            @test swap_bytes(UInt16(0xABCD)) == 0xCDAB
        end
        
        @testset "swap_words" begin
            @test swap_words(UInt32(0x12345678)) == 0x56781234
            @test swap_words(UInt64(0x1234567890ABCDEF)) == 0x567812349ABCDEF0
        end
    end
    
    @testset "Special Bit Checks" begin
        @testset "hassinglebite" begin
            @test hassinglebite(UInt8(0)) == false
            @test hassinglebite(UInt8(1)) == true
            @test hassinglebite(UInt8(2)) == true
            @test hassinglebite(UInt8(128)) == true
            @test hassinglebite(UInt8(3)) == false
            @test hassinglebite(UInt8(0xFF)) == false
        end
        
        @testset "haszerobye" begin
            @test haszerobye(UInt32(0x12345678)) == false
            @test haszerobye(UInt32(0x12340078)) == true
            @test haszerobye(UInt32(0x00345678)) == true
            @test haszerobye(UInt64(0x0102030405060708)) == false
            @test haszerobye(UInt64(0x0102030400060708)) == true
        end
    end
    
    @testset "Bit Permutations" begin
        @testset "nextpermutation/snoob" begin
            @test nextpermutation(UInt8(0b00101100)) == 0b00110001
            @test nextpermutation(UInt8(0b00111)) == 0b01011
            @test snoob(UInt8(0b01011)) == 0b01101
            
            # Check that popcount is preserved
            for val in [UInt8(0b1011), UInt8(0b10110), UInt16(0b110011)]
                next = nextpermutation(val)
                @test count_ones(val) == count_ones(next)
            end
        end
    end
    
    @testset "Edge Cases and Type Stability" begin
        @testset "Type preservation" begin
            # Ensure operations preserve input types
            @test typeof(setbit(Int8(0), 3)) == Int8
            @test typeof(setbit(UInt16(0), 3)) == UInt16
            @test typeof(rotateleft(Int32(1), 5)) == Int32
            @test typeof(reversebits(UInt64(1))) == UInt64
        end
        
        @testset "Boundary conditions" begin
            # Test with minimum and maximum values
            for T in [UInt8, UInt16, UInt32, UInt64]
                @test popcount(typemax(T)) == sizeof(T) * 8
                @test popcount(zero(T)) == 0
                @test leadingzeros(typemax(T)) == 0
                @test trailingzeros(zero(T)) == sizeof(T) * 8
            end
        end
        
        @testset "Negative numbers (signed types)" begin
            @test testbit(Int8(-1), 7) == true  # Sign bit
            @test popcount(Int8(-1)) == 8  # All bits set
            @test signextend(Int8(0x7F), 7) == Int8(127)
            @test signextend(Int8(0x80), 8) == Int8(-128)
        end
    end
    
    @testset "Advanced Bit Operations" begin
        @testset "bitselect/bitmerge" begin
            mask = UInt8(0b11110000)
            a = UInt8(0b10101010)
            b = UInt8(0b01010101)
            @test bitselect(mask, a, b) == 0b10100101
            @test bitmerge(mask, a, b) == 0b10100101
        end
        
        @testset "butterfly operations" begin
            x = UInt8(0b11001100)
            mask = UInt8(0b00001111)
            @test butterfly(x, mask, 4) == 0b00111100
            @test inverse_butterfly(butterfly(x, mask, 4), mask, 4) == x
        end
        
        @testset "bit_scan" begin
            @test bit_scan_forward(UInt8(0b00001000)) == 4
            @test bit_scan_forward(UInt8(0)) == 0
            @test bit_scan_reverse(UInt8(0b00001000)) == 4
            @test bit_scan_reverse(UInt16(0x8000)) == 16
        end
        
        @testset "bitpack/bitunpack" begin
            values = UInt8[0x3, 0x7, 0xF, 0x1]
            packed = bitpack(values, 4)
            unpacked = bitunpack(packed, 4, 4)
            @test unpacked == values
        end
        
        @testset "3D Morton encoding" begin
            x, y, z = UInt32(100), UInt32(200), UInt32(300)
            m = interleave3(x, y, z)
            x2, y2, z2 = deinterleave3(m)
            @test (x2, y2, z2) == (x, y, z)
        end
    end
    
    @testset "Arithmetic via Bits" begin
        @testset "add_via_bits" begin
            @test add_via_bits(UInt8(5), UInt8(3)) == 8
            @test add_via_bits(UInt16(1000), UInt16(2345)) == 3345
            @test add_via_bits(UInt32(0), UInt32(0)) == 0
        end
        
        @testset "subtract_via_bits" begin
            @test subtract_via_bits(UInt8(10), UInt8(3)) == 7
            @test subtract_via_bits(UInt16(5000), UInt16(2345)) == 2655
        end
        
        @testset "average without overflow" begin
            @test average_floor(UInt8(100), UInt8(200)) == 150
            @test average_ceil(UInt8(100), UInt8(201)) == 151
            # Test with values that would overflow if added first
            @test average_floor(typemax(UInt32), typemax(UInt32) - 2) == typemax(UInt32) - 1
        end
        
        @testset "branchless min/max" begin
            @test min_branchless(UInt8(10), UInt8(20)) == 10
            @test max_branchless(UInt8(10), UInt8(20)) == 20
            @test min_branchless(Int8(-5), Int8(5)) == -5
            @test max_branchless(Int8(-5), Int8(5)) == 5
        end
        
        @testset "sign operations" begin
            @test is_even(UInt8(4)) == true
            @test is_odd(UInt8(5)) == true
            @test sign_of(Int8(10)) == 1
            @test sign_of(Int8(-10)) == -1
            @test sign_of(Int8(0)) == 0
        end
    end
end

# Run benchmarks for performance-critical functions
@testset "Performance Benchmarks" begin
    using BenchmarkTools
    
    println("\n=== Performance Benchmarks ===")
    
    # Test data
    x8 = UInt8(0xAB)
    x16 = UInt16(0xABCD)
    x32 = UInt32(0x12345678)
    x64 = UInt64(0x123456789ABCDEF0)
    
    println("\nBasic operations (UInt32):")
    @btime setbit($x32, 15)
    @btime testbit($x32, 15)
    @btime popcount($x32)
    
    println("\nBit manipulation hacks (UInt32):")
    @btime isolate_rightmost_set($x32)
    @btime turnoff_rightmost_set($x32)
    @btime ispowerof2($x32)
    
    println("\nComplex operations:")
    @btime reversebits($x32)
    @btime interleave2($x32, $x32)
    @btime rotateleft($x32, 13)
    
    println("\nParallel bit operations:")
    mask = UInt32(0xF0F0F0F0)
    @btime pdep($x32, $mask)
    @btime pext($x32, $mask)
end

println("\n✅ All tests passed!")
