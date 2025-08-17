# BitOps.jl Documentation

A comprehensive Julia module for bit manipulation operations and bit-level hacks, optimized for Julia v1.12+ with emphasis on performance, correctness, and completeness.

## Table of Contents

- [Installation](#installation)
- [Quick Start](#quick-start)
- [API Reference](#api-reference)
  - [Basic Bit Operations](#basic-bit-operations)
  - [Bit Counting](#bit-counting)
  - [Bit Extraction and Manipulation](#bit-extraction-and-manipulation)
  - [Power of 2 Operations](#power-of-2-operations)
  - [Bit Masks](#bit-masks)
  - [Parity and Gray Code](#parity-and-gray-code)
  - [Bit Interleaving (Morton Encoding)](#bit-interleaving-morton-encoding)
  - [Sign Operations](#sign-operations)
  - [Bit Rotation](#bit-rotation)
  - [Parallel Bit Operations](#parallel-bit-operations)
  - [Bit Manipulation Hacks](#bit-manipulation-hacks)
  - [Swap Operations](#swap-operations)
  - [Special Bit Checks](#special-bit-checks)
  - [Bit Permutations](#bit-permutations)
  - [Advanced Bit Operations](#advanced-bit-operations)
  - [Arithmetic via Bits](#arithmetic-via-bits)
- [Performance Notes](#performance-notes)
- [Common Use Cases](#common-use-cases)
- [Examples](#examples)

## Installation

```julia
include("BitOps.jl")
using .BitOps
```

## Quick Start

```julia
using .BitOps

# Basic operations
x = UInt32(0x12345678)
y = setbit(x, 0)           # Set bit 0
z = clearbit(y, 31)        # Clear bit 31
count = popcount(x)        # Count set bits

# Power of 2 operations
ispowerof2(16)             # true
nextpowerof2(100)          # 128

# Bit manipulation hacks
isolate_rightmost_set(0b10110000)  # 0b00010000

# Morton encoding for spatial indexing
morton = interleave2(UInt32(123), UInt32(456))
x_coord, y_coord = deinterleave2(morton)
```

## API Reference

### Basic Bit Operations

#### `setbit(x::BitInteger, n::Integer)`
Set the nth bit (0-indexed) to 1.

```julia
setbit(UInt8(0b00000000), 3)  # Returns: 0b00001000
```

#### `setbits(x::BitInteger, positions)`
Set multiple bits at once. Positions can be any iterable.

```julia
setbits(UInt8(0), [0, 2, 4, 6])  # Returns: 0b01010101
```

#### `clearbit(x::BitInteger, n::Integer)`
Clear the nth bit (0-indexed) to 0.

```julia
clearbit(UInt8(0b11111111), 3)  # Returns: 0b11110111
```

#### `clearbits(x::BitInteger, positions)`
Clear multiple bits at once.

```julia
clearbits(UInt8(0xFF), [0, 2, 4, 6])  # Returns: 0b10101010
```

#### `togglebit(x::BitInteger, n::Integer)`
Toggle the nth bit (0-indexed).

```julia
togglebit(UInt8(0b10101010), 0)  # Returns: 0b10101011
```

#### `testbit(x::BitInteger, n::Integer)`
Test if the nth bit (0-indexed) is set.

```julia
testbit(UInt8(0b10101010), 1)  # Returns: true
testbit(UInt8(0b10101010), 0)  # Returns: false
```

### Bit Counting

#### `popcount(x::BitInteger)` / `countones(x::BitInteger)`
Count the number of set bits (population count).

```julia
popcount(UInt8(0b10110111))  # Returns: 6
```

#### `countzeros(x::BitInteger)`
Count the number of zero bits.

```julia
countzeros(UInt8(0b10110111))  # Returns: 2
```

#### `leadingzeros(x::BitInteger)`
Count leading zero bits.

```julia
leadingzeros(UInt8(0b00001111))  # Returns: 4
```

#### `trailingzeros(x::BitInteger)`
Count trailing zero bits.

```julia
trailingzeros(UInt8(0b11110000))  # Returns: 4
```

#### `leadingones(x::BitInteger)`
Count leading one bits.

```julia
leadingones(UInt8(0b11110000))  # Returns: 4
```

#### `trailingones(x::BitInteger)`
Count trailing one bits.

```julia
trailingones(UInt8(0b10001111))  # Returns: 4
```

### Bit Extraction and Manipulation

#### `extractbits(x::BitInteger, pos::Integer, len::Integer)`
Extract `len` bits starting at position `pos` (0-indexed).

```julia
extractbits(UInt16(0x1234), 4, 8)  # Returns: 0x23
```

#### `insertbits(x::BitInteger, y::BitInteger, pos::Integer, len::Integer)`
Insert the lower `len` bits of `y` into `x` starting at position `pos`.

```julia
insertbits(UInt8(0b11110000), UInt8(0b1010), 2, 4)  # Returns: 0b11101000
```

#### `copybits(dst::BitInteger, src::BitInteger, dst_pos, src_pos, len)`
Copy `len` bits from `src` at `src_pos` to `dst` at `dst_pos`.

```julia
copybits(UInt8(0xFF), UInt8(0x00), 4, 0, 4)  # Returns: 0xF0
```

#### `reversebits(x::BitInteger)`
Reverse all bits in x. Uses hardware intrinsics when available.

```julia
reversebits(UInt8(0b10110000))  # Returns: 0b00001101
```

#### `swapbits(x::BitInteger, i::Integer, j::Integer)`
Swap bits at positions i and j (0-indexed) using XOR trick.

```julia
swapbits(UInt8(0b10000001), 0, 7)  # Returns: 0b00000011
```

### Power of 2 Operations

#### `ispowerof2(x::BitInteger)`
Check if x is a power of 2 using Brian Kernighan's algorithm.

```julia
ispowerof2(64)   # Returns: true
ispowerof2(100)  # Returns: false
```

#### `nextpowerof2(x::BitInteger)` / `ceilpowerof2(x::BitInteger)`
Find the next power of 2 greater than or equal to x.

```julia
nextpowerof2(100)  # Returns: 128
nextpowerof2(128)  # Returns: 128
```

#### `prevpowerof2(x::BitInteger)` / `floorpowerof2(x::BitInteger)`
Find the previous power of 2 less than or equal to x.

```julia
prevpowerof2(100)  # Returns: 64
prevpowerof2(128)  # Returns: 128
```

#### `roundtopowerof2(x::BitInteger)`
Round x to the nearest power of 2.

```julia
roundtopowerof2(100)  # Returns: 128 (closer to 128 than 64)
roundtopowerof2(50)   # Returns: 64  (closer to 64 than 32)
```

### Bit Masks

#### `lowmask(::Type{T}, n::Integer)`
Create a mask with the lower n bits set.

```julia
lowmask(UInt8, 4)   # Returns: 0b00001111
lowmask(UInt16, 8)  # Returns: 0x00FF
```

#### `highmask(::Type{T}, n::Integer)`
Create a mask with the upper n bits set.

```julia
highmask(UInt8, 4)   # Returns: 0b11110000
highmask(UInt16, 8)  # Returns: 0xFF00
```

#### `rangemask(::Type{T}, low::Integer, high::Integer)`
Create a mask with bits from low to high (inclusive, 0-indexed) set.

```julia
rangemask(UInt8, 2, 5)    # Returns: 0b00111100
rangemask(UInt16, 4, 11)  # Returns: 0x0FF0
```

#### `singlemask(::Type{T}, n::Integer)`
Create a mask with only the nth bit set.

```julia
singlemask(UInt8, 3)   # Returns: 0b00001000
singlemask(UInt32, 31) # Returns: 0x80000000
```

### Parity and Gray Code

#### `parity(x::BitInteger)`
Calculate the parity (XOR of all bits) efficiently using parallel reduction.

```julia
parity(UInt8(0b11010110))  # Returns: 1 (odd number of 1s)
parity(UInt8(0b11000110))  # Returns: 0 (even number of 1s)
```

#### `togray(x::BitInteger)`
Convert binary to Gray code.

```julia
togray(UInt8(5))  # Binary: 0b101, Gray: 0b111 (returns 7)
```

#### `fromgray(x::BitInteger)`
Convert Gray code to binary using parallel prefix XOR.

```julia
fromgray(UInt8(7))  # Gray: 0b111, Binary: 0b101 (returns 5)
```

### Bit Interleaving (Morton Encoding)

#### `interleave2(x, y)` / `morton2d(x, y)`
Interleave bits of x and y for 2D Morton encoding. Used in spatial indexing.

```julia
z = interleave2(UInt32(3), UInt32(5))  # Returns Morton code
# x = 0b011, y = 0b101 → z = 0b100111
```

#### `deinterleave2(z)` / `morton2d_inverse(z)`
Deinterleave bits (inverse Morton encoding for 2D). Returns (x, y) tuple.

```julia
x, y = deinterleave2(UInt32(39))  # Returns: (3, 5)
```

#### `interleave3(x, y, z)` / `morton3d(x, y, z)`
Interleave bits for 3D Morton encoding.

```julia
morton = interleave3(UInt32(10), UInt32(20), UInt32(30))
```

#### `deinterleave3(m)` / `morton3d_inverse(m)`
Deinterleave bits for 3D. Returns (x, y, z) tuple.

```julia
x, y, z = deinterleave3(morton)  # Returns original coordinates
```

### Sign Operations

#### `signextend(x::SignedBitInteger, nbits::Integer)`
Sign-extend x from nbits to full width of type.

```julia
signextend(Int16(0x007F), 7)  # Returns: 127
signextend(Int16(0x0080), 8)  # Returns: -128
```

#### `zerofill(x::BitInteger, nbits::Integer)`
Zero-fill upper bits beyond nbits.

```julia
zerofill(UInt16(0xFFFF), 8)  # Returns: 0x00FF
```

#### `abs_diff(a::BitInteger, b::BitInteger)`
Compute absolute difference without branching.

```julia
abs_diff(UInt8(10), UInt8(3))   # Returns: 7
abs_diff(UInt8(3), UInt8(10))   # Returns: 7
```

### Bit Rotation

#### `rotateleft(x::BitInteger, n::Integer)`
Rotate bits left by n positions.

```julia
rotateleft(UInt8(0b10000001), 1)  # Returns: 0b00000011
rotateleft(UInt16(0x1234), 8)     # Returns: 0x3412
```

#### `rotateright(x::BitInteger, n::Integer)`
Rotate bits right by n positions.

```julia
rotateright(UInt8(0b10000001), 1)  # Returns: 0b11000000
rotateright(UInt16(0x1234), 8)     # Returns: 0x3412
```

### Parallel Bit Operations

#### `pdep(x::BitInteger, mask::BitInteger)` / `expand_bits`
Parallel bit deposit - scatter bits from x to positions marked in mask.

```julia
pdep(UInt8(0b0011), UInt8(0b10101010))  # Returns: 0b00001010
# Deposits bits 0,1 of x into positions 1,3 (where mask has 1s)
```

#### `pext(x::BitInteger, mask::BitInteger)` / `compress_bits`
Parallel bit extract - gather bits from positions marked in mask.

```julia
pext(UInt8(0b10101010), UInt8(0b11110000))  # Returns: 0b00001010
# Extracts bits from positions 4-7 and compresses them
```

### Bit Manipulation Hacks

#### `isolate_rightmost_set(x::BitInteger)`
Isolate the rightmost set bit.

```julia
isolate_rightmost_set(UInt8(0b10101000))  # Returns: 0b00001000
```

#### `isolate_rightmost_unset(x::BitInteger)`
Isolate the rightmost unset bit.

```julia
isolate_rightmost_unset(UInt8(0b10100111))  # Returns: 0b00001000
```

#### `turnoff_rightmost_set(x::BitInteger)`
Turn off the rightmost set bit (Brian Kernighan's algorithm).

```julia
turnoff_rightmost_set(UInt8(0b10101000))  # Returns: 0b10100000
```

#### `turnon_rightmost_unset(x::BitInteger)`
Turn on the rightmost unset bit.

```julia
turnon_rightmost_unset(UInt8(0b10100111))  # Returns: 0b10101111
```

#### `isolate_trailing_zeros(x::BitInteger)`
Create a mask of trailing zeros.

```julia
isolate_trailing_zeros(UInt8(0b10101000))  # Returns: 0b00000111
```

#### `propagate_rightmost_set(x::BitInteger)`
Propagate the rightmost set bit to the right.

```julia
propagate_rightmost_set(UInt8(0b10101000))  # Returns: 0b10101111
```

### Swap Operations

#### `swap_nibbles(x::BitInteger)`
Swap adjacent nibbles (4-bit groups).

```julia
swap_nibbles(UInt8(0x12))    # Returns: 0x21
swap_nibbles(UInt16(0x1234)) # Returns: 0x2143
```

#### `swap_bytes(x::BitInteger)`
Swap adjacent bytes.

```julia
swap_bytes(UInt16(0x1234))      # Returns: 0x3412
swap_bytes(UInt32(0x12345678))  # Returns: 0x34127856
```

#### `swap_words(x::BitInteger)`
Swap adjacent 16-bit words.

```julia
swap_words(UInt32(0x12345678))  # Returns: 0x56781234
```

### Special Bit Checks

#### `hassinglebite(x::BitInteger)`
Check if x has exactly one bit set (is a power of 2).

```julia
hassinglebite(UInt8(16))  # Returns: true
hassinglebite(UInt8(15))  # Returns: false
```

#### `haszerobye(x::BitInteger)`
Check if x contains a zero byte (useful for string processing). Uses SWAR technique.

```julia
haszerobye(UInt32(0x12345678))  # Returns: false
haszerobye(UInt32(0x12340078))  # Returns: true (third byte is 0)
```

#### `allsame(x::BitInteger)`
Check if all bytes in x are the same.

```julia
allsame(UInt32(0x42424242))  # Returns: true
allsame(UInt32(0x42424241))  # Returns: false
```

### Bit Permutations

#### `nextpermutation(x::BitInteger)` / `snoob(x::BitInteger)`
Generate the next bit permutation with the same number of set bits (Gosper's hack).

```julia
nextpermutation(UInt8(0b00101100))  # Returns: 0b00110001
# Same number of 1s, next lexicographic order
```

#### `prevpermutation(x::BitInteger)`
Generate the previous bit permutation with the same number of set bits.

```julia
prevpermutation(UInt8(0b00110001))  # Returns: 0b00101100
```

### Advanced Bit Operations

#### `bitselect(mask, a, b)`
Select bits from `a` where mask is 1, from `b` where mask is 0.

```julia
bitselect(UInt8(0b11110000), UInt8(0b10101010), UInt8(0b01010101))
# Returns: 0b10100101
```

#### `bitmerge(mask, a, b)`
Merge bits from a and b according to mask using XOR trick.

```julia
bitmerge(UInt8(0b11110000), UInt8(0b10101010), UInt8(0b01010101))
# Returns: 0b10100101
```

#### `butterfly(x, mask, shift)`
Butterfly operation - swap bits at distance `shift` controlled by mask.

```julia
butterfly(UInt8(0b11001100), UInt8(0b00001111), 4)  # Returns: 0b00111100
```

#### `bit_scan_forward(x::BitInteger)`
Find the position of the least significant set bit (1-indexed). Returns 0 if x is zero.

```julia
bit_scan_forward(UInt8(0b00001000))  # Returns: 4
bit_scan_forward(UInt8(0))           # Returns: 0
```

#### `bit_scan_reverse(x::BitInteger)`
Find the position of the most significant set bit (1-indexed). Returns 0 if x is zero.

```julia
bit_scan_reverse(UInt8(0b00001000))  # Returns: 4
bit_scan_reverse(UInt16(0x8000))     # Returns: 16
```

#### `bitpack(values::Vector, bits_per_value::Integer)`
Pack multiple values into a single integer.

```julia
bitpack(UInt8[0x3, 0x7, 0xF, 0x1], 4)  # Returns packed value
```

#### `bitunpack(x::BitInteger, bits_per_value::Integer, count::Integer)`
Unpack multiple values from a single integer.

```julia
bitunpack(packed_value, 4, 4)  # Returns: [0x3, 0x7, 0xF, 0x1]
```

### Arithmetic via Bits

#### `add_via_bits(a::BitInteger, b::BitInteger)`
Add two numbers using only bitwise operations.

```julia
add_via_bits(UInt8(5), UInt8(3))  # Returns: 8
```

#### `subtract_via_bits(a::BitInteger, b::BitInteger)`
Subtract b from a using only bitwise operations.

```julia
subtract_via_bits(UInt8(10), UInt8(3))  # Returns: 7
```

#### `negate_via_bits(x::BitInteger)`
Negate x using two's complement.

```julia
negate_via_bits(Int8(5))  # Returns: -5
```

#### `average_floor(a::BitInteger, b::BitInteger)`
Compute floor((a + b) / 2) without overflow.

```julia
average_floor(UInt8(100), UInt8(200))  # Returns: 150
# Works even with values near typemax
```

#### `average_ceil(a::BitInteger, b::BitInteger)`
Compute ceil((a + b) / 2) without overflow.

```julia
average_ceil(UInt8(100), UInt8(201))  # Returns: 151
```

#### `is_even(x::BitInteger)` / `is_odd(x::BitInteger)`
Check parity using bit manipulation.

```julia
is_even(4)  # Returns: true
is_odd(5)   # Returns: true
```

#### `sign_of(x::SignedBitInteger)`
Extract sign of x: -1, 0, or 1 using bit manipulation (branchless).

```julia
sign_of(Int8(10))   # Returns: 1
sign_of(Int8(-10))  # Returns: -1
sign_of(Int8(0))    # Returns: 0
```

#### `min_branchless(a::BitInteger, b::BitInteger)`
Compute minimum without branching.

```julia
min_branchless(UInt8(10), UInt8(20))  # Returns: 10
```

#### `max_branchless(a::BitInteger, b::BitInteger)`
Compute maximum without branching.

```julia
max_branchless(UInt8(10), UInt8(20))  # Returns: 20
```

## Performance Notes

### Optimization Techniques Used

1. **Hardware Intrinsics**: Functions like `reversebits` use CPU intrinsics when available
2. **Branchless Algorithms**: Most functions avoid conditional branches for better pipelining
3. **Bit-Parallel Operations**: Operations process multiple bits simultaneously
4. **Inline Hints**: Small functions marked with `@inline` for better optimization
5. **Type Stability**: All functions maintain type stability for optimal compilation

### Performance Characteristics

- **O(1) Operations**: Most bit operations are constant time
- **Cache-Friendly**: Bit operations work within single machine words
- **SIMD-Compatible**: Many operations can be vectorized by the compiler
- **Branch-Free**: Reduces pipeline stalls and improves predictability

## Common Use Cases

### 1. Bit Flags and Masks
```julia
# Managing feature flags
FLAGS_READ = 0x01
FLAGS_WRITE = 0x02
FLAGS_EXECUTE = 0x04

permissions = setbit(UInt8(0), 0)  # Set READ
permissions = setbit(permissions, 1)  # Set WRITE
can_execute = testbit(permissions, 2)  # Check EXECUTE
```

### 2. Spatial Indexing (Morton Codes)
```julia
# Convert 2D coordinates to Morton code for spatial databases
x, y = 1234, 5678
morton_code = morton2d(UInt32(x), UInt32(y))

# Query spatial region
x_recovered, y_recovered = morton2d_inverse(morton_code)
```

### 3. Data Compression
```julia
# Pack multiple small values into one integer
values = UInt8[3, 7, 15, 1]  # 4-bit values
packed = bitpack(values, 4)  # Pack into single integer

# Unpack when needed
unpacked = bitunpack(packed, 4, 4)
```

### 4. Cryptography and Hashing
```julia
# Bit diffusion for hash functions
function diffuse(x::UInt32)
    x = rotateleft(x, 13)
    x = x ⊻ (x >> 17)
    x = rotateleft(x, 5)
    return x
end
```

### 5. Graphics Programming
```julia
# Extract RGB components from packed color
color = UInt32(0xFF8040)  # RGB color
red   = extractbits(color, 16, 8)  # 0xFF
green = extractbits(color, 8, 8)   # 0x80
blue  = extractbits(color, 0, 8)   # 0x40
```

### 6. Network Programming
```julia
# Check for specific packet flags
packet_flags = UInt8(0b10110000)
syn_flag = testbit(packet_flags, 1)
ack_flag = testbit(packet_flags, 4)
```

### 7. Embedded Systems
```julia
# Configure hardware registers
register = UInt32(0)
register = insertbits(register, UInt32(0b1010), 4, 4)  # Set bits 4-7
register = clearbit(register, 15)  # Clear interrupt flag
```

## Examples

### Example 1: Counting Trailing Zeros Efficiently
```julia
# Find the position of the first set bit
function find_first_set(x::UInt32)
    x == 0 && return nothing
    return trailing_zeros(x) + 1
end

find_first_set(0x00000100)  # Returns: 9
```

### Example 2: Power of 2 Allocation
```julia
# Allocate memory in power-of-2 sizes
function allocate_buffer(needed_size::Int)
    actual_size = nextpowerof2(needed_size)
    buffer = Vector{UInt8}(undef, actual_size)
    return buffer
end
```

### Example 3: Bit Field Extraction
```julia
# Parse a network packet header
struct PacketHeader
    version::UInt8
    header_length::UInt8
    total_length::UInt16
end

function parse_header(data::UInt32)
    version = extractbits(data, 28, 4)
    header_length = extractbits(data, 24, 4)
    total_length = extractbits(data, 0, 16)
    return PacketHeader(version, header_length, total_length)
end
```

### Example 4: Generate All Subsets with k Bits
```julia
# Generate all n-bit numbers with exactly k bits set
function generate_combinations(n::Int, k::Int)
    combinations = UInt32[]
    x = (UInt32(1) << k) - 1  # First number with k bits set
    max_val = UInt32(1) << n
    
    while x < max_val
        push!(combinations, x)
        x = nextpermutation(x)
    end
    return combinations
end

generate_combinations(5, 3)  # All 5-bit numbers with 3 bits set
```

### Example 5: Fast Parity Checking
```julia
# Check if number of set bits is odd or even
function has_odd_parity(x::UInt64)
    return parity(x) == 1
end

# Use for error detection
function add_parity_bit(data::UInt32)
    p = parity(data)
    return UInt64(data) | (UInt64(p) << 32)
end
```

### Example 6: Branchless Absolute Value
```julia
# Compute absolute value without branches
function abs_branchless(x::Int32)
    mask = x >> 31  # Sign bit extended
    return (x ⊻ mask) - mask
end
```

### Example 7: Round to Multiple
```julia
# Round up to nearest multiple of alignment (power of 2)
function align_to(value::UInt32, alignment::UInt32)
    @assert ispowerof2(alignment) "Alignment must be power of 2"
    mask = alignment - 1
    return (value + mask) & ~mask
end

align_to(UInt32(123), UInt32(16))  # Returns: 128
```

## Advanced Topics

### Bit Manipulation in SIMD Operations
Many BitOps functions can be vectorized for SIMD operations:

```julia
# Process multiple values in parallel
function popcount_array(arr::Vector{UInt32})
    return map(popcount, arr)  # Compiler may vectorize
end
```

### Cache-Oblivious Algorithms
Morton codes enable cache-efficient traversal of multidimensional data:

```julia
# Z-order curve traversal for better cache locality
function process_2d_array_morton(arr::Matrix, max_coord)
    for morton in 0:(max_coord^2 - 1)
        x, y = morton2d_inverse(UInt32(morton))
        if x < size(arr, 1) && y < size(arr, 2)
            # Process arr[x+1, y+1] with better cache locality
        end
    end
end
```

### Bit-Level Parallelism
Use bit operations to process multiple small values simultaneously:

```julia
# Check multiple conditions at once
function check_all_conditions(flags::UInt8, required::UInt8)
    return (flags & required) == required
end
```

## Best Practices

1. **Use Appropriate Types**: Choose the smallest type that fits your data
2. **Document Bit Layouts**: Always document what each bit represents
3. **Test Edge Cases**: Test with 0, all 1s, and boundary values
4. **Profile Performance**: Measure actual performance improvements
5. **Consider Endianness**: Be aware of byte order in multi-byte operations
6. **Validate Inputs**: Check for overflow and invalid bit positions

## References

- [Bit Twiddling Hacks](https://graphics.stanford.edu/~seander/bithacks.html)
- [Hacker's Delight](https://en.wikipedia.org/wiki/Hacker%27s_Delight) by Henry S. Warren Jr.
- [BMI Instructions](https://en.wikipedia.org/wiki/X86_Bit_manipulation_instruction_set)
- [Morton Codes](https://en.wikipedia.org/wiki/Z-order_curve)
- [SWAR Techniques](https://en.wikipedia.org/wiki/SWAR)

## License

MIT License - See LICENSE file for details

## Contributing

Contributions are welcome! Please ensure:
- All functions have comprehensive tests
- Documentation is updated for new functions
- Performance benchmarks are included
- Code follows Julia style guidelines