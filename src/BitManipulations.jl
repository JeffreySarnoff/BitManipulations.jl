module BitManipulations

export Float, as, bitsof, bit_string,
       BitInteger,
       mask_lsbs, mask_msbs, maskm1sbs, maskbits,
       filterlo, filterhi, clear_ms1b, clear_ls1b, 
       leading_one, trailing_one

export
    # Basic bit operations
    setbit, clearbit, togglebit, testbit, setbits, clearbits, andnot,
    # Bit counting
    popcount, leadingzeros, trailingzeros, countones, countzeros,
    leadingones, trailingones, bitwidth,
    # Bit extraction and manipulation
    extractbits, insertbits, reversebits, swapbits, copybits, bextr,
    # Power of 2 operations
    ispowerof2, nextpowerof2, prevpowerof2, roundtopowerof2,
    ceilpowerof2, floorpowerof2,
    # Bit masks
    lowmask, highmask, rangemask, singlemask, bzhi,
    # Parity and gray code
    parity, togray, fromgray,
    # Bit interleaving
    interleave2, deinterleave2, morton2d, morton2d_inverse,
    interleave3, deinterleave3, morton3d, morton3d_inverse,
    # Sign operations
    signextend, zerofill, abs_diff,
    # Bit rotation
    rotateleft, rotateright, rorx,
    # Shift extensions
    shlx, shrx, sarx,
    # Bit spreading and gathering
    pdep, pext,
    # Miscellaneous hacks
    isolate_rightmost_set, isolate_rightmost_unset,
    turnoff_rightmost_set, turnon_rightmost_unset,
    isolate_trailing_zeros, propagate_rightmost_set, blsmsk,
    swap_nibbles, swap_bytes, swap_words,
    hassinglebite, haszerobye, allsame, byteswap,
    nextpermutation, snoob, prevpermutation,
    # Advanced operations
    bitselect, bitmerge, bitpack, bitunpack,
    compress_bits, expand_bits,
    butterfly, inverse_butterfly,
    bit_scan_forward, bit_scan_reverse, rank1, select1,
    # Arithmetic via bits
    add_via_bits, subtract_via_bits, negate_via_bits,
    average_floor, average_ceil,
    is_even, is_odd, sign_of, mulx,
    min_branchless, max_branchless

using Base: BitInteger

abstract type Float <: AbstractFloat end

include("type/support.jl")
include("type/conversions.jl")
include("operators.jl")

end  # BitManipulations

