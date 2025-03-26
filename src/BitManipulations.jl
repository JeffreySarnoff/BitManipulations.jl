module BitManipulations

export Float, as, bitsof, bit_string,
       BitInteger,
       mask_lsbs, mask_msbs, maskm1sbs, maskbits,
       filterlo, filterhi, clear_ms1b, clear_ls1b, 
       leading_one, trailing_one

using Base: BitInteger

abstract type Float <: AbstractFloat end

include("type/support.jl")
include("type/conversions.jl")
include("operators.jl")

end  # BitManipulations

