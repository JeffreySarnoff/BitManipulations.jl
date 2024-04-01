module BitManipulations

export Float, as, bitsof, bit_string,
       BitInteger, bitreverse, bitrotate, bitstring # from Base

using Base: BitInteger, bitreverse, bitrotate, bitstring

abstract type Float <: AbstractFloat end

include("type/support.jl")
include("type/conversions.jl")

end  # BitManipulations

