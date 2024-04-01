module BitManipulations

export Float, as, bitsof,
       BitInteger, bitreverse, bitrotate # from Base

using Base: BitInteger, bitreverse, bitrotate

abstract type Float <: AbstractFloat end

include("type/bitsof.jl")
include("type/conversions.jl")

end  # BitManipulations

