module BitManipulations

export Float, as, bitsof, bit_string,
       BitInteger # from Base

using Base: BitInteger

abstract type Float <: AbstractFloat end

include("type/support.jl")
include("type/conversions.jl")

end  # BitManipulations

