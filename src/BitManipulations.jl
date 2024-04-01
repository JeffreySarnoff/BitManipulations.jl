module BitManipulations

export Float, as, bitsof

using Base: BitInteger

abstract type Float <: AbstractFloat end

include("type/bitsof.jl")
include("type/conversions.jl")

end  # BitManipulations

