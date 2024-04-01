module BitManipulations

export Float, as, bitsof

abstract type Float <: AbstractFloat end

bitsof(::Type{T}) where {T <: BitInteger} = sizeof(T) << 3
bitsof(x::T) where {T <: BitInteger} = sizeof(T) << 3

include("type/conversions.jl")

end  # BitManipulations

