for (T,N) in ((UInt8, 8), (UInt16, 16), (UInt32, 32), (UInt64, 64), (UInt128, 128),
              (Int8, 8), (Int16, 16), (Int32, 32), (Int64, 64), (Int128, 128))
    @eval bitsof(::Type{$T}) = Int16($N)
    @eval bitsof(x::$T) = Int16($N)
end

function bit_string(x::T) where {T<:BitInteger}
    s = Base.bitstring(x)
    string("0b", join(s[i:i+3] for i=1:4:bitsof(T)))
end

