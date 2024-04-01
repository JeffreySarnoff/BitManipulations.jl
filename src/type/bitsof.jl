for (T,N) in ((UInt8, 8), (UInt16, 16), (UInt32, 32), (UInt64, 64), (UInt128, 128),
              (Int8, 8), (Int16, 16), (Int32, 32), (Int64, 64), (Int128, 128),
              (Float16, 16), (Float32, 32), (Float64, 64))
    @eval bitsof(::Type{$T}) = $N
end