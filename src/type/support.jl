for (T,N) in ((UInt8, 8), (UInt16, 16), (UInt32, 32), (UInt64, 64), (UInt128, 128),
              (Int8, 8), (Int16, 16), (Int32, 32), (Int64, 64), (Int128, 128))
    @eval bitsof(::Type{$T}) = Int16($N)
    @eval bitsof(x::$T) = Int16($N)
end

function bit_string(x::T) where {T<:BitInteger}
    s = Base.bitstring(x)
    string("0b", join(s[i:i+3] for i=1:4:bitsof(T)))
end

@inline function signsbit(x::Int16, y::Int16)::Int16
    ((x ⊻ y) >> 15) << 15
end

@inline function signsdiffer(x::Int16, y::Int16)::Bool
    (x ⊻ y) > zero(Int16)
end

# works
function minmax(x::Int16, y::Int16)::Tuple{Int16,Int16}
    xmy = x - y
    adj = xmy & (xmy >> 15)
    (y + adj, x - adj)
end

# https://github.com/randomascii/blogstuff/blob/main/FloatingPoint/CompareAsInt/CompareAsInt.cpp

function minmaxabs(xx::Float16, yy::Float16)::Tuple{Float16,Float16}
    x = reinterpret(Int16, abs(xx))
    y = reinterpret(Int16, abs(yy))
    xmy = x - y
    adj = xmy & (xmy >> 15)
    (reinterpret(Float16, y + adj), reinterpret(Float16, x - adj))
end

# swaps sign
function minmaxmag(xx::Float16, yy::Float16)::Tuple{Float16,Float16}
    x = reinterpret(Int16, abs(xx))
    y = reinterpret(Int16, abs(yy))
    xmy = x - y
    adj = xmy & (xmy >> 15)
    copysign(reinterpret(Float16, y + adj), yy), copysign(reinterpret(Float16, x - adj), xx)
end

# swaps sign
for (I,F,N) in ((:Int16, :Float16, 15%Int16), (:Int32, :Float32, 31%Int32), (:Int64, :Float64, 63%Int64))
  @eval begin
    function minmaxmag(xx::$F, yy::$F)
        x = reinterpret($I, abs(xx))
        y = reinterpret($I, abs(yy))
        xmy = x - y
        adj = xmy & (xmy >> $N)
        copysign(reinterpret($F, y + adj), yy), copysign(reinterpret($F, x - adj), xx)
    end
  end
end