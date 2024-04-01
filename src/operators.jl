masklo(n::T) where {T<:Integer} = ((one(U) << n) - one(U))

masklo(::Type{T1}, n::T2) where {T1<:Integer, T2<:Integer} =
    ((one(T1)) << n) - one(T1)) % T1

maskhi(n::T) where {T<:Integer} = masklo(n) << (8sizeof(T)-n)

maskhi(::Type{T1}, n::T2) where {T1<:Integer, T2<:Integer} = 
    masklo(n) << (8sizeof(T)-n)

filterlo(n::T) where {T<:Integer} = maskhi(8sizeof(T) - n)
filterlo(::Type{T1}, n::T2) where {T1<:Integer, T2<:Integer} =
    maskhi(8sizeof(T1) - n)

filterhi(n::T) where {T<:Integer} = masklo(8sizeof(T) - n)
filterhi(::Type{T1}, n::T2) where {T1<:Integer, T2<:Integer} =
    masklo(8sizeof(T1) - n)

ror(x::UInt64, k::Integer) = (x >>> (0x3f &  k)) | (x << (0x3f & -k))
rol(x::UInt64, k::Integer) = (x >>> (0x3f & -k)) | (x << (0x3f &  k))

ror(x::UInt32, k::Integer) = (x >>> (0x1f &  k)) | (x << (0x1f & -k))
rol(x::UInt32, k::Integer) = (x >>> (0x1f & -k)) | (x << (0x1f &  k))

ror(x::UInt16, k::Integer) = (x >>> (0x0f &  k)) | (x << (0x0f & -k))
rol(x::UInt16, k::Integer) = (x >>> (0x0f & -k)) | (x << (0x0f &  k))

ror(x::UInt8, k::Integer)  = (x >>> (0x07 &  k)) | (x << (0x07 & -k))
rol(x::UInt8, k::Integer)  = (x >>> (0x07 & -k)) | (x << (0x07 &  k))

clear_left_one(x::T) where {T<:BitInteger} = x & ~(leading_one(x))
clear_right_one(x::T) where {T<:BitInteger} = x & ~(trailing_one(x))

leading_one(x::T) where {T<:BitInteger} = 
    one(T) << ((bitsof(T) - one(Int16)) - Base.ctlz_int(x))

trailing_one(x::T) where {T<:BitInteger} = 
    one(T) << ((bitsof(T) - one(Int16)) -  Base.cttz_int(x))

function bitreverse(x::UInt8)
    x = ((x >> 1) & 0x55) | ((x & 0x55) << 1)
    x = ((x >> 2) & 0x33) | ((x & 0x33) << 2)
    x = (x >> 4) | (x << 4)
    x
end

function bitreverse(x::UInt16)
    x = ((x >> 1) & 0x5555) | ((x & 0x5555) << 1)
    x = ((x >> 2) & 0x3333) | ((x & 0x3333) << 2)
    x = ((x >> 4) & 0x0F0F) | ((x & 0x0F0F) << 4)
    x = (x >> 8) | (x << 8)
    x
end

function bitreverse(x::UInt32)
    x = ((x >> 1) & 0x55555555) | ((x & 0x55555555) << 1)
    x = ((x >> 2) & 0x33333333) | ((x & 0x33333333) << 2)
    x = ((x >> 4) & 0x0F0F0F0F) | ((x & 0x0F0F0F0F) << 4)
    x = ((x >> 8) & 0x00FF00FF) | ((x & 0x00FF00FF) << 8)
    x = (x >> 16)  | (x << 16)
    x
end

function bitreverse(x::UInt64)
    x = ((x >> 1) & 0x555555555555555) | ((x & 0x555555555555555) << 1)
    x = ((x >> 2) & 0x3333333333333333) | ((x & 0x3333333333333333) << 2)
    x = ((x >> 4) & 0x0F0F0F0F0F0F0F0F) | ((x & 0x0F0F0F0F0F0F0F0F) << 4)
    x = ((x >> 8) & 0x00FF00FF00FF00FF) | ((x & 0x00FF00FF00FF00FF) << 8)
    x = ((x >> 16) & 0x0000FFFF0000FFFF) | ((x & 0x0000FFFF0000FFFF) << 16)
    x = (x >> 32)  | (x << 32)
    x
end

function bitreverse(x::UInt128)
    x = ((x >> 1) & 0x555555555555555555555555555555) | ((x & 0x555555555555555555555555555555) << 1)
    x = ((x >> 2) & 0x33333333333333333333333333333333) | ((x & 0x33333333333333333333333333333333) << 2)
    x = ((x >> 4) & 0x0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F) | ((x & 0x0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F) << 4)
    x = ((x >> 8) & 0x00FF00FF00FF00FF00FF00FF00FF00FF) | ((x & 0x00FF00FF00FF00FF00FF00FF00FF00FF) << 8)
    x = ((x >> 16) & 0x0000FFFF0000FFFF0000FFFF0000FFFF) | ((x & 0x0000FFFF0000FFFF0000FFFF0000FFFF) << 16)
    x = ((x >> 32) & 0x00000000FFFFFFFF00000000FFFFFFFF) | ((x & 0x00000000FFFFFFFF00000000FFFFFFFF) << 32)
    x = (x >> 64)  | (x << 64)
    x
end

bitreverse(x::Int8)  = reinterpret(Int8, bitreverse(reinterpret(UInt8,x)))
bitreverse(x::Int16) = reinterpret(Int16, bitreverse(reinterpret(UInt16,x)))
bitreverse(x::Int32) = reinterpret(Int32, bitreverse(reinterpret(UInt32,x)))
bitreverse(x::Int64) = reinterpret(Int64, bitreverse(reinterpret(UInt64,x)))
bitreverse(x::Int128) = reinterpret(Int128, bitreverse(reinterpret(UInt128,x)))

