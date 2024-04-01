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

