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

clear_ms1b(x::T) where {T<:BitInteger} = x & ~(leading_one(x))
clear_ls1b(x::T) where {T<:BitInteger} = x & ~(trailing_one(x))

leading_one(x::T) where {T<:BitInteger} = 
    one(T) << ((bitsof(T) - one(Int16)) - Base.ctlz_int(x))

trailing_one(x::T) where {T<:BitInteger} = 
    one(T) << ((bitsof(T) - one(Int16)) -  Base.cttz_int(x))

# use Base.bitreverse
