mask_lsbs(n::T) where {T<:BitInteger} = ((one(T) << n) - one(T))

mask_lsbs(::Type{T1}, n::T2) where {T1<:BitInteger, T2<:Integer} =
    (((one(T1)) << n) - one(T1)) % T1

mask_msbs(n::T) where {T<:BitInteger} = mask_lsbs(n) << (bitsof(T)-n)

mask_msbs(::Type{T1}, n::T2) where {T1<:BitInteger, T2<:Integer} = 
    mask_lsbs(n) << (bitsof(T)-n)

# mask after the sign bit, isolate exponent bitfield
mask_m1sbs(n::T) where {T<:BitInteger} = mask_lsbs(n) << (bitsof(T)-(n+1))

# tolsbs_m1sbs(x::T, n:T) where {T<:BitInteger} = (x & (mask_lsbs(n) << (bitsof(T)-(n+1))) ) >> (Int16(7)-n)

maskbits(::Type{T1}, n::T2, offset::T2) where {T1<:BitInteger, T2<:Integer} =
    mask_lbs(T1, n) << offset

filterlo(n::T) where {T<:Integer} = maskhi(bitsof(T) - n)
filterlo(::Type{T1}, n::T2) where {T1<:Integer, T2<:Integer} =
    maskhi(8sizeof(T1) - n)

filterhi(n::T) where {T<:Integer} = masklo(bitsof(T) - n)
filterhi(::Type{T1}, n::T2) where {T1<:Integer, T2<:Integer} =
    masklo(8sizeof(T1) - n)

clear_ms1b(x::T) where {T<:BitInteger} = x & ~(leading_one(x))
clear_ls1b(x::T) where {T<:BitInteger} = x & ~(trailing_one(x))

leading_one(x::T) where {T<:BitInteger} = 
    one(T) << ((bitsof(T) - one(Int16)) - Base.ctlz_int(x))

trailing_one(x::T) where {T<:BitInteger} = 
    one(T) << ((bitsof(T) - one(Int16)) -  Base.cttz_int(x))

#=
    use Base: 
    bitreverse, bitrotate,
    leading_zeros, leading_ones,
    trailing_zeros, trailing_ones, 
    count_ones, count_zeros,
    bswap
=#
#=

julia> bswap(0x12)
0x12
julia> bswap(0x0102)
0x0201
julia> bswap(0x01020304)
0x04030201
julia> bswap(0x0102030405060708)
0x0807060504030201

=#
