"""
    as(AbstractType, x)

Provide `x` recast the size-matched concrete type <: AbstractType

- as(Unsigned,    0x05) == 0x05
- as(Unsigned, Int8(5)) == 0x05

- as(Signed,    5) = 5
- as(Signed, 0x05) = Int8(5)

- as(AbstractFloat, 5.0) = 5.0
- as(AbstractFloat, UInt16(5)) = Float16(5.0)
- as(AbstractFloat, Int16(-5)) = Float16(-5.0)
"""
Base.@nospecializeinfer as(::Type{T}, @nospecialize(x::T)) where {T} = x

Base.@nospecializeinfer as(::Type{Integer},  @nospecialize(x::Integer))  = x
Base.@nospecializeinfer as(::Type{Unsigned}, @nospecialize(x::Unsigned)) = x
Base.@nospecializeinfer as(::Type{Signed},   @nospecialize(x::Signed))   = x
Base.@nospecializeinfer as(::Type{AbstractFloat}, @nospecialize(x::AbstractFloat)) = x
Base.@nospecializeinfer as(::Type{Float}, @nospecialize(x::AbstractFloat)) = x

as(::Type{Signed}, @nospecialize(x::Unsigned)) = signed(x)
as(::Type{Unsigned}, @nospecialize(x::Signed)) = unsigned(x)

for (F, U, I) in ((Float16, UInt16, Int16), (Float32, UInt32, Int32),
                  (Float64, UInt64, Int64))
    @eval begin
        as(Type{Unsigned}, x::$F) = reinterpret($U, x)
        as(Type{Signed}, x::$F) = reinterpret($I, x)
        as(Type{AbstractFloat}, x::$U) = reinterpret($F, x)
        as(Type{AbstractFloat}, x::$I) = reinterpret($F, x)
        as(Type{Float}, x::$U) = reinterpret($F, x)
        as(Type{Float}, x::$I) = reinterpret($F, x)
    end
end

