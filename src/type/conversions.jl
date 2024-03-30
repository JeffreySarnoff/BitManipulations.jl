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

as(::Type{Unsigned}, @nospecialize(x::Signed)) = unsigned(x)
as(::Type{Signed}, @nospecialize(x::Unsigned)) = signed(x)
