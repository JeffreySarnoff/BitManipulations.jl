@testset "size ang bitwise consistent type casts"

@test as(Unsigned, UInt16(1)) == UInt16(1)
@test as(Signed,    Int16(1)) == Int16(1)
@test as(AbstractFloat, Float16(1)) == Float16(1)

@test as(Unsigned, Int16(1)) == UInt16(1)
@test as(Signed, UInt16(1))  == Int16(1)

@test as(AbstractFloat, UInt16(1)) == reinterpret(Float16, UInt16(1))
@test as(AbstractFloat, Int16(1))  == reinterpret(Float16, Int16(1))
@test as(AbstractFloat, Int16(-1)) == reinterpret(Float16, Int16(-1))

@test as(Unsigned, Float16(1))  == reinterpret(UInt16, Float16(1))
@test as(Unsigned, Float16(-1)) == reinterpret(UInt16, Float16(-1))
@test as(Signed, Float16(1))    == reinterpret(Int16, Float16(1))
@test as(Signed, Float16(-1))   == reinterpret(Int16, Float16(-1))
