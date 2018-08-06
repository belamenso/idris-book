StringOrInt : (isInt : Bool) -> Type
StringOrInt False = String
StringOrInt True = Int

valToString : (isInt : Bool) ->
              (case isInt of False => String
                             True => Int) ->
               String
valToString False s = trim s
valToString True i = cast i

