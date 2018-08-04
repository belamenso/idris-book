import Data.Vect

data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
     Bicycle : Vehicle Pedal
     Unicycle : (fuel : Nat) -> Vehicle Pedal
     Car : (fuel : Nat) -> Vehicle Petrol
     Bus : (fuel : Nat) -> Vehicle Petrol
     Motorcycle : (fuel : Nat) -> Vehicle Petrol
     Tram : Vehicle Electric

total wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels Tram = 16
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels (Unicycle fuel) = 1
wheels (Motorcycle fuel) = 2

total refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel (Motorcycle fuel) = Motorcycle 50

vectTake : (n : Nat) -> Vect (n + m) a -> Vect n a
vectTake Z xs = []
vectTake (S k) (x :: xs) = x :: vectTake k xs

sumEntries : Num a => (pos : Integer) ->
                      Vect n a -> 
                      Vect n a ->
                      Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                                Nothing => Nothing
                                Just n => Just $ index n xs + index n ys

