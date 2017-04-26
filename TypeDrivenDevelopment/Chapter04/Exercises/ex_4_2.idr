import Data.Vect

-- Exercise 2
data PowerSource = Petrol | Pedal | Electric

-- Exercise 1
data Vehicle : PowerSource -> Type where
    Unicycle : Vehicle Pedal
    Bicycle : Vehicle Pedal
    Motorcycle : (fuel : Nat) -> Vehicle Petrol
    Car : (fuel : Nat) -> Vehicle Petrol
    Bus : (fuel : Nat) -> Vehicle Petrol

wheels : Vehicle power -> Nat
wheels Unicycle = 1
wheels Bicycle = 2
wheels (Motorcycle _) = 2
wheels (Car _) = 4
wheels (Bus _) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle _) = Motorcycle 100
refuel (Car _) = Car 100
refuel (Bus _) = Bus 100
refuel Unicycle impossible
refuel Bicycle impossible

-- Exercise 3 and Exercise 4
vectTake : (n: Nat) -> Vect (n + m) a -> Vect n a
vectTake Z xs = []
vectTake (S k) (x :: xs) = x :: vectTake k xs

-- Exercise 5
sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys
  = case integerToFin pos n of
      Nothing => Nothing
      (Just idx) => Just $ (Vect.index idx xs) + (Vect.index idx ys)
