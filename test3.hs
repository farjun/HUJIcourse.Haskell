data Person = Person{firstName::String,lastName::String,age::Int} deriving (Show)
data PersonFetures = PersonName String | PersonAge Int deriving (Show)

class ClassPerson a where 
  getAge::Person->a 
  getLastNme::Person->a 


instance ClassPerson PersonFetures where
  getAge (Person _ _ age) = PersonAge age
  getLastNme (Person _ lastName _) = PersonName lastName
