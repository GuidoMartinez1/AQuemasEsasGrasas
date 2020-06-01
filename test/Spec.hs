import Test.Hspec
import Lib

main :: IO()
main = hspec $ do
   describe "Ejercicio Gimnasta" $ do
      it "pancho luego de hacer relax" $ do
         relax 10 pancho `shouldBe` pancho
      it "pancho es saludable?" $ do
         saludable pancho `shouldBe` False
      it "andres es saludable?" $ do
         saludable andres `shouldBe` True
      it "quemarCalorias a pancho 300" $ do
         quemarCalorias pancho 300 `shouldBe` (Gimnasta "Francisco" 40.0 118.0 1.0)
      it "quemarCalorias a andres 300" $ do
         quemarCalorias andres 300 `shouldBe` (Gimnasta "Andy" 22.0 79.829544 6.0) --SI NO PONGO TODOS ESTOS DECIMALES ROMPE
      it "el ejercicio es caminataEnCinta " $ do
         caminataEnCinta 40 pancho `shouldBe` (Gimnasta "Francisco" 40.0 118.666664 1.0)
      it "el ejercicio es entrenamientoEnCinta " $ do
         entrenamientoEnCinta 40 pancho `shouldBe` (Gimnasta "Francisco" 40.0 117.333336 1.0)
      it "pesas a pancho por 50 kilos en 15 minutos" $ do
         pesas 50 15 pancho `shouldBe` (Gimnasta "Francisco" 40.0 120.0 6.0)
      it "el ejercicio es colina con una inclinacion de 5 y por 40 minutos a Pancho " $ do
         colina 5 40 pancho `shouldBe` (Gimnasta "Francisco" 40.0 117.333336 1.0)
      it "el ejercicio es montania con una inclinacion de 5 y por 40 minutos a Pancho " $ do
         montania 5 40 pancho `shouldBe` (Gimnasta "Francisco" 40.0 116.53333 2.0)

