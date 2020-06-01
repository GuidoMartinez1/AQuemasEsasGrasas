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
         quemarCalorias andres 300 `shouldBe` (Gimnasta "Andy" 22.0 79.829544 6.0) --SI NO LE AGREGO TODOS ESOS DECIMALES ROMPE
     
