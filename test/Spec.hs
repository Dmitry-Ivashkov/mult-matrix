import Test.Hspec
import Lib

m0 = zeros 2
m1 = ones 2
m1_ = concatMatrix a a a a where a = ones 1
m2 = createSimpleMatrix 2 2
m4 = createSimpleMatrix 4 2
m8_ = concatMatrix a a a a where a = createSimpleMatrix 8 1

m1234 = concatMatrix (f 1) (f 2) (f 3) (f 4) where f = \x -> createSimpleMatrix x 1
m2468 = concatMatrix (f 2) (f 4) (f 6) (f 8) where f = \x -> createSimpleMatrix x 1

testAdd :: Spec
testAdd = describe "testing add:" $ do
    it "zero add zero" $
      m0 +++ m0 `shouldBe` m0
    it "m8 add zero" $
      m8_ +++ m0 `shouldBe` m8_
    it "m1 add m1_" $
      m1 +++ m1_ `shouldBe` m2
    it "m1 add m1 add m1 add m1" $
      m1 +++ m1 +++ m1 +++ m1 `shouldBe` m4
    it "m1 add m2 == m2 add m1" $
      m1 +++ m2 `shouldBe` m2 +++ m1
    it "m4 add m4" $
      m4 +++ m4 `shouldBe` m8_
    it "m1234 add m1234" $
      m1234 +++ m1234 `shouldBe` m2468

testMultiply :: Spec
testMultiply = describe "testing multiply:" $ do
    it "zero multiply zero" $
      m0 *** m0 `shouldBe` m0
    it "m8 multiply zero" $
      m8_ *** m0 `shouldBe` m0
    it "m1 multiply m1_" $
      m1 *** m1_ `shouldBe` m1
    it "m1 multiply m1 multiply m1 multiply m1" $
      m1 *** m1 *** m1 *** m1 `shouldBe` m8_
    it "m1 multiply m2 == m2 multiply m1" $
      m1 *** m2 `shouldBe` m2 *** m1
    it "m4 multiply m4" $
      m2 *** m2 `shouldBe` m8_
    it "m1234 multiply m1234" $
      m1234 *** m1234 `shouldBe` concatMatrix (f 9) (f 8) (f 17) (f 16) where f = \x -> createSimpleMatrix x 1


main :: IO ()
main = hspec $ do
  testAdd
  testMultiply