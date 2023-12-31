import qualified LoadSpec (spec)
import qualified MachineSpec (spec)
import qualified OperationSpec (spec)
import qualified RecipeSpec (spec)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import qualified ToolSpec (spec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    ToolSpec.spec
    RecipeSpec.spec
    MachineSpec.spec
    OperationSpec.spec
    LoadSpec.spec
