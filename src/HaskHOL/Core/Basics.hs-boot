module HaskHOL.Core.Basics where
 
import HaskHOL.Core.Kernel

unsafeGenVar :: HOLType -> HOLTerm

stripComb :: HOLTerm -> (HOLTerm, [HOLTerm])