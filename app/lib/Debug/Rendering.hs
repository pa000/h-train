module Debug.Rendering (renderDebugInformation) where

import Apecs
import Types

renderDebugInformation :: System World ()
renderDebugInformation = do
  renderNodeTypes
