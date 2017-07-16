wibble :: Deformation V2 V2 Double
wibble = Deformation $ \p ->
  ((p^._x) + 0.3 * cos ((p ^. _y) * tau)) ^& (p ^. _y)
  -- perturb x-coordinates by the cosine of the y-coordinate

