transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose xs     = map head xs : transpose (map tail xs)
