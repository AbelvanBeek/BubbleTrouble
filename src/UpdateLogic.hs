module UpdateLogic where
    
import Model

class Update a where 
    update :: a -> a
  
instance Update GameObjects where
    update (Player        (P1 (PlayerInfo (ObjectInfo clr (Vec vx vy) (Pt px py)           size) d o n)))
          = Player        (P1 (PlayerInfo (ObjectInfo clr (Vec 0 0)   (Pt (px+vx) (py+vy)) size) d o n))
    update o@(Player        (P2 (PlayerInfo objectinfo _ _ _))) = o
    update o@(PlayerObjects (Arrow          objectinfo))        = o
    update (EnemyObjects  (Ball           (ObjectInfo clr (Vec vx vy) (Pt px py)           size))) 
          = EnemyObjects  (Ball           (ObjectInfo clr (Vec 0 0)   (Pt (px+vx) (py+vy)) size))
    update o@(LevelObjects  (Wall           objectinfo))        = o

updateLevel :: Level -> Level
updateLevel (Level p1 p1o p2 p2o enemies lvl) 
            = Level (update p1) (map update p1o) (update p2) (map update p2o) (map update enemies) (map update lvl)
  
