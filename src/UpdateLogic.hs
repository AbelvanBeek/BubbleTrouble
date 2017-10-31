module UpdateLogic where
    
import Model

class Update a where 
    update :: a -> a
  
instance Update GameObjects where
    update (Player         (P1 (PlayerInfo                 objectinfo  d o n)))
          = Player         (P1 (PlayerInfo (updatePosition objectinfo) d o n))
    update (Player         (P2 (PlayerInfo                 objectinfo  d o n)))
          = Player         (P2 (PlayerInfo (updatePosition objectinfo) d o n))
    update (PlayerObjects  (Arrow                          objectinfo))
          = PlayerObjects  (Arrow          (updatePosition objectinfo))
    update (EnemyObjects   (Ball objectinfo)) 
          = EnemyObjects   (Ball           (updatePosition objectinfo))
    update o@(LevelObjects (Wall                           objectinfo))
          = LevelObjects   (Wall           (updatePosition objectinfo))

updateLevel :: Level -> Level
updateLevel (Level p1 p1o p2 p2o enemies lvl) 
            = Level (update p1) (map update p1o) (update p2) (map update p2o) (map update enemies) (map update lvl)

updatePosition :: ObjectInfo -> ObjectInfo
updatePosition (ObjectInfo clr (Vec vx vy) (Pt px py)           size)
            =  (ObjectInfo clr (Vec vx vy)   (Pt (px+vx) (py+vy)) size)
