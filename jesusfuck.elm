let
        fullRows = 
          List.map fst 
            <| List.filter (\(_, s) -> s == 10) -- filter out all rows without 10 blocks
            <| List.reverse 
            <| Dict.toList
            <| model.rows
            
                   
        removeFullRows fullRow acc =
          Dict.foldr (\(r, c) v a ->
            if List.member r fullRows then a -- r == fullRow then a
            else
              { a
              | pieces = Dict.insert (r,c) v a.pieces
              , rows = Dict.update r maybeAddOne a.rows
              }
          ) acc model.pieces
        
        withoutRows = List.foldl removeFullRows { model | pieces = Dict.empty, rows = Dict.empty } fullRows
        
        foldModelForward =
          Dict.foldr (\(r, c) v a ->
            let
              rOffset = List.length <| List.filter (\fr -> r < fr) fullRows
            in
            
            if rOffset > 0 then
              { a
              | pieces = Dict.insert (r+rOffset, c) (Just ((r+rOffset, c), 1)) a.pieces
              , rows = Dict.update (r+rOffset) maybeAddOne a.rows
              }
              
            else
              { a
              | pieces = Dict.insert (r,c) v a.pieces
              , rows = Dict.update r maybeAddOne a.rows
              }
          ) { withoutRows | pieces = Dict.empty, rows = Dict.empty } withoutRows.pieces
          
          
        
        
            -- else if r < fullRow then 
            --   { a
            --   | pieces = Dict.insert (r+1, c) (Just ((r+1,c), 1)) a.pieces -- move columns forward a row in BOTH `rows` and `pieces`
            --   , rows = Dict.update (r+1) maybeAddOne a.rows  
            --   }
                   
      in
      if List.length fullRows > 0 then
        foldModelForward
        => Cmd.none
      else
        model
        => Cmd.none